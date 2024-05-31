library(MASS)
library(rjags)
library(MCMCvis)

set.seed(123)

data(Boston)

# Standardize the predictors
X_scaled <- scale(Boston[, -which(names(Boston) == "medv")])

model_string <- "
model {
  for (i in 1:n) {
    mu[i] <- alpha + inprod(beta * gamma, X[i,])  # Properly use inprod to sum products
    medv[i] ~ dnorm(mu[i], tau)
  }
  alpha ~ dnorm(0, 0.01)
  tau ~ dgamma(1, 1)
  for (j in 1:p) {
    beta[j] ~ dnorm(0, tau_beta)
    gamma[j] ~ dbern(0.5)
  }
  tau_beta <- pow(sigma_beta, -2)
  sigma_beta ~ dunif(0, 100)
}
"

# parameters
chains <- 3
names <- setdiff(names(Boston), "medv")


# Prepare data for JAGS
data_jags <- list(medv = Boston$medv, X = X_scaled, n = nrow(Boston), p = ncol(Boston)-1)

# Initial Values
inits <- function() {
  list(alpha = 0, beta = rep(0, 13), gamma = rep(0, 13), tau = 1, sigma_beta = 1)
}

# Run Model
model <- jags.model(textConnection(model_string), data = data_jags, inits = inits, n.chains = 3, n.adapt = 1000)
update(model, 1000)
samples <- coda.samples(model, variable.names = c("beta"), n.iter = 5000)

# Output
summary(samples)

MCMCsummary(samples,
            params = c("beta"),
            Rhat = TRUE,
            n.eff = TRUE,
            probs = c(0.025, 0.5, 0.975), round = 2)


# Analysis

# Extract beta coefficients from samples and combine chains
beta_cols <- grep("beta", varnames(samples[[1]]), value = TRUE)  # find all beta column names
beta_combined <- do.call(rbind, lapply(samples, function(x) x[, beta_cols]))

# Convert to 'mcmc' object
beta_mcmc <- as.mcmc(beta_combined)

# Set column names for beta based on predictor names
colnames(beta_mcmc) <- names

par(mfrow=c(5, 3))
# Histograms for beta coefficients with predictor names
for(j in 1:ncol(beta_mcmc)) {
  hist(beta_mcmc[,j], xlab=names[j], ylab="Density", main=paste("Histogram of", names[j]), breaks=30, freq=FALSE)
}
par(mfrow=c(1, 1))

# Define a threshold for considering coefficients as non-zero
threshold <- 0.01

# Calculate marginal inclusion probabilities
Inc_Prob <- colMeans(abs(beta_mcmc) > threshold)

# Calculate 95% credible intervals for each beta coefficient
Q <- t(apply(beta_mcmc, 2, quantile, probs = c(0.025, 0.5, 0.975)))

# Combine inclusion probabilities and credible intervals
out <- cbind(Inc_Prob, Q)
print(round(out, 2))

# Generate model formulas based on non-zero coefficients, incorporating predictor names
models <- apply(beta_mcmc, 1, function(row) {
  included_terms <- names[which(abs(row) > threshold)]
  if (length(included_terms) > 0) {
    paste("medv ~", paste(included_terms, collapse=" + "))
  } else {
    "medv ~ 1"  # Intercept-only model
  }
})

# Calculate and display model probabilities
model_probs <- table(models)
model_probs <- sort(model_probs, decreasing = TRUE)
print(round(model_probs / sum(model_probs), 3)[1:3])
