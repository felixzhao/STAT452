library(MASS)
library(rjags)
library(MCMCvis)

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



# Prepare data for JAGS
data_jags <- list(medv = Boston$medv, X = X_scaled, n = nrow(Boston), p = ncol(Boston)-1)

# Model string remains the same (as provided in the earlier code snippet)

# Initial Values
inits <- function() {
  list(alpha = 0, beta = rep(0, 13), gamma = rep(0, 13), tau = 1, sigma_beta = 1)
}

# Run Model
model <- jags.model(textConnection(model_string), data = data_jags, inits = inits, n.chains = 3, n.adapt = 1000)
update(model, 1000)
samples <- coda.samples(model, variable.names = c("beta", "gamma"), n.iter = 5000)

# Output
summary(samples)

MCMCsummary(samples,
            params = c("beta"),
            Rhat = TRUE,
            n.eff = TRUE,
            probs = c(0.025, 0.5, 0.975), round = 2)


# Combine chains into a single matrix
beta <- NULL
for(l in 1:chains){
  beta <- rbind(beta,samples[[l]])
}
colnames(beta) <- names
# Draw histograms that summarise marginal posteriors
for(j in 1:p){
  hist(beta[,j],xlab=expression(beta[j]),ylab="Posterior density",
       breaks=100,main=names[j])
}


# Calculate marginal inclusion probabilities for predictors
# and 95% credible intervals for coefficients
Inc_Prob <- apply(beta!=0,2,mean)
Q <- t(apply(beta,2,quantile,c(0.5,0.025,0.975)))
out <- cbind(Inc_Prob,Q)
kable(round(out,2))

##Identify model at each iteration
model <- "Intercept"
for(j in 1:p){
  model <- paste(model,ifelse(beta[,j]==0,
                              ""
                              ,"+"))
  model <- paste(model,ifelse(beta[,j]==0,
                              "",names[j]))
}
model[1:p]

beta[1:p,]


# Create table of model posterior probabilities
model_probs <- table(model)/length(model)
model_probs <- sort(model_probs,dec=T)
round(model_probs,3)[1:3]
