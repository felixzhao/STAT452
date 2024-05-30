# week 9

# Install necessary packages if not already installed
install.packages(c("titanic", "rjags", "coda"))

# Load the libraries
library(titanic)
library(rjags)
library(coda)

# Load the Titanic dataset
data("titanic_train", package = "titanic")
dat <- titanic_train

# Prepare the data
Y <- dat[, 2]  # Survival
age <- dat[, 6]  # Age
gender <- dat[, 5]  # Gender
class <- dat[, 3]  # Class

# Create the design matrix X
X <- cbind(1, scale(age), ifelse(gender == "male", 1, 0), ifelse(class == 2, 1, 0), ifelse(class == 3, 1, 0))
colnames(X) <- c("Intercept", "Age", "Gender", "Class2", "Class3")

# Remove missing values
miss <- is.na(rowSums(X))
X <- X[!miss, ]
Y <- Y[!miss]

# Define the JAGS model
model_string <- "
model {
  for (i in 1:N) {
    Y[i] ~ dbern(p[i])
    logit(p[i]) <- beta[1] + beta[2] * Age[i] + beta[3] * Gender[i] + beta[4] * Class2[i] + beta[5] * Class3[i]
  }
  for (j in 1:5) {
    beta[j] ~ dnorm(0, 0.001)
  }
}
"

# Data list for JAGS
data_jags <- list(
  Y = Y,
  Age = X[, 2],
  Gender = X[, 3],
  Class2 = X[, 4],
  Class3 = X[, 5],
  N = nrow(X)
)

# Initial values
inits <- function() {
  list(beta = rnorm(5))
}

# Parameters to monitor
params <- c("beta")

# Run the JAGS model
jags_model <- jags.model(textConnection(model_string), data = data_jags, inits = inits, n.chains = 3, n.adapt = 500)
update(jags_model, 1000)  # Burn-in

# Sample from the posterior
samples <- coda.samples(jags_model, variable.names = params, n.iter = 5000)

# Summary of the posterior samples
summary(samples)

par(mfrow = c(3, 2))
# Trace plots
traceplot(samples)

par(mfrow = c(3, 2))
# Density plots
densplot(samples)

# Check convergence using Gelman-Rubin diagnostic
gelman.diag(samples)

# Extract credible intervals
credible_intervals <- summary(samples)$quantiles[, c("2.5%", "97.5%")]
print(credible_intervals)
