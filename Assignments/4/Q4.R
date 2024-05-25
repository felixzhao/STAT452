# Load libraries
library(MASS)
library(rjags)
library(coda)

# Data setup
data(Boston, package="MASS")
y <- as.numeric(Boston$medv)
X <- as.matrix(Boston[, -which(names(Boston) == "medv")])
data_jags <- list(X = X, y = y, N = nrow(X), p = ncol(X))

# Define the model
model_string <- "
model {
  for (i in 1:N) {
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- beta0 + inprod(beta[], X[i,])
  }
  beta0 ~ dnorm(0, 0.0001)
  tau ~ dgamma(0.01, 0.01)
  for (j in 1:p) {
    beta[j] ~ dnorm(0, tau.beta[j])
    tau.beta[j] <- pow(sigma[j], -2)
    sigma[j] ~ dgamma(1.0, 1.0)
  }
}
"

# Data for JAGS
data_jags <- list(X = X, y = y, N = nrow(X), p = ncol(X))

# Initial values
inits <- function(){
  list(beta = rnorm(ncol(X), 0, 1), beta0 = rnorm(1, 0, 1), sigma = runif(ncol(X), 1, 2))
}

# Setup model
parameters <- c("beta", "beta0")
model <- jags.model(textConnection(model_string), data = data_jags, inits = inits, n.chains = 3)
update(model, 2000) # Burn-in

# Run the model
samples <- coda.samples(model, parameters, n.iter=15000, thin=5)

# Analysis
print(summary(samples))
plot(samples)
effectiveSize(samples)
autocorr.diag(samples)
