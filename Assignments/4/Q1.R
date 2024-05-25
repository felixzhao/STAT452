# Install and load necessary packages
install.packages("rjags")
install.packages("coda")
library(rjags)
library(coda)

# Define the JAGS model
cat("
model {
  for (i in 1:N) {
    Y[i] ~ dpois(lambda[i])
    log(lambda[i]) <- alpha + beta * t[i]
  }
  
  alpha ~ dnorm(0, 0.01)
  beta ~ dnorm(0, 0.01)
}
", file = "model.txt")

# Data
Y <- c(64, 13, 33, 18, 30, 20)
t <- 1:length(Y)
data <- list(Y = Y, t = t, N = length(Y))

# Initial values
inits <- list(
  list(alpha = 0, beta = 0),
  list(alpha = 1, beta = 1),
  list(alpha = -1, beta = -1)
)

# Create the JAGS model
jags_model <- jags.model("model.txt", data = data, inits = inits, n.chains = 3, n.adapt = 1000)

# Burn-in phase
update(jags_model, 1000)

# Sample from the posterior distribution
samples <- coda.samples(jags_model, variable.names = c("alpha", "beta"), n.iter = 10000)

# Convert samples to MCMC list
mcmc_samples <- as.mcmc.list(samples)

# Plot trace plots
plot(mcmc_samples)

# Summarize the posterior distributions
summary(mcmc_samples)

# Check acceptance rates (for Metropolis-Hastings, this is done internally by JAGS)
acceptance_rates <- 1 - rejectionRate(mcmc_samples)
print(acceptance_rates)
