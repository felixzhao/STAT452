setwd("/Users/felixzhao/Documents/workspace/STAT452/Assignments/3")

library(rjags)

# Assume the dataset is loaded into a dataframe `data`
data <- read.csv("gambia.csv")

# Define the model
model_string <- "
  model {
    for (i in 1:N) {
      y[i] ~ dbern(p[i])
      logit(p[i]) <- beta0 + beta1 * x[i]
    }
    beta0 ~ dnorm(0, 0.01)  # Prior for beta0
    beta1 ~ dnorm(0, 0.01)  # Prior for beta1
  }
"

# Data list for JAGS
data_jags <- list(y = data$pos, x = data$netuse, N = nrow(data))

# Parameters to monitor
params <- c("beta0", "beta1")

# Initialize the model
model <- jags.model(textConnection(model_string), data = data_jags, n.chains = 3)

# Burn-in
update(model, 1000)

# MCMC sampling
samples <- coda.samples(model, variable.names = params, n.iter = 5000)

# Summary of posterior distribution
summary(samples)

# Diagnostics
#plot(samples)
#gelman.diag(samples)
#gelman.plot(samples)

########MCMC DIAGNOSTICS########################

theta.samp <- samples

## Plots: traceplot and histogram/density plot
par(mar=c(3,2,1,2))
plot(theta.samp)

## Calculate effective sample size
##ESS summed across all chains
effectiveSize(theta.samp)

##ESS for each chain
lapply(theta.samp, effectiveSize)

##PSRF - Gelman and Rubin diagnostic
gelman.diag(theta.samp)

## Plot of PSRF
gelman.plot(theta.samp)

## Geweke time series diagnostic
geweke.diag(theta.samp)

## Plot of Geweke diagnostic
geweke.plot(theta.samp)

## View summary of generated samples
summary(theta.samp)

