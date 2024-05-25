setwd("/Users/felixzhao/Documents/workspace/STAT452/Assignments/3")

library(rjags)

# q3b

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
plot(samples)
gelman.diag(samples)
gelman.plot(samples)

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


# q3c

# Assuming 'samples' contains the MCMC output as previously generated
beta1_samples <- as.matrix(samples[, "beta1"])

# Calculate the odds ratio for each sampled value of beta1
odds_ratio_samples <- exp(beta1_samples)

# Compute median and 95% credible interval
or_median <- median(odds_ratio_samples)
or_ci <- quantile(odds_ratio_samples, probs = c(0.025, 0.975))

# Print the results
cat("The median of the odds ratio is:", or_median, "\n")
cat("The 95% credible interval for the odds ratio is:", or_ci, "\n")

