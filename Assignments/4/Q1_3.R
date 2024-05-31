# Define the parameters and data
y <- c(64, 13, 33, 18, 30, 20)
t <- 1:6  # Years from 2010 to 2015, relative to 2009 + t

# Priors
alpha_mean <- 0
alpha_sd <- 10
beta_mean <- 0
beta_sd <- 10

# Initial values
alpha_current <- 0
beta_current <- 0
n_iter <- 10000

# Store the chains
alpha_chain <- numeric(n_iter)
beta_chain <- numeric(n_iter)
alpha_chain[1] <- alpha_current
beta_chain[1] <- beta_current

# Proposal standard deviations
sd_alpha <- 0.2
sd_beta <- 0.05

# Acceptance counters
accepted_alpha <- 0
accepted_beta <- 0

# Log posterior function
log_posterior <- function(alpha, beta, y, t) {
  lambda <- exp(alpha + beta * t)
  likelihood <- sum(dpois(y, lambda, log = TRUE))
  prior_alpha <- dnorm(alpha, alpha_mean, alpha_sd, log = TRUE)
  prior_beta <- dnorm(beta, beta_mean, beta_sd, log = TRUE)
  return(likelihood + prior_alpha + prior_beta)
}

# Metropolis-Hastings sampler
set.seed(123)
for (i in 2:n_iter) {
  # Propose new values
  alpha_prop <- rnorm(1, alpha_current, sd_alpha)
  beta_prop <- rnorm(1, beta_current, sd_beta)
  
  # Calculate log acceptance ratio
  log_alpha_ratio <- log_posterior(alpha_prop, beta_current, y, t) - log_posterior(alpha_current, beta_current, y, t)
  log_beta_ratio <- log_posterior(alpha_prop, beta_prop, y, t) - log_posterior(alpha_prop, beta_current, y, t)
  
  # Decide to accept/reject alpha
  if (log(runif(1)) < log_alpha_ratio) {
    alpha_current <- alpha_prop
    accepted_alpha <- accepted_alpha + 1
  }
  
  # Decide to accept/reject beta
  if (log(runif(1)) < log_beta_ratio) {
    beta_current <- beta_prop
    accepted_beta <- accepted_beta + 1
  }
  
  # Update the chains
  alpha_chain[i] <- alpha_current
  beta_chain[i] <- beta_current
}

# Calculate acceptance rates
acceptance_rate_alpha <- accepted_alpha / (n_iter - 1)
acceptance_rate_beta <- accepted_beta / (n_iter - 1)

# Print acceptance rates
cat("Acceptance rate for alpha:", acceptance_rate_alpha, "\n")
cat("Acceptance rate for beta:", acceptance_rate_beta, "\n")

# Plotting results to check convergence
par(mfrow = c(2, 1))
plot(alpha_chain, type = 'l', main = 'Trace plot for alpha')
plot(beta_chain, type = 'l', main = 'Trace plot for beta')
