# week 8

# Define the data
Y <- c(64, 13, 33, 18, 30, 20)
t <- 1:length(Y)

# Initial values for alpha and beta
alpha <- rnorm(1, 0, 10)
beta <- rnorm(1, 0, 10)

# Parameters for the proposal distributions
sigma_alpha <- 0.2
sigma_beta <- 0.05

# Number of iterations
iterations <- 10000

# Storage for the samples
samples_alpha <- numeric(iterations)
samples_beta <- numeric(iterations)
accept_alpha <- numeric(iterations)
accept_beta <- numeric(iterations)

# Log-posterior function
log_posterior <- function(alpha, beta, Y, t) {
  lambda <- exp(alpha + beta * t)
  sum(dpois(Y, lambda, log = TRUE)) + dnorm(alpha, 0, 10, log = TRUE) + dnorm(beta, 0, 10, log = TRUE)
}

# Metropolis-Hastings sampling
for (i in 2:iterations) {
  # Propose new alpha
  alpha_star <- rnorm(1, alpha, sigma_alpha)
  log_acceptance_ratio_alpha <- log_posterior(alpha_star, beta, Y, t) - log_posterior(alpha, beta, Y, t)
  acceptance_alpha <- exp(log_acceptance_ratio_alpha)
  
  if (runif(1) < acceptance_alpha) {
    alpha <- alpha_star
    accept_alpha[i] <- 1
  }
  
  # Propose new beta using a random walk
  beta_star <- rnorm(1, beta, sigma_beta) # Random-walk proposal for beta
  log_acceptance_ratio_beta <- log_posterior(alpha, beta_star, Y, t) - log_posterior(alpha, beta, Y, t)
  acceptance_beta <- exp(log_acceptance_ratio_beta)
  
  if (runif(1) < acceptance_beta) {
    beta <- beta_star
    accept_beta[i] <- 1
  }
  
  # Store the samples
  samples_alpha[i] <- alpha
  samples_beta[i] <- beta
}

# Trace plots
par(mfrow = c(2, 1))
plot(samples_alpha, type = 'l', main = 'Trace plot for alpha')
plot(samples_beta, type = 'l', main = 'Trace plot for beta')

# Acceptance rates
acceptance_rate_alpha <- mean(accept_alpha)
acceptance_rate_beta <- mean(accept_beta)

cat('Acceptance rate for alpha:', acceptance_rate_alpha, '\n')
cat('Acceptance rate for beta:', acceptance_rate_beta, '\n')
