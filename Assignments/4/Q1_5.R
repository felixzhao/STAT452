# Define the Metropolis-Hastings sampler for parameters alpha and beta
mh_sampler <- function(N, sd_alpha, sd_beta, y, t) {
  # Initialize storage for alpha and beta
  alpha_vec <- numeric(N)
  beta_vec <- numeric(N)
  
  # Start values set as random draws from a normal distribution
  alpha_vec[1] <- rnorm(1, mean=0, sd=1)
  beta_vec[1] <- rnorm(1, mean=0, sd=1)
  
  # Initialize acceptance counters
  accepted_alpha <- 0
  accepted_beta <- 0
  
  # Loop over the number of iterations
  for (i in 2:N) {
    # Propose new alpha and calculate its acceptance probability
    alpha_prop <- rnorm(1, alpha_vec[i-1], sd_alpha)
    lambda_prop_alpha <- exp(alpha_prop + beta_vec[i-1] * t)
    lambda_current_alpha <- exp(alpha_vec[i-1] + beta_vec[i-1] * t)
    prob_prop_alpha <- prod(dpois(y, lambda=lambda_prop_alpha)) * dnorm(alpha_prop, mean=0, sd=10)
    prob_current_alpha <- prod(dpois(y, lambda=lambda_current_alpha)) * dnorm(alpha_vec[i-1], mean=0, sd=10)
    r_alpha <- prob_prop_alpha / prob_current_alpha
    
    # Accept or reject the new alpha value
    if (runif(1) < r_alpha) {
      alpha_vec[i] <- alpha_prop
      accepted_alpha <- accepted_alpha + 1  # Increment alpha acceptance counter
    } else {
      alpha_vec[i] <- alpha_vec[i-1]
    }
    
    # Propose new beta and calculate its acceptance probability
    beta_prop <- rnorm(1, beta_vec[i-1], sd_beta)
    lambda_prop_beta <- exp(alpha_vec[i] + beta_prop * t)
    lambda_current_beta <- exp(alpha_vec[i] + beta_vec[i-1] * t)
    prob_prop_beta <- prod(dpois(y, lambda=lambda_prop_beta)) * dnorm(beta_prop, mean=0, sd=10)
    prob_current_beta <- prod(dpois(y, lambda=lambda_current_beta)) * dnorm(beta_vec[i-1], mean=0, sd=10)
    r_beta <- prob_prop_beta / prob_current_beta
    
    # Accept or reject the new beta value
    if (runif(1) < r_beta) {
      beta_vec[i] <- beta_prop
      accepted_beta <- accepted_beta + 1  # Increment beta acceptance counter
    } else {
      beta_vec[i] <- beta_vec[i-1]
    }
  }
  
  # Calculate and print acceptance rates
  acceptance_rate_alpha <- accepted_alpha / (N - 1)
  acceptance_rate_beta <- accepted_beta / (N - 1)
  cat("Acceptance rate for alpha:", acceptance_rate_alpha, "\n")
  cat("Acceptance rate for beta:", acceptance_rate_beta, "\n")
  
  list(alpha = alpha_vec, beta = beta_vec)
}

# Define parameters
N <- 10000
sd_alpha<-0.2
sd_beta<-0.05
y<-c(64, 13, 33, 18, 30, 20)
t<-1:6  # years 2010 to 2015

# Run the Metropolis-Hastings sampler
results<-mh_sampler(N, sd_alpha, sd_beta, y, t)

# Plot the results to check convergence and distribution
par(mfrow=c(2, 1))
plot(results$alpha, type = 'l', main = 'Trace plot for alpha')
plot(results$beta, type = 'l', main = 'Trace plot for beta')
