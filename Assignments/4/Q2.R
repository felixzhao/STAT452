setwd("/Users/felixzhao/Documents/workspace/STAT452/Assignments/4")

# Load necessary libraries
library(rjags)
library(coda)

# Load the data
load("Election_2008_2016_data.RData")

# Standardize and prepare data
X <- scale(X)  # Standardize covariates
X <- cbind(1, X)  # Add intercept
short <- c("Intercept", "Pop change", "65+", "African American",
           "Hispanic", "HS grad", "Bachelor's",
           "Homeownership rate", "Home value",
           "Median income", "Poverty")
colnames(X) <- short

# Question (a)

# Define the JAGS model
model_string <- "
model {
  for (i in 1:N) {
    Y[i] ~ dnorm(mu[i], tau)
    mu[i] <- inprod(beta[], X[i, ])
  }
  
  for (j in 1:P) {
    beta[j] ~ dnorm(0, 0.0001)
  }
  
  tau ~ dgamma(0.001, 0.001)
  sigma <- 1 / sqrt(tau)
}
"

# Prepare data for JAGS
data_jags <- list(Y = Y, X = X, N = nrow(X), P = ncol(X))

# Initial values
inits <- function() {
  list(beta = rnorm(ncol(X)), tau = 1)
}

# Parameters to monitor
params <- c("beta", "sigma")

# Run the JAGS model
model <- jags.model(textConnection(model_string), data = data_jags, inits = inits, n.chains = 3)
update(model, 1000)  # Burn-in

# Draw samples
samples <- coda.samples(model, variable.names = params, n.iter = 5000)

# Summarize the posterior distribution
summary(samples)

# TODO: interptation of the summary 


# Question (b)

# Extract posterior means of the coefficients
beta_hat <- summary(samples)$statistics[, "Mean"]

# Compute the predicted values
Y_hat <- X %*% head(beta_hat, -1)

# Compute the residuals
residuals <- Y - Y_hat

par(mfrow = c(1, 2))
# Check if residuals follow a normal distribution
# Plotting histogram
hist(residuals, main = "Histogram of Residuals", xlab = "Residuals")

# Q-Q plot
qqnorm(residuals)
qqline(residuals, col = "red")

par(mfrow = c(1, 1))

# Shapiro-Wilk test for normality
shapiro.test(residuals)


# Question (c)


# Prepare state variable
state <- as.character(all_dat[, 3])
AKHI <- state == "AK" | state == "HI" | state == "DC"
fips <- fips[!AKHI]
Y <- Y[!AKHI]
X <- X[!AKHI, ]
state <- state[!AKHI]

# Assign numeric id to the states
st <- unique(state)
id <- rep(NA, length(Y))
for (j in 1:48) {
  id[state == st[j]] <- j
}

# Convert state to factor
state <- factor(state)

# Define the JAGS model with random effects
model_string_re <- "
model {
  for (i in 1:N) {
    Y[i] ~ dnorm(mu[i], tau)
    mu[i] <- inprod(beta[], X[i, ]) + alpha[state[i]]
  }
  
  for (j in 1:P) {
    beta[j] ~ dnorm(0, 0.0001)
  }
  
  for (k in 1:M) {
    alpha[k] ~ dnorm(0, tau_alpha)
  }
  
  tau ~ dgamma(0.001, 0.001)
  tau_alpha ~ dgamma(0.001, 0.001)
  sigma <- 1 / sqrt(tau)
}
"

# Prepare data for JAGS
data_jags_re <- list(Y = Y, X = X, N = nrow(X), P = ncol(X), state = as.numeric(state), M = length(unique(state)))

# Initial values
inits_re <- function() {
  list(beta = rnorm(ncol(X)), tau = 1, tau_alpha = 1, alpha = rnorm(length(unique(state))))
}

# Parameters to monitor
params_re <- c("beta", "sigma", "alpha")

# Run the JAGS model
model_re <- jags.model(textConnection(model_string_re), data = data_jags_re, inits = inits_re, n.chains = 3)
update(model_re, 1000)  # Burn-in

# Draw samples
samples_re <- coda.samples(model_re, variable.names = params_re, n.iter = 5000)

# Summarize the posterior distribution of fixed effects and random effects
summary(samples_re)

# Extract and summarize random effects
random_effects_means <- summary(samples_re)$statistics[grep("alpha", rownames(summary(samples_re)$statistics)), "Mean"]

# Identify states with highest and lowest random effects
highest_effects <- sort(random_effects_means, decreasing = TRUE)[1:5]
lowest_effects <- sort(random_effects_means, decreasing = FALSE)[1:5]

cat("States with highest posterior mean random effects:\n")
print(highest_effects)

cat("States with lowest posterior mean random effects:\n")
print(lowest_effects)
