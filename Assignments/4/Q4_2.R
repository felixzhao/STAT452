# week 10

# install.packages("knitr")
# install.packages("MCMCvis")

# Load necessary libraries
library(MASS)  # For the dataset
library(rjags)  # For Bayesian analysis
library(knitr)
library(MCMCvis)
library(coda)
library(gridExtra)  # for arranging multiple plots

# Load the Boston dataset
data(Boston)

# Define the response and predictors
Y <- Boston$medv
names <- setdiff(names(Boston), "medv")

X <- as.matrix(Boston[, names])

# Number of predictors
p <- ncol(X)
n <- length(Y)
r <- nrow(X)

X[1:5,]
Y[1:5]

# boxplot of X
par(mfrow=c(2,2))
for(j in 1:p){
  boxplot(X[,j]~Y,main=names[j])
}

# Standardize X
X <- scale(X)
X[1:5,]

# Number of chains
chains <- 3

burn <- 5000
iters <- 200000
thin <- 5

# Define the tau_scale
tau_scale <- 1  # Adjust this value based on your knowledge or sensitivity analysis

# Prepare data for JAGS
data_jags <- list(Y = Y, X = X, n = n)

# Model specification
model_string <- "
model {
    for (i in 1:length(Y)) {
        Y[i] ~ dnorm(mu[i], tau)
        mu[i] <- alpha + inprod(X[i,], beta)
    }

    for (j in 1:p) {
        beta[j] <- gamma[j] * delta[j]
        gamma[j] ~ dbern(0.5)  # Bernoulli prior for inclusion
        delta[j] ~ dnorm(0, tau_beta)  # Normal prior for coefficient
    }

    alpha ~ dnorm(0, 0.0001)
    tau ~ dgamma(0.1, 0.1)
    tau_beta <- pow(tau_scale, -2)
}
"

# Initialize and run the JAGS model
model <- jags.model(textConnection(model_string), data = data_jags, n.chains = chains)
update(model, burn)  # Burn-in phase
samples <- coda.samples(model, variable.names = c("beta"), n.iter = iters, thin = thin)


# Generate trace and density plots for the beta coefficients
plot(samples)  # Set ask=FALSE to automatically plot all without pausing


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
