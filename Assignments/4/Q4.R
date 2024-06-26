# week 10

install.packages("knitr")
install.packages("MCMCvis")

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

# Number of chains
chains <- 3

# Define the tau_scale
tau_scale <- 1  # Adjust this value based on your knowledge or sensitivity analysis

# Prepare data for JAGS
data_jags <- list(Y = Y, X = X, p = p, tau_scale = tau_scale)

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
update(model, 1000)  # Burn-in phase
samples <- coda.samples(model, variable.names = c("alpha", "beta", "gamma"), n.iter = 200000, thin = 100)

# Output results
print(summary(samples))


MCMCsummary(samples,
            params = c("beta"),
            Rhat = TRUE,
            n.eff = TRUE,
            probs = c(0.025, 0.5, 0.975), round = 2)

# Plot posterior distributions of beta coefficients
# png("output.png", width=800, height=600)
# plot(samples)
# dev.off()

# Assuming 'samples' is your mcmc.list object from coda.samples
params <- c("beta[1]", "beta[2]", "beta[3]", "beta[4]", "beta[5]", 
            "beta[6]", "beta[7]", "beta[8]", "beta[9]", "beta[10]", 
            "beta[11]", "beta[12]", "beta[13]")  # List all beta parameters

plots <- list()
for (param in params) {
  # Extract samples for each parameter and make a plot
  plot_obj <- plot(samples, varname = param, auto.layout = FALSE)
  plots[[param]] <- plot_obj
}

# Combine all plots into one
grid.arrange(grobs = plots, ncol = 4)  # Adjust the number of columns based on your preference

png("all_beta_parameters_output.png", width=1600, height=2400)  # Adjust size as needed
grid.arrange(grobs = plots, ncol = 4)  # Adjust ncol to fit your layout needs
dev.off()


# Combine all plots into one
# grid.arrange(grobs = plots, ncol = 4)  # Adjust the number of columns based on your preference


# Combine chains into a single matrix
beta <- NULL
for(l in 1:chains){
  beta <- rbind(beta,samples[[l]])
}
colnames(beta)[-1] <- names
# Draw histograms that summarise marginal posteriors
par(mfrow=c(2, 3))  # Adjusts layout to have 2 rows and 3 columns
for(j in 1:p){
  hist(beta[,j],xlab=expression(beta[j]),ylab="Posterior density",
       breaks=100,main=names[j])
}
par(mfrow=c(1, 1))  # Reset to default layout
# while(dev.cur() > 1) dev.off()  # This closes all graphics devices

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
round(model_probs,3)
