setwd("/Users/felixzhao/Documents/workspace/STAT452/Assignments/3")

# Q1 d

library(rjags)
library(coda)

set.seed(5)


## Step 1: Specify model
cat("
    model {
      for (i in 1:N){
        yvec[i] ~ dpois(theta*xvec[i])
      }
      theta ~ dgamma(220, 100)}",
    file="q1dmodel.txt")

mcmc <- function(x, y) {
  ##Step 2: Specify data
  ## Create list object containing data
  df.dat<-list(yvec=c(y),xvec=c(x),
                 N=length(x))
  ## Step 3: Specify starting/initial values
  df.init<-list(list(theta=0.66), list(theta=0.68), list(theta=0.70))
  ## Step 4: Compile and adapt model in JAGS
  jagsModel<-jags.model(file="q1dmodel.txt",
                        data=df.dat,
                        n.chains=3,
                        inits=df.init,
                        n.adapt=2000
  )
  
  jagsModel
  
  ##Burn-in of 3000 samples
  # update(jagsModel, n.iter=3000)
  ## Step 5: Run sampler using coda
  df.par<-c("theta")

  df.samp<-coda.samples(jagsModel,
                         var=df.par,
                         n.iter=100000,
                         thin=100)
  return(df.samp)
}

#crdf
crdf <- read.csv("cancer_react.csv", header = TRUE)
crdf.samp <- mcmc(crdf$x, crdf$y)
crdf.mat<-as.matrix(crdf.samp)

# cndf
cndf <- read.csv("cancer_noreact.csv", header = TRUE)
cndf.samp <- mcmc(cndf$x, cndf$y)
cndf.mat<-as.matrix(cndf.samp)


# result
print("Pr (θ1 > θ2|y1, y2, x1, x2)")
mean(crdf.mat > cndf.mat)

print("95% quantile-based posterior intervals for θ")
print(quantile(crdf.mat, probs = c(0.025, 0.5, 0.975)))

print("95% quantile-based posterior intervals for θ")
print(quantile(cndf.mat, probs = c(0.025, 0.5, 0.975)))

########MCMC DIAGNOSTICS########################

#theta.samp <- crdf.samp
theta.samp <- cndf.samp

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
