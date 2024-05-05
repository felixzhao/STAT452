setwd("/Users/felixzhao/Documents/workspace/STAT452/Assignments/3")

# Q1 d

library(rjags)
library(coda)

crdf <- read.csv("cancer_react.csv", header = TRUE)

# cndf <- read.csv("cancer_noreact.csv", header = TRUE)

## Step 1: Specify model
cat("
    model {
      for (i in 1:N){
        yvec[i] ~ dpois(theta*xvec[i])
      }
      theta ~ dgamma(220, 100)}",
file="q1dmodel.txt")

##Step 2: Specify data
## Create list object containing data
crdf.dat<-list(yvec=c(crdf$y),xvec=c(crdf$x),
              N=length(crdf$x))
## Step 3: Specify starting/initial values
crdf.init<-list(list(theta=0.66), list(theta=0.68), list(theta=0.70))
## Step 4: Compile and adapt model in JAGS
jagsModel<-jags.model(file="q1dmodel.txt",
                      data=crdf.dat,
                      n.chains=3,
                      inits=crdf.init,
                      n.adapt=2000
)

jagsModel

##Burn-in of 3000 samples
update(jagsModel, n.iter=3000)
## Step 5: Run sampler using coda
hgt.par<-c("theta")
set.seed(5)
hgt.samp<-coda.samples(jagsModel,
                       var=hgt.par,
                       n.iter=100000,
                       thin=100)


# result
hgt.mat<-as.matrix(hgt.samp)
head(hgt.mat)

########MCMC DIAGNOSTICS########################
## Plots: traceplot and histogram/density plot
par(mar=c(3,2,1,2))
plot(hgt.samp)

## Calculate effective sample size
##ESS summed across all chains
effectiveSize(hgt.samp)

##ESS for each chain
lapply(hgt.samp, effectiveSize)

##PSRF - Gelman and Rubin diagnostic
gelman.diag(hgt.samp)

## Plot of PSRF
gelman.plot(hgt.samp)

## Geweke time series diagnostic
geweke.diag(hgt.samp)

## Plot of Geweke diagnostic
geweke.plot(hgt.samp)

## View summary of generated samples
summary(hgt.samp)
