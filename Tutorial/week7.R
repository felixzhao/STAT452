setwd("~/Documents/workspace/STAT452/Tutorial")

## Read in heights data
hgt<-read.csv("heights.csv")
head(hgt)

library(rjags)
library(coda)

## Step 1: Specify model
cat("
model {
for (i in 1:N){
mu[i]<-beta0 + beta1*xvec[i]
yvec[i] ~ dnorm(mu[i],tau)
}
beta0 ~ dnorm(0, 0.0001)
beta1 ~ dnorm(1, 0.1)
tau ~ dgamma(0.0001, 0.0001)
sigma2<-1/tau
}",
file="hgtmodel.txt")
##Step 2: Specify data
## Create list object containing data
hgt.dat<-list(yvec=c(hgt$Dheight),xvec=c(hgt$Mheight),
              N=length(hgt$Dheight))
## Step 3: Specify starting/initial values
hgt.init<-list(list(beta0=-10, beta1=-10, tau=0.001),
               list(beta0=0, beta1=1, tau=1),
               list(beta0=10, beta1=10, tau=1000))
## Step 4: Compile and adapt model in JAGS
jagsModel<-jags.model(file="hgtmodel.txt",
                      data=hgt.dat,
                      n.chains=3,
                      inits=hgt.init,
                      n.adapt=2000
)

jagsModel


##Burn-in of 3000 samples
update(jagsModel, n.iter=3000)
## Step 5: Run sampler using coda
hgt.par<-c("beta0", "beta1", "sigma2")
set.seed(5)
hgt.samp<-coda.samples(jagsModel,
                       var=hgt.par,
                       n.iter=100000,
                       thin=100)
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
