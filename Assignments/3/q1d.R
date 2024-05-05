setwd("/Users/felixzhao/Documents/workspace/STAT452/Assignments/3")

# Q1 d

library(rjags)
library(coda)

crdf <- read.csv("cancer_react.csv", header = TRUE)

cndf <- read.csv("cancer_noreact.csv", header = TRUE)

## Step 1: Specify model
cat("
    model {
      for (i in 1:N){
        fatal[i] ~ dpois(theta)
      }
      theta ~ dgamma(220, 100)}",
file="q1dmodel.txt")