# Q1.a.

theta <- c(0.1, 0.3, 0.5, 0.7, 0.9)

values <- function(theta) {
  theta^2*(1- theta)^6
}

calculated_values <- sapply(theta, values)

barplot(calculated_values, names.arg=round(theta, 2), las=2, xlab="Theta", ylab="p(y|Theta)", main="Barplot of Theta vs. p(y|Theta)", cex.names=0.8)

# Q1.b.

prior_p_theta <- c(2/5, 2/30,2/30,2/30,2/5)

calculated_postrior_theta <- function(theta, prior_p_theta) {
  theta^2*(1- theta)^6*prior_p_theta
}

prob_postrior_theta <- mapply(calculated_postrior_theta, theta, prior_p_theta)
barplot(prob_postrior_theta, names.arg=round(theta, 2), las=2, xlab="Theta", ylab="p(y|Theta)", main="Barplot of Theta vs. posterior p(y|Theta)", cex.names=0.8)

## postrior mean
postrior_mean <- theta %*% prob_postrior_theta
postrior_mean

# Q1.c.
# theta_0 = 0.1, w = 4
pbeta(0.5, 2.4, 9.6, lower.tail = FALSE)

# theta_0 = 0.1, w = 20
pbeta(0.5, 4, 24, lower.tail = FALSE)

# theta_0 = 0.3
# w = 4
pbeta(0.5, 3.2, 8.8, lower.tail = FALSE)
# w = 20
pbeta(0.5, 8, 20, lower.tail = FALSE)


# theta_0 = 0.5
# w = 4
pbeta(0.5, 4, 8, lower.tail = FALSE)
# w = 20
pbeta(0.5, 12, 16, lower.tail = FALSE)


# theta_0 = 0.7
# w = 4
pbeta(0.5, 4.8, 7.2, lower.tail = FALSE)
# w = 20
pbeta(0.5, 16, 12, lower.tail = FALSE)


# theta_0 = 0.9
# w = 4
pbeta(0.5, 5.6, 6.4, lower.tail = FALSE)
# w = 20
pbeta(0.5, 20, 8, lower.tail = FALSE)





