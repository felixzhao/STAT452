setwd("/Users/felixzhao/Documents/workspace/STAT452/Assignments/3")

# Q1 b

crdf <- read.csv("cancer_react.csv", header = TRUE)

cndf <- read.csv("cancer_noreact.csv", header = TRUE)


# Define a function that accepts a DataFrame and two column names for summation
summarize_data <- function(df, col_x='x', col_y='y') {
  # Counting the number of rows
  n <- nrow(df)
  
  # Summing the specified 'x' column, handling possible NA values
  sum_x <- sum(df[[col_x]], na.rm = TRUE)
  
  # Summing the specified 'y' column, handling possible NA values
  sum_y <- sum(df[[col_y]], na.rm = TRUE)
  
  # Creating a formatted string to display the results
  result_string <- paste("n=", n, ", sum of ", col_x, " = ", sum_x, ", sum of ", col_y, " = ", sum_y, sep="")
  
  # Printing the formatted string
  print(result_string)

  post_dist_string <- paste("theta | y ~ Beta(",sum_y,", ", n*sum_x,")")
  print(post_dist_string)
  
  return(list(sum_y=sum_y, n_sum_x=n*sum_x))
}

print("cancer_noreact.csv")
par_1 <- summarize_data(cndf)

print("cancer_react.csv")
par_2 <- summarize_data(crdf)





# Q1 c

set.seed(50)

mc_theta <- function(a, b, par){
  a1_post <- a + par$sum_y
  b1_post <- b + par$n_sum_x
  
  q1ci_post_dixt_theta_1 <- paste("θ | y ~ Beta(",a1_post,", ", b1_post,")")
  print(q1ci_post_dixt_theta_1)
  
  theta1.mc = rbeta(5000, a1_post, b1_post)
  mean(theta1.mc)
  theta_mean <- paste("E[θ|y,x] = ",mean(theta1.mc))
  print(theta_mean)
  
  print("95% quantile-based posterior intervals for θ")
  print(quantile(theta1.mc, probs = c(0.025, 0.5, 0.975)))
  
  return(theta1.mc)
}

mc_theta_compare <- function(a1, b1, a2, b2, par1, par2){
  print("option 1, theta 1:")
  theta1 <- mc_theta(a1, b1, par1)
  
  
  print("option 1, theta 2:")
  theta2 <- mc_theta(a2, b2, par2)
  
  print("Pr (θ1 > θ2|y1, y2, x1, x2)")
  mean(theta1 > theta2)
}

# i
i_a1 <- 2.2 * 100
i_a2 <- i_a1
i_b1 <- 100
i_b2 <- i_b1

mc_theta_compare(i_a1, i_b1, i_a2, i_b2, par_1, par_2)

# ii