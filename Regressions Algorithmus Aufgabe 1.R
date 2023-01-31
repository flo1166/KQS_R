## Load Library
#library(matlib)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)

## Generate Data
set.seed(342243)
n <- 20
x <- rnorm(n = n, mean = 1:20, sd = 0.3)
e <- rnorm(n = n, sd = 1)
a <- 5
b <- 0.5
gen_norm_obs <- a + b*x + e

num_obs <- 1:length(gen_norm_obs)
tbl_beob <- tibble(x = num_obs, y = gen_norm_obs)

## Gernerate horizontal line
mean_obs <- mean(gen_norm_obs)
tbl_horiz_line <- tibble(x = num_obs, y = rep(mean_obs, length(num_obs)))

## Initialize start error vector
error1 <- sum((tbl_beob$y - tbl_horiz_line$y)^2)

## Function to calculate hypothenuse
hypo_cal <- function(dataframe_to_calc) {
  # take first and last observation of the horizontal line
  first_obsv <- dataframe_to_calc[1,]
  last_obsv <- tail(dataframe_to_calc, n=2)[2,]
  
  # calculate Pythogoras
  distance <- last_obsv - first_obsv
  distance_quad_sum <- sum(distance^2)
  hypothenuse <- sqrt(distance_quad_sum)
}

## Function to calculate angle
angle_cal <- function(dataframe_to_calc) {
  # take first and last observation of the horizontal line
  first_obsv <- dataframe_to_calc[1,]
  last_obsv <- tail(dataframe_to_calc, n=2)[2,]
  
  # calculate Pythogoras
  distance <- last_obsv - first_obsv
  
  # calculate alpha
  alpha <- atan(distance$y / distance$x)
  alpha_grad <- alpha / pi * 180
}

## Function to calculate m and intercept
m_inter_func <- function(df) {
  m <- diff(df$y) / diff(df$x)
  intercept <- mean_point[1] - m * mean_point[2]
  tibble(m = m, intercept = intercept)
}

## Function to calculate errors
error_func <- function(m, intercept, beobachtungen) {
  sum_errors <- sum((beobachtungen[, 2] - lapply(list(beobachtungen[, 1]), function(x) x * as.double(m$m) + as.double(intercept$intercept)))^2)
}

## Loop
iteration_count <- 0
int_distance <- 10
mean_point = c(length(num_obs)/2, mean_obs)
Results_Linear_Regression <- tibble(intercept = numeric(), m = numeric(), x1 = numeric(), y1 = numeric(), x2 = numeric(), y2 = numeric(), sum_errors = numeric(), angle = numeric(), best_error = numeric(), revert_flag = numeric(), iteration = numeric())
revert_flag = FALSE

while (TRUE && iteration_count <= 30) {
  iteration_count = iteration_count + 1
  if (iteration_count == 1) {
    hypothenuse <- hypo_cal(tbl_horiz_line)
    angle_calc <- angle_cal(tbl_horiz_line)
    angle_chang <- angle_calc + int_distance
  }
  else if (error1 > sum_errors && !revert_flag) {
    error1 = sum_errors
    angle_chang <- angle_chang + int_distance
  }
  else if (error1 > sum_errors && revert_flag) {
    error1 = sum_errors
    angle_chang <- angle_chang - int_distance
  }
  else if (error1 <= sum_errors && !revert_flag) {
    revert_flag = TRUE
    if (int_distance >= 0.1) {
      int_distance <- int_distance / 2
    }
    angle_chang <- angle_chang - int_distance
  }
  else if (error1 <= sum_errors && revert_flag) {
    if (int_distance >= 0.1) {
      int_distance <- int_distance / 2
      revert_flag = FALSE
      angle_chang <- min(Results_Linear_Regression %>% filter(sum_errors == min(sum_errors)) %>% select(angle))
    } else {
      print(Results_Linear_Regression %>% top_n(-1, sum_errors))
      break
    }
  }
  
  # calculate new ankathete and gegenkathete
  alpha <- angle_chang * pi / 180
  b <- cos(alpha) * hypothenuse
  a <- sin(alpha) * hypothenuse
  
  # calculate new coordinates
  x_coordinates <- c(mean_point[1] + b, mean_point[1] - b)
  y_coordinates <- c(mean_point[2] + a, mean_point[2] - a)
  
  # generate tibble
  tbl_new_coordinates <- tibble(x = x_coordinates, y = y_coordinates)
  
  # calculate m and intercept
  m_inter <- m_inter_func(tbl_new_coordinates)
  
  # calculate errors
  sum_errors <- error_func(m_inter[1,1], m_inter[1,2], tbl_beob)

  # Save in tibble  
  Results_Linear_Regression <- add_row(Results_Linear_Regression, intercept = as.double(m_inter$intercept), m = as.double(m_inter$m), x1 = as.double(tbl_new_coordinates[1,1]), y1 = as.double(tbl_new_coordinates[1,2]), x2 = as.double(tbl_new_coordinates[2,1]), y2 = as.double(tbl_new_coordinates[2,2]), sum_errors = as.double(sum_errors), angle = angle_chang, best_error = error1, revert_flag = as.integer(revert_flag), iteration = iteration_count)
}

## Plotting everything
plot_regression_problem <- ggplot() 

for (i in 1: nrow(Results_Linear_Regression)) {
  new_data <- tibble(x = numeric(), y = numeric(), iteration = numeric())
  new_data[1,1:2] <- Results_Linear_Regression[i,3:4]
  new_data[2,1:2] <- Results_Linear_Regression[i,5:6]
  new_data[,3] <- Results_Linear_Regression[i,11]
  plot_regression_problem <- plot_regression_problem + geom_line(data = new_data, aes(x = x, y = y, group = factor(iteration), col = factor(iteration)), size = 1) 
}

# Define perfect (LM-Model of R) and best_line (of our Model)
best_line <- Results_Linear_Regression %>% top_n(-1, sum_errors)
best_line_p <- tibble(x = numeric(), y = numeric()) 
best_line_p[1,] <- best_line[1, 3:4]
best_line_p[2,] <- best_line[1, 5:6]
perfect_line <- lm(data = tbl_beob)

# Define the number of colors you want
#nb.cols <- nrow(Results_Linear_Regression) + 1

plot_regression_problem <- plot_regression_problem + 
  geom_point(data = tbl_beob, aes(x = x, y = y)) +
  geom_hline(yintercept = mean_obs) + 
  geom_line(data = best_line_p, aes(x = x, y= y), col = "red", size = 1) +
  geom_smooth(formula = y~x, method = lm, data = tbl_beob, aes(x, y), se = FALSE, col = "aquamarine3") +
  theme(aspect.ratio=1) 

print(plot_regression_problem)

# Test Regression to validate
test <- lm(data = tbl_beob, y~x)
sum(test$residuals^2)

