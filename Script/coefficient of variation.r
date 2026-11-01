# treeData <- read.csv("./DATA/Raw DATA/bagmati_raw_inventory.csv")

# # Keep DBH measurements between 0 and 150 cm, excluding missing values.
# validDBH <- !is.na(treeData$DBH_cm) &
#   treeData$DBH_cm >= 0 &
#   treeData$DBH_cm <= 150
# dbh <- treeData$DBH_cm[validDBH]

# if (length(dbh) < 2) {
#   stop("At least two valid DBH measurements are required.")
# }

# meanDBH <- mean(dbh)
# sdDBH <- sd(dbh)
# if (meanDBH == 0) {
#   stop("The mean DBH is zero, so the coefficient of variation is undefined.")
# }

# # cvDBH <- sdDBH / meanDBH * 100
# # cat("Valid DBH measurements:", length(dbh), "\n")
# # cat("Mean DBH (cm):", meanDBH, "\n")
# # cat("Standard deviation (cm):", sdDBH, "\n")
# # cat("Coefficient of variation (%):", cvDBH, "\n")

# # Function 1: Calculate Standard Deviation
# calculate_sd <- function(args) {
#   sd_value <- sd(args)
#   return(sd_value)
# }

# # Function 2: Calculate Mean
# calculate_mean <- function(args) {
#   mean_value <- mean(args)
#   return(mean_value)
# }

# # Function 3: Calculate Coefficient of Variation
# calculate_cv <- function(args) {

#   # Call SD function
#   sd_result <- calculate_sd(args)

#   # Call Mean function
#   mean_result <- calculate_mean(args)

#   # Calculate CV
#   cv_result <- sd_result / mean_result

#   return(cv_result)
# }

# # Function 4: Convert CV into percentage
# calculate_cv_percent <- function(args) {

#   cv_result <- calculate_cv(args)

#   cv_percent <- cv_result * 100

#   return(cv_percent)
# }

# # Call the final function
# result <- calculate_cv_percent(treeData$DBH_cm[validDBH])

# # Print result
# print(result)


# function 1: calcualte mean
calculate_mean <- function(x) {
  sum(x)/length(x)
}
calculate_mean(c(1,2,3,4,5,6,7,8,9,10))

#function 2: calculate standard deviation
calculate_sd <- function(x) {
  mean_value <- calculate_mean(x)
  squaring <- (x - mean_value)^2
  sum_squaring <- sum(squaring)
  (sum_squaring / (length(x) - 1))^0.5
}
calculate_sd(c(1,2,3,4,5,6,7,8,9,10))
