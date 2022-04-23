
#using the script "1_a.R" as a library
source("1_a.R")

# -----------methods-----------
# Calculate the mean estimator 
get_mean_estimator <- function(data)
{
  sum <- 0
  
  # Sum of the elements of data
  for (i in data) {
    sum <- sum + i  
  }
  
  return(sum/length(data))
}

# Calculate the confident interval of the mean
get_mean_confidence_interval <- function(data)
{
  mean_estimator <- get_mean_estimator(data)
  
  lower <- mean_estimator - qnorm(0.975) * (sqrt(get_variance_estimator(data)) /
    sqrt(length(data)))
  upper <- mean_estimator + qnorm(0.975) * (sqrt(get_variance_estimator(data)) /
                                              sqrt(length(data)))
  
  print(paste("The confidence interval of the mean with probability 0.95 is", 
              paste(lower, paste("-", upper))))
}

# Calculate the confident interval of the variance
get_variance_confidence_interval <- function(data)
{
  variance_estimator <- get_variance_estimator(data)
  
  lower <- (length(data) - 1) * variance_estimator / (qchisq(0.975, 
            df=length(data) - 1))
  upper <- (length(data) - 1) * variance_estimator / (qchisq(0.025, 
            df=length(data) - 1))
  
  print(paste("The confidence interval of the mean with probability 0.95 is", 
              paste(lower, paste("-", upper))))
}


# -----------load dataset-----------
data <- read.csv(file.path("../forestfires.csv"), header=T)


# -----------1_b-----------
print("----- Temperature -----")
get_mean_confidence_interval(data$temp)
get_variance_confidence_interval(data$temp)

print("----- Relative Humidity (%) -----")
get_mean_confidence_interval(data$RH)
get_variance_confidence_interval(data$RH)

print("----- Wind (km/h) -----")
get_mean_confidence_interval(data$wind)
get_variance_confidence_interval(data$wind)

