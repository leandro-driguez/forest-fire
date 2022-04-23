
# -----------methods-----------
# Method for calculating the size of the intervals
get_size_intervals <- function(size)
{ 
  interval_size <- as.integer(sqrt(size))
  if (interval_size < 5)
  {
    interval_size <- as.integer(size/5)
  }
  if (interval_size > 20)
  {
    interval_size <- as.integer(size/20)
  }
  return(interval_size)
}

# Method to count absolute frecuency of the intervals
get_frecuency <- function(data, lower, upper, ptr)
{
  n_j <- 0
  i <- ptr
  
  while (data[i] < upper) 
  {
    if ((data[i] >= lower & data[i] < upper)) {
      n_j <- n_j + 1
    } else { break }
    
    i <- i + 1
    if (i - 1 == length(data))
      break
  }
  
  return(c(n_j, i))
}

# Method to get the lower end of the interval
get_lower <- function(interval)
{
  str <- strsplit(interval, " ")[[1]] 
  lower <- as.double(str[2])
  return(lower)
}

# Method to get the upper end of the interval
get_upper <- function(interval)
{
  str <- strsplit(interval, " ")[[1]] 
  upper <- as.double(str[4])
  return(upper)
}

# Empirical Frequency Distribution Table
get_EFDT <- function(data)
{
  data <- sort(data) # sort data
    
  min_value <- data[1] # get minimum value
  max_value <- data[length(data)] # get maximum value
  
  # get the size intervals
  size_intervals <- get_size_intervals(max_value - min_value) 
  
  # create the columns of the empirical frequency distribution table
  MC <- c()
  n_i <- c()
  N_i <- c()
  f_i <- c() 
  F_i <- c()
  Intervals <- c()
  
  lower <- min_value # lower end of range
  ptr <- 1 # initialize a pointer for iterating the data set
  
  # insert each row for column independently
  while (lower < max_value) {
    upper <- lower + size_intervals # upper end of the range
    
    # construct the interval
    interv <- paste("[", lower, ",", upper, ")")
    if (max_value - upper < size_intervals){
      upper <- max_value
      interv <- paste("[", lower, ",", upper, "]")
    }
    
    Intervals <- append(Intervals, interv) # add a row to column Intervals
    MC <- append(MC, (lower + upper)/2)  # add a row to column Intervals
    
    # get frecuency of the data between the lower and upper end of the range
    tmp <- c(1)
    if (upper == max_value)
      tmp <- get_frecuency(data, lower, upper + 1, ptr)
    else
      tmp <- get_frecuency(data, lower, upper, ptr)
    n_j <- tmp[1] # amount of elements into the range
    ptr <- tmp[2] # update pointer of the data
    n_i <- append(n_i, n_j) # add a row to column n_i
    
    # add a row to column N_i
    if (lower == min_value)
      N_i <- append(N_i, n_j)
    else
      N_i <- append(N_i, N_i[length(N_i)] + n_j)

    f_i <- append(f_i, n_j / length(data)) # add a row to column f_i
    
    # add a row to column F_i
    if (lower == min_value)
      F_i <- append(F_i, n_j / length(data))
    else
      F_i <- append(F_i, F_i[length(F_i)] + n_j / length(data))
    
    lower <- upper # now, the lower end of the range will be the upper
  }  
  
  EFDT <- data.frame(Intervals, MC, n_i, N_i, f_i, F_i ) 
  
  return(EFDT)
}

# Calculate the index of the modal class 
get_moda_index <- function(EFDT)
{
  count <- 1 # iterator from 1 to length(EFDT$n_i)
  maximum <- -1 # maximum value
  indexes <- c() # keep the index of the modal class/classes
  
  for (i in EFDT$n_i)
  {
    if (i > maximum)
    {
      maximum <- i
    }
  }
  
  for (i in EFDT$n_i)
  {
    if (i == maximum)
    {
      indexes <- append(indexes, count)
    }
    count <- count + 1
  }
  
  return(indexes)
}

# Calculate the mode
get_moda <- function(data)
{
  EFDT <- get_EFDT(data)
  indexes <- get_moda_index(EFDT)
  modes <- c()
  
  for (i in indexes)
  {
    interval <- EFDT$Intervals[i]
    lower <- get_lower(interval)
    upper <- get_upper(interval)
    
    if (i == 1)
    {
      Mo <- lower + 
        (EFDT$n_i[i]/(EFDT$n_i[i] + (EFDT$n_i[i] - EFDT$n_i[i+1]))) * 
        (upper - lower)
    }
    else if(EFDT$N_i[i] == length(data))
    {
      Mo <- lower + 
        ((EFDT$n_i[i] - EFDT$n_i[i-1])/((EFDT$n_i[i] - EFDT$n_i[i-1]) + 
                                          EFDT$n_i[i])) * 
        (upper - lower)
    }
    else
    {
    Mo <- lower +  
      ((EFDT$n_i[i] - EFDT$n_i[i-1])/((EFDT$n_i[i] - EFDT$n_i[i-1]) + 
                                        (EFDT$n_i[i] - EFDT$n_i[i+1]))) * 
      (upper - lower)
    }
    
    modes <- append(modes, c(interval, Mo))
  }
  
  return(modes)
}

# Calculate the mean
get_mean <- function(data)
{
  EFDT <- get_EFDT(data)
  
  mean <- 0 
  count <- 1
  
  for (i in EFDT$n_i)
  {
    # carries the sum of the class mark multiplied by the frequency 
    mean <- mean + i * EFDT$MC[count] # of elements in the interval
    count <- count + 1
  }
  mean <- mean / EFDT$N_i[count-1]
  
  return(mean)
}

# Calculate the median 
get_median <- function(data)
{
  EFDT <- get_EFDT(data)
  n <- length(data)
  index <- 1
  
  for (i in 1:length(EFDT$n_i)) 
  {
    if (EFDT$N_i[i] > n/2)
    {
      index <- i
      break
    }
  }
  
  lower <- get_lower(EFDT$Intervals[index])
  upper<- get_upper(EFDT$Intervals[index])
  
  if(index > 1)
  {
  median <- lower + ((n/2 + EFDT$N_i[index - 1])/(EFDT$N_i[index] - 
                                   EFDT$N_i[index - 1])) * (upper - lower)
  }
  else
  {
    median <- lower + (n/2/EFDT$N_i[index]) * (upper - lower)
  }
  return(median)
}

# Calculate the variance
get_variance <- function(data)
{
  EFDT <- get_EFDT(data)
  
  variance <- 0
  mean <- get_mean(data)
  
  for (i in 1:length(EFDT$n_i)) {
    variance <- variance + EFDT$n_i[i] * (EFDT$MC[i] - mean)^2
  }
  variance <- variance/(EFDT$N_i[length(EFDT$N_i)]-1) 
  
  return(variance)
}

# Calculate the standard desviation
get_standard_desviation <- function(data)
{
  return(sqrt(get_variance(data)))
}

# Calculate the coefficient of variation
get_coefficient_variation <- function(data)
{
  return(sqrt(get_variance(data))/get_mean(data))
}

# Descriptive statistics calculation method
descriptive_statistics <- function(data, rounds_digit)
{
  EFDT <- get_EFDT(data)
  
  # Measures of central tendency
  modes <- get_moda(data)
  
  mode_i <- 1
  while (mode_i <= length(modes))
  {
    print(paste("The mode corresponding to modal class", modes[mode_i], "is", round(as.double(modes[mode_i + 1]), digits = rounds_digit)))
    mode_i <- mode_i + 2
  }
  print(paste("The mean is", round(get_mean(data), digits = 2)))
  print(paste("The median is", round(get_median(data), digits = 2)))
  
  
  # Measures of dispersion
  print(paste("The variance is", round(get_variance(data), digits = 2)))
  print(paste("The standard desviation is", round(get_standard_desviation(data), digits = 2)))
  print(paste("The coefficient of variation is", round(get_coefficient_variation(data), digits = 2)))
}


# -----------load dataset-----------
data <- read.csv(file.path("../forestfires.csv"), header=T)

print(paste("The parameters chosen to be analized are temperature, relative",
      paste("humidity and wind due to their importance in building machine",
      paste("learning models, used in several countries, capable of predicting",
      paste("and supporting fire management decisions since these systems were",      
            "first designed in 1970.")))))


# -----------1_a-----------
print("----- Temperature -----")
print(get_EFDT(data$temp))
descriptive_statistics(data$temp, 1)


print("----- Relative Humidity (%) -----")
print(get_EFDT(data$RH))
descriptive_statistics(data$RH, 0)


print("----- Wind (km/h) -----")
print(get_EFDT(data$wind))
descriptive_statistics(data$wind, 1)

