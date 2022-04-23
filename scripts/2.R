
# using the scripts "1_a.R" and "1_b.R" as libraries
source("1_a.R")
source("1_b.R")

# -----------methods-----------
# Calculate the variance estimator 
get_variance_estimator <- function(data)
{
  mean_est <- get_mean_estimator(data)
  sum <- 0
  
  # sums the square of the difference of each value in the data set with the 
  for (i in data) {                                   # estimator of the mean
    sum <- (i - mean_est)^2  
  }  
  
  return(sum/(length(data) - 1))
}

# Take a list with the data of a specific month
get_list_areas_month <- function(data, month)
{
  result <- c()
  
  for (i in 1:length(data$area)) {
    if (data$month[i] == month)
      result <- append(result, data$area[i])
  }
  
  return(result)
}

# removes the value x from the dataset
remove <- function(data, x)
{
  result <- c()
  
  for (item in data) {
    if (item != x)
      result <- append(result, item)
  }
  
  return(result)
}


# -----------load dataset-----------
data <- read.csv(file.path("../forestfires.csv"), header=T)


# filter the data depending on the month
aug <- get_list_areas_month(data, "aug")
sep <- get_list_areas_month(data, "sep")


# show the distribution of the dataset
hist(aug, col='coral2', main='August (Original)')
hist(sep, col='steelblue', main='September (Original)')


# -----------prepare dataset-----------
# let's remove the value 0 from the dataset because it makes no sense
aug <- remove(aug, 0) # to consider fires that cover 0 ha of burned area, 
sep <- remove(sep, 0) # this adds a large bias to the data set

# normalize the dataset
aug <- log(aug)
sep <- log(sep)


# show the distribution of the dataset again to check if it is already 
# normally distributed
hist(aug, col='coral2', main='August (Log Transformed)')
hist(sep, col='steelblue', main='September (Log Transformed)')


# -----------2-----------
# get the mean and variance estimator for august
aug_mean_est <- get_mean_estimator(aug) 
aug_variance_est <- get_variance_estimator(aug)

# get the mean and variance estimator for september
sep_mean_est <- get_mean_estimator(sep)
sep_variance_est <- get_variance_estimator(sep)



# hypothesis test to see if the variances are different
F <- aug_variance_est/sep_variance_est
alpha <- 0.05


# hypothesis test to see if the variances are different
if (F < qf(alpha/2, df1=length(aug)-1, df2=length(sep)-1) | 
    F > qf(1-alpha/2, df1=length(aug)-1, df2=length(sep)-1)){
  print(paste("The null hypothesis is refuted, so it can be assumed that the,",
        "variances are different."))
} else {
  # hypothesis test to find out if the variance of the first is greater than the second
  if (F > qf(1-alpha, df1=length(aug)-1, df2=length(sep)-1)){
    print(paste("The null hypothesis is refuted, therefore it can be assumed that the",
                "variance of the first is greater than the second."))
  } else{
    # prueba de hipotesis para saber si la varianza de la segunda es mayor que la primera
    if (F < qf(alpha, df1=length(aug)-1, df2=length(sep)-1)){
      print(paste("The null hypotesis is rejected,  therefore it can be assumed that the",
                  "variance of the second month is greater than the variance of the", 
                  "first one"))
    } else{
      print("Since we could not reject any hypothesis, let us assume that the variances are equal") 
    }
  }  
}
  

T <- ((aug_mean_est - sep_mean_est) / sqrt( (length(aug)-1) * aug_variance_est  +
     (length(sep) - 1) * sep_variance_est ) ) * sqrt((length(aug) * length(sep) *
     (length(aug) + length(sep) - 2)) / (length(aug) + length(sep)))


# value in the t-student distribution with freedom level 'n' and a significance 
# level of 5%
alpha = 0.05
t_n <- qt(c(1-alpha), df=length(aug)+length(sep)-2)

if (T < -1*t_n)
  print(paste("Given that the T statistician is less than the opposite of the value",
        "of the t-student distribution with a significance level of 5%,",
        "then H_0 is rejected and it is accepted that the mean of the areas",
        "burned in the month of August is less than the ones of September"))

