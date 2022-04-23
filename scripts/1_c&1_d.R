
#using the script "1_a.R" as a library
source("1_a.R")


# -----------methods-----------
graphic <- function(data, title, y_lim, bar_color)
{
  EFDT <- get_EFDT(data)
  
  data <- data.frame(name=letters[1:length(EFDT$n_i)], value=EFDT$n_i)
  
  par(mar=c(11,4,4,4)) # decrease the size of the margin
  
  barp <- barplot(height=data$value, col=bar_color, names.arg=EFDT$Intervals, 
                  las=2, width = 1, ylim = c(0, y_lim), space = 0)
  title(main=title, ylab="Frequency")

}


# -----------load dataset-----------
data <- read.csv(file.path("../forestfires.csv"), header=T)


# -----------1_c & 1_d-----------
# Temperature
graphic(data$temp, "Temperature", 250, "darkorange1")
print(paste("The temperature range oscillates mainly (80%) between 12 and 27",
              "degrees centigrade"))

# Relative Humidity
graphic(data$RH, "Relative Humidity", 160, "darkcyan") 
print(paste("About 60% of the time expressed in the measurements says that the",
            "relative humidity remains low, between 24 and 50"))

# Rain
graphic(data$wind, "Wind", 120, "blue4")
print(paste("Low relative humidity causes low rainfall, which is reflected in",
            "the data, less than 2% of the data show occurrences of rain"))

