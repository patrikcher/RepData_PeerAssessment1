# Peer Assignment 1

######################################################################################################
# Download and unzip data
install.packages("downloader")
require(downloader)

fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
zipfile="activity_monitoring_data.zip"
message("Downloading data")
download(fileURL, destfile=zipfile, mode="wb")
unzip(zipfile)

# Load data
data <- read.csv("activity.csv", colClasses= c("integer", "Date", "factor"))

######################################################################################################
# A1. Calculate the total number of steps taken per day
# A2. Make a histogram of the total number of steps taken each day
# A3. Calculate and report the mean and median of the total number of steps taken per day
#total_steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)

# A1
totalSteps = aggregate(x=list(steps=data$steps), list(date=data$date), FUN=sum, na.rm=TRUE)

# A2
library(ggplot2)

ggplot(totalSteps, aes(x=date, y=steps)) + 
  geom_bar(stat = "identity", width = 0.7) + 
  labs(title = "Total number of steps per day", x = "Date", y = "Total number of steps")

# A3
meanTotalSteps <- mean(totalSteps$steps)
medianTotalSteps <- median(totalSteps$steps)
meanTotalSteps
medianTotalSteps

######################################################################################################
# B1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
# the average number of steps taken, averaged across all days (y-axis)
# B2. Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?

# B1
averages <- aggregate(x=list(steps=data$steps), 
                      by=list(interval=as.numeric(as.character(data$interval))), 
                      FUN=mean, na.rm=TRUE)
names(averages)[2] <- "meanOfSteps"

library(ggplot2)

ggplot(data=averages, aes(x=interval, y=meanOfSteps)) +
  geom_line(size=0.7) +
  xlab("5-minute interval") +
  ylab("Average number of steps taken")

# B2
highestAverage <- averages[which.max(averages$meanOfSteps),]
highestAverage

######################################################################################################
# C1. Calculate and report the total number of missing values in the dataset.
# C2. Devise a strategy for filling in all of the missing values in the dataset. 
# C3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
# C4. Make a histogram of the total number of steps taken each day and 
# C5. Calculate and report the mean and median total number of steps taken per day. 
# C6. Do these values differ from the estimates from the first part of the assignment? 

# C1
numOfNA <- sum(is.na(data))
numOfNA

# C2
# Strategy - replace NAs with average of 5 minute interval
filledData <- data

for (i in 1:nrow(filledData)) {
  if (is.na(filledData$steps[i])) {
    filledData$steps[i] <- averages[which(filledData$interval[i] == averages$interval),]$meanOfSteps
  }
}

# C3
library(ggplot2)

ggplot(filledData, aes(x=date, y=steps)) + 
  geom_bar(stat = "identity", width = 0.7) + 
  labs(title = "Total number of steps per day", x = "Date", y = "Total number of steps")

# C4
newTotalSteps = aggregate(x=list(steps=filledData$steps), 
                            list(date=filledData$date), FUN=sum, na.rm=TRUE)

# C5
newMeanTotalSteps <- mean(newTotalSteps$steps)
newMedianTotalSteps <- median(newTotalSteps$steps)
newMeanTotalSteps
newMedianTotalSteps

# C6
meanDiff <- newMeanTotalSteps - meanTotalSteps
medianDiff <- newMedianTotalSteps - medianTotalSteps
meanDiff
medianDiff

######################################################################################################
# D1. Create a new factor variable in the dataset with two levels 
# - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
# D2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
# See the README file in the GitHub repository to see an example of what this plot should look like 
# using simulated data.

# D1
weekdayOrWeekend <- function(date){
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
    return("weekday") 
  else if (day %in% c("Saturday", "Sunday")) 
      return("weekend") 
  else stop("invalid date")
}

filledData$date <- as.Date(filledData$date)
filledData$day <- sapply(filledData$date, FUN=weekdayOrWeekend)

# D2
weekdayOrWeekdayAverages <- aggregate(x=list(steps=filledData$steps), 
                      by=list(interval=as.numeric(as.character(filledData$interval)), 
                              day=filledData$day), 
                      FUN=mean)

library(lattice)
xyplot(weekdayOrWeekdayAverages$steps ~ weekdayOrWeekdayAverages$interval | weekdayOrWeekdayAverages$day, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")