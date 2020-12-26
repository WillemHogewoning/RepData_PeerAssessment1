#script for analysis

activity <- unzip("activity.zip")
data <- read.csv("activity.csv", header=TRUE, sep=",")

data$date <- as.Date(data$date, format="%Y-%m-%d")
data$interval <- as.factor(data$interval)

#What is mean total number of steps taken per day?

stepsperday <- aggregate(steps ~ date, data=data, FUN=sum)
colnames(stepsperday) <- c("date", "steps")

# Make a histogram of the total number of steps taken each day

library(ggplot2)
ggplot(stepsperday, aes(x = steps)) + 
  geom_histogram(binwidth = 1000) + 
  labs(title = "sSeps taken each day", x = "Steps", y = "Frequency")

# Calculate and report the mean and median of the total number of steps taken per day
meansteps <- mean(stepsperday$steps)
print(meansteps)

mediansteps <- median(stepsperday$steps)
print(mediansteps)

# What is the average daily activity pattern?


stepsinterval <- aggregate(steps ~ interval, data = data, FUN = mean, na.rm = TRUE)

stepsinterval$interval <- as.integer(levels(stepsinterval$interval)[stepsinterval$interval])
colnames(stepsinterval) <- c("interval", "steps")

ggplot(stepsinterval, aes(x = interval, y = steps)) + 
  geom_line( size = 1) + 
  labs(title = "timeseries 5-minute interval", x = "Interval", y = "Steps")

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

max_steps <- stepsinterval[which.max(stepsinterval$steps),]
print(max_steps)

#Imputing missing values


nas <- sum(is.na(data$steps))
sum(nas)

#Devise a strategy for filling in all of the missing values in the dataset.

data2 <- data
indexna <- which(is.na(data2$steps))
for (i in indexna) {
  data2$steps[i] <- with(stepsinterval, steps[interval = data2$interval[i]])
}
  
nas2 <- sum(is.na(data2$steps))
sum(nas2)


# Make a histogram of the total number of steps taken each day

stepsperday2 <- aggregate(steps ~ date, data=data2, FUN=sum)
colnames(stepsperday2) <- c("date", "steps")
  

ggplot(stepsperday2, aes(x = steps)) + 
  geom_histogram(binwidth = 1000) + 
  labs(title = "sSeps taken each day", x = "Steps", y = "Frequency")
  
meansteps2 <- mean(stepsperday2$steps)
print(meansteps2)

mediansteps2 <- median(stepsperday2$steps)
print(mediansteps2)
  
  
#Are there differences in activity patterns between weekdays and weekends?
library(data.table)

data3 <- data.table(data2)
data3[, weekday := ifelse(weekdays(date) %in% c("zaterdag", "zondag"), "Weekend", "Weekday")]
data3$weekday <- as.factor(data3$weekday)
data3$interval <- as.integer(levels(data3$interval)[data3$interval])


stepsweekweekend <- aggregate(steps ~ interval+weekday, data = data3, FUN = mean)
ggplot(stepsweekweekend, aes(x = interval, y = steps)) + 
  geom_line( size = 1) + 
  facet_wrap(~ weekday, nrow=2, ncol=1) + 
  labs(x = "Interval", y = "Steps")







