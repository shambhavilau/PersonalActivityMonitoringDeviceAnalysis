activity <- read.csv('activity.csv')
activity$date <- as.Date(activity$date)

# 1. What is mean total number of steps taken per day?
library(ggplot2)
# a. total number of steps per day
total.steps <- tapply(activity$steps, activity$date, FUN=sum, na.rm=TRUE)
# b. plot of the steps taken per day
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
# c. mean and median of the total number of steps taken per day
mean(total.steps, na.rm=TRUE)
median(total.steps, na.rm=TRUE)


# 2.What is the average daily activity pattern?
library(ggplot2)
# a. timeseries plot of daily activity pattern
averages <- aggregate(x=list(steps=activity$steps), by=list(interval=activity$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5-minute interval") +
  ylab("average number of steps taken")

# b. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
  averages[which.max(averages$steps),]

# 3. Imputing missing values
# a. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
print(paste("The total number of rows with NA is: ",sum(is.na(activity$steps)))) 
  
#c. Devise a strategy for filling in all of the missing values in the dataset. (c) Create a new dataset that is equal to the original dataset but with the missing data filled in.
fill.value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (averages[averages$interval==interval, "steps"])
  return(filled)
}
filled.activity <- activity
filled.activity$steps <- mapply(fill.value, filled.activity$steps, filled.activity$interval)
  
# d.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  
total.steps <- tapply(filled.activity$steps, filled.activity$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total.steps)
median(total.steps)  
  

# 4.Are there differences in activity patterns between weekdays and weekends?
# a. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
weekday.or.weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}
filled.activity$date <- as.Date(filled.activity$date)
filled.activity$day <- sapply(filled.activity$date, FUN=weekday.or.weekend)
  

# b. Make a panel plot containing a time series plot (i.e. type = "l" of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

averages <- aggregate(steps ~ interval + day, data=filled.activity, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")
