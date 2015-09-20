library(ggplot2)
library(dplyr)

# Loading and preprocessing the data
df <- read.csv(unz("activity.zip", "activity.csv"), sep =",")
df <- na.omit(df)

# What is mean total number of steps taken per day?
stepsByDay <- group_by(df, date)
stepsByDay <- summarise(stepsByDay, steps_per_day = sum(steps))

ggplot(stepsByDay, aes(x = steps_per_day)) + geom_histogram(aes(fill = ..count..), binwidth = 1000) + xlab("Number of Steps per Day") + scale_fill_gradient("Count", low = "green", high = "red")

mean(stepsByDay$steps_per_day)
median(stepsByDay$steps_per_day)

# What is the average daily activity pattern?
stepsByInterval <- group_by(df, interval)
avgStepsByInterval <- summarise(stepsByInterval, steps_per_interval = mean(steps))
ggplot(avgStepsByInterval, aes(x = interval, y = steps_per_interval)) + geom_line() + ylab("Average Number of Steps per Interval")

arrange(avgStepsByInterval, desc(steps_per_interval))[1,]$interval

# Imputing missing values
df <- read.csv(unz("activity.zip", "activity.csv"), sep =",")

sum(is.na(df$steps))

for (i in 1:nrow(df)) {
  if (is.na(df[i, ]$steps)) {
    df[i, ]$steps <- avgStepsByInterval[df[i, ]$interval == avgStepsByInterval$interval,]$steps_per_interval
  }
}

stepsByDay <- group_by(df, date)
stepsByDay <- summarise(stepsByDay, steps_per_day = sum(steps))

ggplot(stepsByDay, aes(x = steps_per_day)) + geom_histogram(aes(fill = ..count..), binwidth = 1000) + xlab("Number of Steps per Day (Imputed)") + scale_fill_gradient("Count", low = "green", high = "red")

mean(stepsByDay$steps_per_day)
median(stepsByDay$steps_per_day)

# Are there differences in activity patterns between weekdays and weekends?
df$date <- as.Date(df$date)
df$days <- weekdays(df$date)
weekdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
df$day_type <- as.factor(ifelse(df$days %in% weekdays, 'Weekday', 'Weekend'))

aggdata <- aggregate(steps ~ interval + day_type, df, mean)
ggplot(aggdata, aes(x = interval, y = steps)) + ylab("Number of steps") + geom_line() + facet_grid(day_type ~ .)

