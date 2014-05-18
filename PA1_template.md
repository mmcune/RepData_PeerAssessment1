# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
data <- read.csv("activity.csv", header = TRUE, colClasses = c("numeric", "character", 
    "numeric"))
data$date <- as.Date(data$date, format = "%Y-%m-%d")
data$interval <- factor(data$interval)
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```

```r
summary(data)
```

```
##      steps            date               interval    
##  Min.   :  0.0   Min.   :2012-10-01   0      :   61  
##  1st Qu.:  0.0   1st Qu.:2012-10-16   5      :   61  
##  Median :  0.0   Median :2012-10-31   10     :   61  
##  Mean   : 37.4   Mean   :2012-10-31   15     :   61  
##  3rd Qu.: 12.0   3rd Qu.:2012-11-15   20     :   61  
##  Max.   :806.0   Max.   :2012-11-30   25     :   61  
##  NA's   :2304                         (Other):17202
```



## What is mean total number of steps taken per day?

```r
# sum the number of steps by day
by_day <- aggregate(data$steps ~ data$date, FUN = sum, na.rm = TRUE)
hist(by_day[, 2], main = "Histogram of Steps per Day", xlab = "Steps per Day")
```

![plot of chunk mean_by_day](figure/mean_by_day.png) 

```r
mean_by_day <- sprintf("%8.2f", mean(by_day[, 2]))
median_by_day <- sprintf("%8.2f", median(by_day[, 2]))
```


The mean number of steps per day is 10766.19

The median number of steps per day is 10765.00


## What is the average daily activity pattern?

```r
# sum the number of steps by interval
by_interval <- aggregate(data$steps ~ data$interval, FUN = mean, na.rm = TRUE)
names(by_interval) <- c("interval", "steps")
plot(as.numeric(levels(by_interval$interval)), by_interval$steps, type = "l", 
    main = "Average Number of Steps by Time Interval", xlab = "Interval", ylab = "Average Number of Steps")
```

![plot of chunk mean_by_interval](figure/mean_by_interval.png) 

The interval containing the maximum average number of steps is 835

## Imputing missing values

The number of missing values is 2304


```r
# dataframe containing only missing values
d_na <- data[is.na(data$steps), ]
# dataframe containing only non-missing values (complete cases)
d_cc <- data[complete.cases(data), ]

# use by_interval to populate missing values with mean value for that
# interval
d_na$steps <- unlist(lapply(d_na$interval, FUN = function(interval) {
    by_interval$steps[by_interval$interval == interval]
}))

# recreate complete dataset
data_cc <- rbind(d_na, d_cc)

# recalculate histogram of steps per day
by_day_cc <- aggregate(data_cc$steps ~ data_cc$date, FUN = sum, na.rm = FALSE)
hist(by_day_cc[, 2], main = "Histogram of Steps per Day", xlab = "Steps per Day")
```

![plot of chunk replace_missing](figure/replace_missing.png) 

```r
mean_by_day_cc <- sprintf("%8.2f", mean(by_day_cc[, 2]))
median_by_day_cc <- sprintf("%8.2f", median(by_day_cc[, 2]))
```

The mean number of steps per day is 10766.19

The median number of steps per day is 10766.19

Using the average value for the interval to replace the missing records did not change the mean number of steps per day.  The median of the adjusted values shifted and is now equal to the mean.

Imputing the missing data introduces error into the estimates of the total daily number of steps.

## Are there differences in activity patterns between weekdays and weekends?


```r
data$date <- strptime(data$date, format = "%Y-%m-%d")
# create factor variable to indicate weekday or weekend
data$day_type <- as.factor(ifelse((weekdays(data$date) == "Sunday" | weekdays(data$date) == 
    "Saturday"), "weekend", "weekday"))

data_weekend <- data[data$day_type == "weekend", ]
data_weekday <- data[data$day_type == "weekday", ]

we_by_interval <- aggregate(data_weekend$steps ~ data_weekend$interval, FUN = mean, 
    na.rm = TRUE)
names(we_by_interval) <- c("interval", "steps")

wd_by_interval <- aggregate(data_weekday$steps ~ data_weekday$interval, FUN = mean, 
    na.rm = TRUE)
names(wd_by_interval) <- c("interval", "steps")

par(mfrow = c(2, 1))
plot(as.numeric(levels(we_by_interval$interval)), we_by_interval$steps, type = "l", 
    main = "Weekend", xlab = "Interval", ylab = "Average Number of Steps")
plot(as.numeric(levels(wd_by_interval$interval)), wd_by_interval$steps, type = "l", 
    main = "Weekday", xlab = "Interval", ylab = "Average Number of Steps")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

