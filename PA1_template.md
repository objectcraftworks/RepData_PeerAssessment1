# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Unzip the file to data folder.

```r
data <- "./activity.zip"
csv <- "./data/activity.csv"
unzip(data,exdir="./data")
activity <- read.csv(csv)
activity$date <- as.Date(activity$date,"%Y-%m-%d")
activity.complete_cases <- activity[complete.cases(activity),]
```
## What is mean total number of steps taken per day?


```r
stepsPerDay <- aggregate(steps ~ date, data=activity.complete_cases,sum)
hist(stepsPerDay$steps,xlab="Total number of steps per day",main="Histogram of Total number of steps taken each day") 
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

Mean and Median of the total number of steps taken per day


```
##     Mean   Median 
## 10766.19 10765.00
```

## What is the average daily activity pattern?

```r
stepsByInterval <- aggregate(steps ~ interval, data=activity.complete_cases,mean)
plot(stepsByInterval,type="l",xlab="Interval", ylab="Average Steps of All Days", main
="Average Steps of All days by interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 



 
    On average across all the days in the dataset, interval 08:35 contains the maximum number of steps.

## Imputing missing values
 Total number of missing values in the dataset are 2304
 
 Imputing Strategy:
 
 Imputing missing values in an interval using the average steps of all the days in that interval.
 

```r
activity.imputed_nas <- activity
intervals <- unique(activity$interval)
for (interval in intervals){
  activity.imputed_nas[which(activity$interval==interval & is.na(activity$steps)),"steps"] <-
    stepsByInterval[which(stepsByInterval$interval==interval),"steps"]
}
stepsPerDay.imputes_nas <- aggregate(steps ~ date, data=activity.imputed_nas,sum)
```


```r
hist(stepsPerDay.imputes_nas$steps,xlab="Total number of steps per day",main="Histogram of Total number of steps taken each day with imputed missed values") 
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 
After imputing the missing values, the Mean and the Median of the total number of steps taken per day


```
##     Mean   Median 
## 10766.19 10766.19
```
> There is a negligible change in the median, and mean after imputing the missing values.

## Are there differences in activity patterns between weekdays and weekends?
```r
library(ggplot2)
activity.imputed_nas$wday <- as.POSIXlt(activity.imputed_nas$date)$wday

activity.imputed_nas$weekdayorweekend <- "Weekday"
# 0=sunday,6 = saturday
activity.imputed_nas[which(activity.imputed_nas$wdat ==6 | activity.imputed_nas$wdat ==0),]$bizday <- "Weekend"

avergastepsPerInterval.weekendorweekday <- aggregate(steps ~ interval +weekdayorweekend, data=activity.imputed_nas,mean)

plot <- qplot(interval, steps,data=avergastepsPerInterval.weekendorweekday , 
       geom=c("point","line") ,
      facets= . ~ weekdayorweekend,
     main="Averag steps of all days per interval averaged across all weekends and weekdays",
     xlab="interval",
     ylab="Average step")
plot

```
