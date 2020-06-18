---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(ggplot2)
activity <- read.csv("activity.csv")
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
## converting date in date format
activity$date <- as.Date(activity$date, format="%Y-%m-%d")
```


## What is mean total number of steps taken per day?

```r
steps_per_day <- aggregate(steps ~ date, data = activity, FUN = sum)

## histogram
hist(steps_per_day$steps, xlab = "Steps per day", main = "Histogram of steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
## mean and median

mean_of_steps <- mean(steps_per_day$steps)
mean_of_steps
```

```
## [1] 10766.19
```

```r
median_of_steps <- median(steps_per_day$steps)
median_of_steps
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
steps_per_int <- aggregate(steps ~ interval,data = activity, FUN = mean, na.rm=T)
plot(steps_per_int$steps ~ unique(activity$interval), type="l", xlab = "5-min interval",
     ylab = "Steps", main = "Average daily activity pattern")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
## maximum steps
max_steps <- steps_per_int[which.max(steps_per_int$steps),]
max_steps
```

```
##     interval    steps
## 104      835 206.1698
```


## Imputing missing values

```r
table(is.na(activity) == TRUE)
```

```
## 
## FALSE  TRUE 
## 50400  2304
```
we will impute the missing values with the mean value of interval across all days


```r
activity_new <- activity
index_na <- which(is.na(activity_new$steps))
activity_new <- merge(activity, steps_per_int, by = "interval")
missing <- is.na(activity_new$steps.x)
activity_new$steps.x[missing] <- activity_new$steps.y[missing]
activity_new <- activity_new[, c(2, 3, 1)]
names(activity_new) <- c("steps", "date", "interval")
```

### Calculate all the things as above using data with no missing values

```r
steps_per_day_new <- aggregate(steps ~ date, data = activity_new, FUN = sum)

## histogram
hist(steps_per_day_new$steps, xlab = "Steps per day", main = "Histogram of steps per day with new data")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
## mean and median

mean_of_steps <- mean(steps_per_day_new$steps)
mean_of_steps
```

```
## [1] 10766.19
```

```r
median_of_steps <- median(steps_per_day_new$steps)
median_of_steps
```

```
## [1] 10766.19
```

### Creating factor variable

```r
activity_new$day <- ifelse(weekdays(activity_new$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
activity_new$day <- as.factor(activity_new$day)
```

## Are there differences in activity patterns between weekdays and weekends?

```r
steps_per_wday <- aggregate(steps ~ interval + day, data = activity_new, FUN = mean)
ggplot(steps_per_wday, aes(x = interval, y = steps)) + 
  geom_line(col = "blue", size = 1) + 
  facet_wrap(~ day, nrow=2, ncol=1) + 
  labs(x = "Interval", y = "Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

The plots reveals that the activity is more spread out during weekends, while in weekdays the individual is more active during the morning.
