---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## 1) Loading and preprocessing the data



```r
activity <- read.csv( unzip("activity.zip",file = "activity.csv"))
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```



## 2) Total number of steps taken per day

```r
total_steps_day <- tapply(activity$steps,activity$date,sum)
hist(total_steps_day [!is.na(total_steps_day)],breaks = 30, main = "Histogram total steps per day", xlab = "Total steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

## 3) Median number of steps taken each day

```r
steps_median <- tapply(activity$steps,activity$date,median)
steps_median[!is.na(steps_median)]
```

```
## 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 2012-10-09 
##          0          0          0          0          0          0          0 
## 2012-10-10 2012-10-11 2012-10-12 2012-10-13 2012-10-14 2012-10-15 2012-10-16 
##          0          0          0          0          0          0          0 
## 2012-10-17 2012-10-18 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 
##          0          0          0          0          0          0          0 
## 2012-10-24 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##          0          0          0          0          0          0          0 
## 2012-10-31 2012-11-02 2012-11-03 2012-11-05 2012-11-06 2012-11-07 2012-11-08 
##          0          0          0          0          0          0          0 
## 2012-11-11 2012-11-12 2012-11-13 2012-11-15 2012-11-16 2012-11-17 2012-11-18 
##          0          0          0          0          0          0          0 
## 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 2012-11-24 2012-11-25 
##          0          0          0          0          0          0          0 
## 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##          0          0          0          0
```
The median is 0 steps for all measured days, because it is the most frequent result. This result is also reasonable considering that the average person ussually walks for short periods of time. 

## Mean number of steps taken each day

```r
steps_mean <- tapply(activity$steps,activity$date,mean, na.rm = TRUE)
steps_mean[!is.na(steps_mean)]
```

```
## 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 2012-10-09 
##  0.4375000 39.4166667 42.0694444 46.1597222 53.5416667 38.2465278 44.4826389 
## 2012-10-10 2012-10-11 2012-10-12 2012-10-13 2012-10-14 2012-10-15 2012-10-16 
## 34.3750000 35.7777778 60.3541667 43.1458333 52.4236111 35.2048611 52.3750000 
## 2012-10-17 2012-10-18 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 
## 46.7083333 34.9166667 41.0729167 36.0937500 30.6284722 46.7361111 30.9652778 
## 2012-10-24 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
## 29.0104167  8.6527778 23.5347222 35.1354167 39.7847222 17.4236111 34.0937500 
## 2012-10-31 2012-11-02 2012-11-03 2012-11-05 2012-11-06 2012-11-07 2012-11-08 
## 53.5208333 36.8055556 36.7048611 36.2465278 28.9375000 44.7326389 11.1770833 
## 2012-11-11 2012-11-12 2012-11-13 2012-11-15 2012-11-16 2012-11-17 2012-11-18 
## 43.7777778 37.3784722 25.4722222  0.1423611 18.8923611 49.7881944 52.4652778 
## 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 2012-11-24 2012-11-25 
## 30.6979167 15.5277778 44.3993056 70.9270833 73.5902778 50.2708333 41.0902778 
## 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
## 38.7569444 47.3819444 35.3576389 24.4687500
```

## 4)Time series of average number of steps taken

```r
df_steps_mean <- data.frame( Date = as.Date(names(steps_mean)), Mean = as.vector(steps_mean))
plot(df_steps_mean$Date[!is.na(steps_mean)],df_steps_mean$Mean[!is.na(steps_mean)], main = "Time series plot of the mean number of steps taken", ylab = "Mean", xlab = "Date",type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
## 5) 5-minute interval with the maximum number of steps


```r
act_interval <- tapply(activity$steps, activity$interval, median, na.rm = TRUE)
max_number_steps <- as.integer(names(which.max(act_interval)))
max_number_steps
```

```
## [1] 845
```
835 is the time interval with the highest number of mean steps.

## 6) Code to describe and show a strategy for imputing missing data. 



Missing values:

```r
Number_na <- sum(is.na(activity$steps))
Number_na
```

```
## [1] 2304
```
Missing values were solved by inference, it is known that time of the day, day of the week and month can have an effect on the person's routine, so it is justified to group by those variables, to calculate the median number of steps and to use that result to fill the missing values. The median was used instead of the mean, since the median is more robust towards extreme values. A new dataset "activity1" was created. 

Create new rows with the month and the day of the week.


```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
activity1 <- activity
activity1$dayofweek <- wday(activity1$date)
```

```
## Warning: tz(): Don't know how to compute timezone for object of class factor;
## returning "UTC". This warning will become an error in the next major version of
## lubridate.
```

```r
activity1$month <- month(activity1$date)
```

```
## Warning: tz(): Don't know how to compute timezone for object of class factor;
## returning "UTC". This warning will become an error in the next major version of
## lubridate.
```

```r
na_fill <- tapply(activity1$steps, list(activity1$dayofweek, activity1$month, activity1$interval), median, na.rm = TRUE )
```
Once the median steps per day of week, month and interval had been calculated; the "na" in the "steps" column were completed with those medians.  


```r
rows_to_fill <- which(is.na(activity1$steps))

for(i in rows_to_fill){
        activity1$steps[i] <- na_fill[activity1$dayofweek[i],as.character(activity1$month[i]),as.character(activity1$interval[i])]
  
}

sum(is.na(activity1$steps))
```

```
## [1] 0
```
This proves that the new "steps" column has none "NA". 

## 7) Histogram of the total number of steps taken each day after imputing missing values. 


```r
total_steps_day_filled <- tapply(activity1$steps,activity1$date,sum)

hist(total_steps_day_filled,breaks = 30, main = "Histogram total steps per day", xlab = "Total steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
With the "NA" filled, there is a slight observable increase in the frequency of some baskets in the <5000 steps/day region. 

New median:


```r
steps_median1 <- tapply(activity1$steps,activity1$date,median)
steps_median1
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
##          0          0          0          0          0          0          0 
## 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 2012-10-13 2012-10-14 
##          0          0          0          0          0          0          0 
## 2012-10-15 2012-10-16 2012-10-17 2012-10-18 2012-10-19 2012-10-20 2012-10-21 
##          0          0          0          0          0          0          0 
## 2012-10-22 2012-10-23 2012-10-24 2012-10-25 2012-10-26 2012-10-27 2012-10-28 
##          0          0          0          0          0          0          0 
## 2012-10-29 2012-10-30 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 
##          0          0          0          0          0          0          0 
## 2012-11-05 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##          0          0          0          0          0          0          0 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 2012-11-18 
##          0          0          0          0          0          0          0 
## 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 2012-11-24 2012-11-25 
##          0          0          0          0          0          0          0 
## 2012-11-26 2012-11-27 2012-11-28 2012-11-29 2012-11-30 
##          0          0          0          0          0
```

New mean: 


```r
steps_mean1 <- tapply(activity1$steps,activity1$date,mean, na.rm = TRUE)
steps_mean1
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 2012-10-07 
##  9.9513889  0.4375000 39.4166667 42.0694444 46.1597222 53.5416667 38.2465278 
## 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 2012-10-13 2012-10-14 
##  9.9513889 44.4826389 34.3750000 35.7777778 60.3541667 43.1458333 52.4236111 
## 2012-10-15 2012-10-16 2012-10-17 2012-10-18 2012-10-19 2012-10-20 2012-10-21 
## 35.2048611 52.3750000 46.7083333 34.9166667 41.0729167 36.0937500 30.6284722 
## 2012-10-22 2012-10-23 2012-10-24 2012-10-25 2012-10-26 2012-10-27 2012-10-28 
## 46.7361111 30.9652778 29.0104167  8.6527778 23.5347222 35.1354167 39.7847222 
## 2012-10-29 2012-10-30 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 
## 17.4236111 34.0937500 53.5208333 10.9184028 36.8055556 36.7048611 22.3854167 
## 2012-11-05 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
## 36.2465278 28.9375000 44.7326389 11.1770833 11.1215278 15.7951389 43.7777778 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 2012-11-18 
## 37.3784722 25.4722222 25.1631944  0.1423611 18.8923611 49.7881944 52.4652778 
## 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 2012-11-24 2012-11-25 
## 30.6979167 15.5277778 44.3993056 70.9270833 73.5902778 50.2708333 41.0902778 
## 2012-11-26 2012-11-27 2012-11-28 2012-11-29 2012-11-30 
## 38.7569444 47.3819444 35.3576389 24.4687500 11.1215278
```

## 8) Panel plot comparing average number of steps taken per 5-minutes interval across weekdays and weekends.

```r
activity1$factor_weekday <- as.factor(sapply(activity1$dayofweek, function(x) {if(x %in% 2:6) "weekday" else "weekend"})) 
par(mfrow=c(2,1))
activity_weekday <- subset(activity1, activity1$factor_weekday == "weekday")
activity_weekend <- subset(activity1, activity1$factor_weekday == "weekend")


par(mfrow=c(2,1))

plot( as.integer(names(tapply(activity_weekday$steps,activity_weekday$interval,mean))),tapply(activity_weekday$steps,activity_weekday$interval,mean), type = "l", xlab = "Interval", ylab = "Number of steps")

plot( as.integer(names(tapply(activity_weekend$steps,activity_weekend$interval,mean))),tapply(activity_weekend$steps,activity_weekend$interval,mean), type = "l",xlab = "Interval", ylab= "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
