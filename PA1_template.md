# PA1_template

#Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

#Loading and preprocessing the data
Show any code that is needed to

Load the data (i.e. read.csv())

Process/transform the data (if necessary) into a format suitable for your analysis


```r
setwd('~/R/data_activity')
active <-read.csv('activity.csv')
str(active)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
head(active)
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

#What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

Make a histogram of the total number of steps taken each day

```r
total_step_perday <-with(active, tapply(steps, as.factor(active$date),sum,na.rm=T))
hist(total_step_perday)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Calculate and report the mean and median total number of steps taken per day

```r
mean1 <- mean(total_step_perday)
median1 <- median(total_step_perday)
print(mean1)
```

```
## [1] 9354.23
```

```r
print(median1)
```

```
## [1] 10395
```
The mean is 9354.2295082 and the median is 10395.

#What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
active1 <-active[!is.na(active$steps),]
mean_step <-with(active1, tapply(steps, active1$interval,mean))
interval <- levels(as.factor(active1$interval))
plot(interval, mean_step,type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
table <- data.frame(mean_step, interval)
table[table$mean_step==max(table$mean_step),][2]
```

```
##     interval
## 835      835
```

#Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
miss <- active[is.na(active$steps),]
dim(miss)
```

```
## [1] 2304    3
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I'm going to be lazy and just make it equal to the mean value.

```r
miss$steps <-mean_step
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
active_new <-rbind(active1,miss)
active_new <-active_new[order(active_new$date),]
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
New_total_step_perday <-with(active_new, tapply(steps, as.factor(active_new$date),sum,na.rm=T))
hist(New_total_step_perday)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
mean2 <-mean(New_total_step_perday)
median2 <-median(New_total_step_perday)
print(mean2)
```

```
## [1] 10766.19
```

```r
print(median2)
```

```
## [1] 10766.19
```

The original mean is 9354.2295082 while the revised mean is 1.0766189\times 10^{4}. 
The original median is 10395 while the revised median is 1.0766189\times 10^{4}.

#Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
active_new$days <-weekdays(as.Date(active_new$date))
find_weekend <-grep("Saturday|Sunday", active_new$days,ignore.case=T)
weekend_dt <- active_new[find_weekend,]
weekend_dt$weekday <-"weekend"
weekday_dt <-subset(active_new, active_new$days!=find_weekend)
```

```
## Warning in active_new$days != find_weekend: longer object length is not a
## multiple of shorter object length
```

```r
weekday_dt$weekday <-"weekday"
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
mean_step_WE <-with(weekend_dt, tapply(steps, weekend_dt$interval,mean))
interval_WE <- levels(as.factor(weekend_dt$interval))
mean_step_WD <-with(weekday_dt, tapply(steps, weekday_dt$interval,mean))
interval_WD <- levels(as.factor(weekday_dt$interval))
par(mfrow=c(2,1))
plot(interval_WE, mean_step_WE,type="l",main="Weekends")
plot(interval_WD, mean_step_WD,type="l", main="Weekdays")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
