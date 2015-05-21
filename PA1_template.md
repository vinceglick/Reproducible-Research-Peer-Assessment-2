---
title: "Rep Research Peer Assessment 1"
author: "Vince Glick"
date: "Sunday, February 15, 2015"
output: html_document
---


```r
echo = TRUE
options(scipen = 1)
```
  
  
####Loading and preprocessing the data


```r
require(ggplot2)
```

**1. Load the data (i.e. read.csv() )**


```r
activity <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor"))
```

**2. Process/transform the data (if necessary) into a format suitable for your analysis**


```r
activclean <- na.omit(activity)
activity$month <- as.numeric(format(activity$date, "%m"))
```

####What is mean total number of steps taken per day?

**1. Make a histogram of the total number of steps taken each day**


```r
gbarplot <- ggplot(data=activclean , aes(x=date, y=steps))+ geom_bar(stat = "identity")
gbarplot
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

**2. Calculate and report the mean and median total number of steps taken per day**


```r
aggregate_steps <- aggregate(activclean$steps, list(date = activclean$date), FUN = "sum")$x

meanAvgSteps <- mean(aggregate_steps)
meanAvgSteps
```

```
## [1] 10766.19
```

```r
medianAvgSteps <- median(aggregate_steps)
medianAvgSteps
```

```
## [1] 10765
```

####What is the average daily activity pattern?

**1. Make a time series plot (i.e. type = "l" ) of the 5minute interval (xaxis)**
**and the average number of steps taken, averaged across all days (yaxis)**


```r
avgsteps <- aggregate(activclean$steps, list(interval = as.numeric(as.character(activclean$interval))), 
                      FUN = "mean")
names(avgsteps)[2] <- "meanAvgSteps"

ggplot(avgsteps, aes(interval, meanAvgSteps)) + 
  geom_line(color = "#d41e1e", size = 0.5) + 
  labs(title = "Time Series Plot", x = "5 Min. intervals", y = "AVG # Steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

```r
head(activclean)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
```

**2. Which 5-minute interval, on average across all the days in the dataset,** 
**contains the maximum number of steps?**


```r
avgsteps[avgsteps$meanAvgSteps==max(avgsteps$meanAvgSteps),]
```

```
##     interval meanAvgSteps
## 104      835     206.1698
```

####Imputing missing values

**1. Calculate and report the total number of missing values in the dataset** 
**(i.e. the total number of rows with NA s)**


```r
sum(is.na(activity[1]))
```

```
## [1] 2304
```

**2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to**
**be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5minute**
**interval, etc.**

**3. Create a new dataset that is equal to the original dataset but with the missing data filled in.**


```r
fillActivity <- activity 
for (i in 1:nrow(fillActivity)) {
  if (is.na(fillActivity$steps[i])) {
    fillActivity$steps[i] <- avgsteps[which(fillActivity$interval[i] == avgsteps$interval), ]$meanAvgSteps
  }
}

head(fillActivity)
```

```
##       steps       date interval month
## 1 1.7169811 2012-10-01        0    10
## 2 0.3396226 2012-10-01        5    10
## 3 0.1320755 2012-10-01       10    10
## 4 0.1509434 2012-10-01       15    10
## 5 0.0754717 2012-10-01       20    10
## 6 2.0943396 2012-10-01       25    10
```

```r
sum(is.na(fillActivity[1]))
```

```
## [1] 0
```

**4. Make a histogram of the total number of steps taken each day and Calculate and report the mean**
**and median total number of steps taken per day. Do these values differ from the estimates from the**
**first part of the assignment?**


```r
gbarplot2 <- ggplot(data=fillActivity, aes(x=date, y=steps))+ geom_bar(stat = "identity")
gbarplot2
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

```r
avgsteps2 <- aggregate(fillActivity$steps, list(interval = as.numeric(as.character(fillActivity$interval))), 
                       FUN = "mean")$x
meanAvgSteps2 <- mean(avgsteps2)
meanAvgSteps2
```

```
## [1] 37.3826
```

```r
medianAvgSteps2 <- median(avgsteps2)
medianAvgSteps2
```

```
## [1] 34.11321
```

####What is the impact of imputing missing data on the estimates of the total
####daily number of steps?


```r
aggregate_steps2 <- aggregate(fillActivity$steps, list(date = fillActivity$date), FUN = "sum")$x


t.test(aggregate_steps, aggregate_steps2)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  aggregate_steps and aggregate_steps2
## t = 0, df = 107.145, p-value = 1
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -1539.144  1539.144
## sample estimates:
## mean of x mean of y 
##  10766.19  10766.19
```

```r
wilcox.test(aggregate_steps, aggregate_steps2)
```

```
## 
## 	Wilcoxon rank sum test with continuity correction
## 
## data:  aggregate_steps and aggregate_steps2
## W = 1612.5, p-value = 0.9841
## alternative hypothesis: true location shift is not equal to 0
```

```r
var.test(aggregate_steps, aggregate_steps2)
```

```
## 
## 	F test to compare two variances
## 
## data:  aggregate_steps and aggregate_steps2
## F = 1.1538, num df = 52, denom df = 60, p-value = 0.5901
## alternative hypothesis: true ratio of variances is not equal to 1
## 95 percent confidence interval:
##  0.6822128 1.9714650
## sample estimates:
## ratio of variances 
##           1.153846
```


####Are there differences in activity patterns between weekdays and weekends?

**1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating**
**whether a given date is a weekday or weekend day.**


```r
head(fillActivity)
```

```
##       steps       date interval month
## 1 1.7169811 2012-10-01        0    10
## 2 0.3396226 2012-10-01        5    10
## 3 0.1320755 2012-10-01       10    10
## 4 0.1509434 2012-10-01       15    10
## 5 0.0754717 2012-10-01       20    10
## 6 2.0943396 2012-10-01       25    10
```

```r
fillActivity$day <- factor(format(fillActivity$date, "%A"))
levels(fillActivity$day)
```

```
## [1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"  
## [7] "Wednesday"
```

```r
levels(fillActivity$day) <- 
  list(weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), 
       weekend = c("Saturday", "Sunday"))
head(fillActivity)
```

```
##       steps       date interval month     day
## 1 1.7169811 2012-10-01        0    10 weekday
## 2 0.3396226 2012-10-01        5    10 weekday
## 3 0.1320755 2012-10-01       10    10 weekday
## 4 0.1509434 2012-10-01       15    10 weekday
## 5 0.0754717 2012-10-01       20    10 weekday
## 6 2.0943396 2012-10-01       25    10 weekday
```

**2. Make a panel plot containing a time series plot (i.e. type = "l" ) of the 5minute**
**interval (xaxis) and the average number of steps taken, averaged across all weekday** 
**days or weekend days (yaxis). See the README file in the GitHub repository to see an** 
**example of what this plot should look like using simulated data.**


```r
avgsteps <- aggregate(fillActivity$steps, 
                      list(interval = as.numeric(as.character(fillActivity$interval)), 
                           day = fillActivity$day), 
                      FUN = "mean")
names(avgsteps)[3] <- "meanSteps"
library(lattice)
xyplot(avgsteps$meanSteps ~ avgsteps$interval | avgsteps$day, 
       layout = c(1, 2), type = "l", 
       xlab = "INTERVAL", ylab = "# OF STEPS")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 

