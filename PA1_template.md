---
title: "PA1_template"
author: "Mariano Viola"
date: "9/22/2020"
output: 
        html_document:
                keep_md: true
---

1. Load and pre-process the data


```r
if (!file.exists("activity.csv") ) {
        unzip("activity.zip")
}
raw_data <- read.csv("activity.csv", header = TRUE)
main_data <- na.omit(raw_data)
```

2. What is mean total number of steps taken per day?

#Calculate total number of steps per day as data frame with 2 columns - day and sum.


```r
steps_per_day <- aggregate(main_data$steps, by = list(Steps.Date = main_data$date), FUN = "sum")
```

#Make a histogram "Rplot 1" with the frequency of total numbers


```r
hist(steps_per_day$x, col = "green", 
     breaks = 20,
     main = "Total number of steps taken each day",
     xlab = "Number of steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

#Calculate the mean and median of the total number of steps


```r
mean_steps <- mean(steps_per_day[,2])
print (mean_steps)
```

```
## [1] 10766.19
```

```r
median_steps <- median(steps_per_day[,2])
print (median_steps)
```

```
## [1] 10765
```

The mean number of steps is 10,766.2 and the median number of steps is 10,765.


3. What is the average daily activity pattern?

#Plot the number of steps taken averaged across all days along 5-min. intervals - "Rplot2"


```r
average_day <- aggregate(main_data$steps, 
                          by = list(Interval = main_data$interval), 
                          FUN = "mean")

plot(average_day$Interval, average_day$x, type = "l", 
     main = "Average daily activity pattern", 
     ylab = "Average number of steps taken", 
     xlab = "5-min intervals")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

#Define the interval with the maximum number of steps


```r
interval_row <- which.max(average_day$x)
max_interval <- average_day[interval_row,1]
print (max_interval)
```

```
## [1] 835
```

The 5-min. interval of 835 has the maximum number of steps.


4. Imputing missing values

#Calculate the total number of NA values in the dataset


```r
NA_number <- length(which(is.na(raw_data$steps)))
print (NA_number)
```

```
## [1] 2304
```

#Use 'impute' function in Hmisc package to fill in NA values. Create a new
#dataset with the missing values filled in.


```r
library(Hmisc)
```

```
## Loading required package: lattice
```

```
## Loading required package: survival
```

```
## Loading required package: Formula
```

```
## Loading required package: ggplot2
```

```
## 
## Attaching package: 'Hmisc'
```

```
## The following objects are masked from 'package:base':
## 
##     format.pval, units
```

```r
raw_data_filled <- raw_data
raw_data_filled$steps <- impute(raw_data$steps, fun=mean)
```

#Make histogram with new frequencies of total number of steps taken after filling in NA values - "Rplot3"


```r
steps_per_day_noNA <- aggregate(raw_data_filled$steps, 
                                by = list(Steps.Date = raw_data_filled$date), 
                                FUN = "sum")

hist(steps_per_day_noNA$x, col = "green", 
     breaks = 20,
     main = "Total number of steps taken each day (filled data)",
     xlab = "Number of steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

#Calculate the new mean and median of the total number of steps after imputing missing values


```r
mean_steps_noNA <- mean(steps_per_day_noNA[,2])
print (mean_steps_noNA)
```

```
## [1] 10766.19
```

```r
median_steps_noNA <- median(steps_per_day_noNA[,2])
print (median_steps_noNA)
```

```
## [1] 10766.19
```

The new mean and median number of steps equal 10,766.2. They are identical to the earlier mean
and median in the first part of the assignment.


5. Are there differences in activity patterns between weekdays and weekends?

#Create a new variable in the dataset that tells whether a given date is a weekday or a weekend day


```r
raw_data_filled$date <- as.Date(raw_data_filled$date)
raw_data_filled$weekday <- weekdays(raw_data_filled$date)
raw_data_filled$day_type <- ifelse(raw_data_filled$weekday=="Saturday" |
                                           raw_data_filled$weekday=="Sunday","Weekend","Weekday")
raw_data_filled$day_type <- factor(raw_data_filled$day_type)
```

#Plot the number of steps for all 5-min. intervals, averaged across weekdays and weekends separately
#See Rplot4


```r
day_types_data <- aggregate(steps ~ interval + day_type, data=raw_data_filled, mean)

library(ggplot2)
ggplot(day_types_data, aes(interval, steps)) + 
        geom_line() + 
        facet_grid(day_type ~ .) +
        xlab("5-minute intervals") + 
        ylab("Avarage number of steps taken") +
        ggtitle("Weekdays and weekends activity patterns")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

The number of steps on weekend tend to be higher on average during the day, but the peak
in the morning is higher on weekdays.

