---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
df <- read.csv("./data/activity.csv")
df$date <- as.Date(df$date, "%Y-%m-%d")
head(df)
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



## What is mean total number of steps taken per day?

```r
library(ggplot2)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
## Histogram of the total number of steps taken each day
total_df <- df %>% group_by(date) %>% summarise(total_steps = sum(steps)) 
total_df %>% ggplot(aes(date, total_steps)) + geom_bar(stat = "identity")
```

```
## Warning: Removed 8 rows containing missing values (position_stack).
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
## Mean and median total number of steps taken per day
mean_steps <- mean(total_df$total_steps, na.rm = TRUE)
median_steps <- median(total_df$total_steps, na.rm = TRUE)
```
The mean total number of steps taken per day are 1.0766189\times 10^{4} and median total
number of steps taken per day are 10765


## What is the average daily activity pattern?

```r
## Plot on the basis of 5 minute interval with average number of steps taken
## across all days
interval_df <- df %>% group_by(interval) %>% summarise(average_steps = 
                                            mean(steps, na.rm = TRUE))
interval_df %>% ggplot(aes(interval, average_steps)) + geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
which_interval <- interval_df$interval[which.max(interval_df$average_steps)]
```
The 835 interval contains maximum number of steps on average 
across all the dates.


## Imputing missing values

```r
## Total number of rows with NA's
sum(!complete.cases(df))
```

```
## [1] 2304
```

```r
## Using the median of 5-minute interval to impute missing values
interval_medians <- df %>% group_by(interval) %>% summarise(median_steps = median(steps, na.rm = TRUE))
imputed_df <- merge(df, interval_medians, by="interval")
imputed_df <- imputed_df %>% mutate(steps = case_when(
    is.na(steps) ~ median_steps,
    TRUE ~ steps
))

imputed_df <- imputed_df %>% select(-median_steps)

## Histogram of the total number of steps taken each day
imputed_total_df <- imputed_df %>% group_by(date) %>% summarise(total_steps = sum(steps)) 
imputed_total_df %>% ggplot(aes(date, total_steps)) + geom_bar(stat = "identity")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
## Mean and median total number of steps taken per day
imputed_mean_steps <- mean(imputed_total_df$total_steps, na.rm = TRUE)
imputed_median_steps <- median(imputed_total_df$total_steps, na.rm = TRUE)
```
In the imputed dataset mean steps are 1262.3198268 lesser
than raw dataset and median steps are 370
lesser than raw dataset.  
It might be because when we use `na.rm = TRUE` we're removing the number of 
observations from our calculation which reduces the denominator hence resulting 
in a larger number.


## Are there differences in activity patterns between weekdays and weekends?

```r
## Creating a factor variable in imputed dataset to denote weekend or weekday
imputed_df <- imputed_df %>% mutate(wday = case_when(
    as.POSIXlt(date)$wday <= 5 ~ "weekday",
    as.POSIXlt(date)$wday > 5 ~ "weekend"
))
imputed_df$wday <- as.factor(imputed_df$wday)

## Panel plot comparing number of steps taken across weekdays and weekends.
imputed_interval_df <- imputed_df %>% group_by(wday, interval) %>% summarise(
    average_steps = mean(steps)
)

imputed_interval_df %>% ggplot(aes(interval, average_steps)) + geom_line() + 
    facet_grid(wday~.)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

