---
output: 
  md_document: 
    keep_md: true
---





# Coursera Reproducible Research Assignment 1

## Loading and pre-processing the data


``` r
unzip("activity.zip")
```

```
## Warning in unzip("activity.zip"): error 1 in extracting from zip file
```

``` r
data <- read.csv("activity.csv")
```

## What is the mean total number of steps taken per day?


``` r
library(ggplot2)
library(dplyr)

total_steps <- data %>%
  group_by(date) %>%
  summarise(daily_steps = sum(steps, na.rm = TRUE))

ggplot(total_steps, aes(daily_steps)) + geom_histogram(binwidth = 2000) +
  xlab("Total number of steps taken each day") + 
  ylab("Frequency")
```

![plot of chunk unnamed-chunk-2](./figure/unnamed-chunk-2-1.png)

``` r
meanStepsPerDay <- mean(total_steps$daily_steps, na.rm=TRUE)
medianStepsPerDay <- median(total_steps$daily_steps, na.rm=TRUE)
```

## What is the average daily activity pattern?


``` r
interval_steps <- data %>% 
  group_by(interval) %>%
  summarise(steps = mean(steps, na.rm =TRUE))
```


``` r
ggplot(data=interval_steps, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("5-minute intervals") +
  ylab("Average number of steps taken")
```

![plot of chunk unnamed-chunk-4](./figure/unnamed-chunk-4-1.png)

## Imputing missing values


``` r
missing <- !complete.cases(data)

## impute missing steps with interval averages across days

imputed_data <- data %>%
  mutate(
    steps = case_when(
      is.na(steps) ~ interval_steps$steps[match(data$interval, interval_steps$interval)],      
      TRUE ~ as.numeric(steps)
    ))

imputed_total_steps <- imputed_data %>% group_by(date) %>% summarise(daily_steps = sum(steps))

ggplot(imputed_total_steps, aes(daily_steps)) + 
  geom_histogram(binwidth = 2000) + 
  xlab("Total number of steps taken each day") + 
  ylab("Frequency")
```

![plot of chunk unnamed-chunk-5](./figure/unnamed-chunk-5-1.png)

``` r
imputed_mean = mean(imputed_total_steps$daily_steps, na.rm=TRUE)
imputed_median = median(imputed_total_steps$daily_steps, na.rm=TRUE)

mean_diff <- imputed_mean - meanStepsPerDay 
median_diff <- imputed_median - medianStepsPerDay
```

## Are there differences in activity patterns between weekdays and weekends?


``` r
library(lubridate)

day_of_week <- imputed_data %>%
  mutate(
    date = ymd(date),
    weekday_or_weekend = case_when(wday(date) %in% 2:6 ~ "Weekday",
                                   wday(date) %in% c(1,7) ~ "Weekend")
  ) %>% select(-date) %>%
  group_by(interval, weekday_or_weekend) %>%
  summarise(
    steps = mean(steps)
  )
```

```
## `summarise()` has grouped output by 'interval'. You can override using the `.groups`
## argument.
```

``` r
ggplot(day_of_week, aes(interval, steps)) + 
  geom_line() + 
  facet_wrap(~weekday_or_weekend, nrow = 2) +
  scale_fill_brewer(palette="BrBG") +
  theme_bw() + guides(fill=FALSE) +
  xlab("5-Minute intervals") + 
  ylab("Average number of steps")
```

![plot of chunk unnamed-chunk-6](./figure/unnamed-chunk-6-1.png)


