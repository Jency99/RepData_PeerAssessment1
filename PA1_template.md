---
title: "Untitled"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Reproducible Research:  Assignment 1

## Loading and preprocessing of the given data
```{r data}
fils<-"C:/Users/jency/OneDrive/Documents/activity.csv"
assi<-read.csv(fils)
```

## Mean total number of steps taken per day?
```{r}
library(ggplot2)
totalsteps <- tapply(assi$steps, assi$date, FUN=sum, na.rm=TRUE)
qplot(totalsteps, binwidth=1000, xlab="total number of steps taken each day")
mean(totalsteps, na.rm=TRUE)
median(totalsteps, na.rm=TRUE)
```

## The average daily activity pattern?
```{r}
library(ggplot2)
averages <- aggregate(x=list(steps=assi$steps), by=list(interval=assi$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken")
```

On average across all the days in the dataset, the 5-minute interval contains
the maximum number of steps?
```{r}
averages[which.max(averages$steps),]
```

## Imputing missing values

```{r how_many_missing}
missing <- is.na(assi$steps)
# No of missing data
table(missing)
```

```{r}
# Replace each missing value with the mean value of its 5-minute interval
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averages[averages$interval==interval, "steps"])
    return(filled)
}
filled.data <- assi
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)
```
a histogram of the total number of steps taken each day and calculate the mean and median total number of steps.

```{r}
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
mean(total.steps)
median(total.steps)
```


## Are there differences in activity patterns between weekdays and weekends?


```{r}
weekdayorweekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekdayorweekend)
```

a panel plot containing plots of average number of steps taken
on weekdays and weekends.
```{r}
averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")
```