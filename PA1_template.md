# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
```r{set working directory, echo = FALSE}
setwd("Z:/Coursera/Data Science/05 - Repoducible Research/Week 2/Project 1/RepData_PeerAssessment1")
```
```r{read csv file, echo = TRUE}
activity <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?
```r{mean of total number of steps}
mean(activity$steps, na.rm=TRUE)
```


## What is the average daily activity pattern?
```r{average daily activity pattern}
mean(activity$interval, na.rm=TRUE)
```

## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
