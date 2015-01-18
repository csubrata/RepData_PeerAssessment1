# Reproducible Research: Peer Assessment 1
## Set global variables
```r{set envionments}
library(knitr)

opts_chunk$set(echo=TRUE, results='asis')
```

## Loading and preprocessing the data
```r{set working directory, echo = FALSE}

setwd("Z:/Coursera/Data Science/05 - Repoducible Research/Week 2/Project 1/RepData_PeerAssessment1")

```

```r{read csv file, echo = TRUE}
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
activity <- read.csv('activity.csv')

activity$date <- as.Date(activity$date, "%Y-%m-%d")
```


## What is mean total number of steps taken per day?
```r{mean of total number of steps}
total.steps <- with(activity,tapply(steps, date, sum, na.rm=TRUE))
total.mean <- mean(total.steps)
total.median <- median(total.steps)
library(ggplot2)
qplot(total.steps, xlab='Total steps', ylab='Frequency', binwidth=1000) +
geom_vline(xintercept = total.mean, colour="red") + 
geom_text(aes(x=total.mean, label="\nMean", y=1), colour="red", angle=90, text=element_text(size=12)) +
geom_vline(xintercept = total.median, colour="green") +
geom_text(aes(x=total.median, label="\nMedian", y=1), colour="green", angle=90, text=element_text(size=12)) 

```


## What is the average daily activity pattern?
```r{average daily activity pattern}
average.steps <- with(activity,tapply(steps, interval, mean, na.rm=TRUE))

plot(row.names(average.steps), average.steps, type = "l", xlab = "5-min interval", ylab = "Average across all Days", main = "Average number of steps taken", col = "red")

max.interval <- as.numeric(names(which.max(average.steps)))
abline(v=max.interval,col="blue")
mtext(side=3,line=-2,adj=1," -- Max Interval",col="blue")
```

## Imputing missing values

# How many missing
```r{missing steps}
activity.missing <- sum(is.na(activity$steps))
```

## Are there differences in activity patterns between weekdays and weekends?
