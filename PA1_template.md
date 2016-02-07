---
title: "Week1 Assignment"
author: "Kyu Cho"
date: "February 6, 2016"
output: html_document
---
# Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

# Variables
- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
- date: The date on which the measurement was taken in YYYY-MM-DD format
- interval: Identifier for the 5-minute interval in which measurement was taken
```{r cache=T, warning=F, message=F}
library(ggplot2)
library(lubridate) # Time stamp
library(mosaic)
library(caret)
```

## 1) Code for reading in the dataset and/or processing the data
```{r cache=T, warning=F, message=F}
setwd("E:/Google Drive/College/1-Data Science/2-Reproducible Research")
data = read.csv("activity.csv")
summary(data)

data$date = as.POSIXct(strptime(data$date, "%Y-%m-%d"))
data$day = day(data$date)
data$weekdays = weekdays(data$date)
str(data)
```

## 2) Histogram of the total number of steps taken each day
```{r cache=T, warning=F, message=F}
data2 = tally(group_by(data, steps, day))
head(data2)
tail(data2)
ggplot(data2, aes(day)) + geom_bar()
table(data2$day)
```

## 3) Mean and median number of steps taken each day
```{r cache=T, warning=F, message=F}
data2 = aggregate(steps~day, data2, mean)
data2$steps = round(data2$steps, digits=2)
data2
```

## 4) Time series plot of the average number of steps taken
```{r cache=T, warning=F, message=F}
ggplot(data2, aes(x=day, y=steps)) + 
     geom_line(size=1) +
     ylab("Average number of steps")
```

## 5) The 5-minute interval that, on average, contains the maximum number of steps
```{r cache=T, warning=F, message=F}
data2 = aggregate(steps~interval, data, mean)
head(data2)
tail(data2)

which.max(data2$steps)
data2[104,]
```

## 6) Code to describe and show a strategy for imputing missing data
```{r cache=T, warning=F, message=F}
# Build the regression model to predict the missing value
mdl_lm = lm(steps~day+weekdays+interval, data=data)
pred_lm = predict(mdl_lm, data)

# Write a function that return prediction value if it's missing or ragular value
impute = function (a, a.impute){
     ifelse (is.na(a), a.impute, a)
}
data$steps = impute(data$steps, pred_lm)
summary(data)
```

## 7) Histogram of the total number of steps taken each day after missing values are impute
```{r cache=T, warning=F, message=F}
data_hist = tally(group_by(data, steps, day))
head(data_hist)
tail(data_hist)

ggplot(data_hist, aes(day)) + geom_bar()
table(data_hist$day)
```


## 8) Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
```{r cache=T, warning=F, message=F}
data_mrged = merge(data, data2, by=c("interval")) 
colnames(data_mrged)[2] = c("steps")
colnames(data_mrged)[6] = c("avgSteps")
head(data_mrged)
tail(data_mrged)

ggplot(data_mrged, aes(x=interval, y=avgSteps)) + 
     geom_line() +
     facet_wrap(~weekdays, nrow=2) +
     ylab("Average number of steps")
```

