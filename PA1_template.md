---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
unzip("activity.zip")
x<-read.csv("activity.csv")
library(dplyr)
library(ggplot2)
d<-summarise(group_by(x,date),p = sum(steps))
c<-na.omit(d)

## What is mean total number of steps taken per day?
ggplot(c, aes(x=p)) + geom_histogram(binwidth = 2000)
summary(c$p)


## What is the average daily activity pattern?
v<-summarise(group_by(x,interval),h = mean(steps, na.rm = TRUE))
ggplot(v, aes(x=interval,y=h)) + geom_col()

v<-summarise(group_by(x,interval),t = sum(steps, na.rm = TRUE))
ggplot(v, aes(x=interval,y=t)) + geom_col()



## Imputing missing values
sum(!complete.cases(x))

for (i in 1:17568) {
  if (is.na(x[i,1])) { 
    for (a in 1:288) {
      if (x[i,3]==v[a,1]) {
        x[i,1]<-v[a,2]}
      }
    }
}


## Are there differences in activity patterns between weekdays and weekends?

z$Days<-z$date
z$Days<-as.Date(z$Days)
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
z$Days <- factor((weekdays(z$Days) %in% weekdays1), levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))
v5<-summarise(group_by(z,interval,date),h5 = mean(steps, na.rm = TRUE))
ggplot(subset(v5,Days %in% "weekend"), aes(x=interval,y=h5))+geom_point()

