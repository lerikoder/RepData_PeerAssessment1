---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
# Reproducible Research: Peer Assessment 1  
## Author: Leocadio Rivas  
  

## Loading the data

```{r loadingData, echo = TRUE}
    activity <- read.csv("./activity.csv")
    "This is the sumary of the data"
    summary(activity)
```               
  
## Preprocessing the data  
- Dates come as class string, mut be converted to class Date  

```{r convertingDates, echo = TRUE}
    activity$date <- as.Date(activity$date)
```
  
- There are NA in the stepsByDay  
```{r lookingNAsteps, echo = TRUE}
    head(activity[is.na(activity$steps),], 20)
```
  These NA are going to be repleaced by the mean of steps
```{r convertingNAsteps, echo = TRUE}
    newActivity <- activity
    media <- mean(newActivity$steps, na.rm = TRUE)
    len <- length(newActivity$steps)
    for(i in 1:len){
        if(is.na(newActivity$steps[i])) newActivity$steps[i] <- media
    }
    head(newActivity[is.na(newActivity$steps),], 20)


```

```{r calculatingStepsByDay, echo = TRUE}
stepsByDay <- function(){
    actNew <- activity
    actNew$date <- as.Date(actNew$date)
    j <- 1
    date <- actNew$date[1]
    tSteps <- 0
    sumSteps <- 0
    cdate <- actNew$date[1]
    csteps <- 0
    if(!is.na(actNew$steps[1]))
        csteps <- actNew$steps[1]
    # CALCULATION LOOP
    lenAct <<- dim(actNew)[1]
    for(i in 2:lenAct){
        if(cdate == actNew$date[i]){
            if(!is.na(actNew$steps[i])){
                csteps <- csteps + actNew$steps[i]
            }
        }
        else{
                date[j] <- cdate
                tSteps[j] <- csteps
                cdate <- actNew$date[i]
                csteps <- 0
                if(!is.na(actNew$steps[i])){
                    csteps <- actNew$steps[i]
                }
                j <- j + 1
            }
    }
    date[j] <- cdate
    tSteps[j] <- csteps
    data.frame(date = date, steps = tSteps)
}
sbd <- stepsByDay()
summary(sbd)
library(graphics)
plot(sbd$date, sbd$steps, type = "l", main = "Steps by day", ylab = "Steps", xlab  = "Date")
```

## What is mean total number of steps taken per day?  
The *mean total number of steps* by day is the sum of all steps  
divided by the number of days  

```{r calculatingMeanStepsByDay, echo = TRUE, cache = TRUE}
    library(chron)
    diffDate <- function(desde, hasta){
        fdesde <- as.chron(desde)
        fhasta <- as.chron(hasta)
        1 + fhasta - fdesde
    }      
    totalDays <- diffDate(min(activity$date), max(activity$date))
    sumSteps <- sum(activity$steps, na.rm = TRUE)
    meanStepsDaily <- sumSteps / totalDays
    paste("Days:", totalDays, "Steps:", sumSteps, "Mean Steps Daily:", meanStepsDaily)

```

## What is the average daily activity pattern?
## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
```{r calculatingPatternWdWe, echo = TRUE, cache = TRUE}
        dayOfWeek <- weekdays(actNew$date) != "Saturday" & weekdays(actNew$date) != "Sunday"
        fWeekOfDay <- factor(dayOfWeek, labels = c("weekend", "weekday"))
        actNew <- cbind(actNew, fWeekOfDay)
        plot(actNew$interval[actNew$fWeekOfDay == "weekend"], actNew$steps[actNew$fWeekOfDay == "weekend"], 
        type = "l", main = "Pattern WeekEnd", xlab = "Interval", ylab = "Steps")
        plot(actNew$interval[actNew$fWeekOfDay == "weekday"], actNew$steps[actNew$fWeekOfDay == "weekday"], 
        type = "l", main = "Pattern WeekDay", xlab = "Interval", ylab = "Steps")
```


