knit---
output:
  html_document: default
  word_document: default
---

#Reproducible Research Project Work

##Loading and Processing the data
================================
####1. Load the data(i.e.read.csv())

```r
if(!file.exists('activity.csv')){
unzip('activity.zip')
}
activityData<-read.csv('activity.csv')
```
####2. Process/Transform the data into a format suitable for analysis

```r
#activityData$interval<-strptime(gsub("([0-9]{1,2})","\\1:\\2".activityData$interval),format='%H:%M')
```

##What is mean total number of steps taken per day?
=================================================

```r
stepsByDay<-tapply(activityData$steps,activityData$date,sum,na.rm=TRUE)
####1.Make Histogram of the total number of steps taken each day
qplot(stepsByDay,xlab='Total Steps per Day',ylab='Frequency using binwidth 500',binwidth=500)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)
####2.Calculate and report mean and median total number of steps taken per day

```r
stepsByDaymean<-mean(stepsByDay)
stepsByDaymedian<-median(stepsByDay)
```
*Mean:'r stepsByDaymean'
*Median:'r stepsByDaymedian'

##What is the average daily activity Pattern?
===========================================

```r
averageStepsPerTimeBlock<-aggregate(x=list(meanSteps=activityData$steps),
by=list(interval=activityData$interval),FUN=mean,na.rm=TRUE)
```
####1.Make a time series plot

```r
ggplot(data=averageStepsPerTimeBlock,aes(x=interval,y=meanSteps))+
geom_line()+
xlab("5-minute interval")+
ylab("average number of steps taken")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)
####2.Which 5 minutes interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
mostSteps<-which.max(averageStepsPerTimeBlock$meanSteps)
timeMostSteps<-gsub("([0-9]{1,2})([0-9]{2})","\\1:\\2",averageStepsPerTimeBlock[mostSteps,'interval'])
```
*MostStepsat:'r timeMostSteps'

##Imputing Missing Values
=======================
####1.Calculate and Report the total number of missing values in the dataset

```r
numMissingValues<-length(which(is.na(activityData$steps)))
```
*Number of missing values : 'r numMissingValues'
####2.Device a strategy for filling in all of the missing values in the dataset
####3.Create a new dataset that is equal to the original dataset but with the missing data filledin

```r
activityDataImputed<-activityData
activityDataImputed$steps<-impute(activityData$steps,FUN=mean)
```
####4.Make a histogram of the total number of steps taken each day

```r
stepsByDayImputed<-tapply(activityDataImputed$steps,activityDataImputed$date,sum)
qplot(stepsByDayImputed,xlab='Total steps per day (Imputed)',
ylab='Frequency using binwidth 500', binwidth=500)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)
####... and calculate and report the mean and median total number of steps taken per day

```r
stepsByDayMeanImputed<-mean(stepsByDayImputed)
stepsByDayMedianImputed<-median(stepsByDayImputed)
```
*Mean(Imputed):'r stepsByDayMeanImputed'
*Median(Imputed):'r stepsByDayMedianImputed'

##Are there differences in activity patterns between weekdays and weekends?
####1.Create a new factor variable in the dataset with two levels "weekday"and "weekend" indicating whether a given data is a weekday or weekend day

```r
activityDataImputed$dateType<-ifelse(as.POSIXlt(activityDataImputed$date)$wday%in%c(0,6),'weekend','weekday')
```
####2.Make a panel plot containing a time series plot

```r
averageActivityDataImputed<-aggregate(steps~interval+dateType,data=activityDataImputed,mean)
ggplot(averageActivityDataImputed,aes(interval,steps))+
geom_line()+
facet_grid(dateType~.)+
xlab("5-minute interval")+
ylab("average number of steps")
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png)
