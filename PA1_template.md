---
title: "Reproducible Research- Peer Assessment 1"
output: html_document
author: Julia Breitenbruch
date: March 6th, 2016
---

## Reproducible Research- Peer Assessment 1

#### Julia Breitenbruch (March 6th, 2016)

### Loading and preprocessing of activity data

```r
activity<-read.csv("activity.csv",header=TRUE,sep=",")
activity$date<-as.Date(activity$date,format="%Y-%m-%d")
```

### Mean and median of total steps per day


```r
library(dplyr)
 TotalSteps<-summarize(group_by(activity,date),totsteps=sum(steps))
 head(TotalSteps)
```

```
## Source: local data frame [6 x 2]
## 
##         date totsteps
##       (date)    (int)
## 1 2012-10-01       NA
## 2 2012-10-02      126
## 3 2012-10-03    11352
## 4 2012-10-04    12116
## 5 2012-10-05    13294
## 6 2012-10-06    15420
```

```r
 meanSteps<-summary(TotalSteps$totsteps)["Mean"]
 meanSteps
```

```
##  Mean 
## 10770
```

```r
 medianSteps<-summary(TotalSteps$totsteps)["Median"]
 medianSteps
```

```
## Median 
##  10760
```

### Histogram of steps taken each day


```r
library(ggplot2)
TotalStepsPlot<-ggplot(TotalSteps,aes(x=totsteps))
TotalStepsPlot<-TotalStepsPlot+geom_histogram(bins=40)
print(TotalStepsPlot)
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![plot of chunk histogram](figure/histogram-1.png)

### Average daily pattern

```r
library(dplyr)
AverageSteps<-summarize(group_by(activity,interval),avsteps=mean(steps,na.rm=TRUE))
head(AverageSteps)
```

```
## Source: local data frame [6 x 2]
## 
##   interval   avsteps
##      (int)     (dbl)
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

### Time interval with the maximal counts of steps

```r
MaxSteps<-summary(AverageSteps$avsteps)["Max."]
MaxSteps
```

```
##  Max. 
## 206.2
```

```r
MaxInterval<-subset(AverageSteps,AverageSteps$avsteps>200)
MaxInterval$interval
```

```
## [1] 835
```
#### Result: The 5-minute-interval, which starts at 8.35 a.m., contains the maximal steps (206.2) on average across all the dates in the dataset.
### Time series plot

```r
library(ggplot2)
AverageStepsPlot<-ggplot(AverageSteps,aes(interval,avsteps))
AverageStepsPlot<-AverageStepsPlot+geom_line()
print(AverageStepsPlot)
```

![plot of chunk time_series](figure/time_series-1.png)
### Imputing missing values

```r
table(complete.cases(activity))
```

```
## 
## FALSE  TRUE 
##  2304 15264
```

Result: There are 2304 "NA"-values in the data set.
#### Imputing strategy: Impute missing values with the average number of steps for the respective interval across all the dates in the dataset:

```r
ImputedData<-activity
ImputedData$steps<-ifelse(is.na(ImputedData$steps),
 AverageSteps$avsteps[match(ImputedData$interval,AverageSteps$interval)],ImputedData$steps)
table(complete.cases(ImputedData))
```

```
## 
##  TRUE 
## 17568
```
Result: ImputedData does not contain any "NA"-values.

```r
library(dplyr)
 TotalStepsImputed<-summarize(group_by(ImputedData,date),totsteps=sum(steps))
 head(TotalStepsImputed)
```

```
## Source: local data frame [6 x 2]
## 
##         date totsteps
##       (date)    (dbl)
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```
### Histogram with imputed data 

 
 ```r
 library(ggplot2)
 TotalStepsImputedPlot<-ggplot(TotalStepsImputed,aes(x=totsteps))
 TotalStepsImputedPlot<-TotalStepsImputedPlot+geom_histogram(bins=40)
 print(TotalStepsImputedPlot)
 ```
 
 ![plot of chunk histogram_imputed](figure/histogram_imputed-1.png)
### Mean and median of total steps per day (imputed data)


```r
library(dplyr)
 TotalImpSteps<-summarize(group_by(ImputedData,date),totsteps=sum(steps))
 head(TotalImpSteps)
```

```
## Source: local data frame [6 x 2]
## 
##         date totsteps
##       (date)    (dbl)
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

```r
 meanSteps<-summary(TotalImpSteps$totsteps)["Mean"]
 meanSteps
```

```
##  Mean 
## 10770
```

```r
 medianSteps<-summary(TotalImpSteps$totsteps)["Median"]
 medianSteps
```

```
## Median 
##  10770
```

```r
  SumNA<-summarize(group_by(activity,date),totna=sum(is.na(steps)))
 head(table(SumNA))
```

```
##             totna
## date         0 288
##   2012-10-01 0   1
##   2012-10-02 1   0
##   2012-10-03 1   0
##   2012-10-04 1   0
##   2012-10-05 1   0
##   2012-10-06 1   0
```

```r
 SumNA<-mutate(SumNA,nacat=1*(totna==288))
 TotalNADays<-summarize(SumNA,nadays=sum(nacat))
 TotalNADays
```

```
## Source: local data frame [1 x 1]
## 
##   nadays
##    (dbl)
## 1      8
```
Result: The mean has not changed. Furthermore,the above calculations show the following scenario:If any day has "NA"- entries, it has no other entries, in other words. The days in the dataset consist either exclusively of missing values or do not have any missing values at all.
So every "imputed" day has a value of 10766.19, and there are 8 such days altogether. As the dataset consists of 61 days altogether, we have for the "imputed" mean:

```r
newmean<-round((53*10770+8*10766.19)/61,1)
newmean
```

```
## [1] 10769.5
```
Hence, the fact that the imputed value is slightly below the old mean has almost no effect, taken into consideration that only 8 days were imputed, but 53 did not.Contrarily the median changes a little bit: Here the imputation has an effect: The imputated values are slightly higher than the "old" median, but as the median measures a rank, this has an effect in spite of the low number of imputed days.

### Differences in activity patterns between weekdays and weekends

```r
activity$day<-weekdays(activity$date)
weekday<-c("Montag","Dienstag","Mittwoch","Donnerstag","Freitag")
weekend<-c("Samstag","Sonntag")
activity$weekpart<-ifelse(activity$day %in% weekday, "weekday","weekend"
    )
act1<-subset(activity,weekpart=="weekday")
act2<-subset(activity,weekpart=="weekend")
AvSteps1<-mutate(summarize(group_by(act1,interval),avsteps=mean(steps,na.rm=TRUE)),weekpart="weekday")
AvSteps2<-mutate(summarize(group_by(act2,interval),avsteps=mean(steps,na.rm=TRUE)),weekpart="weekend")
AvSteps<-rbind(AvSteps1,AvSteps2)
```
### Creating facet plot

```r
library(ggplot2)
AvStepsPlot<-ggplot(AvSteps,aes(interval,avsteps))
AvStepsPlot<-AvStepsPlot+geom_line()
AvStepsPlot<-AvStepsPlot+facet_grid(weekpart~.  )
print(AvStepsPlot)
```

![plot of chunk facet_plot](figure/facet_plot-1.png)
