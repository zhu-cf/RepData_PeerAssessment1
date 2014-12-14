# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip("./activity.zip")
activity = read.csv("activity.csv")
```


## What is mean total number of steps taken per day?
1. Historgram of the total number of steps taken each day


```r
NAfreeactivity <- activity[!is.na(activity$steps) & !is.na(activity$date), ]
stepsEachDay = tapply(NAfreeactivity$steps,NAfreeactivity$date,sum)
stepsEachDay = stepsEachDay[!is.na(stepsEachDay)]
hist(stepsEachDay)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

2. The mean and median total number of steps taken per day

```r
mean(stepsEachDay)
```

```
## [1] 10766.19
```

```r
median(stepsEachDay)
```

```
## [1] 10765
```
## What is the average daily activity pattern?
1. Time series plot

```r
meanOverIntervals = tapply(NAfreeactivity$steps,NAfreeactivity$interval,mean)
intervalList = unique(NAfreeactivity$interval)
plot(intervalList,meanOverIntervals,type = "l", xlab = "Interval",ylab = "Steps averaged over all dats")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

2. The following 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps.

```r
intervalList[meanOverIntervals == max(meanOverIntervals)]
```

```
## [1] 835
```


## Imputing missing values
1. calculate total number of NA

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```
2&3. replace NA with mean
I don't know how to do it in the smart way,
so I checked via bash to identify the dates with NA value and confirmed that only on these days, steps are NA and they spread over all intervals of that specific day, so here's the code


```r
activityR = activity
index1 = activityR$date == "2012-10-01"
index2 = activityR$date == "2012-10-08"
index3 = activityR$date == "2012-11-01"
index4 = activityR$date == "2012-11-04"
index5 = activityR$date == "2012-11-09"
index6 = activityR$date == "2012-11-10"
index7 = activityR$date == "2012-11-14"
index8 = activityR$date == "2012-11-30"
activityR$steps[index1] = mean(NAfreeactivity$steps)
activityR$steps[index2] = mean(NAfreeactivity$steps)
activityR$steps[index3] = mean(NAfreeactivity$steps)
activityR$steps[index4] = mean(NAfreeactivity$steps)
activityR$steps[index5] = mean(NAfreeactivity$steps)
activityR$steps[index6] = mean(NAfreeactivity$steps)
activityR$steps[index7] = mean(NAfreeactivity$steps)
activityR$steps[index8] = mean(NAfreeactivity$steps)
```
4. 

```r
Replaced_stepsEachDay = tapply(activityR$steps,activityR$date,sum)
hist(Replaced_stepsEachDay)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

```r
mean(Replaced_stepsEachDay)
```

```
## [1] 10766.19
```

```r
median(Replaced_stepsEachDay)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
1. time series plot

```r
Days = weekdays(as.Date(activityR$date))
for(i in 1:length(Days)){
        if(Days[i] == "Saturday" | Days[i] == "Sunday"){
                Days[i] = "weekend"
        }else{
                Days[i] = "weekday"
        }
}
Days = as.factor(Days)
activityRDays = cbind(activityR,Days)
splitDays = split(activityRDays,activityRDays$Days)
activity_weekday = splitDays[[1]]
activity_weekend = splitDays[[2]]

meanOverIntervals_wd = tapply(activity_weekday$steps,activity_weekday$interval,mean)
meanOverIntervals_we = tapply(activity_weekend$steps,activity_weekend$interval,mean)

par(mfcol = c(2,1),mar = c(4,4,2,2))
plot(intervalList,meanOverIntervals_we,type = "l",xlab = "",ylab ="" , main = "weekend")
plot(intervalList,meanOverIntervals_wd,type = "l",xlab = "interval",ylab = "Number of steps",main = "weekday",mar = c(2,4,2,2)) 
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

2. The following 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps.

```r
meanOverIntervals_Replaced = tapply(activityRDays$steps,activityRDays$interval,mean)
intervalList[meanOverIntervals_Replaced == max(meanOverIntervals_Replaced)]
```

```
## [1] 835
```
