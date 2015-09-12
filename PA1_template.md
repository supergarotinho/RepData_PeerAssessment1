# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
activityFile <- unz("activity.zip","activity.csv")
activityData <- read.csv(activityFile, colClasses = c("numeric","Date","numeric"))
```


## What is mean total number of steps taken per day?

```r
## Removing NA's values as they are unkown values - CAN NOT BE TREATED AS ZERO
activityDataWithoutNAs = activityData[!is.na(activityData$steps),]

stepsByDay = aggregate(activityDataWithoutNAs$steps,
                       by=list(date=activityDataWithoutNAs$date), FUN=sum)
hist(stepsByDay$x,main = "Histogram of total number os steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
## Calculating the mean value
mean(stepsByDay$x)
```

```
## [1] 10766.19
```

```r
## Calculating the median value
median(stepsByDay$x)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

Aggregating and removing the NA's in the same function (Just to train in a different way!)

```r
avgStepsByTime <- aggregate(steps ~ interval, data = activityData, mean, na.rm=TRUE)

## Plotting the result
plot(avgStepsByTime,type="l",main="Average daily steps by time interval",ylab="Agerave steps taken across all days")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avgStepsByTime[avgStepsByTime$steps == max(avgStepsByTime$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```


## Imputing missing values
The total number of missing values in the dataset (i.e. the total number of rows with NAs)



## Are there differences in activity patterns between weekdays and weekends?
