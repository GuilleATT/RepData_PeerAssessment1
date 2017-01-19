# Report PeerAssessment1
Guillermo ATT  
11 de enero de 2017  



## 1. Code for reading in the data and/or processing the data


```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.3.2
```

```r
unzip("activity.zip")
data<-read.csv("activity.csv",stringsAsFactors = FALSE)
data$date<-as.Date(data$date,format="%Y-%m-%d")
data$interval<-as.factor(data$interval)
## The structure of the data frame is shown below
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```


## 2. Histogram of the total number of steps each day

```r
## We first load the neccesary library
library(ggplot2)
stepseachday<-tapply(data$steps,data$date,sum)
qplot(stepseachday,binwidth=500)+labs(title="Histogram of Steps Taken per Day",  x = "Number of Steps per Day", y = "Days Count")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
str(stepseachday)
```

```
##  int [1:61(1d)] NA 126 11352 12116 13294 15420 11015 NA 12811 9900 ...
##  - attr(*, "dimnames")=List of 1
##   ..$ : chr [1:61] "2012-10-01" "2012-10-02" "2012-10-03" "2012-10-04" ...
```

```r
typeof(stepseachday)
```

```
## [1] "integer"
```

## 3. Mean and median number of steps taken

```r
mean(stepseachday,na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
median(stepseachday,na.rm=TRUE)
```

```
## [1] 10765
```

## 4. Timeserie plot of the average number of steps taken for each 5 min interval

```r
steps_per_interval<-tapply(data$steps,data$interval,mean,na.rm=TRUE)
x<-unlist(attr(steps_per_interval,"dimnames"))
plot(x,steps_per_interval,
     type="l",xlab="Interval",ylab="Steps",
     main="Daily activity pattern for average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

## 5. The 5 min interval that, on average, contains the max number of steps

```r
max<- which.max(steps_per_interval)
names(steps_per_interval[max])
```

```
## [1] "835"
```
**835** is the 5 min interval with the maximum number of steps on average that are **206.1698113**

## 6. Code to describe and show a strategy for imputing missing data
For imputing missing data we can do it by assigning the mean value of the same interval.
First, we will calculate the total number of missing values

```r
missing<-sum(is.na(data$steps))
percentage<-mean(is.na(data$steps))
```
The number of missing values is **2304**. This represents a **0.1311475** % of the total amount of data.

We now proceed to impute the missing values with the mean values for the same intervals

```r
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (steps_per_interval[interval])
    return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)
str(filled.data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
```
We can check that there are no missing values

```r
sum(is.na(filled.data$steps))
```

```
## [1] 0
```


## 7. Histogram of the total number of steps taken each day after missing values are imputed

```r
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

## 8. Panel plot comparing the average number of steps taken per 5 minute interval across weekdays and weekends

```r
weekdayorweekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("lunes", "martes", "miércoles", "jueves", "viernes"))
        return("weekday")
    else if (day %in% c("sábado", "domingo"))
        return("weekend")
    else
        stop("invalid date")
}
filled.data$day <- sapply(filled.data$date, FUN=weekdayorweekend)
table(filled.data$day)
```

```
## 
## weekday weekend 
##   12960    4608
```

```r
str(filled.data)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: Factor w/ 288 levels "0","5","10","15",..: 1 2 3 4 5 6 7 8 9 10 ...
##  $ day     : chr  "weekday" "weekday" "weekday" "weekday" ...
```



```r
averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(averages, aes(interval, steps,group=1)) + geom_line() + facet_grid(day ~ .)+ scale_x_discrete(breaks=c(0,400,800,1200,1600,2000,2400)) +  xlab("5-minute interval") + ylab("Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->
