### Peer Assessment 1

#### Loading and preprocessing the data



```r
library(ggplot2)
library(lattice)
data <- read.csv(file="activity.csv",head=TRUE,sep=",",stringsAsFactors=FALSE)
dat <- as.Date(data$date,format="%Y-%m-%d")
data$date <- dat
data["daytype"] <- NA
for (x in 1:nrow(data))
{
  if ((weekdays(data[x,2]) == "Saturday") | (weekdays(data[x,2]) == "Sunday")) { data[x,4] = "weekend" } else  {data[x,4] = "weekday"}
}
dat.f <- factor(data$daytype)
data$daytype <- dat.f
ag <- aggregate(steps ~ date, data = data, FUN = sum)
agint <- aggregate(steps ~ interval, data = data, FUN = mean)
agint2 <- aggregate(steps ~ interval +daytype, data = data, FUN = mean)
```



#### What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

*1.Make a histogram of the total number of steps taken each day*


```r
ggplot(ag, aes(x=ag$step)) + geom_histogram(colour="black", fill="white",binwidth=800)+
xlab("Steps per Day")
```

![plot of chunk unnamed-chunk-2](./PA1_template_files/figure-html/unnamed-chunk-2.png) 

*2.Calculate and report the mean and median total number of steps taken per day*

##### Mean

```r
mean(ag$steps)
```

```
## [1] 10766
```
##### Median

```r
median(ag$steps)
```

```
## [1] 10765
```


#### What is the average daily activity pattern?

*1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)*

```r
ggplot(agint, aes(x=agint$interval,y=agint$steps)) +geom_line()+ylab("Steps")+ xlab("5-minute Intervals")
```

![plot of chunk unnamed-chunk-5](./PA1_template_files/figure-html/unnamed-chunk-5.png) 

*2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?*


```r
agint3 <- aggregate(steps ~ date +interval , data = data, FUN = mean,na.rm=TRUE)
index <- match(max(agint3$steps),agint3$steps)
agint3[index,2]
```

```
## [1] 615
```

#### Imputing missing values ####

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
colSums(is.na(data[, c(1:3)])) 
```

```
##    steps     date interval 
##     2304        0        0
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

*Strategy - replace NA by the average of the 5-minute interval*



3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
data1 <-data

for (i in 1:nrow(data1)) 
{
  if (is.na(data1[i,1]))
{
    index <- match(data1[i,3],agint$interval)
    data1[i,1] <- agint[index,2]
  }
}
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
agimpute <- aggregate(steps ~ date, data = data1, FUN = sum)
ggplot(agimpute, aes(x=agimpute$step)) + geom_histogram(colour="black", fill="white",binwidth=800)+
xlab("Steps per Day")
```

![plot of chunk unnamed-chunk-9](./PA1_template_files/figure-html/unnamed-chunk-9.png) 

##### Mean


```r
mean(agimpute$steps)
```

```
## [1] 10766
```
##### Median

```r
median(agimpute$steps)
```

```
## [1] 10766
```
*Conclusion - the mean and median indicate that adding the missing values had minimal effect*

####Are there differences in activity patterns between weekdays and weekends?####

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:


```r
xyplot(steps ~ interval|daytype, agint2, panel = panel.lines,layout = c(1,2))
```

![plot of chunk unnamed-chunk-12](./PA1_template_files/figure-html/unnamed-chunk-12.png) 

*Conclusion - It would appear that more steps were taken on the week-ends vs week-days*

