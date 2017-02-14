``` r
knitr::opts_chunk$set( fig.path = "images/")
```

Reproducible Research: Peer Assessment 1
========================================

Loading the file
----------------

``` r
setwd('M:/03_Plan_net_media/13_Insights/02_Intelligence/04_Interne_Projekte/Melanie/Coursera/Kurs 5/Assigment1')
data <- read.csv("activity.csv")
```

### Calculate and report the total number of missing values in the dataset

And filled it with missing data

``` r
data_na <- na.omit(data)
gesamt_data_na <- aggregate(steps ~ date, data_na, sum)
```

What is mean total number of steps taken per day?
=================================================

``` r
hist(
        gesamt_data_na$steps, 
        main = "total number of steps taken each day", 
        xlab="Steps per day", 
        ylab="Frequency", 
        col = "blue", 
        breaks = 15
)
```

![](images/unnamed-chunk-3-1.png)

### mean /median

``` r
meanx <- mean(gesamt_data_na$steps)
print(meanx)
```

    ## [1] 10766.19

``` r
median <- median(gesamt_data_na$steps)
print(median)
```

    ## [1] 10765

What is the average daily activity pattern?
===========================================

Print a series Plot. At first aggregate the date for series.

``` r
serien <- aggregate(steps ~ interval, data = data, FUN = function(x) {
        mean(x, na.rm = TRUE)})
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.3.2

``` r
qplot(interval,steps, data=serien,geom="line",main ="5-minute interval")
```

![](images/unnamed-chunk-5-1.png) Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
serien[which.max(serien$steps),]
```

    ##     interval    steps
    ## 104      835 206.1698

Imputing missing values
=======================

1.  Calculate the total number.

``` r
na<-sum(is.na(data))
part<-paste(round(100*(na/nrow(data)), 3), "%")
print(part)
```

    ## [1] "13.115 %"

1.  Devise a strategy for filling in all of the missing values in the dataset...

``` r
full_data <- data
```

Replace each missing value with the mean value.

``` r
fuehlenMean <- function(steps, interval) {
        fuehle <- NA
        if (!is.na(steps))
                fuehle <- c(steps)
        else
                fuehle <- (serien[serien$interval==interval, "steps"])
        return(fuehle)
}
full_data$steps <- mapply(fuehlenMean, full_data$steps, full_data$interval)
```

Make a histogram of the total number.

``` r
total <- tapply(full_data$steps, full_data$date, sum, na.rm=TRUE)
hist(total, main = 'total number of steps taken each day', col = "red")
```

![](images/unnamed-chunk-10-1.png) \#\#\# mean/median of the steps

``` r
mean1 <- mean(total, na.rm= TRUE)
print(mean1)
```

    ## [1] 10766.19

``` r
median1 <- median(total, na.rm= TRUE)
print(median1)
```

    ## [1] 10766.19

Are there differences in activity patterns between weekdays and weekends
========================================================================

Create a new weekday/Weekend variable in a df.

``` r
df_new <- data
df_new$datum <- as.Date(strptime(df_new$date, format="%Y-%m-%d")) 
df_new$tag <-  factor(ifelse(as.POSIXlt(df_new$date)$wday %in% c(0,6), 'weekend', 'weekday'))
```

Aggregate the steps to the different days.

``` r
week_average_df <- aggregate(full_data$steps, list(interval = data$interval, day = df_new$tag), mean)
names(week_average_df) <- c("interval", "day", "steps")
```

Plot it.

``` r
ggplot(week_average_df, aes(interval, steps)) + geom_line(color = "purple3", lwd = 1) + 
        xlab("Interval: 5 minute") + ylab("Steps numbers") + facet_grid(day ~ .)
```

![](images/unnamed-chunk-14-1.png)
