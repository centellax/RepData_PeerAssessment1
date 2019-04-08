pre steps
=========

    setwd('D:/Users/Centella/Desktop/DS Specialization/no5/repdata_data_activity')
    library(lattice)
    library(timeDate)

    ## Warning: package 'timeDate' was built under R version 3.2.5

    dat<- read.csv('activity.csv')

What is mean total number of steps taken per day?
=================================================

    day_steps <- sum(dat$steps, na.rm = TRUE) # 570608
    hist_day_steps <- aggregate(steps~date, data=dat, FUN=sum, na.rm=TRUE)
    hist(hist_day_steps$steps,main = "Total Steps per Day",xlab = "Number of Steps")

![](test_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    mean(hist_day_steps$steps, na.rm=TRUE) #10766.19

    ## [1] 10766.19

    median(hist_day_steps$steps, na.rm=TRUE) #10765

    ## [1] 10765

What is the average daily activity pattern?
===========================================

    five_minutes_average <- aggregate(steps~interval, data=dat, FUN=mean, na.rm=TRUE)
    plot(x = five_minutes_average$interval, y = five_minutes_average$steps, type = "l") 

![](test_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    max_steps <- max(five_minutes_average$steps)
    for (i in 1:288) 
    {
      if (five_minutes_average$steps[i] == max_steps)
        five_minute_interval_at_max_steps <- five_minutes_average$interval[i]
    } #835

Imputing missing values
=======================

    na_ct <- sum(is.na(dat$steps)) #2304
    na_ct

    ## [1] 2304

    #replace missing with mean
    rm(na_ct)
    na_pos <- which(is.na(dat$steps))
    mean_vec <- rep(mean(dat$steps, na.rm=TRUE), times=length(na_pos))
    dat[na_pos, "steps"] <- mean_vec
    rm(mean_vec, na_pos)
    hist2 <- aggregate(dat$steps, by=list(dat$date), FUN=sum)
    names(hist2) <- c("date", "total")
    hist(hist2$total, 
         breaks=seq(from=0, to=25000, by=2500),
         col="black", 
         xlab="Total number of steps", 
         ylim=c(0, 30), 
         main="Histogram of the total number of steps taken each day\n(NA replaced by mean value)")

![](test_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    mean(hist2$total) #10766.19

    ## [1] 10766.19

    median(hist2$total) #10766.19

    ## [1] 10766.19

Are there differences in activity patterns between weekdays and weekends?
=========================================================================

    rm(hist2)
    dat$day_of_week <- ifelse(isWeekday(dat$date)==TRUE, "weekday", "weekend")
    xyplot(steps ~ interval | day_of_week, layout = c(1, 2), data=dat, type="l")

![](test_files/figure-markdown_strict/unnamed-chunk-5-1.png)
