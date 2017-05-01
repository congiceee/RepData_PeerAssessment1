# Reproducible Research: Peer Assessment 1

## Part1: Loading and preprocessing the data


```r
setwd('./activity/')
Data<-read.csv('activity.csv',sep=',')
k<-format(seq.POSIXt(as.POSIXct(Sys.Date()), as.POSIXct(Sys.Date()+1), by = "5 min"),"%H:%M",tz="GMT")
```

```
## Warning in as.POSIXlt.POSIXct(Sys.time()): unable to identify current timezone 'C':
## please set environment variable 'TZ'
```

```r
k<-k[-length(k)]
library(dplyr)
Data<-mutate(Data,time=rep(k,sum(!is.na(unique(Data$date)))))
```



## Part2: What is mean total number of steps taken per day?

1. Determine the total number of steps taken per day using 'tapply' and the result is an array DM

```r
DM<-tapply(Data$steps,Data$date,sum,na.rm=TRUE)
```

2. Reshape the array into data frame

```r
DM<-data.frame('Date'=names(DM),'No.Steps.pd'=DM)
```

3. Plot DM

```r
library(ggplot2)
ggplot(DM,aes(No.Steps.pd))+geom_histogram(bins = 45)+labs(title='Counts of various numbers of steps taken per day')
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

4. Calculate the mean and median of the total number of steps taken per day

```r
mean_steps_pd<-mean(DM[,2],na.rm=TRUE)
median_steps_pd<-median(DM[,2],na.rm=TRUE)
```

Therefore, the mean and the median are **9354.2295082** and **10395** respectively.

## Part3: What is the average daily activity pattern?
1. Determine the mean number of steps taken per interval across all days using 'tapply' and reshape the result as date frame

```r
DI<-tapply(Data$steps,Data$interval,mean,na.rm=TRUE)
ks<-strptime(k,format='%H:%M')
DIs<-data.frame('Interval'=ks,'No.Steps.pi'=DI)
```

2. Plot the time series

```r
library(scales)
xlim_p<-as.POSIXct(c(ks[1],ks[length(ks)]),tz='GMT')
ggplot(DIs,aes(Interval,No.Steps.pi))+geom_line()+scale_x_datetime(breaks = date_breaks("2 hours"),labels=date_format("%H:%M"),limits=xlim_p)+xlab('5-minute interval')
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

3. Determine the interval that contains the maximum number of steps

```r
maxnostep<-which.max(DIs$No.Steps.pi)
T_maxnostep<-k[maxnostep]
```
Therefore, on average across all the days in the dataset, **08:35** contains the maximum number of steps, i.e. **104**.

## Part4: Imputing missing values
1. Calculate the total number of missing values. 

```r
sona<-sum(is.na(Data$steps))
```
Hence, in the column 'steps', the total number of rows with 'NA' is **2304**.


2. Fill in the detected 'NA's with the mean for that 5-minute interval, which is determined and saved as DI in Part3.1.

```r
NewData<-Data
NewData$impedsteps<-NewData$steps
for (i in 1:sona){
  NewData$impedsteps[i]<-DI[as.character(NewData$interval[i])]
}
```

3. Redefine the new data set which only contained the imputed information on steps.

```r
NewData<-NewData[,-1]
```

4. Determine the total number of steps taken per day using 'tapply' and the result is an array NewDM 

```r
NewDM<-tapply(NewData$impedsteps,NewData$date,sum,na.rm=TRUE)
```

5. Reshape the array into data frame

```r
NewDM<-data.frame('Date'=names(NewDM),'No.Steps.pd.after.imputing'=NewDM)
```

6. Plot NewDM

```r
ggplot(NewDM,aes(No.Steps.pd.after.imputing))+geom_histogram(bins = 45)+labs(title='After imputing the data set, counts of various numbers of steps taken per day')
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

7. For the new dataset, calculate the mean and median of the total number of steps taken per day

```r
AI_mean_steps_pd<-mean(NewDM[,2],na.rm=TRUE)
AI_median_steps_pd<-median(NewDM[,2],na.rm=TRUE)
```
Therefore, after imputing over the missing data, the mean and the median are **9728.106712** and **1.06\times 10^{4}** respectively.

8. As can be shown the above figure, the distribution is normal expect for the parts when steps are small since people will have to rest and sleep. The peak value is now centered at the plot rather than at x=0. While the mean and median increase a little bit which is reasonable as we include some values for the previous missing values.



## Part5: Are there differences in activity patterns between weekdays and weekends?
1. Refine the data set to include the factor column to indicate whether it is weekend or weekday.

```r
NewData$wds<-weekdays(as.Date(NewData$date))
NewData$fwds<-factor(1*(NewData$wds == 'Saturday'|NewData$wds == 'Sunday'),labels=c('weekday','weekend'))
```

2. Use the simliar appraoch as above to calculate the mean number of steps taken per interval across all weekdays or all weekends using 'tapply'.

```r
id_wd<-((as.character(NewData$fwds)=='weekday'))
id_we<-((as.character(NewData$fwds)=='weekend'))
NewDM_wd<-tapply(NewData$impedsteps[id_wd],NewData$interval[id_wd],mean,na.rm=TRUE)
NewDM_we<-tapply(NewData$impedsteps[id_we],NewData$interval[id_we],mean,na.rm=TRUE)
```

3. Construct the data frame for plotting.

```r
NewDM_wd<-data.frame('Interval'=ks,'No.Steps.pi'=NewDM_wd,'Type'='weekday')
NewDM_we<-data.frame('Interval'=ks,'No.Steps.pi'=NewDM_we,'Type'='weekend')
NewDM_week<-rbind(NewDM_wd,NewDM_we)
```

4. Use 'ggplot' to panel plot the time series of the 5-minute interval and the average number of steps taken, averaged across all weekdays or all weekends. 

```r
ggplot(NewDM_week,aes(Interval,No.Steps.pi))+geom_line()+facet_grid(Type~.)+scale_x_datetime(breaks = date_breaks("2 hours"),labels=date_format("%H:%M"),limits=xlim_p)+xlab('5-minute interval')
```

![](PA1_template_files/figure-html/unnamed-chunk-19-1.png)<!-- -->
