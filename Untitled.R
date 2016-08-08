##Code for reading in the dataset and/or processing the data
activity=read.csv("activity.csv")
activity2=subset(activity,activity$steps!="NA")
head(activity)

##Histogram of the total number of steps taken each day
ag.data=aggregate(activity2$steps, by=list(activity2$date), FUN=sum)
hist(ag.data$x)
library(ggplot2)
qplot(ag.data$x)+stat_bin(bins=5)

#Mean and median number of steps taken each day
c(mean(ag.data$x),median(ag.data$x))

ag.mean=aggregate(activity2$steps, by=list(activity2$date), FUN=mean)
ag.median=aggregate(activity2$steps, by=list(activity2$date), FUN=median)

#Time series plot of the average number of steps takent
qplot(ag.mean$Group.1,ag.mean$x)
plot(ag.mean$x,type = "l")

#The 5-minute interval that, on average, contains the maximum number of steps
interval.mean=aggregate(activity2$steps, by=list(activity2$interval), FUN=mean)
which.max(interval.mean$x)
max(interval.mean$x)


#Code to describe and show a strategy for imputing missing data

# replacing na with 0
activity[is.na(activity)]=0

#Histogram of the total number of steps taken each day after missing values are imputed
hist(aggregate(activity$steps, by=list(activity$date), FUN=sum)[,2])


#Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
activity$wkDY=rep("weekday",length(activity$date))

activity$wkDY[(weekdays(as.Date(activity$date))=="Sunday") | (weekdays(as.Date(activity$date))=="Saturday")]="weekend"


par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
        steps.type <- aggregate(steps ~ interval, data = activity, subset = activity$wkDY == 
                                        type, FUN = mean)
        plot(steps.type, type = "l", main = type)
}
