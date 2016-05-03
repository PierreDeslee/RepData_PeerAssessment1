dat<-read.csv("activity.csv")
library(ggplot2)

SumbyDay<-tapply(dat$steps, dat$date, sum, na.rm =TRUE)
hist(SumbyDay, breaks = 61, xlab = "Total number of steps taken each day", main ="")
moy<-mean(SumbyDay)
moy
mediane<-median(SumbyDay)
mediane

averages <- aggregate(steps ~ interval, data = dat, mean, na.rm = TRUE)

ggplot(averages, aes(interval, steps)) + geom_line() + 
  xlab("5-minute interval") + ylab("Number of steps")

missing<-sum(is.na(dat$steps))
missing

full<-dat
for (i in 1:nrow(full)){
  if(is.na(full$steps[i])){
    full$steps[i]<-averages[(i-1) %% nrow(averages) +1,"steps"]
  }
}

SumbyDay2<-tapply(full$steps, full$date, sum, na.rm =TRUE)
hist(SumbyDay2, breaks = 61,xlab = "Total number of steps taken each day", main ="")
moy2<-mean(SumbyDay2)
moy2
mediane2<-median(SumbyDay2)
mediane2

library(chron)
full$week<-is.weekend(full$date)
full$weekday[full$week==FALSE]<-"weekday"
full$weekday[full$week==TRUE]<-"weekend"

averages <- aggregate(steps ~ interval + weekday, data = full, mean, na.rm = TRUE)
ggplot(averages, aes(interval, steps)) + geom_line() +  facet_grid(.~ weekday) +
  xlab("5-minute interval") + ylab("Number of steps")