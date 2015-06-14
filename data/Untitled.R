
getwd()
activity <- read.csv("./data/activity.csv")
activity$date <- as.Date(activity$date,"%Y-%m-%d")
activity.complete_cases <- activity[complete.cases(activity),]

#mean by steps
stepsByInterval <- aggregate(steps ~ interval, data=activity.complete_cases,mean)
sum(is.na(activity))
activity.imputed_nas <- activity
#
intervals <- unique(activity$interval)
for (interval in intervals){
  activity.imputed_nas[which(activity$interval==interval & is.na(activity$steps)),"steps"] <-
    stepsByInterval[which(stepsByInterval$interval==interval),"steps"]
}
stepsPerDay.imputes_nas <- aggregate(steps ~ date, data=activity.imputed_nas,sum)


