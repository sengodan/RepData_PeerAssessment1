meanSteps <- function() {
  library(dplyr)
  
  
## Read Data 
  activity <- read.csv("activity.csv")
  activity$date <- as.Date( activity$date )
  activity_valid <- subset(activity, !is.na(steps))
  
  activity_group <- group_by(activity_valid, date)
  activity_daily <- summarize(activity_group, steps_daily = sum(steps) )
  
  mean(activity_daily$steps_daily)
  median(activity_daily$steps_daily)

  hist(activity_daily$steps_daily, main = "Total number of steps taken per day", xlab="Daily Steps")
  

  activity_valid$weekdays <- ifelse(wday(activity_valid$date) == 1 | wday(activity_valid$date) == 7, "Weekend", "Weekday"     ) 

  activity_group <- group_by(activity_valid, interval, weekdays)
  activity_weekly <- summarize(activity_group, average_steps = mean(steps) )
  ggplot(activity_weekly, aes(interval, average_steps)) + geom_line(aes(group=weekdays)) + aes(color=weekdays) +labs(title="Avg Steps")

  qplot(interval, average_steps, data=activity_weekly ,geom="line", binwidth =2) + facet_grid(weekdays ~ . )

}