---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Note: I ran out of time, and this code still has a issues:-) 

# load libraries
library(dplyr)
library(ggplot2)
library(lattice)
library(Hmisc)

# Read csv file
my_data <- read.csv("Activity.csv")

# Group the data by date
my_grouped_data <- group_by(my_data, date )

# Omit NAs
my_grouped_data2 <- na.omit(my_grouped_data)

# Calculate the total number of steps taken per day
sum_grouped_data <- summarise(my_grouped_data2, dailystepsum = sum(steps))

# Create a histogram
hist <- qplot(sum_grouped_data$dailystepsum, geom="histogram", xlab = "daily step sum") 
hist

#Write file to disk. 
dev.cur()
window()
png(filename = "hist.png", width = 480, height = 480, units = "px", pointsize = 12, bg = "white", res = NA, family = "", restoreConsole = TRUE,
    type = c("windows", "cairo", "cairo-png"), antialias)
dev.off()  

# Calculate and report the mean and median of the total number of steps taken per day
summary(sum_grouped_data$dailystepsum)
# str(sum_grouped_data)
# str(my_grouped_data2)

# split by date 
splitByInterval <- split(my_grouped_data2,my_grouped_data2$interval, drop=TRUE)  

# Avg. steps per interval & max steps 
interval_Avg <- sapply(splitByInterval, function(x) mean(x$steps))     
plot(interval_Avg, type="l",  
     main="5 minute Interval Time Series", 
     ylab="Average # of Steps", 
     xlab="Interval INDEX", col="red")                          
abline(v=which.max(interval_Avg), lty=3, col="blue")                  
text(which.max(interval_Avg),max(interval_Avg),  
     labels=paste("max = ",as.character(round(max(interval_Avg)))), 
     pos=4, col="blue")                                               

# Report on the number of missing values (NA's)
summary(my_grouped_data)

# Replace all NAs with the median of 10760
my_grouped_data[is.na(my_grouped_data)] <- 10760
summary(my_grouped_data)
str(my_grouped_data)
View(my_grouped_data)

# Calculate the total number of steps taken per day
sum_grouped_data_filled <- summarise(my_grouped_data, dailystepsum = sum(steps))

# Create a histogram of the filled values
hist2 <- qplot(sum_grouped_data_filled$dailystepsum, geom="histogram", xlab = "daily step sum") 
hist2

sum_grouped_data_filled$date <- as.Date(strptime(sum_grouped_data_filled$date, format="%Y-%m-%d")) # convert date to a date() class variable  
sum_grouped_data_filled$day <- sum_grouped_data_filled(newData$date)                              # build a 'day' factor to hold weekday / weekend  
for (i in 1:nrow(sum_grouped_data_filled)) {                                       # for each day  
  if (sum_grouped_data_filled[i,]$day %in% c("Saturday","Sunday")) {             # if Saturday or Sunday,
    sum_grouped_data_filled[i,]$day<-"weekend"                                 #   then 'weekend'
  }
  else{
    sum_grouped_data_filled[i,]$day<-"weekday"                                 #    else 'weekday'
  }
}

## aggregate newData by steps as a function of interval + day  
steps_Day <- aggregate(sum_grouped_data$steps ~ sum_grouped_data$interval + nsum_grouped_data$day, sum_grouped_data, mean)

## rename the columns
names(steps_Day) <- c("interval", "day", "steps")

## plot and compare weekdays to weekends
dev.cur()
window()
par(mfrow=c(1,1))  
with(steps_Day, plot(steps ~ interval, type="n", main="Weekday vs. Weekend Avg."))  
with(steps_Day[steps_Day$day == "weekday",], lines(steps ~ interval, type="l", col="blue"))  
with(steps_Day[steps_Day$day == "weekend",], lines(steps ~ interval, type="l", col="16" ))  
legend("topright", lty=c(1,1), col = c("blue", "16"), legend = c("weekday", "weekend"), seg.len=3)
dev.off()  