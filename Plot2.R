library(sqldf)
library(tcltk)
library(lubridate)

#Read in only the relevant dates (2075260 -> 2880). Note: The sep and header arguments are very important
my_data <- read.csv.sql("household_power_consumption.txt", sep = ";", header = TRUE, sql = "select * from file where Date in ('1/2/2007','2/2/2007')")

#Create a single column which contains both the date and time
my_data$date_time <- dmy_hms(paste(my_data$Date, my_data$Time))

#Combine the new date/time column with the data
my_data2 <- cbind(my_data$date_time, my_data)

#Create plot2
#graphics.off()
dev.cur()
window()
plot2 <- plot(my_data2[ 1], my_data2$Global_active_power, type="l", ylab = "Global Active Power (kilowatts)", xlab = "")

#Write plot2 to disk 
dev.copy(png, file = "plot2.png")
dev.off()

