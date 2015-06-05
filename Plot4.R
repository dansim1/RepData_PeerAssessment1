library(sqldf)
library(tcltk)
library(lubridate)

#Read in only the relevant dates (2075260 -> 2880). Note: The sep and header arguments are very important
my_data <- read.csv.sql("household_power_consumption.txt", sep = ";", header = TRUE, sql = "select * from file where Date in ('1/2/2007','2/2/2007')")

#Create a single column which contains both the date and time
my_data$date_time <- dmy_hms(paste(my_data$Date, my_data$Time))

#Combine the new date/time column with the data
my_data2 <- cbind(my_data$date_time, my_data)

View(my_data2)

#Create plot4
dev.cur()
window()
par(mfrow=c(2,2))
#Create plot4_1
plot4_1 <- plot(my_data2[ 1], my_data2$Global_active_power, type="l", ylab = "Global Active Power", xlab = "")
#Create plot4_2
plot4_2 <- plot(my_data2[ 1], my_data2$Voltage, type="l", ylab = "Voltage", xlab = "datetime")
#Create plot4_3
plot(my_data2$date_time, my_data2$Sub_metering_1, type = "l", col = "black", xlab = "", ylab = "Energy sub meeting")
lines(my_data2$date_time, my_data2$Sub_metering_2, type = "l", col = "red", xlab = "", ylab = "Energy sub meeting")
lines(my_data2$date_time, my_data2$Sub_metering_3, type = "l", col = "blue", xlab = "", ylab = "Energy sub meeting")
legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n", xjust = 1, 
       legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
#Create plot4_4
plot4_2 <- plot(my_data2[ 1], my_data2$Global_reactive_power, type="l", ylab = "Global_reactive_power", xlab = "datetime")
dev.copy(png, file = "plot4.png")
dev.off()  





#Create plot3
dev.cur()
window()
plot(my_data2$date_time, my_data2$Sub_metering_1, type = "l", col = "black", xlab = "", ylab = "Energy sub meeting")
lines(my_data2$date_time, my_data2$Sub_metering_2, type = "l", col = "red", xlab = "", ylab = "Energy sub meeting")
lines(my_data2$date_time, my_data2$Sub_metering_3, type = "l", col = "blue", xlab = "", ylab = "Energy sub meeting")
legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n", xjust = 1, 
       legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
dev.copy(png, file = "plot3.png")
dev.off()      