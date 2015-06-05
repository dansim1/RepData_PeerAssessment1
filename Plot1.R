library(sqldf)
library(tcltk)
library(lubridate)

#Read in only the relevant dates (2075260 -> 2880). Note: The sep and header arguments are very important
my_data <- read.csv.sql("household_power_consumption.txt", sep = ";", header = TRUE, sql = "select * from file where Date in ('1/2/2007','2/2/2007')")

#Create a single column which contains both the date and time
my_data$date_time <- dmy_hms(paste(my_data$Date, my_data$Time))

#Combine the new date/time column with the data
my_data2 <- cbind(my_data$date_time, my_data)

#Create histogram
plot1 <- hist(my_data2$Global_active_power, col = "red", xlab = "Global Active Power (kilowatts)", main = "Global Active Power")

#Write file to disk 
png(filename = "plot1.png", width = 480, height = 480, units = "px")
plot(plot1, col = "red", xlab = "Global Active Power (kilowatts)", main = "Global Active Power")
dev.off()

