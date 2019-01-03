# Reading and formating the data
main_set = read.table("household_power_consumption.txt", header = TRUE, sep =";", na.strings = "?", colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))
main_set$Date = as.Date(main_set$Date, "%d/%m/%Y")
main_set = subset(main_set,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))
dateTime = paste(main_set$Date, main_set$Time)
dateTime = setNames(dateTime, "DateTime")
strptime(dateTime, "%m/%d/%y %H:%M:%S")
main_set = cbind(dateTime, main_set)
# Plot 1
png("plot1.png", width = 480, height = 480)
hist(main_set$Global_active_power, main = "Global Active Power", xlab = "Global Active Power (killowatts)", col="Red")
dev.off()
# Plot 2
png("plot2.png", width = 480, height = 480)
plot(main_set$Global_active_power~main_set$dateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")
dev.off()
# Plot 3
png("plot3.png", width = 480, height = 480)
with(main_set, {
  plot(Sub_metering_1~dateTime, type="l",
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
dev.off()
# Plot 4
png("plot4.png", width = 480, height = 480)
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(main_set, {
  plot(Global_active_power~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  plot(Voltage~dateTime, type="l", 
       ylab="Voltage (volt)", xlab="")
  plot(Sub_metering_1~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  plot(Global_reactive_power~dateTime, type="l", 
       ylab="Global Rective Power (kilowatts)",xlab="")
})
dev.off()
