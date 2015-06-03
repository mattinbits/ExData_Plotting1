read_data <- function(inputpath) {
  csv <- read.csv("household_power_consumption.txt", sep = ";", na.strings = "?")
  csv$Formatted_date <- as.Date(csv$Date, "%d/%m/%Y")
  csv_subset <- subset(csv, 
                       Formatted_date>='2007-02-01' & Formatted_date<='2007-02-02')
  csv_subset$Datetime <- strptime(paste(csv_subset$Date,csv_subset$Time, " "),
                                  "%d/%m/%Y %H:%M:%S")
  return(csv_subset)
}

draw_chart <- function(dataframe, outputpath="SCREEN", width=480, height=480) {
  if(outputpath != "SCREEN") {
    png(outputpath, width = width, height = height, units = "px")
  }
  specific_chart(dataframe)
  if(outputpath != "SCREEN") {
    dev.off()
  }
}

specific_chart <- function(dataframe) {
  with(dataframe, plot(Datetime, 
                       Sub_metering_1, 
                       type = "l", 
                       col="black",
                       xlab = "",
                       ylab = "Energy sub metering"))
  with(dataframe, lines(Datetime, Sub_metering_2, col="red"))
  with(dataframe, lines(Datetime, Sub_metering_3, col="blue"))
  legend("topright", 
         col = c("black", "red", "blue"), 
         legend = c("Sub_metering_1",
                    "Sub_metering_2",
                    "Sub_metering_3"),
         lwd = 1
  )
}

draw_chart(read_data("household_power_consumption.txt"), outputpath = "plot3.png")