p4 <- function() {
  
  #Reading the source dataset from the net source
  d2 <- unzip('a1.zip')
  cc1 <-  read.csv(d2, sep = ';', dec = ',', header = T)
  c <- dim(cc1)
  
  # Finding ? in rows and delete them every where
  cb1 <- cc1
  for (i in 3:9) {
    cb1 <- subset(cb1, cb1[,i] != '?')
  }
  
  # Formatting the dates and subsetting the main dataset
  cb1[,10] <- as.Date(cb1[,1], format = '%d/%m/%Y')
  d4 <- as.Date('2007-02-01', format='%Y-%m-%d')
  d5 <- as.Date('2007-02-02', format='%Y-%m-%d')
  cb2 <- subset(cb1, V10 >= d4 & V10 <= d5)
  cv1 <- paste(cb2[,1], cb2[,2]) 
  cv2 <- strptime(cv1, format = '%d/%m/%Y %H:%M:%OS', tz='EST')
  
  # Plotting the data
  cb4 <- as.data.frame(sapply(cb2[,3:9], as.numeric))
  png(filename = 'plot4.png', width = 480, height = 480)
  par(mfrow=c(2,2), mar=c(5,5,1,2))
  with(cb4, {
    #1
    plot(cv2, Global_active_power, xlab = 'Days', ylab = 'Global Active Power', type = 'l', lwd = 1, col = 'black')
    #2
    plot(cv2, Voltage, xlab = 'Days', ylab = 'Voltage', type = 'l', lwd = 1, col = 'black')
    #3
    plot(cv2, Sub_metering_1, type = 'n', xlab = 'Days', ylab = 'Energy sub metering')
    points(cv2, Sub_metering_1, type = 'l', lwd = 1, col = 'black')
    points(cv2, Sub_metering_2, type = 'l', lwd = 1, col = 'red')
    points(cv2, Sub_metering_3, type = 'l', lwd = 1, col = 'blue')
    legend('topright', lty = 'solid', legend = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'), col = c('black', 'red', 'blue'), y.intersp=0.8, x.intersp=0.2)
    #4
    plot(cv2, Global_reactive_power, type = 'l', lwd = 1, col = 'black', xlab = 'Days', ylab = 'Global reactive power')
  })
    dev.off()
}