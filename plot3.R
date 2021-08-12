p3 <- function() {
  
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
  cd1 <- as.numeric(cb2$Sub_metering_1)
  cd2 <- as.numeric(cb2$Sub_metering_2)
  cd3 <- as.numeric(cb2$Sub_metering_3)
  png(filename = 'plot3.png', width = 480, height = 480)
  plot(cv2, cd1, type = 'n', main = 'The sub metering measurement vs days', xlab = 'Days', ylab = 'Energy sub metering')
  points(cv2, cd1, type = 'l', lwd = 1, col = 'black')
  points(cv2, cd2, type = 'l', lwd = 1, col = 'red')
  points(cv2, cd3, type = 'l', lwd = 1, col = 'blue')
  legend('topright', lty = 'solid', legend = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'), col = c('black', 'red', 'blue'))
  dev.off()
}