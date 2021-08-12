p2 <- function() {
  
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
  cv3 <- as.numeric(cb2$Global_active_power)
  png(filename = 'plot2.png', width = 480, height = 480)
  plot(cv2, cv3, col='black', type= 'l', lwd = 1, main = 'The Global Active Power vs days', xlab = 'Days', ylab = 'Global Active Power (kilowatts)')
  dev.off()
}