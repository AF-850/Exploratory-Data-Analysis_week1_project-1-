p1 <- function() {
  
  #Reading the source dataset from the net source
  d1 <- download.file('https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip', destfile = 'a1.zip')
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
  
  # Plotting the data
  cv3 <- as.numeric(cb2$Global_active_power)
  png(filename = 'plot1.png', width = 480, height = 480)
  hist(cv3, col='red', main = 'The histogram of Global Active Power', xlab = 'Global Active Power (kilowatts)')
  dev.off()
}