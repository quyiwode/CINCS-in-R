# Distribution of annual precipitation for one station

data <- read.csv("C:/Users/yi/Desktop/Cincs/Precipitation analysis/Annual Precipitation.csv")
fix(data)
names(data)
mydata <- as.numeric(data[1,6:39])
#par(mfrow = c(1,2))
hist(mydata,main=paste(data[1,2]),xlab="Annual Precip (inch)")
plot(mydata,type="l",x= seq(1980,2013), xlab="Year",
     ylab="Annual Precip (inch)", main =paste(data[1,2]) )
