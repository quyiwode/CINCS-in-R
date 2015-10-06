# collect precipitation data for one location
# calculate 90% 95% quantile for extreme data

data <- read.csv("C:/Users/yi/Desktop/Cincs/Database/Precipitation/Precipitation Projection.csv")
names(data)
myLat <- 25.760148
myLon <- -80.191830
index <- which.min((myLat-data$Lat)^2 + (myLon-data$Lon)^2)
data[index,3:5]
mydata <- as.numeric(data[index,3:5])
mylabel <- c("",paste(round(data[index,8:9]),"%"))

mp <- barplot(height=mydata, names.arg = c("Historical","2020","2050"),
              ylab = "Annual precipitation inches", col=c(8,4,4),space=TRUE,ylim=c(0,70))
text(mp, mydata, labels = mylabel, pos = 3)



#
data <- read.csv("C:/Users/yi/Desktop/Cincs/Precipitation analysis/Annual Precipitation.csv")
fix(data)
data <- data[1:182,1:38]
L <- dim(data)[1]
q95 <- vector(length=L)
q90 <- vector(length=L)
for(i in 1:L){
  #i =2
  temp <- data[i,5:38]
  temp <- temp[temp!=0]
  q95[i] <- quantile(temp,prob=0.95)
  q90[i] <- quantile(temp,prob=0.90)
}
Output <- cbind(data,q95,q90)
colnames(Output)[39] <- "quantile 95%"
colnames(Output)[40] <- "quantile 90%"
fix(Output)
colnames(Output)[5:38] <- seq(1980,2013)
write.csv(Output,"C:/Users/yi/Desktop/Cincs/Precipitation analysis/Annual Precipitation.csv")
