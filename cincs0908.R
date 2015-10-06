#Read data and library
data <- read.csv("C:/Users/yi/Desktop/Cincs/Damage and Duration analysis/Duration0908.csv")
library(zoo)
names(data)
fix(data)

#filter data first
dataless <- data[which(as.Date(data$Incident.Begin.Date,"%m/%d/%Y") >= as.Date("1/1/1990", "%m/%d/%Y")),]
#Use start date and end date to calculate days of duration in each month.
#Initial output mytable
mytable <- as.data.frame(matrix(nrow = dim(dataless)[1], ncol=12))
names(mytable) <- c("Jan","Feb","Mar","Apr","May","Jun",
                    "Jul","Aug","Sep","Oct","Nov","Dec")
mytable[is.na(mytable)] <- 0
#Two loop to calculate
for(j in 1:dim(dataless)[1]){
  event_start_date <- as.Date(dataless[j,4], "%m/%d/%Y")
  event_end_date   <- as.Date(dataless[j,5], "%m/%d/%Y")
  M <- table(as.yearmon(seq(event_start_date, event_end_date, "day")))
for(i in 1:length(M)){
  mo <- substr(names(M)[i],1,3)
  mytable[j,which(mo==names(mytable))] <- mytable[j,which(mo==names(mytable))] + M[[i]]
}
}
fix(mytable)

# Use start date and end date to calculate days in each year.
myyear <- as.data.frame((matrix(nrow = dim(dataless)[1], ncol=26)))
names(myyear) <- seq(1990,2015)
myyear[is.na(myyear)] <- 0
for(j in 1:dim(dataless)[1]){
  event_start_date <- as.Date(dataless[j,4], "%m/%d/%Y")
  event_end_date   <- as.Date(dataless[j,5], "%m/%d/%Y")
  M <- table(as.yearmon(seq(event_start_date, event_end_date, "day")))
  for(i in 1:length(M)){
    mo <- substr(names(M)[i],5,8)
    myyear[j,which(mo==names(myyear))] <- myyear[j,which(mo==names(myyear))] + M[[i]]
  }
}

# search orange county in each month.
index <- which(data$Declared.County.Area == "Orange (County)")
Output <- cbind(data[index,c(1,3,4,5,8)],mytable[index,])
Nu <-colSums(mytable[index,]!= 0)
All <- colSums(mytable[index,])
Aver <- colSums(mytable[index,])/colSums(mytable[index,]!= 0)
par(mfrow=c(3,1))
barplot(Nu,main="Number of Disaster in each month",col=2)
barplot(All, main="Cumulative Days in each month",col=3)
barplot(Aver,main="Average Days in each month",col=4)

# search orange county in each year.
index <- which(dataless$Declared.County.Area == "Orange (County)")
Output2 <- cbind(dataless[index,c(1,4,5)],myyear[index,])
plot(seq(1990,2015),colSums(myyear[index,]),type="l",ylab="Days",
     xlab="Year",main="Flood Days from 1990 to 2014",col=4,lwd=2)


write.csv(Output,"C:/Users/yi/Desktop/Cincs/Damage and Duration analysis/DurationOutput0908.csv")


