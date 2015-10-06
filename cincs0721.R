# calculate all 9 indexs of extreme precipitation


setwd("C:/Users/yi/Desktop/Cincs/Database/Precipitation")

Preci1 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/Precipitation/Monthly_1980-1984.csv")
Preci2 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/Precipitation/Monthly_1985-1999.csv")
Preci3 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/Precipitation/Monthly_2000-2013.csv")
Preci <- rbind(Preci1,Preci2,Preci3)
names(Preci)
Preci.filter <- Preci[Preci[, 11]!=-9999,]

Extreme <- Preci.filter$Extreme.maximum.daily.precipitation..0.01.inch.
Thr <- quantile(Extreme,prob=0.9)[[1]]
quantile(Extreme)
#0%  25%  50%  75% 100% 
#0   75  137  209 8000 
Unique.station <- unique(Preci.filter[,1])
L <- length(Unique.station)
Count <- vector(length=L)
Mean <- vector(length=L)
for(i in 1: L){
  #i=165
  Temp <- Preci.filter[Unique.station[i] == Preci.filter[,1],]
  Count[i]<- sum(Temp[,11]>=Thr)
  Mean[i]<- round(mean(Temp[Temp[,11]>=Thr,11]))/100
}
Output <-cbind(paste(Unique.station),Count,Mean)
fix(Output)
colnames(Output)[1] <- "Station"
write.csv(Output,file="C:/Users/yi/Desktop/Cincs/ExtremeDay.csv")




# 11 indices
Preci1 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/1980-1982.csv")
Preci2 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/1983-1985.csv")
Preci3 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/1986-1988.csv")
Preci4 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/1989-1991.csv")
Preci5 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/1992-1995Nov.csv")
Preci6 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/1995Dec-1998.csv")
Preci7 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/1999-2002Nov.csv")
Preci8 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/2002Dec-2005.csv")
Preci9 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/2006-2009Nov.csv")
Preci10 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/2009Dec-2012.csv")
Preci11 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/2013-2015Jul.csv")

Preci <- rbind(Preci1,Preci2,Preci3,Preci4,Preci5,Preci6,Preci7,Preci8,Preci9,Preci10,Preci11)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
substrLeft <- function(x, n){
  substr(x, 1, n)
}
#fix(Preci)
names (Preci)
Unique.station <- unique(Preci[,1])
L <- length(Unique.station)
Output <- data.frame()
for(i in 1:L){
  #i=1
  print(i)
  Temp <- Preci[Preci[,1]==Unique.station[i],]
  Year <- substrLeft(Temp[,6],4)
  Unique.year <- unique(Year)
  Y <- length(Unique.year)
  result <- data.frame(matrix(ncol=14,nrow=Y))
  for(j in 1: Y){
    Temp2 <- Temp[substrLeft(Temp[,6],4) == Unique.year[j],]
    result[j,1] <- paste(Temp2[1,1]) # Station
    result[j,2] <- paste(Temp2[1,2]) # Station_name
    result[j,3] <- paste(Temp2[1,4]) # Latitude
    result[j,4] <- paste(Temp2[1,5]) # Longitude
    result[j,5] <- Unique.year[j] # Year
    result[j,6] <- sum(Temp2[,7]/10 > 1) # wet day number
    result[j,7] <- sum(Temp2[,7]/10 > 10) # heavy rain days
    result[j,8] <- sum(Temp2[,7]/10 > 20) # very heavy rain days
    result[j,9] <- sum(Temp2[Temp2[,7]/10>1,7]/10) # wet day annual precipitation
    result[j,10] <- sum(Temp2[,7]/10)/result[j,6] # Simple daily intensity index
    q <- quantile(Temp2[,7],probs=c(0.95,0.99))
    q95 <- q[[1]]
    q99 <- q[[2]]
    result[j,11] <- sum(Temp2[Temp2[,7]>q95,7])/10 # very wet day annual precipitation
    result[j,12] <- sum(Temp2[Temp2[,7]>q99,7])/10 # extreme wet day annual precipitation
    result[j,13] <- result[j,11]/result[j,9]
    result[j,14] <- result[j,12]/result[j,9]
  }
  Output <- rbind(Output,result)
  }

 fix(Output)
 colnames(Output) <- c("Station","Station_name","Latitude","Longitude","Year",
                       "Number of Wet Days","Number of heavy precipitation days",
                       "Number of very heavy precipitation days",
                       "Annual total wet day precipitation","Simple daily intensity index",
                       "Very wet days","Extremely wet days","Contribution from very wet days",
                       "Contribution from Extremely wet days")
 write.csv(Output,file="C:/Users/yi/Desktop/Cincs/DailyAnalysis.csv")
  
  
  
# calculate annual extreme data
data <- read.csv("C:/Users/yi/Desktop/Cincs/DailyAnalysis.csv")
fix(data)
names(data)
Unique.station <- unique(data[,2])
L <- length(Unique.station)
Output <- data.frame(matrix(nrow=L, ncol=13))
for(i in 1:L){
  # i=1
  temp <- data[data$Station==Unique.station[i],]
  n <- nrow(temp)
  Output[i,1] <- paste(Unique.station[i])
  Output[i,2] <- paste(temp[1,3])
  Output[i,3] <- temp[1,4]
  Output[i,4] <- temp[1,5]
  Output[i,5] <- temp[1,6]
  Output[i,6] <- mean(as.numeric(paste(temp[,13])))
  Output[i,7] <- mean(temp[,8])
  Output[i,8] <- mean(temp[,9])
  Output[i,9] <- mean(temp[,11])
  Output[i,10] <- mean(temp[,15])
  Output[i,11] <- mean(temp[,17])
  Output[i,12] <- mean(temp[,18])
  Output[i,13] <- mean(temp[,19])
}
colnames(Output) <- c("Station","Station_name","Latitude","Longitude","Year",
                      "Simple daily intensity index(inch)","Number of heavy precipitation days",
                      "Number of very heavy precipitation days",
                      "Annual total wet day precipitation(inch)",
                      "Very wet days","Extremely wet days","Contribution from very wet days",
                      "Contribution from Extremely wet days")
warnings()
fix(Output)

Output2 <- Output[rowSums(is.na(Output)) == 0,]
write.csv(Output2,"C:/Users/yi/Desktop/Cincs/DailyAnalysis(Average).csv")










