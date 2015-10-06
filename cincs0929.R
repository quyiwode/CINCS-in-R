# Read FL data
Preci1 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/FL/1980-1982.csv")
Preci2 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/FL/1983-1985.csv")
Preci3 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/FL/1986-1988.csv")
Preci4 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/FL/1989-1991.csv")
Preci5 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/FL/1992-1995Nov.csv")
Preci6 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/FL/1995Dec-1998.csv")
Preci7 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/FL/1999-2002Nov.csv")
Preci8 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/FL/2002Dec-2005.csv")
Preci9 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/FL/2006-2009Nov.csv")
Preci10 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/FL/2009Dec-2012.csv")
Preci11 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/FL/2013-2015Jul.csv")

PreciDaily <- rbind(Preci1,Preci2,Preci3,Preci4,Preci5,Preci6,Preci7,
                    Preci8,Preci9,Preci10,Preci11)

# Read LA data
Preci1 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/LA/1980-1985.csv")
Preci2 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/LA/1985-1990.csv")
Preci3 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/LA/1990-1995.csv")
Preci4 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/LA/1995-2000.csv")
Preci5 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/LA/2000-2005.csv")
Preci6 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/LA/2005-2010.csv")
Preci7 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/DailyPrecip/LA/2010-2015.csv")

PreciDaily <- rbind(Preci1,Preci2,Preci3,Preci4,Preci5,Preci6,Preci7)

# implements two functions
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
substrLeft <- function(x, n){
  substr(x, 1, n)
}


# get unique stations from database
names(PreciDaily)
Unique.station <- unique(PreciDaily[,1])
L <- length(Unique.station) # L is number of stations

# create monthly precip from PreciDaily, (mm) 
PreciMonthly <- data.frame()
for(i in 1:L){
  print(i)
  Temp <- PreciDaily[PreciDaily[,1]==Unique.station[i],] #one station all data
  YearMonth <- substrLeft(Temp[,6],6)
  Unique.month <- unique(YearMonth)
  M <- length(Unique.month)
  Temp2 <- Temp[1:M,] #initial one station with all year&month
  for(j in 1:M){
    Temp2[j,7] <- sum(Temp[YearMonth == Unique.month[j],7])/10 #one station one month
    Temp2[j,6] <- Unique.month[j] 
  }
  PreciMonthly <- rbind(PreciMonthly,Temp2)
}

# creat yearly precip from PreciMonthly, (mm)
PreciYearly <- data.frame()
for(i in 1:L){
  print(i)
  Temp <- PreciMonthly[PreciMonthly[,1]==Unique.station[i],] #one station all data
  Year <- substrLeft(Temp[,6],4)
  Unique.year <- unique(Year)
  Y <- length(Unique.year)
  Temp2 <- Temp[1:Y,] #initial one station with all year
  for(j in 1:Y){
    Temp2[j,7] <- sum(Temp[Year == Unique.year[j],7]) #one station one month
    Temp2[j,6] <- Unique.year[j] 
  }
  PreciYearly <- rbind(PreciYearly,Temp2)
}

# Use PreciDaily to get extreme index (inch)
DailyAnalysis <- data.frame(matrix(ncol=12,nrow=L))
for(i in 1:L){
  print(i)
  Temp <- PreciDaily[PreciDaily[,1]==Unique.station[i],]
  Year <- substrLeft(Temp[,6],4)
  Unique.year <- unique(Year)
  Y <- length(Unique.year)
  result <- data.frame(matrix(ncol=14,nrow=Y))
  for(j in 1: Y){
    Temp2 <- Temp[substrLeft(Temp[,6],4) == Unique.year[j],] # one station on year
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
  DailyAnalysis[i,1] <- paste(Temp2[1,1]) # Station
  DailyAnalysis[i,2] <- paste(Temp2[1,2]) # Station_name
  DailyAnalysis[i,3] <- paste(Temp2[1,4]) # Latitude
  DailyAnalysis[i,4] <- paste(Temp2[1,5]) # Longtitude
  DailyAnalysis[i,5] <- mean(na.omit(result[,10]))*0.039370 # Simple daily intensity index(inch)
  DailyAnalysis[i,6] <- mean(result[,7]) # Number of heavy precipitation days(day)
  DailyAnalysis[i,7] <- mean(result[,8]) # Number of very heavy precipitation days(day)
  DailyAnalysis[i,8] <- mean(result[,9])*0.039370 #Annual total wet day precipitation(inch)
  DailyAnalysis[i,9] <- mean(result[,11])*0.039370 # Very wet days annual precipitation(inch)
  DailyAnalysis[i,10] <- mean(result[,12])*0.039370 # Extremely wet days annual precipitation(inch)
  DailyAnalysis[i,11] <- mean(na.omit(result[,13])) #Contribution from very wet days
  DailyAnalysis[i,12] <- mean(na.omit(result[,14]))#Contribution from Extremely wet days
}

colnames(DailyAnalysis) <- c("Station","Station_name","Latitude","Longitude",
                      "Simple daily intensity index(inch)","Number of heavy precipitation days",
                      "Number of very heavy precipitation days",
                      "Annual total wet day precipitation(inch)",
                      "Very wet days","Extremely wet days","Contribution from very wet days",
                      "Contribution from Extremely wet days")

# Use PreciMonthly to get month index (inch)
MonthlyAnalysis <- data.frame(matrix(nrow = L, ncol = 20))
colnames(MonthlyAnalysis) <- c("Station","Station_name","Latitude","Longitude",
                         "Jan","Feb","Mar","Apr","May","Jun",
                         "Jul","Aug","Sep","Oct","Nov","Dec",
                         "Extreme Month","Wet Season","Precip Percentage of Year",
                         "Average Precip in Wet Season")
for(i in 1:L){
  print(i)
  Temp <- PreciMonthly[PreciMonthly[,1]==Unique.station[i],]
  Month <- c("01","02","03","04","05","06",
             "07","08","09","10","11","12")
  MonthlyAnalysis[i,1] <- paste(Temp[1,1]) # Station
  MonthlyAnalysis[i,2] <- paste(Temp[1,2]) # Station_name
  MonthlyAnalysis[i,3] <- paste(Temp[1,4]) # Latitude
  MonthlyAnalysis[i,4] <- paste(Temp[1,5]) # Longtitude
  j=7
  for(j in 1:12){
    Temp2 <- Temp[substrRight(Temp[,6],2)==Month[j],7] # Temp2 is one station on month
    if(length(Temp2)==0){
      MonthlyAnalysis[i,4+j] <- 0
    }else{
    MonthlyAnalysis[i,4+j]<- round(mean(Temp2),2)*0.039370
    }
  }
  MonthlyAnalysis[i,17] <- names(which.max(MonthlyAnalysis[i,5:16]))
  MonthlySum <-sum(MonthlyAnalysis[i,5:16]) # already inch
  Temp3 <- vector(length=12) # five month wet season table.
  names(Temp3) <- c("Jan-May","Feb-June","Mar-July","Apr-Aug","May-Sep",
                        "Jun-Oct","Jul-Nov","Aug-Dec","Sep-Jan","Oct-Feb","Nov-Mar","Dec-Apr")
  for(j in 1:12){
    if(j<=8){
      Temp3[j] <- round(sum(MonthlyAnalysis[i,c(j+4,j+5,j+6,j+7,j+8)])/MonthlySum,2)
    }else if(j==9){
      Temp3[j] <- round(sum(MonthlyAnalysis[i,c(1+4,9+4,10+4,11+4,12+4)])/MonthlySum,2)
    }else if(j==10){
      Temp3[j] <- round(sum(MonthlyAnalysis[i,c(1+4,2+4,10+4,11+4,12+4)])/MonthlySum,2)
    }else if(j==11){
      Temp3[j] <- round(sum(MonthlyAnalysis[i,c(1+4,2+4,3+4,11+4,12+4)])/MonthlySum,2)
    }else {Temp3[j] <- round(sum(MonthlyAnalysis[i,c(1+4,2+4,3+4,4+4,12+4)])/MonthlySum,2)
    }
  }
  MonthlyAnalysis[i,18] <- names(which.max(Temp3))
  MonthlyAnalysis[i,19] <- Temp3[which.max(Temp3)]
  MonthlyAnalysis[i,20] <- Temp3[which.max(Temp3)]*MonthlySum
}

# Use PreciYearly to get year index (inch)
YearlyAnalysis <- data.frame(matrix(nrow = L, ncol = 42))
colnames(YearlyAnalysis) <- c("Station","Station_name","Latitude","Longitude",
                               paste(seq(1980,2015)),"quantile 95%","quantile 90%")
for(i in 1:L){
  Temp <- PreciYearly[PreciYearly[,1]==Unique.station[i],]
  YearlyAnalysis[i,1] <- paste(Temp[1,1]) # Station
  YearlyAnalysis[i,2] <- paste(Temp[1,2]) # Station_name
  YearlyAnalysis[i,3] <- paste(Temp[1,4]) # Latitude
  YearlyAnalysis[i,4] <- paste(Temp[1,5]) # Longtitude
  Year <- paste(seq(1980,2015))
  for(j in 1:36){
    if(sum(Temp[,6]==Year[j])==0){
      YearlyAnalysis[i,j+4] <- 0
    } else {
      YearlyAnalysis[i,j+4] <- Temp[Temp[,6]== Year[j],7]*0.039370
    }
  }
  YearlyAnalysis[i,41] <- quantile(YearlyAnalysis[i,5:40],prob=0.95)
  YearlyAnalysis[i,42] <- quantile(YearlyAnalysis[i,5:40],prob=0.90)
}





# LA write out all the data table
LA.Preci <- cbind(DailyAnalysis,MonthlyAnalysis[,5:ncol(MonthlyAnalysis)],
                  YearlyAnalysis[,5:ncol(YearlyAnalysis)])
write.csv(DailyAnalysis,file="C:/Users/yi/Desktop/Cincs/Precipitation analysis/LA/DailyAnalysis.csv")
write.csv(MonthlyAnalysis,file="C:/Users/yi/Desktop/Cincs/Precipitation analysis/LA/MonthlyAnalysis.csv")
write.csv(YearlyAnalysis,file="C:/Users/yi/Desktop/Cincs/Precipitation analysis/LA/YearlyAnalysis.csv")
write.csv(PreciDaily,file="C:/Users/yi/Desktop/Cincs/Precipitation analysis/LA/PreciDaily.csv")
write.csv(PreciMonthly,file="C:/Users/yi/Desktop/Cincs/Precipitation analysis/LA/PreciMonthly.csv")
write.csv(PreciYearly,file="C:/Users/yi/Desktop/Cincs/Precipitation analysis/LA/PreciYearly.csv")
write.csv(LA.Preci,file="C:/Users/yi/Desktop/Cincs/Precipitation analysis/LA/LA_Preci.csv")

# FL write out all the data table
FL.Preci <- cbind(DailyAnalysis,MonthlyAnalysis[,5:ncol(MonthlyAnalysis)],
                  YearlyAnalysis[,5:ncol(YearlyAnalysis)])
write.csv(DailyAnalysis,file="C:/Users/yi/Desktop/Cincs/Precipitation analysis/FL/DailyAnalysis.csv")
write.csv(MonthlyAnalysis,file="C:/Users/yi/Desktop/Cincs/Precipitation analysis/FL/MonthlyAnalysis.csv")
write.csv(YearlyAnalysis,file="C:/Users/yi/Desktop/Cincs/Precipitation analysis/FL/YearlyAnalysis.csv")
write.csv(PreciDaily,file="C:/Users/yi/Desktop/Cincs/Precipitation analysis/FL/PreciDaily.csv")
write.csv(PreciMonthly,file="C:/Users/yi/Desktop/Cincs/Precipitation analysis/FL/PreciMonthly.csv")
write.csv(PreciYearly,file="C:/Users/yi/Desktop/Cincs/Precipitation analysis/FL/PreciYearly.csv")
write.csv(FL.Preci,file="C:/Users/yi/Desktop/Cincs/Precipitation analysis/FL/FL_Preci.csv")
