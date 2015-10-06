# Monthly Precip analysis

setwd("C:/Users/yi/Desktop/Cincs/Database/Precipitation")
Preci1 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/Precipitation/Monthly_1980-1984.csv")
Preci2 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/Precipitation/Monthly_1985-1999.csv")
Preci3 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/Precipitation/Monthly_2000-2013.csv")
Preci <- rbind(Preci1,Preci2,Preci3)
names(Preci)
fix(Preci)
Preci.filter <- Preci[(Preci[, 12]!= 0 & Preci[, 12]!=-9999),]
#fix(Preci.filter)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
Month <- substrRight(Preci.filter[,6],2)
Preci.filter2 <- cbind(Preci.filter,Month)
names(Preci.filter2)
Unique.station <- unique(Preci.filter[,1])
Unique.Month <- unique(Month)
M <- length(Unique.Month)
L <- length(Unique.station)
PreciMean <- matrix(nrow = L, ncol = M)
colnames(PreciMean) <- c("Jan","Feb","Mar","Apr","May",
                      "Jun","Jul","Aug","Sep","Oct","Nov","Dec")
for(i in 1:L)
{
  # i=1
  Temp <- Preci.filter2[Unique.station[i] == Preci.filter2[,1],]
  for(j in 1:M)
  {
    #j=1
    PreciMonth <- Temp[Unique.Month[j] == Temp[,13],12]
    PreciMean[i,j]<- round(mean(PreciMonth))/100
  }
}
#fix(PreciMean)
PreciSum <- rowSums(PreciMean)

Extreme <- vector(length=L)
Season <- matrix(nrow = L, ncol = M)

for(i in 1: L)
{
  #i=1
  Extreme[i] <- names(which.max(PreciMean[i,]))
    for(j in 1:M)
  {
    #j=6
    if(j<=8){
      Season[i,j] <- round(sum(PreciMean[i,c(j,j+1,j+2,j+3,j+4)])/PreciSum[i],2)
    }else if(j==9){
        Season[i,j] <- round(sum(PreciMean[i,c(1,9,10,11,12)])/PreciSum[i],2)
    }else if(j==10){
      Season[i,j] <- round(sum(PreciMean[i,c(1,2,10,11,12)])/PreciSum[i],2)
    }else if(j==11){
          Season[i,j] <- round(sum(PreciMean[i,c(1,2,3,11,12)])/PreciSum[i],2)
    }else {Season[i,j] <- round(sum(PreciMean[i,c(1,2,3,4,12)])/PreciSum[i],2)
          }
  }
}
# fix(Season)

colnames(Season) <- c("Jan-May","Feb-June","Mar-July","Apr-Aug","May-Sep",
                         "Jun-Oct","Jul-Nov","Aug-Dec","Sep-Jan","Oct-Feb","Nov-Mar","Dec-Apr")

ExtremeSeason = matrix(nrow = L, ncol = 3)

for(i in 1: L){
  if(i!=102 & i!=179){
    ExtremeSeason[i,1] <- names(which.max(Season[i,]))
    ExtremeSeason[i,2] <- max(Season[i,])
    ExtremeSeason[i,3] <- max(Season[i,])*PreciSum[i]/5
  }else{
    ExtremeSeason[i,1] <-9999
    ExtremeSeason[i,2] <-9999
    ExtremeSeason[i,3] <-9999
  }
}
#fix(ExtremeSeason)
colnames(ExtremeSeason) <- c("Wet Season","Precip Percentage of Year",
                             "Average Precip in Wet Season")

Output <- cbind(paste(Unique.station),PreciMean, PreciSum,Extreme,ExtremeSeason)
fix(Output)
colnames(Output)[1] <- "Station"
colnames(Output)[14] <- "Year"
colnames(Output)[15] <- "Extreme Month"
which.max(PreciMean[1,])
#fix(Output)
# Add text to plot
mp <- barplot(PreciMean[1,],ylab = "Average precipitation inches", 
        main=c("Station",paste(Unique.station[1])),ylim=c(0,10),col=4)
text(mp, PreciMean[1,], labels =PreciMean[1,], pos = 3)

write.csv(Output,file="C:/Users/yi/Desktop/Cincs/MonthlyAnalysis.csv")



# plot of monthly data
data <- read.csv("C:/Users/yi/Desktop/Cincs/MonthlyAnalysis.csv")
names(data)
data$LATITUDE
myLat <- 25.760148
myLon <- -80.191830
index <- which.min((myLat-data$LATITUDE)^2 + (myLon-data$LONGITUDE)^2)
data[index,2]
mydata <- as.numeric(data[42,6:17])
class(mydata)
mp <- barplot(height=mydata, names.arg = c("Jan","Feb","Mar","Apr","May",
                                           "Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
              ylab = "Average precipitation inches", 
             main=c("Station",paste(data[42,3])),col=4,ylim=c(0,9))
text(mp, mydata, labels = mydata, pos = 3)






