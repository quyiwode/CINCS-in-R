# two source of Yearly precip analysis.

setwd("C:/Users/yi/Desktop/Cincs/Database/Precipitation")
Preci <- read.csv("C:/Users/yi/Desktop/Cincs/Database/Precipitation/Monthly_filter.csv")
names(Preci)
substrLeft <- function(x, n){
  substr(x, 1, 4)
}
Year <- substrLeft(Preci.filter[,6],4)
Unique.station <- unique(Preci.filter[,1])
Unique.Year <- unique(Year)
Y <- length(Unique.Year)
L <- length(Unique.station)
Preci2 <- cbind(Preci,Year)

PreciAnnual <- matrix(nrow = L, ncol = Y)
for(i in 1:L)
{
  # i=1
  Temp <- Preci2[Unique.station[i] == Preci2[,2],]
  for(j in 1:Y)
  {
    
    PreciAnnual[i,j] <- sum(Temp[Unique.Year[j] == Temp[,14],13])/100
  }
}
# fix(PreciAnnual)
colnames(PreciAnnual) <- Unique.Year
Output<- cbind(paste(Unique.station),PreciAnnual)
#fix(Output)

write.csv(Output,file="C:/Users/yi/Desktop/Cincs/YearlyAnalysis.csv")

choose <- c(1,16,27)
I <- length(choose)
par(mfrow=c(I,1))

for(i in 1:I)
{
  barplot(PreciAnnual[choose[i],],ylab = "Annual precipitation inches", 
              main=c("Station",paste(Unique.station[choose[i]])),col=i)
}
for(i in 1:I)
{
  plot(Unique.Year,PreciAnnual[choose[i],],col=i,type="l",ylim=c(30,85),
       main=c("Station",paste(Unique.station[choose[i]])))
}
# text(mp, PreciAnnual[1,], labels =PreciAnnual[1,], pos = 3)
OldYear <- seq(1970,1999)
O <- length(OldYear)
temp70 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1970.csv"))
temp71 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1971.csv"))
temp72 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1972.csv"))
temp73 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1973.csv"))
temp74 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1974.csv"))
temp75 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1975.csv"))
temp76 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1976.csv"))
temp77 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1977.csv"))
temp78 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1978.csv"))
temp79 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1979.csv"))
temp80 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1980.csv"))
temp81 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1981.csv"))
temp82 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1982.csv"))
temp83 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1983.csv"))
temp84 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1984.csv"))
temp85 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1985.csv"))
temp86 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1986.csv"))
temp87 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1987.csv"))
temp88 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1988.csv"))
temp89 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1989.csv"))
temp90 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1990.csv"))
temp91 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1991.csv"))
temp92 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1992.csv"))
temp93 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1993.csv"))
temp94 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1994.csv"))
temp95 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1995.csv"))
temp96 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1996.csv"))
temp97 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1997.csv"))
temp98 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1998.csv"))
temp99 <- read.csv(c("C:/Users/yi/Desktop/Cincs/Database/OldPrecip/1999.csv"))

latt <- colnames(temp70)
latt <- latt[2:length(latt)]
latt <- substrRight(latt,6)
lat <- as.numeric(latt)
class(lat) # numerical lat
lon <- temp70[,1]
class(lon) # numerical lon

Lon <- dim(temp70)[1]
Lat <- dim(temp70)[2]-1

lat.all <- vector(length=Lon*Lat)
lon.all <- vector(length=Lon*Lat)
per70 <- vector(length=Lon*Lat)
per71 <- vector(length=Lon*Lat)
per72 <- vector(length=Lon*Lat)
per73 <- vector(length=Lon*Lat)
per74 <- vector(length=Lon*Lat)
per75 <- vector(length=Lon*Lat)
per76 <- vector(length=Lon*Lat)
per77 <- vector(length=Lon*Lat)
per78 <- vector(length=Lon*Lat)
per79 <- vector(length=Lon*Lat)
per80 <- vector(length=Lon*Lat)
per81 <- vector(length=Lon*Lat)
per82 <- vector(length=Lon*Lat)
per83 <- vector(length=Lon*Lat)
per84 <- vector(length=Lon*Lat)
per85 <- vector(length=Lon*Lat)
per86 <- vector(length=Lon*Lat)
per87 <- vector(length=Lon*Lat)
per88 <- vector(length=Lon*Lat)
per89 <- vector(length=Lon*Lat)
per90 <- vector(length=Lon*Lat)
per91 <- vector(length=Lon*Lat)
per92 <- vector(length=Lon*Lat)
per93 <- vector(length=Lon*Lat)
per94 <- vector(length=Lon*Lat)
per95 <- vector(length=Lon*Lat)
per96 <- vector(length=Lon*Lat)
per97 <- vector(length=Lon*Lat)
per98 <- vector(length=Lon*Lat)
per99 <- vector(length=Lon*Lat)
for(i in 1:Lon){
  for(j in 1: Lat){
    lat.all[(i-1)*Lat+j] <- lat[j]
    lon.all[(i-1)*Lat+j] <- temp70[i,1]
    per70[(i-1)*Lat+j] <- as.numeric(paste(temp70[i,j+1]))
    per71[(i-1)*Lat+j] <- as.numeric(paste(temp71[i,j+1]))
    per72[(i-1)*Lat+j] <- as.numeric(paste(temp72[i,j+1]))
    per73[(i-1)*Lat+j] <- as.numeric(paste(temp73[i,j+1]))
    per74[(i-1)*Lat+j] <- as.numeric(paste(temp74[i,j+1]))
    per75[(i-1)*Lat+j] <- as.numeric(paste(temp75[i,j+1]))
    per76[(i-1)*Lat+j] <- as.numeric(paste(temp76[i,j+1]))
    per77[(i-1)*Lat+j] <- as.numeric(paste(temp77[i,j+1]))
    per78[(i-1)*Lat+j] <- as.numeric(paste(temp78[i,j+1]))
    per79[(i-1)*Lat+j] <- as.numeric(paste(temp79[i,j+1]))
    per80[(i-1)*Lat+j] <- as.numeric(paste(temp80[i,j+1]))
    per81[(i-1)*Lat+j] <- as.numeric(paste(temp81[i,j+1]))
    per82[(i-1)*Lat+j] <- as.numeric(paste(temp82[i,j+1]))
    per83[(i-1)*Lat+j] <- as.numeric(paste(temp83[i,j+1]))
    per84[(i-1)*Lat+j] <- as.numeric(paste(temp84[i,j+1]))
    per85[(i-1)*Lat+j] <- as.numeric(paste(temp85[i,j+1]))
    per86[(i-1)*Lat+j] <- as.numeric(paste(temp86[i,j+1]))
    per87[(i-1)*Lat+j] <- as.numeric(paste(temp87[i,j+1]))
    per88[(i-1)*Lat+j] <- as.numeric(paste(temp88[i,j+1]))
    per89[(i-1)*Lat+j] <- as.numeric(paste(temp89[i,j+1]))
    per90[(i-1)*Lat+j] <- as.numeric(paste(temp90[i,j+1]))
    per91[(i-1)*Lat+j] <- as.numeric(paste(temp91[i,j+1]))
    per92[(i-1)*Lat+j] <- as.numeric(paste(temp92[i,j+1]))
    per93[(i-1)*Lat+j] <- as.numeric(paste(temp93[i,j+1]))
    per94[(i-1)*Lat+j] <- as.numeric(paste(temp94[i,j+1]))
    per95[(i-1)*Lat+j] <- as.numeric(paste(temp95[i,j+1]))
    per96[(i-1)*Lat+j] <- as.numeric(paste(temp96[i,j+1]))
    per97[(i-1)*Lat+j] <- as.numeric(paste(temp97[i,j+1]))
    per98[(i-1)*Lat+j] <- as.numeric(paste(temp98[i,j+1]))
    per99[(i-1)*Lat+j] <- as.numeric(paste(temp99[i,j+1]))
    }
}
Oldper<- as.data.frame(cbind(lat.all,lon.all,
      per70,per71,per72,per73,per74,per75,per76,per77,per78,per79,
      per80,per81,per82,per83,per84,per85,per86,per87,per88,per89,
      per90,per91,per92,per93,per94,per95,per96,per97,per98,per99))
fix(Oldper)
Oldper<- Oldper[rowSums(is.na(Oldper))==0,]
colnames(Oldper) <- c("Lat","Lon",seq(1970,1999))
Oldper[,3:dim(Oldper)[2]] <- Oldper[,3:dim(Oldper)[2]]*0.039370 # change from mm to inch
choose <- c(1,16,27)
fix(Oldper)
par(mfrow=c(2,1))
plot(seq(1970,1999),Oldper[479,3:dim(Oldper)[2]],type="l",ylim=c(30,85),
     main="Coordinate",xlab="Year",ylab="Annual Precipitation (inch)",lwd=2)
#plot(seq(1970,1999),Oldper[802,3:dim(Oldper)[2]],type="l",ylim=c(30,85))
#plot(seq(1970,1999),Oldper[500,3:dim(Oldper)[2]],type="l",ylim=c(30,85))
#plot(seq(1970,1999),Oldper[389,3:dim(Oldper)[2]],type="l",ylim=c(30,85))
#plot(seq(1970,1999),Oldper[81,3:dim(Oldper)[2]],type="l",ylim=c(30,85))

plot(Unique.Year,PreciAnnual[1,],col=2,type="l",ylim=c(30,85),lwd=2,
     xlab="Year",ylab="Annual Precipitation (inch)",main="Station")
#plot(Unique.Year,PreciAnnual[2,],col=2,type="l",ylim=c(30,85),
#     main="Station")
#plot(Unique.Year,PreciAnnual[3,],col=2,type="l",ylim=c(30,85),
#     main="Station")
#plot(Unique.Year,PreciAnnual[4,],col=2,type="l",ylim=c(30,85),
#     main="Station")
#plot(Unique.Year,PreciAnnual[6,],col=2,type="l",ylim=c(30,85),
#     main="Station")

write.csv(Oldper,file="C:/Users/yi/Desktop/Cincs/YearlyAnalysis_Coordinate.csv")

# use coordinate data to repair station data.

fix(Oldper)
newper <- read.csv("C:/Users/yi/Desktop/Cincs/YearlyAnalysis_Station.csv")
fix(newper)
L <- dim(newper)[1]
index <- vector(length=L)
for(i in 1:L){
  #i = 1
  lat.s <- as.numeric(paste(newper[i,4]))
  lon.s <- as.numeric(paste(newper[i,5]))
  temp <- (lat.s-Oldper$Lat)^2 + (lon.s-Oldper$Lon)^2
  index[i] <- which.min(temp)
 }

for(i in 1:L){
  # i = 1
   if(sum(newper[i,6:25]==0) > 0)
     newper[i,6:25] <- Oldper[index[i],13:32]
}
fix(newper)
write.csv(newper,file="C:/Users/yi/Desktop/Cincs/YearlyAnalysis.csv")







