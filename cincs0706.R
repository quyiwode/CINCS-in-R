### transfor two dimension map data into two coloum.

setwd("C:/Users/yi/Onedrive/Cincs/Precipitation analysis")
Pred20 <- read.csv("2020_LA.csv")
Pred50 <- read.csv("2050_LA.csv")

#### lat and lon is wrong
lat <- colnames(Pred20)
lat <- lat[2:length(lat)]
class(lat) # numerical lat
lon <- Pred20[1:30,1]
class(lon) # numerical lon

Lon <- 30
Lat <- length(lat)

lat.all <- vector(length=Lon*Lat)
#lat.allt <- vector(length=Lon*Lat)
lon.all <- vector(length=Lon*Lat)
per.all <- vector(length=Lon*Lat)
per.2020 <- per.all
per.2050 <- per.all

for(i in 1: Lon){
for(j in 1: Lat){
#lat.allt[(i-1)*Lat+j] <- colnames(Historical)[j+1]
lat.all[(i-1)*Lat+j] <- lat[j]
lon.all[(i-1)*Lat+j] <- Pred20[i,1]
#per.all[(i-1)*Lat+j] <- as.numeric(paste(Historical[i,j+1]))
per.2020[(i-1)*Lat+j] <- as.numeric(paste(Pred20[i,j+1]))
per.2050[(i-1)*Lat+j] <- as.numeric(paste(Pred50[i,j+1]))
}
}
per <- as.data.frame(cbind(lat.all,lon.all,per.2020,per.2050))
perr <- per[as.numeric(paste(per$per.2020))<10,]

#revise colnames to correct mistake before.
colnames(perr) <- c("Lon","Lat","Projection2020","Projection2050")
write.csv(perr, "C:/Users/yi/Desktop/LA_Projection.csv")
