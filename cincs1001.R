library(rgdal)
Gulf_Of_Mexico <- readOGR(dsn="C:/Users/yi/Desktop/Cincs/Database/Gulf_Of_Mexico",
                          layer="Gulf_Of_Mexico")
Southeast_Caribbean <- readOGR(dsn="C:/Users/yi/Desktop/Cincs/Database/Southeast_Caribbean",
                          layer="Southeast_Caribbean")

GetGPS <- function(a){
gps <- coordinates(a[1,])[[1]][[1]]
for(i in 2:nrow(a)){
  temp <- coordinates(a[i,])[[1]][[1]][1,]
  gps <- rbind(gps,temp)
  print(i)
}
return(gps)}

Gulf_Of_Mexico_GPS <- GetGPS(Gulf_Of_Mexico)
Southeast_Caribbean_GPS <- GetGPS(Southeast_Caribbean)

setwd("C:/Users/yi/Desktop/Cincs/Database")
write.csv(Gulf_Of_Mexico_GPS,"Gulf_Of_Mexico_GPS.csv")
write.csv(Southeast_Caribbean_GPS,"Southeast_Caribbean_GPS.csv")
