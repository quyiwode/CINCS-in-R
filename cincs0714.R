setwd("C:/Users/yi/Desktop/Cincs/Database/Precipitation")
Preci1 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/Precipitation/Monthly_1980-1984.csv")
Preci2 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/Precipitation/Monthly_1985-1999.csv")
Preci3 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/Precipitation/Monthly_2000-2013.csv")
Preci <- rbind(Preci1,Preci2,Preci3)
names(Preci)
Preci.filter <- Preci[(Preci[, 12]!= 0 & Preci[, 12]!=-9999),]
names(Preci.filter)
write.csv(Preci.filter,file="Monthly_filter.csv")

Precip.M <- read.csv("C:/Users/yi/Desktop/Cincs/Database/Precipitation/Monthly_filter.csv")
names(Precip.M)
P085663 <- Precip.M[Precip.M$STATION=="COOP:085663",c("STATION","YEAR","Total.precipitation..0.01.inch.")]
fix(P085663)
P085658 <- Precip.M[Precip.M$STATION=="COOP:085658",c("STATION","YEAR","Total.precipitation..0.01.inch.")]
fix(P085658)

P085663.Y <- vector(length=2013-1980+1)
for(i in 1:34){
  P085663.Y[i]<-sum(P085663[P085663$YEAR==i+1979,"Total.precipitation..0.01.inch."])
}
Y <- seq(1980,2013,1)
barplot(P085663.Y,space=1,col=4,xpd=0,ylim=c(2000,8000),)
? barplot
