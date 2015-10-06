Declar <- read.csv("C:/Users/yi/Desktop/Cincs/Database/Declaration.csv")
House <- read.csv("C:/Users/yi/Desktop/Cincs/Database/HouseOwner.csv")
Map <- read.csv("C:/Users/yi/Desktop/Cincs/Database/Map.csv")
fix(Declar)
fix(House)
fix(Map)

# prepare the dataset
House <- House[1:7616,]
MapN <- Map[Map$State=="FL",]

# find right county for House data
L <- dim(House)[1]
HouseCounty <- vector(length=L)
for(i in 5:L)
{
  if(sum(House[i,5] == MapN[,1])==1){
  HouseCounty[i] <- paste(MapN[House[i,5] == MapN[,1],4])
  print(i)
  }
  else{
  HouseCounty[i] <- NA 
  }
}

HouseNew<- cbind(House,HouseCounty)
fix(HouseNew)
names(HouseNew)

HouseNew <- HouseNew[is.na(HouseNew[,10])== 0,]
HouseNew <- HouseNew[HouseNew[,10]!="FALSE",]
UniqueCounty <- unique(HouseNew$HouseCounty)
C <- length(UniqueCounty)
DamageCounty <- vector(length=C)
for(i in 1:C){
  # i = 1
  DamageCounty[i]<- sum(HouseNew[paste(HouseNew[,10])== paste(UniqueCounty[i]),9])
}

Damage <- cbind(paste(UniqueCounty),DamageCounty)
fix(Damage)

Duration <- Declar$duration + 1
Cou <- substr(Declar$Declared.County.Area,1,nchar(as.character(Declar$Declared.County.Area))-9)
Unique.Cou <- unique(Cou)

C <- length(Unique.Cou)
DurationCounty <- vector(length=C)
for(i in 1:C){
  # i = 1
  index <- Cou==Unique.Cou[i]
  DurationCounty[i]<-sum(Duration[index])
}

Duration <- cbind(paste(Unique.Cou),DurationCounty)
fix(Duration)

Dam <- vector(length=C)
for(i in 1: 69){
  Dam[i]<- Damage[paste(toupper(Duration[i,1])) == paste(Damage[,1]),2]
}

Output <- cbind(Duration,Dam)
Output <- Output[1:67,]
cor(as.numeric(Output[,2]),as.numeric(Output[,3]))
colnames(Output) <- c("County","Damage","Duration")
fix(Output)
write.csv(Output,"C:/Users/yi/Desktop/Cincs/Correlation.csv")
