setwd("C:/Users/yi/Desktop/Cincs")
Cdata = read.csv("C:/Users/yi/Desktop/Cincs/County0629test.csv") 
Zdata = read.csv("C:/Users/yi/Desktop/Cincs/Zip0629test.csv")
Map = read.csv("C:/Users/yi/Desktop/Cincs/Map0629test.csv")
fix(Map)

#Zipcode data
ZL <- nrow(Zdata)
allCou <- Zdata[0,]
#DamageRank of a zipcode rank in all Zipcode in one couty
DamageRank = vector(length=ZL)
#Number of Zipcode recorded in one couty
NumberZip = vector(length=ZL)
for(i in 1:ZL){
Cou <- Zdata[i,3]
allCou <- Zdata[Cou==Zdata$County,]
allCou <- allCou[allCou$TotalDamage!="#N/A",]
NumberZip[i] <- nrow(allCou)
allCouDamage <- as.numeric(paste(allCou$TotalDamage))
DamageRankt <- rank(-allCouDamage,ties.method="min")
allCou<- cbind(allCou,DamageRankt)
if(sum(allCou$Zipcode==Zdata[i,1])==0) {
DamageRank[i] <- 0
} else {
DamageRank[i] <- allCou[allCou$Zipcode==Zdata[i,1],8]
}
print(i)
}
Zdata=cbind(Zdata,NumberZip,DamageRank)
fix(Zdata)

#County data
Cdata=Cdata[rowSums(is.na(Cdata))==0,] # remove the rows with NA
# Number of a County rank in all Counties
NumberRank <- rank(-Cdata$Numbers,ties.method="min")
DamageRankC <- rank(-Cdata$TotalDamage, ties.method="min")
Cdata <- cbind(Cdata,NumberRank,DamageRankC)
fix(Cdata)

# Input Target ZipCode
Tdata = c(32819,34786,32835,32811,32839,32809,32837,32821,32836)
TL <- length(Tdata)
TZdata <- Zdata[0,]
colnames(TZdata) <- names(Zdata)
for(i in 1:TL){
TZdata[i,]<-Zdata[Zdata$Zipcode==Tdata[i],]
}
fix(TZdata)

# Search Target County
TCdata <- Cdata[0,]
for(i in 1:TL){
Cou <- Map[Map$Zipcode==Tdata[i],4] 
TCdata[i,]<-Cdata[match(Cou,Cdata$County),]
}
fix(TCdata)


? rank
# Fill the blanks
b1 <- TCdata$Numbers
b2 <- b1/60
b3 <- TCdata$NumberRank
b4 <- c(67,67,67,67,67,67,67,67,67)
b5 <- TZdata$State
b6 <- TZdata$NumbersZip
b7 <- TZdata$Zipcode
b8 <- TZdata$TotalDamage
b9 <- as.numeric(paste(TZdata$TotalDamage))/
as.numeric(paste(TCdata$TotalDamage))
# Damage of a zipcode rank in all Zipcode in one couty
b10<- TZdata$DamageRank
b11<- TZdata$NumberZip
b12<- TCdata$DamageRankC
b13<- c(67,67,67,67,67,67,67,67,67)
b14<- TZdata$State
b15<- TCdata$TotalDamage/sum(Cdata$TotalDamage)
b16<- TZdata$State

Result <- data.frame(b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16)
fix(Result)
write.csv(Result,file="CompareZip.csv")
write.csv(TZdata,file="TZ.csv")