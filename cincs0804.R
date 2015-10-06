# Present scoring system in PPT

Durationdata <- read.csv("C:/Users/yi/Desktop/DurationScore.csv")
#fix(Durationdata)
par(mfrow=c(2,1))
hist(Durationdata[,3],breaks=15,main="Duration",xlab="Cumulative Flood Days in 50 years")
hist(Durationdata[,4],breaks=15,main="DurationScore", xlab="Score")
abline(v = 33.33, col = 2, lwd = 2)
abline(v = 66.66, col = 2, lwd = 2)

Precipdata <- read.csv("C:/Users/yi/Desktop/PrecipitationScore.csv")
#fix(Precipdata)
names(Precipdata)
par(mfrow=c(2,1))
hist(Precipdata[,5],breaks=30,main="Precip index",xlab="Average Daily precipitation(inch) in 30 years")
hist(Precipdata[,6],breaks=30,main="PrecipScore",xlab="Score")
abline(v = 33.33, col = 2, lwd = 2)
abline(v = 66.66, col = 2, lwd = 2)

Damagedata <- read.csv("C:/Users/yi/Desktop/DamageScore.csv")
#fix(Damagedata)
names(Damagedata)
par(mfrow=c(2,1))
hist(Damagedata[,2],breaks=30,main="Damage",xlab="Average Damage in 30 years")
hist(Damagedata[,3],breaks=30,main="DamageScore",xlab="Score")
abline(v = 33.33, col = 2, lwd = 2)
abline(v = 66.66, col = 2, lwd = 2)

#########
Wholedata <- read.csv("C:/Users/yi/Desktop/WholeScore.csv")
names(Wholedata)
par(mfrow=c(3,1))
hist(Wholedata[,7],breaks=30,xlim=c(0,100),main="Damage",xlab="score")
abline(v = 33.33, col = 2, lwd = 2)
abline(v = 66.66, col = 2, lwd = 2)

# test <- scale(Wholedata[,7]) 
# hist(test) 

hist(Wholedata[,9],breaks=30,xlim=c(0,100),main="Duration",xlab="score")
abline(v = 33.33, col = 2, lwd = 2)
abline(v = 66.66, col = 2, lwd = 2)
hist(Wholedata[,5],breaks=30,xlim=c(0,100),main="Precip",xlab="score")
abline(v = 33.33, col = 2, lwd = 2)
abline(v = 66.66, col = 2, lwd = 2)


Average = (Wholedata[,5]+Wholedata[,7]+Wholedata[,9])/3
AverageScale <- (Average-min(Average))/(max(Average)-min(Average))*100
hist(AverageScale,xlim=c(0,100),main="MVP",xlab="score",breaks=15)
hist(Average,xlim=c(0,100),main="MVP",xlab="score",breaks=15)

hist(AverageScale,prob=TRUE,xlim=c(0,100),main="MVP",xlab="score",breaks=15)
curve(dnorm(x, mean=mean(AverageScale), sd=sd(AverageScale)), add=TRUE,col=2,lwd=2)
# fix(Wholedata)
L <- dim(Wholedata)[1]
for(i in 1:L){
  # i =1
  index <- which.min((Wholedata[i,2]-Precipdata[,3])^2 + (Wholedata[i,3]-Precipdata[,4])^2)
  Wholedata[i,5]= Precipdata[index,6]
}

quantile(Output$DamageScore,prob= c(0.33,0.66))
quantile(Output$DurationScore,prob=c(0.33,0.66))
quantile(Output$Station,prob=c(0.33,0.66))

FloodZone <- read.csv("C:/Users/yi/Desktop/FloodZoneMap.csv")
names(FloodZone)
names(Wholedata)
Fz = vector(length=L)
FzScore = vector(length=L)
latt = vector(length=L)
longg = vector(length=L)

for(i in 1: L){
  print(i)
  index <- which.min((FloodZone$lat-Wholedata[i,2])^2 +
    (FloodZone$long-Wholedata[i,3])^2)
  latt[i] <- FloodZone[index,3]
  longg[i] <- FloodZone[index,2]
  Fz[i] = paste(FloodZone[index,1])
  if(Fz[i]=="X"){
    FzScore[i] = 10
  } else if (Fz[i]=="AE" | Fz[i]=="A" | Fz[i]=="VE" | Fz[i]=="AH" |
             Fz[i]=="AO" | Fz[i]=="OPEN WATER"){
    FzScore[i] = 90
  } else{
    FzScore[i] = 0
  }
}
unique(Fz)

Average = (Output$Station + Output$DamageScore + Output$DurationScore + Output$FzScore)/4
hist(AverageScale, xlim = c(0,100), breaks = 20)
AverageScale <- (Average-low)/(high-low)*100
high <- max(Average) # 10
low <- min(Average) # 83

Rank <- rank(Average)
perc.rank <- function(x) trunc(rank(x))/length(x)
Percentage <- perc.rank(Average)

ZipRank = vector(length=L)
ZipCounty = vector(length=L)
# zipcode rank in conunty
for(i in 1:L){
  #print(i)
  this_zip <- Output$zip[i]
  this_county <- Output$County[i]
  Wholecounty <- Output$Average[which(as.character(Output$County) == this_county)]
  this_score <- Output$Average[i]
  ZipRank[i] = sum(Wholecounty > this_score) +1
  ZipCounty[i] = length(Wholecounty)
}

Output <- cbind(Wholedata,Fz,FzScore,Average,AverageScale,ZipRank,ZipCounty,Percentage)
names(Output)
write.csv(Output,"C:/Users/yi/Desktop/Output.csv")

# get quantile of score
Score2 <- read.csv("C:/Users/yi/Desktop/Score2.csv")
fix(Score2)
hist(Score2$Average)
a<-quantile(Score2$Average,probs= seq(0,1,0.1))
write.csv(a,"C:/Users/yi/Desktop/a.csv")

