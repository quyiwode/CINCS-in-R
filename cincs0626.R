data = read.csv("C:/Users/yi/Desktop/Cincs/rawData.csv") 
setwd("C:/Users/yi/Desktop/Cincs")
fix(data)
names(data)
attach(data)
par(mfrow=c(2,1))
hist(TotalDuration,breaks = 100)
Damagehist <- hist(TotalDamageClear,breaks=100)
qua <- quantile(TotalDuration, prob=c(0,0.2,0.4,0.6,0.8,1))
for(i in 1:4){
abline(v=qua[[i+1]],col=i+1)
print(sum((TotalDuration >=qua[[i]]) & (TotalDuration < qua[[i+1]])))
}

#20150626
County = read.csv("C:/Users/yi/Desktop/Cincs/County0626.csv") 
fix(County)
dim(County)
County <- County[rowSums(is.na(County)) == 0,]
Countytest <- County[c(-7,-12,-17,-19,-27,-48),]
dim(Countytest)
names(Countytest) <- c("County","TotalDamage","Payment")
# detach(data)
fix(Countytest)
opt <- options("scipen" = 20)
getOption("scipen")
CountyName <-"MIAMI-DADE"
MiamiDamage <- Countytest[Countytest$County==CountyName,2]
#colfunc <- colorRampPalette(c("black", "white"))
#colfunc(2)

# version 1 his
hist(Countytest$TotalDamage/1000000,breaks=15,prob=TRUE,yaxt="n",ylab="",
xlab="Severity of Damage on County (Millions)",
main="Probability Density Function")
#lines(density(Countytest$TotalDamage/1000000),lwd=3)
abline(v=MiamiDamage/1000000, col=2,lwd=3)

#version 2 density
d<-density(Countytest$TotalDamage/1000000)
plot(d,lwd=3,yaxt="n",ylab="",
xlab="Severity of Damage on County (Millions)",
main="Probability Density Function")
polygon(d, col="grey", border="grey")
abline(v=MiamiDamage/1000000, col=4,lwd=3)
text(x=MiamiDamage/1000000+70, y=0.01, 
labels=paste(CountyName, "County"),font = 3)
arrows(x0=MiamiDamage/1000000+120, y0=0.009,
x1=MiamiDamage/1000000, y1=0.009,lwd=2,col=4)
save.image("C:\\Users\\yi\\Desktop\\Cincs\\0626.RData")
 
# Insurance
names(Countytest)
Countytest$Payment <- as.numeric(paste(Countytest$Payment))
hist(Countytest$Payment,brakes=15)