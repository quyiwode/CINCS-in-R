Score <- read.csv("C:/Users/yi/Desktop/Cincs/Score analysis/Score.csv")
names(Score)
fix(Score)
quantile(Score$X,probs= c(0.33,0.66)) # 51,65
quantile(Score$X,probs= c(0.2,0.8)) # 38, 72
quantile(Score$X,probs= c(0.2,0.6)) # 38, 63

# Histogram with three colors
h <- hist(Score$X, prob=TRUE,xlim = c(0,100), breaks=30,
main="",xlab="",ylab="",yaxt='n',col="grey",
col = c(rep('#66CDAA',5),rep("darkgoldenrod1",6),rep("brown2",7)))
lines(density(Score$X), col="black", lwd=1) 
abline(v=42,col="blue",lwd=2)

# Histogram with gradient colors, without borders
a<-colorRampPalette(c("green","orange","red"))
a(20)
hist(Score$X, prob=TRUE,xlim = c(0,100), breaks=20,
     main="",xlab="",ylab="",yaxt='n',col = a(20),bty='n',lty="blank")
#abline(v=42,col="blue",lwd=2)
curve(dnorm(x, mean=mean(Score$X), sd=sd(Score$X)),add=TRUE,lwd=2,xlim=c(0,100))
