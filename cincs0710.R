setwd("C:/Users/yi/Desktop/Cincs")
Preci2013 <- read.csv("C:/Users/yi/Desktop/Cincs/Monthly_2000-2013_0710.csv")
fix(Preci2013)
names(Preci2013)


# Get Total Precip for ChooseStation
names(Preci2013) <- c("Station","StationName","Elevation","Latitude","Longitude",
"Date","DepartureNormal","NumberDays0.1","NumberDays0.5","NumberDays1.0",
"ExtremePreci","TotalPreci")
names(Preci2013)
ChooseStation <- Preci2013$Station[1]
TotalPreci.Choose <- Preci2013[Preci2013[,"Station"]==ChooseStation,c("TotalPreci")]
TotalPreci.Choose <- TotalPreci.Choose[TotalPreci.Choose!=-9999]


# Fit model to hist by density()
kernels <- eval(formals(density.default)$kernel)
hist(TotalPreci.Choose,breaks=30,prob=TRUE)
for (i in 1:7){
d <- density(TotalPreci.Choose,kernel = kernels[i])
lines(d, col=i) # add a density estimate with defaults
}
#curve(dnorm(x, mean=mean(TotalPreci.Choose), sd=sd(TotalPreci.Choose)), add=TRUE)
quantile(TotalPreci.Choose,probs=c(0.05,0.95))
lines(d)
polygon(d, col = "wheat")

# Fit gumbel model
library("gumbel")
? gumbel
library(fitdistrplus)
dgumbel <- function(x,mu,s){ # PDF
  exp((mu - x)/s - exp((mu - x)/s))/s
}
pgumbel <- function(q,mu,s){ # CDF
  exp(-exp(-((q - mu)/s)))
}
qgumbel <- function(p, mu, s){ # quantile function
  mu-s*log(-log(p))
}
? fitdist
gumbel.fit <- fitdist(TotalPreci.Choose, "gumbel", start=list(mu=5, s=5),
                      method="mle")
summary(gumbel.fit)
plot(gumbel.fit)
mu = gumbel.fit$estimate[[1]]
s = gumbel.fit$estimate[[2]]
qgumbel(p=0.05, mu=mu, s=s) # -63.47657
qgumbel(p=0.95, mu=mu, s=s) # 1192.289

x=seq(-500,2000)
plot(x,dgumbel(x,mu=mu,s=s))

# Fit gamma model
gamma.fit <- fitdist(TotalPreci.Choose, "gamma", method="mme")
plot(gamma.fit)
shape = gamma.fit$estimate[[1]] 
rate = gamma.fit$estimate[[2]] 
qgamma(0.05, shape = shape, rate = rate) # 29.52789
qgamma(0.95, shape = shape, rate = rate) # 1383.781
x=seq(1:2000)
plot(x,dgamma(x,shape=shape, rate=rate),type="l",
     xlab="Monthly Precipitation", ylab="Frequency")

# Fit exp mpdel
exp.fit <- fitdist(TotalPreci.Choose, "exp")
Rate = exp.fit$estimate[[1]]
plot(exp.fit)
qexp(0.05,rate=rate) # 22.49344
qexp(0.95,rate=rate) # 1313.706

# quantile in histogram
quantile(TotalPreci.Choose,c(0.05,0.95)) # 20.00, 1330.25

 



























 