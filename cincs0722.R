# test whether slope is 0 as time series of extreme precipitation data

result <- read.csv("C:/Users/yi/Desktop/Cincs/DailyAnalysis.csv")
#fix(result)
colnames(result) <- c("Index","Station","Station_name","Latitude","Longitude","Year",
                      "Number of Wet Days","Number of heavy precipitation days",
                      "Number of very heavy precipitation days",
                      "Annual total wet day precipitation","Simple daily intensity index",
                      "Very wet days","Extremely wet days","Contribution from very wet days",
                      "Contribution from Extremely wet days")
#Year <- as.numeric(result$Year)
Unique.station <- unique(paste(result[,2]))
L <- length(Unique.station)
Sig <-data.frame(matrix(nrow = L, ncol=8))
fix(Sig)
for(i in 497:500){
  #i=144
  temp <- result[result[,2]==Unique.station[i],]
  #fix(temp)
  #names(temp)
  for(j in 1:8){
  # j=2
  d <- temp[,7+j]
  Year <- as.numeric(temp[,6])
  fit <- lm(d~Year)
  low <- confint(fit,level=0.95)[[2]]
  high <- confint(fit,level=0.95)[[4]]
  
  # acf(d) and a test for monotonic trend in a time series
  MannKendall(d)
  
  # linear regression
  if(low>0 & high>0){
  Sig[i,j]  <- "Significant"
  }
  else{
  Sig[i,j]  <- "NoSignificant"
  }
  }
  print(i)
}

plot(Year,d1,ylab="Number of Wet Days",main=result$Station[1])
abline(fit1)
summary(fit1)
CI <- predict(fit1, data.frame(Year),interval="confidence", level=0.95)

#percent <- abs(CI[1,1]-CI[length(Year),1])/CI[1,1] #(0.132458)

# confidence interval of slope and intercept
confint(fit,level=0.95)
# Hypothesis test, calculate t*, if t* larger than quantile of 95% based on degree of freedom
t <- summary(fit)$Coef[6]
qt <- qt(0.975,length(Year)-2)
