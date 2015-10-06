setwd("C:/Users/yi/Desktop/Cincs")
Preci1 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/Precipitation/Hourly_1980-1995.csv")
Preci2 <- read.csv("C:/Users/yi/Desktop/Cincs/Database/Precipitation/Hourly_1996-2013.csv")
Preci <- rbind(Preci1,Preci2)
names(Preci)
Preci.filter <- Preci[(Preci[, 7]!= 0 & Preci[, 7]!=99.999),]
names(Preci.filter)
write.csv(Preci.filter.test,file="Hourly_filter.csv")


Loss.1000 <- c( 9550, 9620,9820, 12730,12780,15300,15508,15717,15925,16133,16342,16550,
               16796,17042,17288,17533,17779,18025,18271,18517,18763,19008,19254,19500,
               19633,19767,19900,20033,20167,20300,20433,20567,20700,20833,20967,21100,
               21292,21483,21675,21867,22058,22250,22442,22633,22825,23017,23208,23400)
Footage.1000 <- rep(1000,48)
Depth <- seq(from = 2.54, to = 121.92, by = 2.54)
Loss.2000 <- c(18940,19020,19520,25140,25210,29940,30297,30653,31010,31367,31723,32080,
               32411,32742,33073,33403,33734,34065,34396,34727,35058,35388,35719,36050,
               36293,36537,36780,37023,37267,37510,37753,37997,38240,38483,38727,38970,
               39337,39703,40070,40437,40803,41170,41537,41903,42270,42637,43003,43370)
Footage.2000 <- rep(2000,48)
plot(Depth,Loss.2000)
plot(Depth,Loss.1000/100)
plot(Loss.1000,Loss.2000)

Loss.3000 <- c(28964,28420,29220,37550,37640,44580,45085,45590,46095,46600,47105,47610,
               48026,48442,48858,49273,49689,50105,50521,50937,51353,51768,52184,52600,
               52953,53307,53660,54013,54367,54720,55073,55427,55780,56133,56487,56840,
               57382,57923,58465,59007,59548,60690,60632,61173,61715,62257,62798,63340)
Footage.3000 <- rep(3000,48)
plot(Loss.1000,Loss.3000)
plot(Depth[1:3],Loss.1000[1:3])

Footage <- seq(from=500, to=4000, by=500)
Loss.2.54 <- c(4855,9550,14245,18940,23635,28330,33025,37720)
Depth.2.54 <- rep(2.54,8)
lm(Loss.2.54~Footage) # 9.39     160  

Loss.5.08 <- c(4920,9620,14320,19020,23720,28420,33120,37820)
Depth.5.08 <- rep(5.08,8)
lm(Loss.5.08~Footage) # 9.4     220

Loss.7.62 <- c(4970,9820,14670,19520,24370,29220,34070,38920)
Depth.7.62 <- rep(7.62,8)
lm(Loss.7.62~Footage) # 9.7     120
plot(Loss.7.62,Footage)

Loss.10.16 <- c(6525,12730,18935,25140,31345,37550,43775,49960)
Depth.10.16 <- rep(10.16,8)
lm(Loss.10.16~Footage) # 12.41 317.14
plot(Loss.10.16,Footage)

Loss.12.70 <- c(6565,12780,18995,25210,31425,37640,43855,50070)
Depth.12.70 <- rep(12.70,8)
lm(Loss.12.70~Footage) # 12.43  350
plot(Loss.12.70,Footage)

Loss.15.24 <- c(7980,15300,22620,29940,37260,44580,51900,59220)
Depth.15.24 <- rep(15.24,8)
lm(Loss.15.24~Footage) # 14.64 660
plot(Loss.15.24,Footage)

Loss = c(Loss.1000,Loss.2000,Loss.3000,
      Loss.2.54,Loss.5.08,Loss.7.62,Loss.10.16,Loss.12.70,Loss.15.24)
Foot = c(Footage.1000,Footage.2000,Footage.3000,
         Footage,Footage,Footage,Footage,Footage,Footage)
Dep = c(Depth,Depth,Depth,
        Depth.2.54,Depth.5.08,Depth.7.62,Depth.10.16,Depth.12.70,Depth.15.24)

stage1 <-vector(length=192)
stage1[Dep<=7.62] = 1
stage2 <-vector(length=192)
stage2[Dep<=12.7& Dep >7.63] = 1

train <- cbind(Loss,Foot,Dep,stage1,stage2)
train <- data.frame(train)

fit<-lm(Loss~ Foot*stage1 + Foot*stage2 + Dep,data=train)
L<- predict(fit,newdata = train)
test <-cbind(Loss,L)
fix(test)

summary(fit)
plot(Foot,Loss)
plot(Dep,Loss)

slope = c(9.39,9.4,9.7,12.41,12.43,14.64)
plot(Depth[1:6],slope)
