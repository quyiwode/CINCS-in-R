# Test 

Declar <- read.csv("C:/Users/yi/Desktop/Cincs/Database/Declaration.csv")
House <- read.csv("C:/Users/yi/Desktop/Cincs/Database/HouseOwner.csv")
Insur <- read.csv("C:/Users/yi/Desktop/Cincs/Database/Insurance.csv")
Map <- read.csv("C:/Users/yi/Desktop/Cincs/Database/Map.csv")

names(Map)
Map <- Map[Map$State=="FL",] # filter FL
dim(Map) # 1470
fix(Map)
UniqueCity <- unique(paste(Map$City))
UniqueCity <- sort(UniqueCity)
UniqueCounty <- sort(unique(paste(Map$County)))
#UniqueCity[421:435]

names(Insur)
InsurCity<- sort(paste(Insur$COMMUNITY.NAME.OF))
#InsurCity[330:361]
unique(paste(Insur$COUNTY.NAME))

names(Declar)
fix(Declar)
DeclarCounty <- sort(unique(Declar$Declared.County.Area))
DeclarCounty


