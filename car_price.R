#Linear Regression Model- Cars Price

library(tidyr)
library(MASS)

library("car")
cars_price <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = FALSE)

#Let us examine the structure of the dataset

str(cars_price)

#####################################Data Preparation################################################

# factorise categorical data.

cars_price[, c(3:9)] <- lapply(cars_price[,c(3:9)], factor)

cars_price$CarName <- as.factor(cars_price$CarName)



cars_price[, c(15,16,18)] <- lapply(cars_price[,c(15,16,18)], factor)


str(cars_price)

# finding Missing Values.

sum(is.na(cars_price))

# no missing value.

# checking outliers.

# outlier treatment on the price.

quantile(cars_price$price, seq(0,1,0.01))

boxplot(cars_price$price)

cars_price$price[which(cars_price$price>24316.6)] <- 24316.6




# outlier treatment on the wheelbase.

quantile(cars_price$wheelbase, seq(0,1,0.01))

boxplot(cars_price$wheelbase)

cars_price$wheelbase[which(cars_price$wheelbase>110)] <- 110


# outlier treatment on carlength

quantile(cars_price$carlength, seq(0,1,0.01))

boxplot(cars_price$carlength)

cars_price$carlength[which(cars_price$carlength<155.9)] <- 155.9


# Outlier treatment on the carwidth

quantile(cars_price$carwidth, seq(0,1,0.01))

boxplot(cars_price$carwidth)

cars_price$carwidth[which(cars_price$carwidth>68.9)] <- 68.9


# Outlier treatment on the carheight

quantile(cars_price$carheight, seq(0,1,0.01))

boxplot(cars_price$carheight)



# Outlier treatment on the curbweight

quantile(cars_price$curbweight, seq(0,1,0.01))

boxplot(cars_price$curbweight)


# Outlier treatment on the enginesize


boxplot(cars_price$enginesize)

quantile(cars_price$enginesize, seq(0,1,0.01))

cars_price$enginesize[which(cars_price$enginesize>183)] <- 183

# outlier treatment on boreratio

boxplot(cars_price$boreratio)

# outlier treatment on stroke


boxplot(cars_price$stroke)

quantile(cars_price$stroke, seq(0,1,0.01))

cars_price$stroke[which(cars_price$stroke>3.64)] <- 3.64

cars_price$stroke[which(cars_price$stroke<2.7)] <- 2.7


# outlier treatment on compressionratio


boxplot(cars_price$compressionratio)

quantile(cars_price$compressionratio, seq(0,1,0.01))

cars_price$compressionratio[which(cars_price$compressionratio>10.94)] <-10.94
cars_price$compressionratio[which(cars_price$compressionratio<7.5)] <-7.5

# outlier treatment on horsepower


boxplot(cars_price$horsepower)

quantile(cars_price$horsepower, seq(0,1,0.01))

cars_price$horsepower[which(cars_price$horsepower>175.76)] <-175.76

# outlier treatment on peakrpm


boxplot(cars_price$peakrpm)

quantile(cars_price$peakrpm, seq(0,1,0.01))

cars_price$peakrpm[which(cars_price$peakrpm>6000)] <-6000

# outlier treatment on citympg

boxplot(cars_price$citympg)

quantile(cars_price$citympg, seq(0,1,0.01))

cars_price$citympg[which(cars_price$citympg>38)] <-38

# outlier treatment on highwaympg

boxplot(cars_price$highwaympg)

quantile(cars_price$highwaympg, seq(0,1,0.01))

cars_price$highwaympg[which(cars_price$highwaympg>46.92)] <-46.92

# separate car company with car model.


cars_price <-  separate(cars_price , CarName , into = c("CarCompany" , "CarModel") , sep = " ", extra = "drop" , fill = "left")

#removing car model column.

cars_price <- cars_price[,-4]


# data cleaning

cars_price$CarCompany[which(cars_price$CarCompany == "Nissan")] <- "nissan"

cars_price$CarCompany[which(cars_price$CarCompany == "toyouta")] <- "toyota"

cars_price$CarCompany[which(cars_price$CarCompany == "vokswagen")] <- "volkswagen"

cars_price$CarCompany[which(cars_price$CarCompany == "vw")] <- "volkswagen"

cars_price$CarCompany[which(cars_price$CarCompany == "maxda")] <- "mazda"

# creating dummy variables.


# 1. fueltype

fueltype <- data.frame(model.matrix(~fueltype, data = cars_price))
View(fueltype)
summary(fueltype)

fueltype <- fueltype[,-1]

cars_price <- cbind(cars_price[,-4],fueltype)



# aspiration


levels(cars_price$aspiration)
aspiration <- data.frame(model.matrix(~aspiration, data = cars_price))
aspiration <- aspiration[,-1]
cars_price <- cbind(cars_price,aspiration)
cars_price <- cars_price[,-4]



# doornumber 

levels(cars_price$doornumber)

doornumber <- data.frame(model.matrix(~doornumber, data = cars_price))
doornumber <- doornumber[,-1]

cars_price <- cbind(cars_price,doornumber)
cars_price <- cars_price[,-4]


# drivewheel

levels(cars_price$drivewheel)

drivewheel <- data.frame(model.matrix(~drivewheel,data = cars_price))
drivewheel <- drivewheel[,-1]

cars_price <- cbind(cars_price,drivewheel)
cars_price <- cars_price[,-5]

# enginelocation

levels(cars_price$enginelocation)

enginelocation <- data.frame(model.matrix(~enginelocation, data = cars_price))

enginelocation <- enginelocation[, -1]

cars_price <- cbind(cars_price,enginelocation)

cars_price <- cars_price[,-5]

# enginetype

levels(cars_price$enginetype)

enginetype <- data.frame(model.matrix(~enginetype, data = cars_price))

enginetype <- enginetype[, -1]

cars_price <- cbind(cars_price,enginetype)

cars_price <- cars_price[,-10]


# cylindernumber

levels(cars_price$cylindernumber)

cylindernumber <- data.frame(model.matrix(~cylindernumber, data = cars_price))

cylindernumber <- cylindernumber[, -1]

cars_price <- cbind(cars_price,cylindernumber)

cars_price <- cars_price[,-10]

# fuelsystem


levels(cars_price$fuelsystem)

fuelsystem <- data.frame(model.matrix(~fuelsystem, data = cars_price))

fuelsystem <- fuelsystem[, -1]

cars_price <- cbind(cars_price,fuelsystem)

cars_price <- cars_price[,-11]

# carbody

carbody <- data.frame(model.matrix(~carbody, data = cars_price))

carbody <- carbody[, -1]

cars_price <- cbind(cars_price,carbody)

cars_price <- cars_price[,-4]


# symboling

symboling <- data.frame(model.matrix(~symboling, data = cars_price))
symboling <- symboling[, -1]
cars_price <- cbind(cars_price,symboling)
cars_price <- cars_price[,-2]

# carcompany

CarCompany <- data.frame(model.matrix(~CarCompany, data = cars_price))
CarCompany <- CarCompany[,-1]

CarCompany[c(204,205),]<- 0
cars_price <- cbind(cars_price,CarCompany)
cars_price <- cars_price[,-2]


# creating training & testing dataset

#set the seed to 100.

set.seed(100)

trainindices = sample(1:nrow(cars_price),0.7*nrow(cars_price))

train <- cars_price[trainindices,]

test = cars_price[-trainindices,]



model <- lm(price~.,data = train)

summary(model)

#the null hypothesis corresponding to each p-value is that the corresponding independent variable does not impact the dependent variable. 
#The alternate hypothesis is that the corresponding independent variable impacts the response. 
# Now, p-value indicates the probability that the null hypothesis is true.
# Therefore, a low p-value, i.e. less than 0.05, indicates that you can reject the null hypothesis.

step <- stepAIC(model,direction = "both")

model_1 <- lm(formula = price ~ car_ID + wheelbase + curbweight + enginesize + stroke + 
                horsepower + peakrpm + fueltype + aspiration + doornumber + 
                drivewheelrwd + enginelocation + enginetypedohcv + enginetypel + 
                enginetypeohc + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumberfour + cylindernumbersix + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + symboling + 
                CarCompanyaudi + CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                CarCompanyisuzu  + CarCompanymazda + CarCompanymercury + 
                CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen + 
                CarCompanyvolvo, data = train)
summary(model_1)


vif(model_1)

# remove cylindernumberfour variable based on High VIF 
# Make a new model without cylindernumberfour variable

model_2 <- lm(formula = price ~ car_ID + wheelbase + curbweight + enginesize + stroke + 
                horsepower + peakrpm + fueltype + aspiration + doornumber + 
                drivewheelrwd + enginelocation + enginetypedohcv + enginetypel + 
                enginetypeohc + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 cylindernumbersix + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + symboling + 
                CarCompanyaudi + CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                CarCompanyisuzu +  CarCompanymazda + CarCompanymercury + 
                CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen + 
                CarCompanyvolvo, data = train)
summary(model_2)


vif(model_2)

# remove horsepower variable based on High VIF > 2 
# Make a new model without horsepower variable

model_3 <- lm(formula = price ~ car_ID + wheelbase + curbweight + enginesize + stroke + 
                 peakrpm + fueltype + aspiration + doornumber + 
                drivewheelrwd + enginelocation + enginetypedohcv + enginetypel + 
                enginetypeohc + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumbersix + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + symboling + 
                CarCompanyaudi + CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                CarCompanyisuzu +  CarCompanymazda + CarCompanymercury + 
                CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                CarCompanyrenault + CarCompanysaab + CarCompanytoyota + CarCompanyvolkswagen + 
                CarCompanyvolvo, data = train)
summary(model_3)


vif(model_3)

# remove CarCompanytoyota variable based on High VIF 
# Make a new model without CarCompanytoyota variable


model_4 <- lm(formula = price ~ car_ID + wheelbase + curbweight + enginesize + stroke + 
                peakrpm + fueltype + aspiration + doornumber + 
                drivewheelrwd + enginelocation + enginetypedohcv + enginetypel + 
                enginetypeohc + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumbersix + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + symboling + 
                CarCompanyaudi + CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                CarCompanyisuzu +  CarCompanymazda + CarCompanymercury + 
                CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                CarCompanyrenault + CarCompanysaab +  CarCompanyvolkswagen + 
                CarCompanyvolvo, data = train)
summary(model_4)


vif(model_4)

# remove carbodysedan variable based on High VIF & insignificance (p>0.05)
# Make a new model without carbodysedan variable

model_5 <- lm(formula = price ~ car_ID + wheelbase + curbweight + enginesize + stroke + 
                peakrpm + fueltype + aspiration + doornumber + 
                drivewheelrwd + enginelocation + enginetypedohcv + enginetypel + 
                enginetypeohc + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumbersix + cylindernumberthree + 
                fuelsystem2bbl + fuelsystemmpfi + fuelsystemspdi + carbodyhardtop + 
                carbodyhatchback  + carbodywagon + symboling + 
                CarCompanyaudi + CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                CarCompanyisuzu +  CarCompanymazda + CarCompanymercury + 
                CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                CarCompanyrenault + CarCompanysaab +  CarCompanyvolkswagen + 
                CarCompanyvolvo, data = train)
summary(model_5)


vif(model_5)

# remove fuelsystem2bbl variable based on High VIF & insignificance (p>0.05)
# Make a new model without fuelsystem2bbl variable

model_6 <- lm(formula = price ~ car_ID + wheelbase + curbweight + enginesize + stroke + 
                peakrpm + fueltype + aspiration + doornumber + 
                drivewheelrwd + enginelocation + enginetypedohcv + enginetypel + 
                enginetypeohc + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumbersix + cylindernumberthree + 
                 fuelsystemmpfi + fuelsystemspdi + carbodyhardtop + 
                carbodyhatchback  + carbodywagon + symboling + 
                CarCompanyaudi + CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                CarCompanyisuzu +  CarCompanymazda + CarCompanymercury + 
                CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                CarCompanyrenault + CarCompanysaab +  CarCompanyvolkswagen + 
                CarCompanyvolvo, data = train)
summary(model_6)


vif(model_6)

# remove drivewheelrwd variable based on High VIF & insignificance (p>0.05)
# Make a new model without drivewheelrwd variable



model_7 <- lm(formula = price ~ car_ID + wheelbase + curbweight + enginesize + stroke + 
                peakrpm + fueltype + aspiration + doornumber + 
                 enginelocation + enginetypedohcv + enginetypel + 
                enginetypeohc + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumbersix + cylindernumberthree + 
                fuelsystemmpfi + fuelsystemspdi + carbodyhardtop + 
                carbodyhatchback  + carbodywagon + symboling + 
                CarCompanyaudi + CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                CarCompanyisuzu +  CarCompanymazda + CarCompanymercury + 
                CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                CarCompanyrenault + CarCompanysaab +  CarCompanyvolkswagen + 
                CarCompanyvolvo, data = train)
summary(model_7)


vif(model_7)

# remove enginetypeohc variable based on High VIF & insignificance (p>0.05)
# Make a new model without enginetypeohc variable

model_8 <- lm(formula = price ~ car_ID + wheelbase + curbweight + enginesize + stroke + 
                peakrpm + fueltype + aspiration + doornumber + 
                enginelocation + enginetypedohcv + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumbersix + cylindernumberthree + 
                fuelsystemmpfi + fuelsystemspdi + carbodyhardtop + 
                carbodyhatchback  + carbodywagon + symboling + 
                CarCompanyaudi + CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                CarCompanyisuzu +  CarCompanymazda + CarCompanymercury + 
                CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                CarCompanyrenault + CarCompanysaab +  CarCompanyvolkswagen + 
                CarCompanyvolvo, data = train)
summary(model_8)


vif(model_8)


# remove symboling variable based on High VIF & insignificance (p>0.05)
# Make a new model without symboling variable

model_9 <- lm(formula = price ~ car_ID + wheelbase + curbweight + enginesize + stroke + 
                peakrpm + fueltype + aspiration + doornumber + 
                enginelocation + enginetypedohcv + enginetypel + 
                enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumbersix + cylindernumberthree + 
                fuelsystemmpfi + fuelsystemspdi + carbodyhardtop + 
                carbodyhatchback  + carbodywagon  + 
                CarCompanyaudi + CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                CarCompanyisuzu +  CarCompanymazda + CarCompanymercury + 
                CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                CarCompanyrenault + CarCompanysaab +  CarCompanyvolkswagen + 
                CarCompanyvolvo, data = train)
summary(model_9)


vif(model_9)

# remove cylindernumbersix variable based on High VIF & insignificance (p>0.05)
# Make a new model without cylindernumbersix variable


model_10 <- lm(formula = price ~ car_ID + wheelbase + curbweight + enginesize + stroke + 
                 peakrpm + fueltype + aspiration + doornumber + 
                 enginelocation + enginetypedohcv + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + 
                  cylindernumberthree + 
                 fuelsystemmpfi + fuelsystemspdi + carbodyhardtop + 
                 carbodyhatchback  + carbodywagon  + 
                 CarCompanyaudi + CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyisuzu +  CarCompanymazda + CarCompanymercury + 
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysaab +  CarCompanyvolkswagen + 
                 CarCompanyvolvo, data = train)
summary(model_10)


vif(model_10)

# remove peakrpm variable based on High VIF & insignificance (p>0.05)
# Make a new model without peakrpm variable

model_11 <- lm(formula = price ~ car_ID + wheelbase + curbweight + enginesize + stroke + 
                  fueltype + aspiration + doornumber + 
                 enginelocation + enginetypedohcv + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 cylindernumberthree + 
                 fuelsystemmpfi + fuelsystemspdi + carbodyhardtop + 
                 carbodyhatchback  + carbodywagon  + 
                 CarCompanyaudi + CarCompanybmw + CarCompanybuick + CarCompanydodge + 
                 CarCompanyisuzu +  CarCompanymazda + CarCompanymercury + 
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysaab +  CarCompanyvolkswagen + 
                 CarCompanyvolvo, data = train)
summary(model_11)


vif(model_11)

# remove CarCompanybuick variable based on High VIF 
# Make a new model without CarCompanybuick variable


model_12 <- lm(formula = price ~ car_ID + wheelbase + curbweight + enginesize + stroke + 
                 fueltype + aspiration + doornumber + 
                 enginelocation + enginetypedohcv + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 cylindernumberthree + 
                 fuelsystemmpfi + fuelsystemspdi + carbodyhardtop + 
                 carbodyhatchback  + carbodywagon  + 
                 CarCompanyaudi + CarCompanybmw  + CarCompanydodge + 
                 CarCompanyisuzu +  CarCompanymazda + CarCompanymercury + 
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysaab +  CarCompanyvolkswagen + 
                 CarCompanyvolvo, data = train)
summary(model_12)


vif(model_12)




# remove fuelsystemmpfi variable based on High VIF 
# Make a new model without fuelsystemmpfi variable


model_13 <- lm(formula = price ~ car_ID + wheelbase + curbweight + enginesize + stroke + 
                 fueltype + aspiration + doornumber + 
                 enginelocation + enginetypedohcv + enginetypel + 
                 enginetypeohcf + enginetyperotor + cylindernumberfive + 
                 cylindernumberthree + 
                  fuelsystemspdi + carbodyhardtop + 
                 carbodyhatchback  + carbodywagon  + 
                 CarCompanyaudi + CarCompanybmw  + CarCompanydodge + 
                 CarCompanyisuzu +  CarCompanymazda + CarCompanymercury + 
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysaab +  CarCompanyvolkswagen + 
                 CarCompanyvolvo, data = train)
summary(model_13)


vif(model_13)


# remove enginetypeohcf variable based on High VIF 
# Make a new model without enginetypeohcf variable


model_14 <- lm(formula = price ~ car_ID + wheelbase + curbweight + enginesize + stroke + 
                 fueltype + aspiration + doornumber + 
                 enginelocation + enginetypedohcv + enginetypel + 
                  enginetyperotor + cylindernumberfive + 
                 cylindernumberthree + 
                 fuelsystemspdi + carbodyhardtop + 
                 carbodyhatchback  + carbodywagon  + 
                 CarCompanyaudi + CarCompanybmw  + CarCompanydodge + 
                 CarCompanyisuzu + CarCompanymazda + CarCompanymercury + 
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysaab +  CarCompanyvolkswagen + 
                 CarCompanyvolvo, data = train)
summary(model_14)

vif(model_14)

# remove doornumber variable based on High VIF 
# Make a new model without doornumber variable



model_15 <- lm(formula = price ~ car_ID + wheelbase + curbweight + enginesize + stroke + 
                 fueltype + aspiration +  
                 enginelocation + enginetypedohcv + enginetypel + 
                 enginetyperotor + cylindernumberfive + 
                 cylindernumberthree + 
                 fuelsystemspdi + carbodyhardtop + 
                 carbodyhatchback  + carbodywagon  + 
                 CarCompanyaudi + CarCompanybmw  + CarCompanydodge + 
                 CarCompanyisuzu +  CarCompanymazda + CarCompanymercury + 
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysaab +  CarCompanyvolkswagen + 
                 CarCompanyvolvo, data = train)
summary(model_15)
vif(model_15)


# remove cylindernumberfive variable based on High VIF 
# Make a new model without cylindernumberfive variable


model_16 <- lm(formula = price ~ car_ID + wheelbase + curbweight + enginesize + stroke + 
                 fueltype + aspiration +  
                 enginelocation + enginetypedohcv + enginetypel + 
                 enginetyperotor +  
                 cylindernumberthree + 
                 fuelsystemspdi + carbodyhardtop + 
                 carbodyhatchback  + carbodywagon  + 
                 CarCompanyaudi + CarCompanybmw  + CarCompanydodge + 
                 CarCompanyisuzu +  CarCompanymazda + CarCompanymercury + 
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysaab +  CarCompanyvolkswagen + 
                 CarCompanyvolvo, data = train)
summary(model_16)

vif(model_16)

# remove curbweight variable based on High VIF 
# Make a new model without curbweight variable



model_17 <- lm(formula = price ~ car_ID + wheelbase +  enginesize + stroke + 
                 fueltype + aspiration +  
                 enginelocation + enginetypedohcv + enginetypel + 
                 enginetyperotor +  
                 cylindernumberthree + 
                 fuelsystemspdi + carbodyhardtop + 
                 carbodyhatchback  + carbodywagon  + 
                 CarCompanyaudi + CarCompanybmw  + CarCompanydodge + 
                 CarCompanyisuzu +  CarCompanymazda + CarCompanymercury + 
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysaab +  CarCompanyvolkswagen + 
                 CarCompanyvolvo, data = train)
summary(model_17)
vif(model_17)


# remove aspiration variable based on High VIF 
# Make a new model without aspiration variable

model_18 <- lm(formula = price ~ car_ID + wheelbase +  enginesize + stroke + 
                 fueltype +   
                 enginelocation + enginetypedohcv + enginetypel + 
                 enginetyperotor +  
                 cylindernumberthree + 
                 fuelsystemspdi + carbodyhardtop + 
                 carbodyhatchback  + carbodywagon  + 
                 CarCompanyaudi + CarCompanybmw  + CarCompanydodge + 
                 CarCompanyisuzu +  CarCompanymazda + CarCompanymercury + 
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysaab +  CarCompanyvolkswagen + 
                 CarCompanyvolvo, data = train)
summary(model_18)
vif(model_18)

# Remove wheelbase

model_19 <- lm(formula = price ~ car_ID +   enginesize + stroke + 
                 fueltype +   
                 enginelocation + enginetypedohcv + enginetypel + 
                 enginetyperotor +  
                 cylindernumberthree + 
                 fuelsystemspdi + carbodyhardtop + 
                 carbodyhatchback  + carbodywagon  + 
                 CarCompanyaudi + CarCompanybmw  + CarCompanydodge + 
                 CarCompanyisuzu + CarCompanymazda + CarCompanymercury + 
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysaab +  CarCompanyvolkswagen + 
                 CarCompanyvolvo, data = train)
summary(model_19)
vif(model_19)

# Remove car_ID

model_20 <- lm(formula = price ~  enginesize + stroke + 
                 fueltype +   
                 enginelocation + enginetypedohcv + enginetypel + 
                 enginetyperotor +  
                 cylindernumberthree + 
                 fuelsystemspdi + carbodyhardtop + 
                 carbodyhatchback  + carbodywagon  + 
                 CarCompanyaudi + CarCompanybmw  + CarCompanydodge + 
                 CarCompanyisuzu +  CarCompanymazda + CarCompanymercury + 
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysaab +  CarCompanyvolkswagen + 
                 CarCompanyvolvo, data = train)
summary(model_20)
vif(model_20)

# From  VIF one can see the issue of multicolinearity has been addressed.
# We willnow focus on pvalues > 0.05
# remove CarCompanymazda
model_21 <- lm(formula = price ~  enginesize + stroke + 
                 fueltype +   
                 enginelocation + enginetypedohcv + enginetypel + 
                 enginetyperotor +  
                 cylindernumberthree + 
                 fuelsystemspdi + carbodyhardtop + 
                 carbodyhatchback  + carbodywagon  + 
                 CarCompanyaudi + CarCompanybmw  + CarCompanydodge + 
                 CarCompanyisuzu +  CarCompanymercury + 
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysaab +  CarCompanyvolkswagen + 
                 CarCompanyvolvo, data = train)
summary(model_21)
vif(model_21)
# remove CarCompanyvolkswagen

model_22 <- lm(formula = price ~  enginesize + stroke + 
                 fueltype +   
                 enginelocation + enginetypedohcv + enginetypel + 
                 enginetyperotor +  
                 cylindernumberthree + 
                 fuelsystemspdi + carbodyhardtop + 
                 carbodyhatchback  + carbodywagon  + 
                 CarCompanyaudi + CarCompanybmw  + CarCompanydodge + 
                 CarCompanyisuzu +  CarCompanymercury + 
                 CarCompanymitsubishi + CarCompanynissan + CarCompanyplymouth + 
                 CarCompanyrenault + CarCompanysaab +   
                 CarCompanyvolvo, data = train)
summary(model_22)

# Remove CarCompanydodge, CarCompanyisuzu, CarCompanymercury since they are having pvalue > 0.05

model_23 <- lm(formula = price ~  enginesize + stroke + 
                 fueltype +   
                 enginelocation + enginetypedohcv + enginetypel + 
                 enginetyperotor +  
                 cylindernumberthree + 
                 fuelsystemspdi + carbodyhardtop + 
                 carbodyhatchback  + carbodywagon  + 
                 CarCompanyaudi + CarCompanybmw  +  
                    
                 CarCompanymitsubishi +   
                 CarCompanyrenault + CarCompanysaab +   
                 CarCompanyvolvo, data = train)
summary(model_23)

# Remove carbodywagon

model_24 <- lm(formula = price ~  enginesize + stroke + 
                 fueltype +   
                 enginelocation + enginetypedohcv + enginetypel + 
                 enginetyperotor +  
                 cylindernumberthree + 
                 fuelsystemspdi + carbodyhardtop + 
                 carbodyhatchback  +  
                 CarCompanyaudi + CarCompanybmw  +    CarCompanysaab +   
                 CarCompanyvolvo, data = train)
summary(model_24)

# Remove cylindernumberthree

model_24 <- lm(formula = price ~  enginesize + stroke + 
                 fueltype +   
                 enginelocation + enginetypedohcv + enginetypel + 
                 enginetyperotor +  
                                   fuelsystemspdi + carbodyhardtop + 
                 carbodyhatchback  +  
                 CarCompanyaudi + CarCompanybmw  +    CarCompanysaab +   
                 CarCompanyvolvo, data = train)
summary(model_24)

# Remove enginetypel, fuelsystemspdi, carbodyhardtop.

model_25 <- lm(formula = price ~  enginesize + stroke + 
                 fueltype +   
                 enginelocation + enginetypedohcv +  
                 enginetyperotor +  
                 carbodyhatchback  +  
                 CarCompanyaudi + CarCompanybmw  +    CarCompanysaab +   
                 CarCompanyvolvo, data = train)
summary(model_25)

# Remove enginelocation, enginetypedohcv.

model_26 <- lm(formula = price ~  enginesize + stroke + 
                 fueltype +enginetyperotor +  
                 carbodyhatchback  +  
                 CarCompanyaudi + CarCompanybmw  +    CarCompanysaab +   
                 CarCompanyvolvo, data = train)
summary(model_26)

# Remove carbodyhatchback

model_27 <- lm(formula = price ~  enginesize + stroke + 
                 fueltype +enginetyperotor +  
                   
                 CarCompanyaudi + CarCompanybmw  +    CarCompanysaab +   
                 CarCompanyvolvo, data = train)
summary(model_27)

# Reomve fueltype


model_28 <- lm(formula = price ~  enginesize + stroke + 
                 enginetyperotor +   CarCompanyaudi + CarCompanybmw  +    CarCompanysaab +   
                 CarCompanyvolvo, data = train)
summary(model_28)

# Remove CarCompanybmw

model_29 <- lm(formula = price ~  enginesize + stroke + 
                 enginetyperotor +   CarCompanyaudi +   CarCompanysaab +   
                 CarCompanyvolvo, data = train)
summary(model_29)

# Remove CarCompanysaab

model_30 <- lm(formula = price ~  enginesize + stroke + 
                 enginetyperotor +   CarCompanyaudi +    
                 CarCompanyvolvo, data = train)
summary(model_30)

# Remove CarCompanyvolvo

model_31 <- lm(formula = price ~  enginesize + stroke + 
                 enginetyperotor +   CarCompanyaudi   
                 ,data = train)
summary(model_31)
vif(model_31)


write.csv(cars_price, "car_check.csv")


# Assessing the model

# Checking the plot trend of the predicted values obtained from model_31 & actual price. 
# We saw that the both graphs are not close to or matching at all it means.

# we need to move back analyse other model as well.
# considering the model_30. we get the very well fitted plot of actual & predicted value.


# one can see the randomness of the error getting.THus there couldn't be extra variable
# that can be introduced into the model.Thus we can say that model_30 is our final model.

# Price =3179.905+169.11 *enginesize -3728.868*stroke + 9264.857*enginetyperotor+ 4267.418*CarCompanyaudi+ 2956.21*CarCompanyvolvo


# I got R^2 = 0.8197 & adjusted R^2 = 0.8132. Both are very close. Thus of the parameters are not redundant.


# Predict the car prices in the testing dataset

Predict_1 <- predict(model_30,test[,-15])
test$test_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation

r<- cor(test$price,test$test_price)

# calculate R squared by squaring correlation

rsquared <- cor(test$price,test$test_price)^2

# check R-squared

rsquared


# Thus we get R^2 as 0.68.
# The model could explain 68% of the variation, which is a satisfactory level of accuracy.

# interpreting the result model_30.


# enginesize : on ploting the curve of both engine size & Price, we found that both follow the same trend.
# Thus , as size of Engine increases price increases & vice versa.


# stroke  : as stroke of Engine increases price increases & vice versa.

# enginetyperotor: rotor type engine deciding the price. It is being used with stroke of 3.25 Engine.
# CarCompanyaudi : for high stroke cars 3.4 Audi is being preferred, and we know the variation of stroke & price.  
# CarCompanyvolvo: for low stroke cars <3.4 volvo is being preferred,and we know the variation of stroke & price.



