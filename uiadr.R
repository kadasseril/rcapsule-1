
setwd("E:/dropbox/r/uiadr")
# R works with objects; an object has a name, content and attributes , which specifies 
# the kind of data stored in the object
# all bojects have two intrinisic attributes: mode and length
#mod: basic type of the lements of an object (numeric,character,logical,complex)
#and length provides the no. of elements of an object  
#For other objects, other information are necessary 
#and they are given  by non-intrinsic attributes. (like dim for matrix)
# read.table, scan() 
#(in scan we specify the mode of the variable 
# like: scan("file-name',what=list("",0,0))
#For other objects, other information are necessary 
#and they are given  by non-intrinsic attributes.
z=scan() #input data directly from keyboard
#gl(3,5):1 1 1 1 1 2 2 2 2 2 3 3 3 3 3 : generate levels
#expand.grid
sd(c(5,8,12))
which.min(c(4,1,6))
seq(1,100,2)
WHO=read.csv("WHO.csv")
str(WHO)
# str(data,list.len=ncol(data))
names(WHO)
WHO_europe=subset(WHO,WHO$Region=="Europe")
nrow(WHO_europe)
write.csv(WHO_europe,"WHO_europe.csv")
dir()
rm(WHO_europe)
#Analysis
mean(WHO$Under15)
sd(WHO$Under15)
summary(WHO$Under15)
#Notice a country with only 13.2% under 15; gow to find the country
which.min(WHO$Under15)
# This gives 86 ; now list out the country in the 66'th row
WHO$Country[86]
#WHO$Country[which.min(WHO$Under15)]
#Similarly for max percentage
which.max(WHO$Under15)
WHO$Country[124]
#WHO$Country[which.max(WHO$Under15)]
plot(WHO$GNI,WHO$FertilityRate)
#Locating High income countries with high fertility rate
outliers=subset(WHO,GNI>10000 & FertilityRate >2.5)
nrow(outliers)
outliers[,c("Country","GNI","FertilityRate")]
#Now histogram
hist(WHO$CellularSubscribers)
boxplot(WHO$LifeExpectancy ~ WHO$Region)
boxplot(WHO$LifeExpectancy ~ WHO$Region, xlab="Region", ylab="Life expectancy", main="Lfe expectancy of countries by region")
table(WHO$Region)
tapply(WHO$Over60,WHO$Region,mean)
#Above command sorts each of the counries by Region and then compute the mean of the variable Over60

tapply(WHO$LiteracyRate,WHO$Region,min)
# Why NAs?
tapply(WHO$LiteracyRate,WHO$Region,min,na.rm=TRUE)
# WHO$Country[which.max(WHO$LiteracyRate)]

#USDA Data set
USDA = read.csv("USDA.csv")
str(USDA)
#7058 obs. or foods in our database; ID represents identification variable for each of the foods
#calories represnts the amount of calories in 100 gms of this food in kilo cals
summary(USDA)
#Maximum amount of Sodium (in milli grams) is very high
#which food this corresponds to?
which.max(USDA$Sodium)
USDA[265,"Description"]  # SALT,TABLE
# USDA$Description[which.max(USDA$Sodium)]
# 38758.0 milligms of sodium in 100 gms of table salt; but one of us eat 100 gms of salt in one sitting
# Which foods contain more than 10000 milli gms of sodium?
# USDA$Description[USDA$Sodium>10000]
HighSodium=subset(USDA,USDA$Sodium>10000)
nrow(HighSodium)
HighSodium$Description
#To find the level in the food 100 gms of caviar
#For this we need to get the index of caviar and for this we need to track down the word caviar in the varioable Description
#To do this use the match function
match("CAVIAR",USDA$Description)
# we get the index as 4154
USDA$Sodium[4154]
#USDA$Sodium[match("CAVIAR", USDA$Description)]
summary(USDA$Sodium)
sd(USDA$Sodium, na.rm = TRUE)

# Plot
plot(USDA$Protein,USDA$TotalFat)
#triangular shape ; food higher in protein lower in Fat
plot(USDA$Protein, USDA$TotalFat, xlab="Protein", ylab = "Fat", main = "Protein vs Fat", col = "red")
hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C")
#More than 6000 of the foods have less than 200mgms of vitamin C
#Histogram puts all of them into one cell; limit the x-axis to 0-100
hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C", xlim=c(0,100))
#But we see only one big cell
hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C", xlim=c(0,100), breaks=100)
hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main = "Histogram of Vitamin C", xlim=c(0,100), breaks=2000)
boxplot(USDA$Sugar, ylab = "Sugar (g)", main = "Boxplot of Sugar")

#Adding one more variable
HighSodium=as.numeric(USDA$Sodium>mean(USDA$Sodium,na.rm=TRUE))
USDA$HighSodium=HighSodium
USDA$HighCarbs = as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate, na.rm=TRUE))
USDA$HighProtein = as.numeric(USDA$Protein > mean(USDA$Protein, na.rm=TRUE))
USDA$HighFat = as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm=TRUE))
str(USDA)
#How many foods have higher sodium level than average?
table(USDA$HighSodium)
#How many foods have both high sodium and high fat?
table(USDA$HighSodium,USDA$HighFat)
#Average amount of iron sorted by high and low protein?
tapply(USDA$Iron,USDA$HighProtein,mean,na.rm=TRUE)
#Maximum level of Vitamin C in hfoods with high and low carbs?
tapply(USDA$VitaminC,USDA$HighCarbs,max,na.rm=TRUE)
#Using summary function with tapply
tapply(USDA$VitaminC,USDA$HighCarbs,summary,na.rm=TRUE)

# Week 2 Regression
#SSE sum of squared Error
#Root Mean Squared Error (To normalise the SSE
#sqrt(sse/n)
#Base line model does not use any variable. It predicts the average value of the
#dependent variable regardless of the value of the independent variable)
#SST is the sum of squared error of base line model sum((xi-xbar)^2
#SST is also known as total sum of squares
#RSquared=1-SSE/SST ; it explains the value added by the regression model over just predicting using average
#Multiple variables to improve the model
wine=read.csv("wine.csv")
str(wine)
summary(wine)
#AGST- Average Growing Season Temperature
model1=lm(Price~AGST,data=wine)
summary(model1)
#Adjusted RSquared adjusts the squared value to account for the independent variables
#Relative to the number of data points used
model1$residuals
sse=sum(model1$residuals^2)
sse
model2=lm(Price~AGST+HarvestRain,data=wine)
summary(model2)
sse
sse=sum(model2$residuals^2)
model3=lm(Price ~AGST+HarvestRain+WinterRain+Age+FrancePop,data=wine)
summary(model3)
sse=sum(model3$residuals^2)
sse
# Age and France population are not significant in the model
#We should consider removing these variables from the model
#Remove FrancePop
model4=lm(Price ~AGST+HarvestRain+WinterRain+Age,data=wine)
summary(model4)
sse=sum(model4$residuals^2)
sse
#Here Age variable has become very significant; this is due to multicollinarity- with the FrancePop variable
#Multi collinarity refers to the situation when two independent variable are highly correlated
model5=lm(Price ~AGST+HarvestRain+WinterRain,data=wine)
summary(model5)
# We will stick with model4
# cor(wine$HarvestRain,wine$WinterRain)
# m1=lm(Price~WinterRain+HarvestRain,data=wine)
# summary(m1)
# The accuracy of a model on the test data is often referred to as out-of-sample accuracy
winetest=read.csv("wine_test.csv")
str(winetest)
PredictTest=predict(model4,newdata=winetest)
PredictTest
sse=sum((winetest$Price-PredictTest)^2)
sse
sst=sum((winetest$Price-mean(winetest$Price))^2)
sst
sse/sst
1-(sse/sst)

baseball=read.csv("baseball.csv")
str(baseball)
moneyball=subset(baseball,Year<2002)
moneyball$RD=moneyball$RS-moneyball$RA
str(moneyball)
winsreg=lm(W~RD,data=moneyball)
summary(winsreg)
