#Assignment climate change

climate <- read.csv("climate.change.csv")

#Creo un training set fins 2006, i un testing set a partir 2007
training_set <- subset(climate, Year < 2007)
testing_set <- subset(climate, Year > 2006)
train = subset(climate, Year <= 2006)
test = subset(climate, Year > 2006)

#Faig model regressió pel training set

climatelm = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=train)

#Lastly, look at the model using summary(climatelm). The Multiple R-squared value is 0.7509. 

#Correlacions entre gasos
pairs(training_set[ ,c(3, 4, 5, 6, 7, 8, 9, 10)])
cor(training_set)
y <- training_set[8:10]
cor(x,y)

#Nou model reduït pq correlacions N2O molt a¡ltes amb CO2, CH4 i CF12,
#i de CF11 amb CH4 i CF12
model2 <- lm(Temp ~ MEI + N2O + TSI + Aerosols, data=training_set)
#We have observed that, for this problem, when we remove many variables the sign
#of N2O flips. The model has not lost a lot of explanatory power (the model R2
#is 0.7261 compared to 0.7509 previously) despite removing many variables. 
#As discussed in lecture, this type of behavior is typical when building a model
#where many of the independent variables are highly correlated with each other. 
#In this particular problem many of the variables (CO2, CH4, N2O, CFC.11 and CFC.
#12) are highly correlated, since they are all driven by human industrial 
#development. 

#Fent servir l' step function
model3 <- step(model1)
summary(model3)

#Step es base en reducció AIC.
#S'step busca un model simple, però no adreça qüestió multicolineraitat. Model difícil d'intrepretar.

#Making predictions in the testing_set
predictTest <- predict(model3, newdat=testing_set)
# Compute R-squared
SSE = sum((testing_set$Temp - predictTest)^2)
SST = sum((testing_set$Temp - mean(training_set$Temp))^2)
1 - SSE/SST
[1] 0.6286051 #Està bé!!

The R code to calculate the R-squared can be written as follows (your variable names may be different):
tempPredict = predict(climateStep, newdata = test)
SSE = sum((tempPredict - test$Temp)^2)
SST = sum( (mean(train$Temp) - test$Temp)^2)
R2 = 1 - SSE/SST

##Assignament PISA
pisaTrain <- read.csv("pisa2009train.csv")
pisaTest <- read.csv("pisa2009test.csv")
str(pisaTrain)
#Mitjana reading scores per homes i dones
tapply(pisaTrain$readingScore, pisaTrain$male, mean) 
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore <- lm(readingScore ~ ., data=pisaTrain)
summary(lmScore)

#Cauclar training set root Mean Squared Error
#The training-set RMSE can be computed by first computing the SSE
SSE = sum(lmScore$residuals^2)
#and then dividing by the number of observations and taking the square root:
RMSE = sqrt(SSE / nrow(pisaTrain))

#A alternative way of getting this answer would be with the following command:
sqrt(mean(lmScore$residuals^2)). 

#Comparar resultats dos estudiants
# Consider two students A and B. hey have all variable values the same,
#Except that student A is in grade 11 and student B is in grade 9.
#What is the predicted reading score of student A minus the predicted reading score of student B?
Student_a <- 143.766333 + (29.542707*11) #intercepte + estimate*grade
student_b <- 143.766333 + (29.542707*9)
Student_a - student_b

#What is the meaning of the coefficient associated with variable raceethAsian?
#Predicted difference in the reading score between an Asian student and a white 
#student who is otherwise identical

#Calcular prediccions amb la nova bbdd
predTest <- predict(lmScore, newdata=pisaTest)


RMSE = sqrt(SSE / nrow(pisaTest))
#RMSE és més interpretable que SSE. Vol dir que fem un error d'uns 76 punts
#mitjana és de 520.
#What is the predicted test score used in the baseline model?
baseline = mean(pisaTrain$readingScore) 
#What is the sum of squared errors of the baseline model on the testing set? 
SST = sum((baseline - pisaTest$readingScore)^2)

#Calcular l'oUt of sample R-squared BÉ
SSE <- sum((predTest - pisaTest$readingScore)^2)
SST = sum((baseline - pisaTest$readingScore)^2)
R1 <- 1 - SSE/SST
R1

## Flu
fluTrain <- read.csv("fluTrain.csv")
head(fluTrain)
#Trobar setmana on valor màxim visites ILI, i consultes flu
fluTrain$Week[which.max(fluTrain$ILI)]
fluTrain$Week[which.max(fluTrain$Queries)]
subset(fluTrain, Queries == max(Queries)) #una altra manera de fer-ho

#Histrograma ILI
hist(fluTrain$ILI, breaks=100) #right skew (most values are small)
logILI <- log(fluTrain$ILI)
hist(logILI, breaks=100) #està més normalitzada

plot(fluTrain$ILI, fluTrain$Queries)#mall number of unusually large or small
        #observations from having an undue influence on the sum of squared errors 
plot(logILI, fluTrain$Queries) #There is a positive, linear relationship between log(ILI) and Queries

#Anem a fer un model de regressió. D'acord amb gràfic, quin seria?
#log(ILI) = intercept + coefficient x Queries, where the coefficient is positive

FluTrend1 <- lm(logILI ~ Queries, data=fluTrain)
summary(FluTrend1)
#R^2 és 0.709. Per una sola variable, R^2=correlació^2 de les 2 vbles
(cor(logILI, fluTrain$Queries))^2
[1] 0.7090201

#Performance en el test. Problema: la VD és un log.
#No volem el log. Per tant, no serveix: PredTest1 = predict(FluTrend1, newdata=FluTest)
fluTest <- read.csv("fluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=fluTest))
#What is our estimate for the percentage of ILI-related physician visits for the week of March 11, 2012? 
PredTest1[which(fluTest$Week == "2012-03-11 - 2012-03-17")]
#What is the relative error betweeen the estimate (our prediction) and the
#observed value for the week of March 11, 2012? 
#Note that the relative error is calculated as
#(Observed ILI - Estimated ILI)/Observed ILI
obs_ILI <- fluTest$ILI[which(fluTest$Week == "2012-03-11 - 2012-03-17")]
pred_ILI <- PredTest1[which(fluTest$Week == "2012-03-11 - 2012-03-17")]
(error <- (obs_ILI - pred_ILI)/obs_ILI)

#What is the Root Mean Square Error (RMSE) between our estimates and the actual observations
#for the percentage of ILI-related physician visits, on the test set?
SSE <- sum((PredTest1 - fluTest$ILI)^2)
(RMSE = sqrt(SSE / nrow(fluTest))) #0.7490645

##4.1. Training a Time Series Model
# the value of -2 passed to lag means to return 2 observations before the current one
#positive value would have returned future observations
#The parameter na.pad=TRUE means to add missing values for the first two weeks 
#of our dataset, where we can't compute the data from 2 weeks ea
library(zoo)
ILILag2 = lag(zoo(fluTrain$ILI), -2, na.pad=TRUE)
fluTrain$ILILag2 = coredata(ILILag2)
summary(fluTrain$ILILag2)
plot(log(fluTrain$ILILag2), log(fluTrain$ILI))
#regressió lineal que incorpori dades dues setmanes anteriors
FluTrend2 <- lm(log(ILI) ~ Queries + log(ILILag2), data=fluTrain)
summary(FluTrend2)
#afegir ILILag2 a bbdd fluTest
ILILag2 = lag(zoo(fluTest$ILI), -2, na.pad=TRUE)
fluTest$ILILag2 = coredata(ILILag2)
summary(fluTest$ILILag2) #els dos primers són na
#Fill in the missing values for ILILag2 in FluTest (2 setmanes abans: fent ús fulTrain)
head(fluTest) # missin 1, correspon a registre 416 fluTrain
tail(fluTrain) #missing2 fluTest, correspon a registre 417 fluTrain
fluTest$ILILag2[1] = fluTrain$ILI[416]
fluTest$ILILag2[2] = fluTrain$ILI[417]
head(fluTest)
# Predictions for FulTrend2 i calcular RMSE del test set
PredTest2 = exp(predict(FluTrend2, newdata=fluTest))
SSE <- sum((PredTest2 - fluTest$ILI)^2)
(RMSE = sqrt(SSE / nrow(fluTest))) #0.2942029