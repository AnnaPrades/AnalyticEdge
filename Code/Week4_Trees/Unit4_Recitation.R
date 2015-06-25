# Unit 4, Recitation
# we will use discussing regression trees and applying them to the house price data from Boston

#1970s paper about relationship between houe prices and clean air
# Trees can be used for classification, but also for regression
# The output is a number. Regression trees can capture nonlinearities taht linear regression can't.
# With clasification trees we report the avarage outcom at each leaf of our tree.
# e.f if outcome is true 15 times, and false 5 -> leaf=0.75. With a threshold of 0.5 we would say value is true
# with regression trees we report the avarage
#INSERT EXAMPLE why linear regresssion does not fit well
# x less than 10 -> avarage of 1 grup
# x between 10-20 ->...


# VIDEO 2

# Read in data
boston = read.csv("boston.csv")
str(boston) # 506 obs corresponding to 506 census tracks

# Plot observations
plot(boston$LON, boston$LAT) #we plot the longitud and latitud of each census
# dense zone corresponds to boston center city

# Tracts alongside the Charles River. We plot only those points that are in Charles rives (CHAS=1)
#pch command sets that its solid (and not holoww)
points(boston$LON[boston$CHAS==1], boston$LAT[boston$CHAS==1], col="blue", pch=19)
#this are the census in Charles River

# Plot MIT -> census is 3531
points(boston$LON[boston$TRACT==3531],boston$LAT[boston$TRACT==3531],col="red", pch=20)

# Plot polution. How air polution affects prices
summary(boston$NOX)
# let's identify the points that have above avarege air polution in green
points(boston$LON[boston$NOX>=0.55], boston$LAT[boston$NOX>=0.55], col="green", pch=20)

# Plot prices
plot(boston$LON, boston$LAT)
summary(boston$MEDV) #Distribution of housing prices in thousands of dolars
#let's identify points median above avarage
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=20)



# VIDEO 3
# According to the plots we have seen, we do not expect linear regression work well with latittude and longitude
# Linear Regression using LAT and LON
plot(boston$LAT, boston$MEDV)
plot(boston$LON, boston$MEDV)
latlonlm = lm(MEDV ~ LAT + LON, data=boston)
summary(latlonlm)
# R^2 i arund 0.1, which is not great
#latitude is not significant, which means norht-south differences will not be used
# longitude is significant and its negaive -> as we go to the ocean, prices increase
# so it seems kind o unlikelly

# Visualize regression output on a plot
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=20)

latlonlm$fitted.values #it us what linear model predicts for each 506 census tracks
# we can pass characters in pch -> pch="$"
points(boston$LON[latlonlm$fitted.values >= 21.2], boston$LAT[latlonlm$fitted.values >= 21.2], col="blue", pch="$")
# we can see is pretty much vertical -> wrong


# Video 4

# Load CART packages
library(rpart)
library(rpart.plot)

# CART model # we preict price by Lattitude and longitude
latlontree = rpart(MEDV ~ LAT + LON, data=boston)
prp(latlontree) #we predict the number: avarage of house prices in each leaf

# Visualize output
plot(boston$LON, boston$LAT)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=20)

#we want to see what the tree thinks is above the median
fittedvalues = predict(latlontree)
points(boston$LON[fittedvalues>21.2], boston$LAT[fittedvalues>=21.2], col="blue", pch="$")
# we have done a better job than the linear regression
# we still making mistakes. But the tree was too complicated, maybe was overfittin

# Simplify tree by increasing minbucket
latlontree = rpart(MEDV ~ LAT + LON, data=boston, minbucket=50)
plot(latlontree)
text(latlontree) # we have fewer splits -> far more interpretable

# Visualize Output
plot(boston$LON,boston$LAT)
abline(v=-71.07) #the first split was at 71.07. v for vertical line
abline(h=42.21) #line correspond pretty much to Charles river
abline(h=42.17)
points(boston$LON[boston$MEDV>=21.2], boston$LAT[boston$MEDV>=21.2], col="red", pch=20)
# right rectabngle is low price in Boston center


# VIDEO 5
# we will try to predict house prices using all the variables we have
# Let's use all the variables

# Split the data
library(caTools)
set.seed(123)
split = sample.split(boston$MEDV, SplitRatio = 0.7)
train = subset(boston, split==TRUE)
test = subset(boston, split==FALSE)

# Create linear regression
linreg = lm(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data=train)
summary(linreg)
#R^2 is 0.665. LAT and Long are not significant. Crime is very improtant
#someone may be correlated, but it is interestinc
#to calculate accuracy, let's calculate SSE

# Make predictions
linreg.pred = predict(linreg, newdata=test)
linreg.sse = sum((linreg.pred - test$MEDV)^2) #sum of predicted values vs real values
linreg.sse #let's see if we can beat this using regression trees.

# Create a CART model
tree = rpart(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data=train)
prp(tree) # LAT and Long aren't really importnat. Rooms is the most important split
# Polution appears twice, so it's nonlinear -> it does different things accoring to amount of polluton (Nox)
# Romm aperas three times .> very non-linear
# Things that were important in linear rgression (pupil/teacher ratio) do not appear here.
# Dis does not appear either. So how do they compare?

# Make predictions
tree.pred = predict(tree, newdata=test)
tree.sse = sum((tree.pred - test$MEDV)^2)
tree.sse #4328.988
# So regression trees are not as good as linear regression
# Let's see if with cp we can improve our results



# Video 7
#cp stands for complexity parameter
# too many splits -> is bad for generalization
# goal: minimize sum of square differences by making splits, but penalizing too many splits
# S= number of splits, lambda = our penalty
# goal: minimize(sum of RSS at each leaf + lambda*S)
# 
#INSERT TABLE CP WITH LAMBDA
#Relationship of cp with lambda
# cp = lambda/ RSS (no splits)
# RSS with no splits would be the avarage of the data, and calculate the RSS
# small cp -> large trees, big cp -> too simple trees

# Load libraries for cross-validation
library(caret)
library(e1071)

# Number of folds
tr.control = trainControl(method = "cv", number = 10)

# cp values #we need to tell caret which range of cp paramters
# cp varies beteween 0 and 1. It is likely we don't need to explore the whole range.
# the lecturer knows that the value is very small
cp.grid = expand.grid( .cp = (0:10)*0.001)
# 1 * 0.001= 0.001, 10*0.001= 0.01 -> (0.001:0.01)

# What did we just do?
1*0.001 
10*0.001 
0:10
0:10 * 0.001

# Cross-validation
tr = train(MEDV ~ LAT + LON + CRIM + ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO, data = train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)
tr #cp value is very small -> it want to build a very detailed tree
# Extract tree
best.tree = tr$finalModel
prp(best.tree)

# Make predictions -> let's see if it beats the linear model
best.tree.pred = predict(best.tree, newdata=test)
best.tree.sse = sum((best.tree.pred - test$MEDV)^2)
best.tree.sse #3660.149 this tree is better than the other tree. 
# But the linear regression did better
# Gist of this is: trees aren't always the best method, but you should always
#crossvalidate them in order to get a good performance.

