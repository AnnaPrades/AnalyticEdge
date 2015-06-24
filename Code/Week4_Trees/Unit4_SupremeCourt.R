# Unit 4 - "Judge, Jury, and Classifier" Lecture


# VIDEO 4

# Read in the data
stevens = read.csv("stevens.csv")
str(stevens)

# Split the data
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio = 0.7) #first argument is the outcome variable, 2nd is the ratio
Train = subset(stevens, spl==TRUE)
Test = subset(stevens, spl==FALSE)

# Install rpart library #Serveix per fer Trees
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

# CART model #fIRST ARGgument VD, deprés VI separades per +, agument class, li diu qu és un classification tree, minbucket es pq no faci overfitting
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=25)

prp(StevensTree) #to plot our tree. It is easy to interpret

# Make predictions (to check how well works in new dataset)
PredictCART = predict(StevensTree, newdata = Test, type = "class") #"class" if we want in our CART model a majority class
#this is like using a 0.5 threshold

table(Test$Reverse, PredictCART) #first the true outcomes, then the predictions
(41+71)/(41+36+22+71) #o diag/margin.table
#l'accuracy és millor que la que prediu sempre una reverse decision

# ROC curve
library(ROCR)

PredictROC = predict(StevensTree, newdata = Test)
PredictROC
#It gives two columnes, one for prob 0, the other for prob1

pred = prediction(PredictROC[,2], Test$Reverse) #2n argument is the 2nd column
perf = performance(pred, "tpr", "fpr")
plot(perf)

#Caluclate Area Under the Curve (AUC)
as.numeric(performance(pred, "auc")@y.values)
#AUC -> if it is 50% it would be pure gessing. It is the % you will gess correctly if it is 0 or 1

#In order to see how the minbuck affects the nº of splits, build a CART wiht minbuck=5
StevensTree.minb5 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=5)
prp(StevensTree.minb5)

StevensTree.minb100 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", minbucket=100)
prp(StevensTree.minb100)


# VIDEO 5 - Random Forests
#Random Forest method was created to improve CAR accuracy. Unfortunately, this makes it less interpretable.
# -> It is not as sensitive to the parameter values as CART is
#It creates many CART trees. If we do CART many times it would repeat the same tree
# -> each tree splits only a random subset of variable (bagged or bootstrapped sample of data)
# -> selection with replacement
# -> since each tree has different vbles, and diff values-> lots trees
#To make a prediction for a new observation, each tree "votes" on the outcome, and we pick the
#outcome that receives the majority of the votes
#Parameters we need to select:
#a) The nodesize (or minbucket in CART): minimim n of observations
# -> smaller nodesize may take longer (smaller big trees)
#b) Number of trees (ntree parameter): if too small bootstraping may miss observations
# -> more trees take longer to build. 
# -> 200 is tipycally plenty
# Selection of paramters is les important than in CART (less senivive).
# -> as long as it is reasonable, it's OK.

# Install randomForest package
install.packages("randomForest")
library(randomForest)

# Build random forest model
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )
#We see a warning. Trees can be used for regression problems
#RandomForest does not have a method argument 
# -> so we need to make sure that outcome is a factor
# -> let's convert Reverso to a factor in Train and Test

# Convert outcome to factor
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)

# Try again
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25 )
#Now we don't receive warning -> data is ready

# Make predictions
PredictForest = predict(StevensForest, newdata = Test)
table(Test$Reverse, PredictForest) #to calculat accuracy
(40+74)/(40+37+19+74) #This does not match my table!!!
#It is ok because ther is a random component to this method.
#Accuracy is just a little better thant the CART. 
#Sometimes you'll see a small imrpovement, others a big one

#Questions. Set seed to 100 and replicate the Forest from before. Calculate accuracy
#and then repeat it, but with set.seed(200) first. 


set.seed(100)
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25)
PredictForest = predict(StevensForest, newdata = Test)
table(Test$Reverse, PredictForest)
(41 + 75)/ (41 + 18 + 36 +75) #0.6823529
 
set.seed(200)
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, ntree=200, nodesize=25)
PredictForest = predict(StevensForest, newdata = Test)
table(Test$Reverse, PredictForest)
(41 + 75)/ (41 + 18 + 36 +75) #0.6823529

# the random component of the random forest method can change the accuracy. 
#The accuracy for a more stable dataset will not change very much,
#but a noisy dataset can be significantly affected by the random samples. 

# VIDEO 6
#How do we set the minbucket value? Avoid overfittin or oversimplifying
# We can not use the accuracy in the testing set, because then we would be using 
# the testing set implicitly. And the idea is that test data should be new
# We use the k-fold Cross Validation to select parameter value
# This methods works through the following steps:
# 1. We split the training set into k equally sized subsets (e.g. k=5)
# 2. We select k-1 (e.g : 4) to estimate the model and compute predictions to fold 5
# 3. We reapet this for each of the other folds
# 4. Ultamitly corss validation builds one model for each fold
# 5. Then, for each candidate parameter we compute the accuracy model
#               INSERT SLIDE 21 SUPREME COURT
#Tipically the curbe is lower at the beggining because of overfittin
#And lower at the end -> model too simple

#Cross-validation in R
# We use cp parameter: complexity parameter, 
#measures trade-off between model complexity and accuracy on the training set
# cp it's like Adjusted R-squared for linear regression
# like AIC for logistic regression
# smaller cp -> leads to a biiger tree (might overfit)
# if too large -> model too simple

# Install cross-validation packages
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

# Define cross-validation experiment
numFolds = trainControl( method = "cv", number = 10 ) #First, we define the nº of folds. 
# cv for cross validation, and 10 buckets or folds
#then we need to pick the cp values
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01))  #cp paràmtres a testar Van del 01 al 05 amb increments de 0.01

# Perform the cross validation. VD (Reverse), and VI separeted by +. Data set is Train
#method = "rpart" -> since we want to cross validate a CART model
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )
# 1st column is cp. 2nd is the accuracy: we see that it starts lower (overfitting), then goes higher, and then lower agin (too simple).
# I got cp =0.19 -> but in the video cp=0.18

# Create a new CART model (with the cp discovered before)
StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="class", cp = 0.18)

# Make predictions -> type = "class" because we want class predictions
PredictCV = predict(StevensTreeCV, newdata = Test, type = "class")
table(Test$Reverse, PredictCV)
(59+64)/(59+18+29+64) # 0.7235294

#Cross validation helps us to select a good parameter value.

#Plot the tree we have created using cross validation
prp(StevensTreeCV)
#The tree with the best accuracy only has one split! 
# When we were picking different minbucket parameters before, 
#it seemed like this tree was probably not doing a good job of fitting the data.
#However, this tree with one split gives us the best out-of-sample accuracy. 
#This reminds us that sometimes the simplest models are the best!
