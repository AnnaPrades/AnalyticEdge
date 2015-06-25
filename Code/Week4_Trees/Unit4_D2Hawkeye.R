# Unit 4 - "Keeping an Eye on Healthcare Costs" Lecture
#D2Hawkeye is a medical data mining company.
#Company combines expert knowledge with analytics to improve quality and cost management in healthcare
#Data source: diagnoses, procedures, drugs...
# Then -> Aggregate, Clean, Normalize -> Secure Database -> Predictive Models -> Reports
#Company's objective: identify hihg-riks patients, arrange specialist care
# D2Hawkeye's Claims Data: 2.4 million people 2001-2003 -> Preodictions 2003-2005
# They included only people with at least 10 months of data both in the train and test set
# This left 400.000 people
# Since they had thousands of variables they grouped them:
# 13.000 diagnoses -> 217 diagnosis groups
# 20.000 procedures -> 213 procedure groups
# 45.000 prescription drugrs -> 189 therapeutic groups
# additional variable: chronic or acute condition -> different cost profiles
#they also added 269 medically-defined rules
# e.g. interactions with illenesses (obsity and depression)
#e.g. interactions with illness an age
# illness severity
# defined 5 buckets of cost -> each one wiht 20% of people
# so the bucket 1 has the 20% of cost, but 78% of people
#INSERT GRAPH BUCKETS

#Error measures
#a) Tipically we use R^2
# b) Pnealty error: classify a very high-riskc patient as low risk is more costly than the reverse
# -> we developed a penalty error -> asymmetric penalties
# INSERT PENALTY MATRIX

#BASELINE: predict that the cost in the next period will be the cost in the current period


# How do we define the cost buckets?
# Multi-class classifcation: classificationa and regression trees
# INSERT GRAPH: Patient has coronary disease. If yes, has diabetis? -> different buckets


# VIDEO 6

# Read in the data
Claims = read.csv("ClaimsData.csv")

str(Claims)
# Data is from 2008, we will predict outcomes in 2009

# Percentage of patients in each cost bucket
table(Claims$bucket2009)/nrow(Claims) #this gives the proportion of people in each bucket


# Split the data
library(caTools)

set.seed(88)

spl = sample.split(Claims$bucket2009, SplitRatio = 0.6) #60% in the training set

ClaimsTrain = subset(Claims, spl==TRUE)

ClaimsTest = subset(Claims, spl==FALSE)

#Question 1: avarage age of Train? -> summary(ClaimsTrain)
mean(ClaimsTrain$age)
#Question 2: proportion with diabetis
table(ClaimsTrain$diabetes)/nrow(ClaimsTrain)

# VIDEO 7

# Baseline method
table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)

(110138 + 10721 + 2774 + 1539 + 104)/nrow(ClaimsTest)
#accuracy is the sum of the diagonal / total of observations
# 0.6838

# Penalty Matrix: Actual outcomes on left, predicted outcomes on top
PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow=TRUE, nrow=5)

PenaltyMatrix

# Penalty Error of Baseline Method: We multiply the penalty matrix by the baseline matrix
as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix
#now, to compute the penalty error, we just have to sum it up and divida by n
sum(as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix)/nrow(ClaimsTest) #0.7386055

# So our goal in video 8 is to create a CART with accuracy > 68%, and penalty <0.74
#Quick question: Suppose that instead of the baseline method discussed in the 
#previous video, we used the baseline method of predicting the most frequent 
#outcome for all observations. This new baseline method would predict cost bucket 1 for everyone. 
#What would the accuracy of this baseline method be on the test set?
table(ClaimsTest$bucket2009)/nrow(ClaimsTest) #Bucket 1 proportion: 0.67126996
PenaltyMatrix #we have a look at the first column. Penaltioes when predictions is on
penalty <- c(0, 2, 4, 6, 8) # create a vector with these penalties
penalty * (table(ClaimsTest$bucket2009)/nrow(ClaimsTest)) #multiply by proportions
sum(penalty * (table(ClaimsTest$bucket2009)/nrow(ClaimsTest))) # sum of all penalties 

# VIDEO 8

# Load necessary libraries
library(rpart)
library(rpart.plot)

# CART model. We use rpart to predict bucket 2009, using vI. The cp was obtained with Train data (cross validation)
ClaimsTree = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain, method="class", cp=0.00005)

prp(ClaimsTree)


# Make predictions
PredictTest = predict(ClaimsTree, newdata = ClaimsTest, type = "class")

table(ClaimsTest$bucket2009, PredictTest)

(114141 + 16102 + 118 + 201 + 0)/nrow(ClaimsTest)

# Penalty Error
as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix

sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix)/nrow(ClaimsTest)
#we have improved the accuracy ( 0.7126669 vs 0.68), but the penalty has increased also
# (0.7578902 vs 0.74)
#This happens because by default CART model is seen as with the same penalty.
# How can we fix this? rpart allows us to specificy a parameter called loss.

# New CART model with loss matrix
#we add:  parms=list(loss=PenaltyMatrix)
#So it might choose different splits when building to minimize the worst type of errors.
# we'll probably get lower overall accuracy, but the penalty error will be much lower too.
ClaimsTree = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=ClaimsTrain, method="class", cp=0.00005, parms=list(loss=PenaltyMatrix))


# Redo predictions and penalty error
PredictTest = predict(ClaimsTree, newdata = ClaimsTest, type = "class")

table(ClaimsTest$bucket2009, PredictTest)

(94310 + 18942 + 4692 + 636 + 2)/nrow(ClaimsTest)

sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix)/nrow(ClaimsTest)
#the accuracy is 0.647, and the penalty is 0.642