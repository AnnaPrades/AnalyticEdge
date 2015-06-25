##Assignment 1 Logistic Regression: Songs
songs <- read.csv("songs.csv")
str(songs)
#How many songs were in year==2010?
table(songs$year) # 373
#How many songs are from Michael Jackson?
Jackson <- subset(songs,artistname == "Michael Jackson")
MichaelJackson = subset(songs, artistname == "Michael Jackson")
nrow(MichaelJackson) #18
#Which songs made it to top10?
table(Jackson$Top10) # shows that 5 made it, and 13 no
Jackson[ , c(2,39)]
MichaelJackson[c("songtitle", "Top10")]
#Which discrete values takes time signature and which is the most frequent?
table(songs$timesignature)
#which is the song with the highest tempo?
songs$songtitle[which.max(songs$tempo)]

#Split the file: train up to 2009, test=2010 year
SongsTrain <- subset(songs, year < 2010)
SongsTest <- subset(songs, year == 2010)

#We want a model with all the quantitative variables.
SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
#To eliminate the text variables, we first create a nonvar vector:
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
#To remove these variables from your training and testing sets, type the following:
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
# %in% This operator checks for inclusion in a set.

Model1 <- glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(Model1)

#The less complexity (confidence vbles) better the chances
# energy negative, but loudness positive:
cor(SongsTrain$loudness, SongsTrain$energy) #colinearity

#Create Model 2, which is Model 1 without the independent variable "loudness":
Model2 <- glm(Top10 ~ . -loudness, data=SongsTrain, family = binomial)
summary(Model2)

#Create Model 3, like Model 1 but without energy
Model3 <- glm(Top10 ~ . -energy, data=SongsTrain, family = binomial)
summary(Model3)

#AIC from model 3 i lower than from Model2 (4848.7<4937.8)

#Make predictions with Model3 to the Test set
PredicTest <- predict(Model3, type = "response", newdata= SongsTest)

#What is the accuracy of Model 3 on the test set, using a threshold of 0.45? 
t1 <- table(SongsTest$Top10, PredicTest > 0.45)
t1
accuracy <- (309 + 19)/(309 + 5 + 40 + 19) # 0.8793566

#una altra forma de calcular accuracy
accuracy2 <- sum(diag(t1)/margin.table(t1))

#Validating our model. What is the accuracy of the baseline model in the Test set?
# Is the most frequent outcome: not top ten
table(SongsTest$Top10) # O occurs 314 times, 1 in 59
baseline_accuracy <- 314/373 # 0.8418231

#Model 3 gives a small improvement. Does it provide an edge?
#competitive edge if we are able to provide a list with hihgly probable Top10
t1
#number of songs predicted to Top 10: 19 correctly, 5 not correctly
sensitivity <- 19/(19+40)
sensitivity
specificity <- 309/(309+5)
specificity

##Predicting Parole Violators
# inmates deemed not to be a threat to society are released
#and they can be returned to prison if they violate the terms of their parole
#Parole boards are charged with identifying which inmates are good candidates for release
#a model that predicts if an inmate will violate the terms of his or her parole.

#a model that predicts if an inmate will violate the terms of his or her parole.
#parolees who served no more than 6 months in prison and whose maximum was 18

parole <- read.csv("parole.csv")
#how many parolees violated their terms?
table(parole$violator) #78

# variables that are unordered factors with at least 3 levels
#state & crime -> So we need to convert them in factors
parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)
str(parole)

#Splitting into a training and test set
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

#Model with all the variables
mod1 <- glm(violator ~ ., data=train, family=binomial)
summary(mod1)

#What can we say based on the coefficient of the multiple.offenses variable?
exp(1.6119919)
[1] 5.012786
#a parolee who committed multiple offenses has 5.01 times higher odds of being
#a violator than a parolee who did not commit multiple offenses but is otherwise identical

#ln(odds of A) = ln(odds of B) + 1.61
#exp(ln(odds of A)) = exp(ln(odds of B) + 1.61)
#exp(ln(odds of A)) = exp(ln(odds of B)) * exp(1.61)
#odds of A = exp(1.61) * odds of B
#odds of A= 5.01 * odds of B

#In the second step we raised e to the power of both sides. 
#In the third step we used the exponentiation rule that e^(a+b) = e^a * e^b. 
#In the fourth step we used the rule that e^(ln(x)) = x. 

#Consider a parolee who is male, of white race, aged 50 years at prison release
#from the state of Maryland
#served 3 months, had a maximum sentence of 12 months
#did not commit multiple offenses and commited a larceny
#what are the odds this individual is a violator?
person <- c(1, 1, 1, 50, 0, 0, 0, 3, 12, 0, 1, 0, 0)
mod1_person <- sum(person * mod1$coef)
> mod1_person
[1] -1.70063 # This is the logit or the log odds

exp(mod1_person) # These are the odds
[1] 0.1825685

# What is the probability that this individual is a violator?
#p_y1 <- 1/(1 + exp(-logit))
prob_person_violator <- 1/(1 + exp(-mod1_person))
prob_person_violator #0.1543831


#Use the predict function with mod1. What is the maximum value?
predMod1 <- predict(mod1, type="response", newdata=test)
summary(predMod1)

#Evaluating the model in the testing set
t1 <- table(test$violator, predMod1 > 0.50)
t1
sensitivity <- t1[4]/(t1[2]+t1[4]) #TP/(TP+FN)
sensitivity
specificity <- t1[1]/(t1[1]+t1[3]) #TN/(TN+FP)
specificity
accuracy <- sum(diag(t1)/margin.table(t1)) #(167+12)/(167+11+12+12)
accuracy
[1] 0.8861386

#What is the accuracy that predicts that everyparole is a non-violator?
table(test$violator) #The most frequent outcome would be the baseline
179/(179+23)
[1] 0.8861386

#boards tend to be particularily concerned with releasing prisoners who will 
#violate their parole -> they are concerned wiht FALSE NEGATIVES 
#i.e, you predict FALSE, but violater is true
#Since the higher the probability, the more probable the being a violator, you need 
# a lower cutoof to have fewer false positives
t1 <- table(test$violator, predMod1 > 0.50)
t1 #there are 11 false negatives, and 12 false positives
t2 <- table(test$violator, predMod1 > 0.70)
t2 # there are 20 false negatives, and 3 false positives
t3  <- table(test$violator, predMod1 > 0.20)
t3 #there are 6 false negatives, and 25 false positives

#Using the ROC package what is the Area under the curve of the model? AUC
library(ROCR)
ROCRpred = prediction(predMod1, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)

#Meaning of AUC (=0.895834)
# the probability the model can correctly differentiate between a randomly selected
#parole violator and a randomly selected parole non-violator

#Identifying bias in the data
#Our goal has been to predict the outcome of a parole decision
# It is always important to evaluate a dataset for possible sources of bias
#The sample is biased because parolees who have neither violated nor finished their
#parole should be there. Ideally we should monitor them.

## Loan Repayment
#Lenders face the problem of predicting the risk of a borrower being unable to repay a loan.
loans <- read.csv("loans.csv")
str(loans)
t1 <- table(loans$not.fully.paid)
margin.table(t1)
prop.table(t1)

#We have variables with NA. Since we want to predict the no-payment with all the cases
#we will opt for imputing values
# we predicted missing variable values using the available independent variables for each observation.
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid") #this vector has all VI expcet VD not.fully.paid
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

#To be sure to have the exact results we are encouraged to use the imputed.csv_
loans2 <- read.csv("loans_imputed.csv")
#Split the file to select the 70% in the training set
set.seed(144)
library(caTools)
split = sample.split(loans2$not.fully.paid, SplitRatio = 0.7)
train = subset(loans2, split == TRUE)
test = subset(loans2, split == FALSE)
#Us logistic regression in training set to predict not.fully.paid with all VI
mod1 <- glm(not.fully.paid ~ ., data=train, family="binomial")
summary(mod1)

##2 people are equal except for fico (a=700, b=710). Calculate logA - logB -> it's ok
logitA <-  -9.317e-03*700 #Coeficient fico és -9.317e-03
logitB <-  -9.317e-03*710
logitA - logitB
[1] 0.09317
#alternative solucion: Because Application A is identical to Application B
#other than having a FICO score 10 lower
#its predicted log odds differ by -0.009317 * -10 = 0.09317 from the predicted log odds
#of Application B. 

#What is the odds ratio? OddsA/OddsB -> it's ok
oddsA <- exp(logitA)
oddsB <- exp(logitB)
oddsA/oddsB
[1] 1.097648

#Solution given: Using the answer from the previous question, the predicted odds 
#of loan A not being paid back in full are exp(0.09317) = 1.0976 times larger than 
#the predicted odds for loan B. Intuitively, it makes sense that loan A should
#have higher odds of non-payment than loan B, since the borrower has a worse credit score. 

#Predict the probability of the test set loans not being paid back in full
#store it into variable predicted.risk and add it to the test set
predicted.risk <- predict(mod1, type="response", newdata=test)
test$predicted.risk <- predicted.risk
str(test)
#compute the confusion matrix
t1 <- table(test$not.fully.paid, predicted.risk > 0.5)
t1
sensitivity <- t1[4]/(t1[2]+t1[4]) #TP/(TP+FN)
sensitivity
specificity <- t1[1]/(t1[1]+t1[3]) #TN/(TN+FP)
specificity
accuracy <- sum(diag(t1)/margin.table(t1)) #(TP + TN)/ total (sumatori denominadors)
accuracy #The accuracy is:  0.8364079
#Accuracy of the baseline model
table(test$not.fully.paid)
prop.table(table(test$not.fully.paid)) #baselin is the most common, 0: 0.8398886

#compute the AUC wiht the ROCR package
library(ROCR)
ROCRpred = prediction(predicted.risk, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

# LendingClub.com assigns the interest rate to a loan based on their estimate of that loan's risk.
#Build a bivariate logistic regression model (aka a logistic regression model 
#with a single independent variable) that predicts the dependent variable not.fully.paid 
#using only the variable int.rate. 
mod2 <- glm(not.fully.paid ~ int.rate, data=train, family="binomial")
summary(mod2)
#The variable int.rate is highly significant in the bivariate model, 
#but it is not significant at the 0.05 level.
# This is because it is probably highly associated with other risks-related variables 
#and therefore does not incrementally improve the model when those other variables are included

#Make the predictions for the bivariate model
predMod2 <- predict(mod2, type="response", newdata=test)
summary(predMod2) #max = 0.42660
predMod2[which.max(predMod2)] #not exactly the same result that Max. in summary (why?)

#how many loans would be predicted as not being paid in full in the test set with a cutoof 0.5?
table(test$not.fully.paid, predMod2 > 0.5) #None -> 0

#compute the AUC wiht the ROCR package
library(ROCR)
ROCRpred = prediction(predMod2, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values) #0.6239081

#Compute interest revenue¨: investment * exp(interest rate * time)
# e.g: investment 10$, anual interest rate 6%, time 3 years
10 * exp(0.06*3) # [1] 11.97217

#What is the profit to the investor if the investment is paid back in ful
(10 * exp(0.06*3)) -10 #[1] 1.972174

#What if no money was received? -> -10$
# To sum up: c * (exp(rt) - 1) dollars of profit if the loan is paid back in full 
#and -c dollars of profit if the loan is not paid back in full (pessimistically)

#In order to evaluate the quality of an investment strategy, 
#we need to compute this profit for each loan in the test set.

# For this variable, we will assume a $1 investment (aka c=1). 
#o create the variable, we first assign to the profit for a fully paid loan, 
#exp(rt)-1, to every observation, and we then replace this 
#value with -1 in the cases where the loan was not paid in full. 
#All the loans in our dataset are 3-year loans, meaning t=3 in our calculations.

test$profit = exp(test$int.rate*3) - 1

test$profit[test$not.fully.paid == 1] = -1
#What is the maximum profit of a $10 investment in any loan in the testing set?
summary(test$profit) #Max =0.8895 *10 = 8.895

#6.1. An investment strategy based on risk
#Investors seek loans that balance reward with risk, 
#in that they simultaneously have high interest rates and a low risk of not being paid back.
#we will analyze an investment strategy in which the investor only purchases loans 
#with a high interest rate (a rate of at least 15%), but amongst these loans 
#selects the ones with the lowest predicted risk of not being paid back in full. 
#We will model an investor who invests $1 in each of the most promising 100 loans.

#First, use the subset() function to build a data frame called highInterest 
#consisting of the test set loans with an interest rate of at least 15%
highInterest <- subset(test, int.rate >= 0.15)

#what is the avarage profit in this subset?
summary(highInterest$profit)

#what is the percentage of not fully paid?
prop.table(table(highInterest$not.fully.paid))

#Next, we will determine the 100th smallest predicted probability of not paying 
#in full by sorting the predicted risks in increasing order and selecting the 
#100th element of this sorted list.
cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans <- subset(highInterest, predicted.risk <= cutoff)
str(selectedLoans) #there are 100 observations


#what is the avarage profit in this subset?
summary(selectedLoans$profit)
#What is the profit of the investor, who invested $1 in each of these 100 loans
sum(selectedLoans$profit)

#what is the percentage of not fully paid?
table(selectedLoans$not.fully.paid)
prop.table(table(selectedLoans$not.fully.paid))

#Edge:  analytics can be used to select a subset of the high-interest loans 
#that were paid back at only a slightly lower rate than average, resulting in a 
#significant increase in the profit from our investor's $100 investment. 
#Although the logistic regression models developed in this problem did not have 
#large AUC values, we see that they still provided the edge needed to improve 
#the profitability of an investment portfolio.

##BASEBALL
baseball <- read.csv("baseball.csv")
str(baseball) #1232 team/year observations
table(baseball$Year)
#Identify the total number of years included in this dataset (shorter sessions were removed)
length(table(baseball$Year)) #IMPORTANT!!!!

#replace baseball, with a subset that only includes teams that made it to playoffs
baseball <- subset(baseball, Playoffs == 1)
#Number of teams invited by year
t1 <- table(baseball$Year, baseball$Team)
margin.table(t1, 1)
addmargins(t1, margin=seq_along(dim(t1)), FUN = sum)

#alternative approach:
table(table(baseball$Year)) #IMPORTANT!!!!!!!

#Add a new variable that is the number of competitors for each year
PlayoffTable = table(baseball$Year) #store the output of the number of competitors
PlayoffTable
#What best describes the output of names(PlayoffTable)?
str(PlayoffTable)
names(PlayoffTable) #the output is a vector of years as character
PlayoffTable[c("1990", "2001")] #Number of teams for the years selected
#Add a variable with the number of competitors
baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)] #IMPORTANT
#Because PlayoffTable is an object and not a function, we look up elements in it 
#with square brackets instead of parentheses. as.character() is needed to convert
#the Year variable in the dataset to a string, which we know from the previous 
#parts is needed to look up elements in a table.

#How many teams/year pairs played with 8 competitors?
table(baseball$NumCompetitors == 8)

#Add a variable named WorldSeries to the baseball data frame, by typing:
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
table(baseball$WorldSeries)

#Build 12 bivariate models, one for each variable
biv1 <- glm(WorldSeries ~ Year, data = baseball, family = "binomial")
biv2 <- glm(WorldSeries ~ RS, data = baseball, family = "binomial")
biv3 <- glm(WorldSeries ~ RA, data = baseball, family = "binomial")
biv4 <- glm(WorldSeries ~ W, data = baseball, family = "binomial")
biv5 <- glm(WorldSeries ~ OBP, data = baseball, family = "binomial")
biv6 <- glm(WorldSeries ~ SLG, data = baseball, family = "binomial")
biv7 <- glm(WorldSeries ~ BA, data = baseball, family = "binomial")
biv8 <- glm(WorldSeries ~ RankSeason, data = baseball, family = "binomial")
biv9 <- glm(WorldSeries ~ OOBP, data = baseball, family = "binomial")
biv10 <- glm(WorldSeries ~ OSLG, data = baseball, family = "binomial")
biv11 <- glm(WorldSeries ~ NumCompetitors, data = baseball, family = "binomial")
biv12 <- glm(WorldSeries ~ League, data = baseball, family = "binomial")
summary(biv1) #Year -> significant
summary(biv2)
summary(biv3) # RA -> significant
summary(biv4)
summary(biv5)
summary(biv6)
summary(biv7)
summary(biv8) #RankSeason -> signficant
summary(biv9)
summary(biv10)
summary(biv11) #Numcompetitoris -> significant
summary(biv12)

#There are 4 variables with at least one star, and 2 almost significant.

#Build a model using all of the variables that you found to be significant in the bivariate
mod1 <- glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data = baseball, family = "binomial")
summary(mod1) # None of the four are now significant! This is because correlations

#Which correlations are higher than 0.8?
pairs(baseball[ , c("Year", "RA", "RankSeason", "NumCompetitors")])
cor(baseball$Year, baseball$RA)
cor(baseball$Year, baseball$RankSeason)
cor(baseball$Year, baseball$NumCompetitors)
cor(baseball$RA, baseball$RankSeason )
cor(baseball$RA, baseball$NumCompetitors)
cor(baseball$RankSeason, baseball$NumCompetitors)
#Short-cut!!
cor(baseball[c("Year", "RA", "RankSeason", "NumCompetitors")])

# Build the 3 variate models
biv13 <- glm(WorldSeries ~ Year + RA, data = baseball, family = "binomial")
biv14 <- glm(WorldSeries ~ Year + RankSeason, data = baseball, family = "binomial")
biv15 <- glm(WorldSeries ~ Year + NumCompetitors, data = baseball, family = "binomial")
biv16 <- glm(WorldSeries ~ RA + RankSeason, data = baseball, family = "binomial")
biv17 <- glm(WorldSeries ~ RA + NumCompetitors, data = baseball, family = "binomial")
biv18 <- glm(WorldSeries ~ RankSeason + NumCompetitors, data = baseball, family = "binomial")
summary(biv1) #Year -> significant # AIC 232.35
summary(biv3) # RA -> significant AIC: 237.88
summary(biv8) #RankSeason -> signficant AIC: 238.75
summary(biv11) #Numcompetitoris -> significant AIC: 230.96
summary(biv13) #yEAR-ra -> AIC: 233.88
summary(biv14) #yEAR + rANK -> AIC: 233.55
summary(biv15) #yEAR + nUMcompetitors -< AIC: 232.9
summary(biv16) #RA + RankSeason -> AIC: 238.22
summary(biv17) #RA + NumCompetitors -> AIC: 232.74
summary(biv18) #RankSeason + NumCompetitoris -> AIC: 232.52

#Best model is NumCompetitors.
#his seems to confirm the claim made by Billy Beane in Moneyball that all that 
#matters in the Playoffs is luck, since NumCompetitors has nothing to do with 
#the quality of the teams!