##Assignment CART
#1. UNDERSTANDING WHY PEOPLE VOTE
#The researchers grouped about 344,000 voters into different groups randomly -
#about 191,000 voters were a "control" group, and the rest were categorized 
#into one of four "treatment" groups. These five groups correspond to five 
#binary variables in the dataset.
        #1. "Civic Duty" (variable civicduty) group members were sent a letter that 
        #simply said "DO YOUR CIVIC DUTY - VOTE!"
        #2. "Hawthorne Effect" (variable hawthorne) group members were sent a 
        #letter that had the "Civic Duty" message plus the additional message "YOU ARE BEING STUDIED" 
        # 3.     Self" (variable self) group members received the "Civic Duty" message as well as the recent voting record of everyone in that household and a message stating that another message would be sent after the election with updated records.
        # 4."Neighbors" (variable neighbors) group members were given the same message as that for the "Self" group, except the message not only had the household voting records but also that of neighbors - maximizing social pressure.
        #5. "Control" (variable control) group members were not sent anything, and represented the typical voting situation.

        #Additional variables include sex (0 for male, 1 for female), yob (year of birth), and the dependent variable voting (1 if they voted, 0 otherwise).
votes <- read.csv("gerber.csv")

# What proportion of people voted?
prop.table(table(votes$voting)) #baseline most frequent outcome would be the opposite: 0.6841004

#Which of the four "treatment groups" had the largest percentage of people who actually voted (voting = 1)?
tapply(votes$voting, votes$civicduty, mean)

tapply(votes$voting, votes$hawthorne, mean)

tapply(votes$voting, votes$self, mean)

tapply(votes$voting, votes$neighbors, mean)

#Build a model without splitting that uses the four groups as VI
lr.mod1 <- glm(voting ~ hawthorne + civicduty + neighbors + self, data = votes, family ="binomial")
summary(lr.mod1)
#what is the accuracy of the model with a threshold of 0.30?
lr.predict <- predict(lr.mod1, type="response")
t1 <- table(votes$voting, lr.predict > 0.30) #we do not need newdata argument since we didn't split our data
t1
accuracy <- sum(diag(t1)/margin.table(t1))
accuracy

#what is the accuracy with a threshold of 0.50?
t1 <- table(votes$voting, lr.predict > 0.50) #we do not need newdata argument since we didn't split our data
t1
accuracy <- sum(diag(t1)/margin.table(t1))
accuracy

# if we compare the proportion that did not vote (0.6841004), with this accuracy 
# (0.6841004) we see that it is the same. What is happening?
# even though all of the variables are significant, this is a weak predictive model.

#let's see if it is useful as a predactive model (AUC) -> NEITHER
library(ROCR)
ROCRpred = prediction(lr.predict, votes$voting)
as.numeric(performance(ROCRpred, "auc")@y.values) # auc is 0.5308461

#2.1 Built a regression Tree (not method="class") with the 4 treatment variables and observations
# We'd like CART to split our groups if they have different probabilities of 
#voting. If we used method='class', CART would only split if one of the groups 
#had a probability of voting above 50% and the other had a probability of voting
#less than 50% (since the predicted outcomes would be different). However, with
#regression trees, CART will split even if both groups have probability less than 50%.

library(rpart)
library(rpart.plot)

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=votes)
prp(CARTmodel)
#What happens? No variables are used (the tree is only a root node) - 
#none of the variables make a big enough effect to be split on

#Now plot the model
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=votes, cp=0.0)
prp(CARTmodel2)

#Readign the tree: the people on civic duty are the ones on the right hand of the tree (0.31)
# because civicduty > 0.5?

#Make a new model that includes sex, with cp=0.0
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=votes, cp=0.0)
prp(CARTmodel3)

#READING THE TREE
# Then, if you plot the tree with prp(CARTmodel3), you can see that there is a
#split on the "sex" variable after every treatment variable split.
#For the control group, which corresponds to the bottom left, sex = 0 (male) 
#corresponds to a higher voting percentage.
#For the civic duty group, which corresponds to the bottom right, sex = 0 (male)
#corresponds to a higher voting percentage.

#Creat a tree wiht the control group, and another wiht the control and sex, both with cp=0.0
CARTmodel4 = rpart(voting ~ control, data=votes, cp=0.0)
prp(CARTmodel4, digits = 6)

CARTmodel5 = rpart(voting ~ control + sex, data=votes, cp = 0.0)
prp(CARTmodel5, digits=6)

#In the "control" only tree, what is the absolute value of the difference in the
#predicted probability of voting between being in the control group versus being
#in a different group? 
#You can use the absolute value function to get answer, i.e. 
#abs(Control Prediction - Non-Control Prediction). Add the argument "digits = 6"
#to the prp command to get a more accurate estimate.

abs(0.296638 - 0.34)

#Now, using the second tree (with control and sex), determine who is affected
#more by NOT being in the control group (being in any of the four treatment groups):
#sex=0=male
males <- 0.334176 - 0.290456
females <- 0.345818 - 0.302795
males
females

#Create a logitic regression model with control and sex
lr.mod2 <- glm(voting ~ control + sex, data = votes, family ="binomial")
summary(lr.mod2)

#The regression tree calculated the percentage voting exactly for every one of 
#the four possibilities (Man, Not Control), (Man, Control), (Woman, Not Control), 
#(Woman, Control). Logistic regression has attempted to do the same, although it 
#wasn't able to do as well because it can't consider exactly the joint possibility
#of being a women and in the control group.

#We can quantify this precisely. Create the following dataframe (this contains 
#all of the possible values of sex and control), and evaluate your logistic
#regression using the predict function (where "LogModelSex" is the name of your
#logistic regression model that uses both control and sex):

possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(lr.mod2, newdata=possibilities, type="response")

#What is the absolute difference between the tree and the logistic regression for 
#the (Woman, Control) case? Give an answer with five numbers after the decimal point.
lr.woman_control <- 0.2908065
tree.woman_control <- 0.290456 # woman is 1, so sex >0.5 -> bottom left corner
abs(lr.woman_control - tree.woman_control)
round(abs(lr.woman_control - tree.woman_control), 5)

#Add an interaction term with the logistic regression model
LogModel2 = glm(voting ~ sex + control + sex:control, data=votes, family="binomial")
summary(LogModel2)
#Interpretation of the -0.007 for the interaction coeffient:
# if a person is a woman and in the control group, the chance that she voted goes down

# Do the predictions with this last logistic regression model with possibilities
predict(LogModel2, newdata=possibilities, type="response")
#What is the difference between the tree and logistic for woman control case?
lr.woman_control <- 0.2904558
tree.woman_control <- 0.290456 # woman is 1, so sex >0.5 -> bottom left corner
abs(lr.woman_control - tree.woman_control)
round(abs(lr.woman_control - tree.woman_control), 5)
#Now the difference is 0!!

#Should we always include all possible interaction terms of the independent 
#variables when building a logistic regression model? No, overfitting
# have four treatment groups and two values for sex. If we have an interaction 
#term for every treatment variable with sex, we will double the number of variables.
#In smaller data sets, this could quickly lead to overfitting.

