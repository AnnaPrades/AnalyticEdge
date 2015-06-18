# Unit 3, Modeling the Expert
#Suppose the coefficients of a logistic regression model with two independent variables are as follows
b0 <- -1.5
b1 <- 3
b2 <- -0.5
x1 <- 1
x2 <- 5
#What is the value of the logit = log(Odds)?
logit <- b0 +b1*x1 + b2*x2
logit #-1

#What is the value of the Odds for this observation?
odds <- exp(logit)
#What is the value of P(y = 1) for this observation?
p_y1 <- 1/(1 + exp(-logit))
p_y1
[1] 0.2689414

# Video 4

# Read in dataset
quality = read.csv("quality.csv")

# Look at structure
str(quality)

# Table outcome. Good care is 0, poor care 1.
table(quality$PoorCare)

# Baseline accuracy. In regression, baseline model is the avarage outcome.
#In a classification problem, a standard baseline method is to predict the most
#frequent outcome for all observations. Since good care is most common, we will predict that all patients
#are receiving good care.
98/131 #accuracy about 75%. This is the baseline.

# Install and load caTools package. This is in order to split database in training and testing set.
install.packages("caTools")
library(caTools)

# Randomly split data
set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio = 0.75)
split

La seed la creem perquè tothom tingui el mateix data set. Split assegura que les dues mostres tinguin ratio 0.75
Es crea un factor lògic (split) de True, False

# Create training and testing sets
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)

# Logistic Regression Model. Tiro d'un model genral, la family = binomial és perquè sàpiga que és un regressió logística.
QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family=binomial)
summary(QualityLog)

#L'AIC et dóna com la R ajustada (nombre de variables comparat amb el nombre d'observacions).
És una eina per selcció de models. El millor és el què té l'AIC menor. El problema és que només pots comprar amb models del mateix dataset.

# Make predictions on training set. Ens dóna probabilits.
predictTrain = predict(QualityLog, type="response")


# Analyze predictions. Calcula probabilitats, per això valors rang de 0 a 1

summary(predictTrain)
tapply(predictTrain, qualityTrain$PoorCare, mean)

#La Taula em mostra que la probabiiat és més elevada per predir 1 (poor care) que 0, ja ens va bé.

#Quick Questioncreate a logistic regression model to predict "PoorCare" using the 
#independent variables "StartedOnCombination" and "ProviderCount".


model2 = glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualityTrain, family=binomial)
summary(model2)

# Video 5

# Confusion matrix for threshold of 0.5. Em dóna TRUE si em prediu Poor Care (1)
#threshold em marca valor a partir del qual probabilitat Poor care diré que 1 o 0

table(qualityTrain$PoorCare, predictTrain > 0.5)

La Taula em mostra que encerto 70 casos de Good Care (row=0), però tinc 4 falsos positius.
Pel que fa a la Poor care, tinc 15 falsos negatius, i 10 true positius.

# Sensitivity and specificity

sens05 <- 10/25 #TP/(TP+FN). Total positius detectats sobre total positus
spec05 <- 70/74 # TN/(TN+FP). Total negatius detectats sobre total negatius
sens05
spec05

La sensitivity em calcula: TP/(TP+FN). És més gran a més baix el thershold (0.5).

L'especificity em calcula: TN/ (TN+FP). A més alt el thershold, sera més alta (i més baixa la sensitivity)

# Confusion matrix for threshold of 0.7
table(qualityTrain$PoorCare, predictTrain > 0.7)

# Sensitivity and specificity
8/25
73/74
sens07 <- 8/25
spec07 <- 73/74
sens07
spec07



# Confusion matrix for threshold of 0.2
table(qualityTrain$PoorCare, predictTrain > 0.2)

# Sensitivity and specificity
sens02 <- 16/25
spec02 <- 54/74
sens02
spec02

# comparativa
sens <- c(sens02, sens05, sens07)
spec <- c(spec02, spec05, spec07)
t_comp <- rbind(sens, spec)
colnames(t_comp) <- c("thereshold 02", "thereshold 05", "thershold07")
t_comp

# Video 6

# Install and load ROCR package
install.packages("ROCR")
library(ROCR)

# Prediction function. T'ajuda a decidir Threshold
#sensitivity is in the y axis
#the false positive rate, or 1 minus the specificity,in hte x axis

ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr") #Diu què vols que grafiquen i què vols a x i a y axis

# Plot ROC curve
plot(ROCRperf)


#The ROC curve always starts at the point (0, 0), sensitivity = 0. It does not cath Poor cases.
#But you will label all good care cases.meaning you have a false positive rate of 0
#The ROC curve always ends at the point (1,1): sensitivity=1
#you'll catch all of the poor care cases, but you'll label all good cases as poor
# (false positive rate of 1)
#In the middle, around (0.3, 0.8),you're correctly labeling about 80% of the poor care cases,
#with a 30% false positive rate



#Whish threshold choose?
# If you're more concerned with having a high specificity
# or low false positive rate, pick the threshold
#that maximizes the true positive rate
#while keeping the false positive rate really low.
#A threshold around (0.1, 0.5) on this ROC curve
#looks like a good choice in this case.
#On the other hand, if you're more concerned
#with having a high sensitivity or high true positive rate,
#pick a threshold that minimizes the false positive rate
#but has a very high true positive rate.
#A threshold around (0.3, 0.8) looks like a good choice in this case.


# Add colors
plot(ROCRperf, colorize=TRUE)

# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

##Valorar model
#1. Multicolinearity: involves cheking cor between VI
#2. significance: look at the Area Under the Curve (AUC)
#50% is pure gessing
#3. Confusion matrix
#overall accuracy: the number of true negatives and true positives divided by N
#terms in the diagonal divided by the total
#overall error: the other diagonal (false neg + false pos)
#sensitivity: TP / (TP+FN) #true positive rate
#specificity: TN/ (TN + FP) #false positive rate



##Exrecici amb el Test i predict test
This question uses the original model with the independent variables "OfficeVisits" and "Narcotics". Be sure to use this model, instead of the model you built in Quick Question 4.

Compute the test set predictions in R by running the command:
        
        predictTest = predict(QualityLog, type="response", newdata=qualityTest)

You can compute the test set AUC by running the following two commands in R:
        
        ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)

auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)

What is the AUC of this model on the test set?
auc
[1] 0.7994792
#The AUC of a model has the following nice interpretation: given a random patient 
#from the dataset who actually received poor care, and a random patient from the 
#dataset who actually received good care, the AUC is the perecentage of time that
#our model will classify which is which correctly. 