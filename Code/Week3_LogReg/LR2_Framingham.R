# Unit 3, The Framingham Heart Study

# Video 3

# Read in the dataset
framingham = read.csv("framingham.csv")

# Look at structure
str(framingham)

# Load the library caTools
library(caTools)

# Randomly split the data into training and testing sets
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)

# % dades que volem al Training Set. 65%. Quan tens moltes dades, et
pots permetre posar menys dades al Training set i més al Test set. Això en permetrà
més confiança en estendre les dades en un nou set, tot i mantenint un nº gran al 
nostre model. Típticament entre 50-80% al Training set.



# Split up the data using subset
train = subset(framingham, split==TRUE)
test = subset(framingham, split==FALSE)

# Logistic Regression Model. El punt vol dir que es faran servir totes les vbles com VI.
Ves amb compte que no posis id dins (en data sets que tinguin id or name)

framinghamLog = glm(TenYearCHD ~ ., data = train, family=binomial)
summary(framinghamLog)


El model em mostra com home, edat, colestorol i alguna altra són singificatius. 
Cigarretes quasi ho son. Ara, farem servir aquest model per a fer prediccions.

# Predictions on the test set
predictTest = predict(framinghamLog, type="response", newdata=test)

Argument predic: nom model, type="rspone" ens dóna les propbabilitat, i després posem
el nom del testing set.

# Confusion matrix with threshold of 0.5
11

FALSE TRUE
0  1069    6
1   187   11

#Ens surt que molt rarament prediem CHD (11 cops) als deu anys, per sobre 50%
sens <- 11/(11+187) #0.0555
# Accuracy
(1069+11)/(1069+6+187+11)
84.8%
#Si ho comparem amb el baseline model.

# Baseline accuracy
(1069+6)/(1069+6+187+11) 
84.4%
El nostre model no millora gaire el baselin. Però podriem canviar el threshold.

 

# Test set AUC 
library(ROCR)
ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)

[1] 0.7421095 -> això vol dir que model pot predir bastant bé entre alt risc i baix risc.

En resum:
        1. Model no prediu 10 anys risc per sobre 50%. 
                L'accuracy és imilar a la baseline
2. El model pot diferenciar baix-risc d'alt-risc (AUC, 0.74)
3. Algunes variables significatives suggereixen possibles intervencions poden prevenir
CHD. 
