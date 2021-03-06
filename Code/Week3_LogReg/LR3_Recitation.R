# Unit 3, Recitation
#1 ir Republicans won, 0 if Democrats won
#Rasmussen y SurveyUsa: PerRep - PerDemo: -6% 6pp m�s Dem�crates q Republicans
#Diffcounts: conta totes les enq: n1 eq que prediuen Rep �xit - enq que prediuen Demo
#PropR: proporci� enq que prediuen que guanyen republicans

# Video 2

# Read in data
polling = read.csv("PollingData.csv")
str(polling) #veiem que 145 obs, i n'hi hauria d'haver 150
table(polling$Year) # A la table veiem que any 2012 en 5 estats no es van fer enq pq f�cils de predir.
summary(polling) #veiem que pr Rasmussen i SurveyUSA hi ha missing data (NA)

# How to face missing values
Hi ha diferents maneres de veure-ho. Pero no podem eliinar-los pq seria m�s meitat
observacions. I, a m�s, volem fer prediccions de tots els estats.
a) carregar-nos casos -> perd�em observacions
b) carregar-nos vbles -> no, pq esperem que Rasmussen i SurveyUSa qualitativament dif que PropR
c) omplir-los amb mitjanes -> valors propers a 0, indicarien un empat. Pero un valor 0 de PropR t� significat 0
#Multiple imputation: si valor molt negativu SurveyUSA -> tb molt negatiu en Rasmussen
#Per omplir-ho, valors seran diferents en funci� de la seed que prenguem
#Multiple imputation by Chained Equations or Mice package

# Install and load mice package
install.packages("mice")
library(mice)

# Multiple imputation -> volem imputar valors, per� sense considerar Outcome vble
simple = polling[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")] # La creo pq no em vaci servir outcome vble
summary(simple)
set.seed(144) # �s nom�s pq a tothom li surti igual i no importa
imputed = complete(mice(simple)) #output mostra 5 iterations i ara tot compelt
summary(imputed)
#Ara volem copiar valors a la original, a fi de tenir els mateixos valors. No m'agrada! En craria una de depurada
polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed$SurveyUSA
summary(polling)



# Video 3

# Subset data into training set and test set. Aquet cop split el fem per anys. No ho fem amb split.
Train = subset(polling, Year == 2004 | Year == 2008)
Test = subset(polling, Year == 2012)

# Smart Baseline -> ara vull entendre la baseline
table(Train$Republican) # En 53 de les obsrvacions guanye repbulicans, vs 47 de dem�crates
# Per tant, el baseline model sempre prediur� que guanyen els republicans. No bo.

#La funci� sign ens torna 1 si valor positiu, 0 si 0 i -1 si valor negatiu
sign(20)
sign(-10)
sign(0)
table(sign(Train$Rasmussen)) #Transforma els negatius en -1 (guanyen Demo), empats en 0 i 1 republicans
table(Train$Republican, sign(Train$Rasmussen)) #Ara ho contrastem amb el resultat
#El model que surt �s molt millor que l'inicial que sempre guanyen republicans
  -1  0  1
0 42  2  3
1  0  1 52

# Video 4

# Multicollinearity -> �s probable que n'hi hagi pq totes mesuren el mateix
cor(Train) #no funciona pq estats �s un factor i no num�ric
str(Train) #aqu� �s on veig que �s un factor
cor(Train[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount", "Republican")])
#rasynyseb i syrvetUSA tenen una correlaci� molt alta: cobminar-les per model regressi� no molt bo
Rasmussen SurveyUSA     PropR DiffCount Republican
Rasmussen  1.0000000 0.9194508 0.8404803 0.5124098  0.8021191
SurveyUSA  0.9194508 1.0000000 0.8756581 0.5541816  0.8205806
PropR      0.8404803 0.8756581 1.0000000 0.8273785  0.9484204
DiffCount  0.5124098 0.5541816 0.8273785 1.0000000  0.8092777
Republican 0.8021191 0.8205806 0.9484204 0.8092777  1.0000000
#Si volem construir un model amb 1 sola VI -> la m�s correl: PropR

# Logistic Regression Model
mod1 = glm(Republican~PropR, data=Train, family="binomial")
summary(mod1)
#model surt molt b� amb un AIC baix. Anem a veure com va predint

# Training set predictions
pred1 = predict(mod1, type="response")
table(Train$Republican, pred1 >= 0.5)
#threshold �s si m�s gran que 0.5 guanyen republicans -> en total fa 4 errors
    FALSE TRUE
0    45    2
1     2   51



# Two-variable model -> la 2a vble miro que be amb response, per� poc correl amb DiffCount
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
pred2 = predict(mod2, type="response")
table(Train$Republican, pred2 >= 0.5) #nom�s fem 3 errors
summary(mod2) #AIC �s un valor m�s baix, pero cap de les vbles �s significant
#tots dos models tenen fortales i dbilitats -> farem servir el 2n per fer prediccions



# Video 5

# Smart baseline accuracy
table(Test$Republican, sign(Test$Rasmussen)) #4 mistakes and 2 inconclusive results
#la baseline serveix per comprovar model logistic regression

# Test set predictions
TestPrediction = predict(mod2, newdata=Test, type="response")
table(Test$Republican, TestPrediction >= 0.5) #nom�s una mistake
#podr�em canviar 0.5, mirar ROC, pero no t� gaire sentit amb aquestes dades. cutoof is ok 0.5

# Analyze mistake
subset(Test, TestPrediction >= 0.5 & Republican == 0)

#resum: model no fa m�gia, pero overall millor que baseline prediction.