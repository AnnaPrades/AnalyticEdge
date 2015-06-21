# Unit 3, Recitation
#1 ir Republicans won, 0 if Democrats won
#Rasmussen y SurveyUsa: PerRep - PerDemo: -6% 6pp més Demòcrates q Republicans
#Diffcounts: conta totes les enq: n1 eq que prediuen Rep èxit - enq que prediuen Demo
#PropR: proporció enq que prediuen que guanyen republicans

# Video 2

# Read in data
polling = read.csv("PollingData.csv")
str(polling) #veiem que 145 obs, i n'hi hauria d'haver 150
table(polling$Year) # A la table veiem que any 2012 en 5 estats no es van fer enq pq fàcils de predir.
summary(polling) #veiem que pr Rasmussen i SurveyUSA hi ha missing data (NA)

# How to face missing values
Hi ha diferents maneres de veure-ho. Pero no podem eliinar-los pq seria més meitat
observacions. I, a més, volem fer prediccions de tots els estats.
a) carregar-nos casos -> perdíem observacions
b) carregar-nos vbles -> no, pq esperem que Rasmussen i SurveyUSa qualitativament dif que PropR
c) omplir-los amb mitjanes -> valors propers a 0, indicarien un empat. Pero un valor 0 de PropR té significat 0
#Multiple imputation: si valor molt negativu SurveyUSA -> tb molt negatiu en Rasmussen
#Per omplir-ho, valors seran diferents en funció de la seed que prenguem
#Multiple imputation by Chained Equations or Mice package

# Install and load mice package
install.packages("mice")
library(mice)

# Multiple imputation -> volem imputar valors, però sense considerar Outcome vble
simple = polling[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount")] # La creo pq no em vaci servir outcome vble
summary(simple)
set.seed(144) # És només pq a tothom li surti igual i no importa
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
table(Train$Republican) # En 53 de les obsrvacions guanye repbulicans, vs 47 de demòcrates
# Per tant, el baseline model sempre prediurà que guanyen els republicans. No bo.

#La funció sign ens torna 1 si valor positiu, 0 si 0 i -1 si valor negatiu
sign(20)
sign(-10)
sign(0)
table(sign(Train$Rasmussen)) #Transforma els negatius en -1 (guanyen Demo), empats en 0 i 1 republicans
table(Train$Republican, sign(Train$Rasmussen)) #Ara ho contrastem amb el resultat
#El model que surt és molt millor que l'inicial que sempre guanyen republicans
  -1  0  1
0 42  2  3
1  0  1 52

# Video 4

# Multicollinearity -> és probable que n'hi hagi pq totes mesuren el mateix
cor(Train) #no funciona pq estats és un factor i no numèric
str(Train) #aquí és on veig que és un factor
cor(Train[c("Rasmussen", "SurveyUSA", "PropR", "DiffCount", "Republican")])
#rasynyseb i syrvetUSA tenen una correlació molt alta: cobminar-les per model regressió no molt bo
Rasmussen SurveyUSA     PropR DiffCount Republican
Rasmussen  1.0000000 0.9194508 0.8404803 0.5124098  0.8021191
SurveyUSA  0.9194508 1.0000000 0.8756581 0.5541816  0.8205806
PropR      0.8404803 0.8756581 1.0000000 0.8273785  0.9484204
DiffCount  0.5124098 0.5541816 0.8273785 1.0000000  0.8092777
Republican 0.8021191 0.8205806 0.9484204 0.8092777  1.0000000
#Si volem construir un model amb 1 sola VI -> la més correl: PropR

# Logistic Regression Model
mod1 = glm(Republican~PropR, data=Train, family="binomial")
summary(mod1)
#model surt molt bé amb un AIC baix. Anem a veure com va predint

# Training set predictions
pred1 = predict(mod1, type="response")
table(Train$Republican, pred1 >= 0.5)
#threshold és si més gran que 0.5 guanyen republicans -> en total fa 4 errors
    FALSE TRUE
0    45    2
1     2   51



# Two-variable model -> la 2a vble miro que be amb response, però poc correl amb DiffCount
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
pred2 = predict(mod2, type="response")
table(Train$Republican, pred2 >= 0.5) #només fem 3 errors
summary(mod2) #AIC és un valor més baix, pero cap de les vbles és significant
#tots dos models tenen fortales i dbilitats -> farem servir el 2n per fer prediccions



# Video 5

# Smart baseline accuracy
table(Test$Republican, sign(Test$Rasmussen)) #4 mistakes and 2 inconclusive results
#la baseline serveix per comprovar model logistic regression

# Test set predictions
TestPrediction = predict(mod2, newdata=Test, type="response")
table(Test$Republican, TestPrediction >= 0.5) #només una mistake
#podríem canviar 0.5, mirar ROC, pero no té gaire sentit amb aquestes dades. cutoof is ok 0.5

# Analyze mistake
subset(Test, TestPrediction >= 0.5 & Republican == 0)

#resum: model no fa màgia, pero overall millor que baseline prediction.