# VIDEO 2

# Read in data
baseball = read.csv("baseball.csv")
str(baseball)

# Subset to only include moneyball years
moneyball = subset(baseball, Year < 2002)
str(moneyball)

# Compute Run Difference
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)

# Scatterplot to check for linear relationship
plot(moneyball$RD, moneyball$W)

# Regression model to predict wins. Necessito com a mínim 95 wins per arribar a playoffs
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)


# VIDEO 3
#OBP: on base percentage, SLG: número llocs que et mous per potència hitter

str(moneyball)

# Regression model to predict runs scored. Coef Batting avarage is negative .> multicolineraity
RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg)

RunsReg = lm(RS ~ OBP + SLG, data=moneyball) #BA is overated, aquests 2 van millor que els atlres
summary(RunsReg)
#Predir RS d'un jugador que OBP=3.61, i SLG=0.500
-804.63 + (2737.77*0.361) + (1584.91*0.500)
[1] 976.16

#Regression to predict runs allowed
RunsAllowed <- lm(RA ~ OOBP + OSLG, data=moneyball)
summary(RunsAllowed)
#Si un euip de fora oBP= 0.297, i OSLG=0.370, quants rebré?
-837.38 + (2913.60*0.297) + (1514.29*0.370)
[1] 588.2465

#Team Rank
#In 2012 and 2013, there were 10 teams in the MLB playoffs: 
#the six teams that had the most wins in each baseball division,
#nd four "wild card" teams. The playoffs start between the four wild card teams 
#- the two teams that win proceed in the playoffs (8 teams remaining). 
#hen, these teams are paired off and play a series of games. The four teams that 
#win are then paired and play to determine who will play in the World Series. 

#Rank 1: the team that won the World Series
#Rank 2: the team that lost the World Series
#Rank 3: the two teams that lost to the teams in the World Series
#Rank 4: the four teams that made it past the wild card round, but lost to the above four teams
#Rank 5: the two teams that lost the wild card round

teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 <- c()