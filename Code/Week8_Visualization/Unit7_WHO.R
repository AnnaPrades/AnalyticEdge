# Unit 7 - Lecture 1


# VIDEO 4 - A BASIC SCATTERPLOT

# Read in data

WHO = read.csv("WHO.csv")

str(WHO)


# Plot from Week 1

plot(WHO$GNI, WHO$FertilityRate)

# Let's redo this using ggplot 

# Install and load the ggplot2 library:
install.packages("ggplot2")
library(ggplot2)

# Create the ggplot object with the data and the aesthetic mapping:
#1st argument is the data
#2nd argument the aesthetic
# the 3rd shoud be the geometric point
scatterplot = ggplot(WHO, aes(x = GNI, y = FertilityRate))

# Add the geom_point geometry (bars, lines, points..)
# You can build any graph, using the same object
scatterplot + geom_point() #Improvement: we do not have $, we have a nice grind

# Make a line graph instead:
scatterplot + geom_line() # a lines does not make sense

# Switch back to our points:
scatterplot + geom_point()

# Redo the plot with blue triangles instead of circles:
scatterplot + geom_point(color = "blue", size = 3, shape = 17) 
#shape 17 = triangels
#shape 8 = stars

# Another option:
scatterplot + geom_point(color = "darkred", size = 3, shape = 15) 

# Add a title to the plot:
scatterplot + geom_point(colour = "blue", size = 3, shape = 17) + ggtitle("Fertility Rate vs. Gross National Income")

# Save our plot:
fertilityGNIplot = scatterplot + geom_point(colour = "blue", size = 3, shape = 17) + ggtitle("Fertility Rate vs. Gross National Income")

#1st we create the file pdf, then we print, then we close
pdf("MyPlot.pdf")

print(fertilityGNIplot)

dev.off()



# VIDEO 5 - MORE ADVANCED SCATTERPLOTS 


# Color the points by region: We add a thir element aescetic: the color -> seven regions, 7 colors
ggplot(WHO, aes(x = GNI, y = FertilityRate, color = Region)) + geom_point()

# Color the points according to life expectancy: -> numerical variable -> gradient of colors
ggplot(WHO, aes(x = GNI, y = FertilityRate, color = LifeExpectancy)) + geom_point()


# Is the fertility rate of a country was a good predictor of the percentage of the population under 15?
ggplot(WHO, aes(x = FertilityRate, y = Under15)) + geom_point() #it does not seem linear

# Let's try a log transformation:
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point()

# Simple linear regression model to predict the percentage of the population under 15, using the log of the fertility rate:
mod = lm(Under15 ~ log(FertilityRate), data = WHO)
summary(mod)

# Add this regression line to our plot:
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm") #per defecte, level=0.95

# 99% confidence interval
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", level = 0.99)

# No confidence interval in the plot
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", se = FALSE)

# Change the color of the regression line:
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", colour = "orange")

#Quick question. Create a Fertility Rate plot again
#Now colour by region
ggplot(WHO, aes(x = FertilityRate, y = Under15)) + geom_point()
ggplot(WHO, aes(x = FertilityRate, y = Under15, color = Region)) + geom_point() +scale_color_brewer(palette="Dark2")

