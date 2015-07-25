# Unit 7 - Lecture 2, Predictive Policing


# VIDEO 3 - A Basic Line Plot

# Load our data:
mvt = read.csv("mvt.csv", stringsAsFactors=FALSE)

str(mvt)

# Convert the Date variable to a format that R will recognize:
mvt$Date = strptime(mvt$Date, format="%m/%d/%y %H:%M")

# Extract the hour and the day of the week:
mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour

# Let's take a look at the structure of our data again:
str(mvt)

# Create a simple line plot - need the total number of crimes on each day of the week. We can get this information by creating a table:
table(mvt$Weekday)

# Save this table as a data frame:
WeekdayCounts = as.data.frame(table(mvt$Weekday))

str(WeekdayCounts) 


# Load the ggplot2 library:
library(ggplot2)

# Create our plot -> you need aes(group=1, because Var1 is a factor)
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))  

# Make the "Var1" variable an ORDERED factor variable
WeekdayCounts$Var1 = factor(WeekdayCounts$Var1, ordered=TRUE, levels=c("domingo", "lunes", "martes", "miércoles", "jueves", "viernes","sábado"))

# Try again:
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1))

# Change our x and y labels:
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1)) + xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts")

#Quick question: add linetype = 2 after geom_line and see what happens:
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1), linetype=2) + xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts")
#Now, change the alpha parameter to 0.3 by replacing "linetype=2" with "alpha=0.3" in the plot command. What does this do?
ggplot(WeekdayCounts, aes(x=Var1, y=Freq)) + geom_line(aes(group=1), alpha=0.3) + xlab("Day of the Week") + ylab("Total Motor Vehicle Thefts")

# VIDEO 4 - Adding the Hour of the Day
#We can add the hour creating a line for each day of the week and hour in x-axis

# 1st we need to Create a counts table for the weekday and hour:
table(mvt$Weekday, mvt$Hour)

# Save this to a data frame:
DayHourCounts = as.data.frame(table(mvt$Weekday, mvt$Hour))

str(DayHourCounts) #1st vble is the factors with weekdays, 2nd hour, 3rd frequency

# Convert the second variable, Var2, to numbers and call it Hour:
DayHourCounts$Hour = as.numeric(as.character(DayHourCounts$Var2)) #this is how we convert a factor variable into a numerical one

# Create out plot:
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1))

# Change the colors
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Var1), size=2)

# Separate the weekends from the weekdays:
DayHourCounts$Type = ifelse((DayHourCounts$Var1 == "Sunday") | (DayHourCounts$Var1 == "Saturday"), "Weekend", "Weekday")

# Redo our plot, this time coloring by Type:
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Type), size=2) 
  

# Make the lines a little transparent:
ggplot(DayHourCounts, aes(x=Hour, y=Freq)) + geom_line(aes(group=Var1, color=Type), size=2, alpha=0.5) 



# Fix the order of the days: NO FUNCIONA -> NOMS ENS CASTELLÀ??
DayHourCounts$Var1 = factor(DayHourCounts$Var1, ordered=TRUE, levels=c("lunes", "martes", "miércoles", "jueves", "viernes", "sábado", "domingo"))

# Make a heatmap:
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq))

# Change the label on the legend, and get rid of the y-label:
#to get rid of the name of VD (y): axis.title.y = element_blank()
#scale_fill function takes care of our legends
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name="Total MV Thefts") + theme(axis.title.y = element_blank())
  
# Change the color scheme
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill = Freq)) + scale_fill_gradient(name="Total MV Thefts", low="white", high="red") + theme(axis.title.y = element_blank())

ggplot(DayHourCounts, aes(x = Var1, y = Hour)) + geom_tile(aes(fill=Freq)) + scale_fill_gradient(name="Total MV Thefts", low="white", high="red") + theme(axis.title.y=element_blank())
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill=Freq)) + scale_fill_gradient(name="Total MV Thefts", low="white", high="black") + theme(axis.title.y=element_blank())

# VIDEO 5 - Maps

# Install and load two new packages:
install.packages("maps")
install.packages("ggmap")
library(maps)
library(ggmap)

# Load a map of Chicago into R:
chicago = get_map(location = "chicago", zoom = 11) #it gives me an error
barcelona = get_map(location = "barcelona", zoom = 11)

# Look at the map
ggmap(chicago)
ggmap(barcelona)

# Plot the first 100 motor vehicle thefts:
#we plot only the first 100 of observartions -> otherwise we would see a black hole
ggmap(chicago) + geom_point(data = mvt[1:100,], aes(x = Longitude, y = Latitude))

# Round our latitude and longitude to 2 digits of accuracy, and create a crime counts data frame for each area:
LatLonCounts = as.data.frame(table(round(mvt$Longitude,2), round(mvt$Latitude,2)))

str(LatLonCounts) #1638 observations and 3 vbe. 1st and 2nd lat, and third Freq of motor vehcicles crime

# Convert our Longitude and Latitude variable to numbers:
LatLonCounts$Long = as.numeric(as.character(LatLonCounts$Var1)) #this is how we convert a factor to a numerical variable
LatLonCounts$Lat = as.numeric(as.character(LatLonCounts$Var2))

# Plot these points on our map:
ggmap(chicago) + geom_point(data = LatLonCounts, aes(x = Long, y = Lat, color = Freq, size=Freq))

# Change the color scheme:
ggmap(chicago) + geom_point(data = LatLonCounts, aes(x = Long, y = Lat, color = Freq, size=Freq)) + scale_colour_gradient(low="yellow", high="red")

# We can also use the geom_tile geometry
ggmap(chicago) + geom_tile(data = LatLonCounts, aes(x = Long, y = Lat, alpha = Freq), fill="red")

ur heatmap was plotting squares out in the water, which seems a little strange. We can fix this by removing the observations from our data frame that have Freq = 0.

#Quick question: Take a subset of LatLonCounts, only keeping the observations for which Freq > 0, and call it LatLonCounts2.

#Redo the heatmap from the end of Video 5, using LatLonCounts2 instead of LatLonCounts. You should no longer see any squares out in the water, or in any areas where there were no motor vehicle thefts.

#How many observations did we remove?
LatLonCounts2 <- subset(LatLonCounts, LatLonCounts$Freq > 0)
str(LatLonCounts2) #1638-686
ggmap(chicago) + geom_tile(data = LatLonCounts2, aes(x = Long, y = Lat, alpha = Freq), fill="red")

# VIDEO 6 - Geographical Map on US

# Load our data:
murders = read.csv("murders.csv")

str(murders)

# Load the map of the US
library(maps)
library(ggmap)
statesMap = map_data("state")

str(statesMap)

# Plot the map:ç
library(ggplot2)
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black") 
#group = group is the way we say to draw the EUA, by groups of states
#geom_polygon(fill = "white", color = "black") -> fill states with white, and outline them with black

#Before we can plot our data on this map, we need to make sure that
#the state names are the same in the marduers data frame

#In the murders ddbb, states are in the State vble, and start with a capital letter
#In the statesMap datafrane -> region variable, and they are lowercase

# Create a new variable called region with the lowercase names to match the statesMap:
murders$region = tolower(murders$State)

# Join the statesMap data and the murders data into one dataframe:
#1st argument, 1st database, 2nd->2nd data base, 3rd->identifyier to merge
murderMap = merge(statesMap, murders, by="region")
str(murderMap)

# Plot the number of murder on our map of the United States:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Murders)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

#California and Texas are the states with more murders, it is because they have more population?
# Plot a map of the population:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = Population)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

# Create a new variable that is the number of murders per 100,000 population:
murderMap$MurderRate = murderMap$Murders / murderMap$Population * 100000

# Redo our plot with murder rate:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")
#The map is very brown because an small state (washington DC) is an outlier

# Redo the plot, removing any states with murder rates above 10:
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = MurderRate)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend", limits = c(0,10))

#Quick question: Redo the map from Video 6, but this time fill each state with the variable GunOwnership. This shows the percentage of people in each state who own a gun.
ggplot(murderMap, aes(x = long, y = lat, group = group, fill = GunOwnership)) + geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

#Which of the following states has the highest gun ownership rate? 

