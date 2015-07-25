# Unit 7 - Recitation


# VIDEO 3 - Bar Charts

# Load ggplot library
library(ggplot2)

# Load our data, which lives in intl.csv
intl = read.csv("intl.csv")
str(intl)
        #1st vble is the region
        #2nd is the % of students that came from this region

# We want to make a bar plot with region on the X axis
# and Percentage on the y-axis.
ggplot(intl, aes(x=Region, y=PercentOfIntl)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=PercentOfIntl))
        #stat="identity" says: use the value of the y vble (you could specify to use the nª of counts)
        #geom_text: because we whant hte value

# Make Region an ordered factor (before is was in aphabetical order)
# We can do this with the re-order command and transform command. 
intl = transform(intl, Region = reorder(Region, -PercentOfIntl))

# Look at the structure
str(intl)

# Make the percentages out of 100 instead of fractions
intl$PercentOfIntl = intl$PercentOfIntl * 100

# Make the plot
ggplot(intl, aes(x=Region, y=PercentOfIntl)) +
        geom_bar(stat="identity", fill="dark blue") +
        geom_text(aes(label=PercentOfIntl), vjust=-0.4) +
        ylab("Percent of International Students") +
        theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
        
        # vjust=-0.4 moves the lables up a little bit and off the top of the bars
        #a positive value will move it down, and a negative one up
        #hjust = 1, l'ha mogut horitzontalment una mica


# VIDEO 5 - World map

# Load the ggmap package
library(ggmap)

# Load in the international student data
intlall = read.csv("intlall.csv",stringsAsFactors=FALSE)

# Lets look at the first few rows
head(intlall)

# Those NAs are really 0s, and we can replace them easily
intlall[is.na(intlall)] = 0

# Now lets look again
head(intlall) 

# Load the world map
world_map = map_data("world")
str(world_map)
        #lat, long, group (number for each country), region = country name

# Lets merge intlall into world_map using the merge command
world_map = merge(world_map, intlall, by.x ="region", by.y = "Citizenship")
        #by.x -> name of vlbe in world_map, by.y=name of the vlbe in intall
        #we could also add all.x=TRUE, or all.y=TRUE
str(world_map)

# Plot the map
ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", color="black") +
  coord_map("mercator")

# Reorder the data -> order the rows first by group (=country), and then the order variable
world_map = world_map[order(world_map$group, world_map$order),]

# Redo the plot
ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", color="black") +
  coord_map("mercator")
#There are countries missing!! Because they are named different in the 2 database -> merging did not work
# Lets look for China
table(intlall$Citizenship) 

# Lets "fix" that in the intlall dataset
intlall$Citizenship[intlall$Citizenship=="China (People's Republic Of)"] = "China"

# We'll repeat our merge and order from before
world_map = merge(map_data("world"), intlall, 
                  by.x ="region",
                  by.y = "Citizenship")
world_map = world_map[order(world_map$group, world_map$order),]

ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=Total), color="black") +
  coord_map("mercator")


# We can try other projections - this one is visually interesting
ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=Total), color="black") +
  coord_map("ortho", orientation=c(20, 30, 0))
        #orientation=c(20, 30, 0) is where do you want the center: lat, long, and 30 degrees (center above Norht Africa)

ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=Total), color="black") +
  coord_map("ortho", orientation=c(-37, 175, 0)) #New Zeeland



# VIDEO 7 - Line Charts

# First, lets make sure we have ggplot2 loaded
library(ggplot2)

# Now lets load our dataframe
households = read.csv("households.csv")
str(households)
        #The data.frame has a year column and 6 other columns for each different household types
        #this is a problem, what do we put onb the x-axis?
        #ggplot needs year, group and fraction
#Melt will take a 2 dimenasional data frame and convert it to what we need for ggplot
# Load reshape2
library(reshape2)

# Lets look at the first two columns of our households dataframe
households[,1:2]

# First few rows of our melted households dataframe
head(melt(households, id="Year")) 

households[,1:3]

melt(households, id="Year")[1:10,3] #not useful
melt(households, id="Year")[1:10,] #yes! fa un trasponer

# Plot it
ggplot(melt(households, id="Year"),       
       aes(x=Year, y=value, color=variable)) +
  geom_line(size=2) + geom_point(size=5) +  
  ylab("Percentage of Households")
        #color=variable -> color will depend on the group which is called variable in the melt dataframe