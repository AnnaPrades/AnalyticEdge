# Unit 6 - Introduction to Clustering

# Video 6

# After following the steps in the video, load the data into R

movies <- read.table("movieLens.txt", header= FALSE, sep ="|", quote ="\"")

str(movies)

# Add column names
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")

str(movies)

# Remove unnecessary variables
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL

# Remove duplicates
movies = unique(movies)

#How many movies are classified as commedies? How many as Western?
colSums(movies[ ,2:20])
#How many are classified as Romance and Drama?
table(movies$Romance == 1 & movies$Drama == 1)

# Take a look at our data again:
str(movies)



# Video 7
#There are 2 steps
# 1. Compute distances -> we only want to cluster the genre variables
distances = dist(movies[2:20], method = "euclidean")

# 2. Hierarchical clustering -> hclust for hierarachical clustering
#the ward method cares about the distance between clusters using centroid distance, and also the variance in each of the clusters
clusterMovies = hclust(distances, method = "ward") 

# Plot the dendrogram
plot(clusterMovies)
#looking at the dendogram, it seems that with 3 or 4 it would be enough
# but keeping our application in moind .> we probably want more than 4 to make recommendations

# Assign points to clusters
#we can label each of the data points according to what cluster it belongs to using the cutree function
clusterGroups = cutree(clusterMovies, k = 10) #we selected 10 clusters -> k=10

#Now let's figure out what the clusters are like.

# Let's use the tapply function to compute the percentage of movies in each genre and cluster

tapply(movies$Action, clusterGroups, mean)# Action is binary
        # we see that in cluster 1 17% are action, but 0% in cluser 4

tapply(movies$Romance, clusterGroups, mean)

# We can repeat this for each genre. If you do, you get the results in ClusterMeans.ods


# Find which cluster Men in Black is in.

subset(movies, Title=="Men in Black (1997)") #is the 257 row
clusterGroups[257] #is in  cluster 2

# Create a new data set with just the movies from cluster 2
cluster2 = subset(movies, clusterGroups==2)

# Look at the first 10 titles in this cluster:
cluster2$Title[1:10]

#e explain how you can find the cluster centroids by using the function "tapply"
#for each variable in the dataset. While this approach works and is familiar to us,
#it can be a little tedious when there are a lot of variables. 
#An alternative approach is to use the colMeans function. With this approach, 
#you only have one command for each cluster instead of one command
#for each variable. If you run the following command in your R console,
#you can get all of the column (variable) means for cluster 1:

colMeans(subset(movies[2:20], clusterGroups == 1))

#You can repeat this for each cluster by changing the clusterGroups number.
#However, if you also have a lot of clusters, this approach is not that much more 
#efficient than just using the tapply function.

#A more advanced approach uses the "split" and "lapply" functions. 
#The following command will split the data into subsets based on the clusters:
        
        spl = split(movies[2:20], clusterGroups)

#Then you can use spl to access the different clusters, because

spl[[1]]

#is the same as

subset(movies[2:20], clusterGroups == 1)

#so colMeans(spl[[1]]) will output the centroid of cluster 1. 
#But an even easier approach uses the lapply function. 
#The following command will output the cluster centroids for all clusters:
        
        lapply(spl, colMeans)

#The lapply function runs the second argument (colMeans) on each element of 
        #the first argument (each cluster subset in spl). 
        #So instead of using 19 tapply commands, or 10 colMeans commands, 
        #we can output our centroids with just two commands: one to define spl, 
        #and then the lapply command.

#Note that if you have a variable called "split" in your current R session, 
        #you will need to remove it with rm(split) so that you can use the split function.
        
#Run the cutree function again to create the cluster groups, but this time 
        #pick k = 2 clusters. It turns out that the algorithm groups all of 
        #the movies that only belong to one specific genre in one cluster
        #(cluster 2), and puts all of the other movies in the other cluster 
        #(cluster 1). What is the genre that all of the movies in cluster 2 belong to?
        
        clusterGroups = cutree(clusterMovies, k = 2)
        colMeans(subset(movies[2:20], clusterGroups == 1))
        colMeans(subset(movies[2:20], clusterGroups == 2))
