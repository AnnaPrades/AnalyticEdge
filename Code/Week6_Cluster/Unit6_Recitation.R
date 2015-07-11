# Unit 6 - Recitation
#Gray scale -> matrix of pixel intensity ranging from 0 (black) to 1 (white)
#number rows -> height of the image
#number of columns -> width of the image
#E.g. 7 * 7 pixels -> 49 elements or intensity values
#In fact, and image should be treated as a vector of 49 elements, in order to cluster them by intensity
#So we need to morph our data -> using as.vector funcion
#Once we have the vector, we can feed it into the clustering algorithm and assign each element in the vector to a cluster

#Hierarchical clustering
  #1. Calculate the distance matrix -> computs the pairwise distances among the elements of the intensity vector
  #For each element in the intensity vector -> caluclate its distance from the other 48 elements
  #48 calculations per element
  #48 * 49 -> but due to symetry: (49*48)/2
  #So, how many distances we must caluclate: size of intensity vector -> (n*(n-1)/2) pairwise disntaces

# Video 2

flower = read.csv("flower.csv", header=FALSE) #we have to explicitly mention that ther is no header. If not, we would lose the first row.
str(flower) #treats the rows as observations and colums as variables

# Change the data type to matrix
flowerMatrix = as.matrix(flower) #we have 50 pixes in height and 50 in widht -> very small
str(flowerMatrix)

# Turn matrix into a vector that contains all the intensity values raging from 0 to 1
flowerVector = as.vector(flowerMatrix)
str(flowerVector) #we get a vector with 50*50 elements

#if we try as.vector without converting the data to a matrix -> it does not work
flowerVector2 = as.vector(flower)
str(flowerVector2)

#Hierarchical clustering
# 1. Compute distances -> computes pairwise distances between all the intensity values in the flower vector
distance = dist(flowerVector, method = "euclidean")



# Video 3

# 2. Hierarchical clustering
clusterIntensity = hclust(distance, method="ward")

# Plot the dendrogram
plot(clusterIntensity)

# Select 3 clusters
#We will plot 3 rectangles with the functions "rect.hclust"
rect.hclust(clusterIntensity, k = 3, border = "red")
#we wil split the data into these 3 clusters - cutree cuts the dendogram in however many clusters we want
flowerClusters = cutree(clusterIntensity, k = 3)
flowerClusters #it has 2500 elemnts with values 1, 2, 3 which correponds to each cluster

# Find mean intensity values
tapply(flowerVector, flowerClusters, mean)

# Plot the image and the clusters (using image function)
  #1st we need to convert the vector to a matrix
dim(flowerClusters) = c(50,50)
image(flowerClusters, axes = FALSE)

# Original image -> sequence from black (0) to white (1). 
#Lenght = 256 because this corresponds to the convention for grayscale

image(flowerMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))



# Video 4

# Let's try this with an MRI image of the brain

healthy = read.csv("healthy.csv", header=FALSE)
healthyMatrix = as.matrix(healthy)
str(healthyMatrix) #we have a 566 * 646 pixel resolution

# Plot image
image(healthyMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))

# Hierarchial clustering
  #1st convert the matrix to a vector
healthyVector = as.vector(healthyMatrix)
  #2nd calculate the distance
distance = dist(healthyVector, method = "euclidean")

# We have an error - why? -> not enoug memory
str(healthyVector) #it has 365636 elements 
# n * (n-1)/2
365636 * (365636-1)/2 #66844659430 (67 billion values to store in a matrix)
#we can't use hierachical clustering
#can we use k-means??


# Video 5

# Specify number of clusters
k = 5

# Run k-means
set.seed(1) #we need to set a seed since k-means clustering algorith stats by randomly assigning points to clusters
#funciont kmeans -> 1st argument what we try to cluster
#2nd argument: number of clusters or centers (= to k)
#3r maximim number of  itnerations
KMC = kmeans(healthyVector, centers = k, iter.max = 1000)
str(KMC)
    #1st piece of information is the cluster (from 1 to 5) -> assigns each intensity value to a cluster

# Extract clusters -> crate a new object based on the cluster variable
healthyClusters = KMC$cluster
#How do we obteain the mean intensity value within each of our 5 clusters?
  #-> we do not need to compute them (with t-apply) -> we have the vector centers
  #e.g mean intesity for the first cluster is KMC$centers has this information
KMC$centers[2] #of the second cluster
#KMC$size -> has information of the number of elements in each cluster
#we can see that the largest cluster i sht 3rd one (133.000 values), but it has 
#the smallest mean intensity value(black) -> our image is pretty blak

# Plot the image with the clusters
dim(healthyClusters) = c(nrow(healthyMatrix), ncol(healthyMatrix))

image(healthyClusters, axes = FALSE, col=rainbow(k))
#Can we use the clusters found by or k-means on the ehalgy MRI image to identify tumors?

#SCREE PLOTS

#While dendrograms can be used to select the final number of clusters for Hierarchical
#Clustering, we can't use dendrograms for k-means clustering. 
#However, there are several other ways that the number of clusters can be selected. 
#One common way to select the number of clusters is by using a scree plot, 
#which works for any clustering algorithm.

#A standard scree plot has the number of clusters on the x-axis, and the sum of the 
#within-cluster sum of squares on the y-axis. The within-cluster sum of squares 
#for a cluster is the sum, across all points in the cluster, of the squared distance 
#between each point and the centroid of the cluster.  We ideally want very small 
#within-cluster sum of squares, since this means that the points are all very close
#to their centroid. 

#To create the scree plot, the clustering algorithm is run with a range of values 
#for the number of clusters. For each number of clusters, the within-cluster 
#sum of squares can easily be extracted when using k-means clustering. 
#For example, suppose that we want to cluster the MRI image from this video into 
#two clusters. We can first run the k-means algorithm with two clusters:

KMC2 = kmeans(healthyVector, centers = 2, iter.max = 1000)

#Then, the within-cluster sum of squares is just an element of KMC2:
        
        KMC2$withinss

#This gives a vector of the within-cluster sum of squares for each cluster 
#(in this case, there should be two numbers). 

#Now suppose we want to determine the best number of clusters for this dataset.
#We would first repeat the kmeans function call above with centers = 3, centers = 4, etc. to create KMC3, KMC4, and so on.
KMC3 = kmeans(healthyVector, centers = 3, iter.max = 1000)
KMC4 = kmeans(healthyVector, centers = 4, iter.max = 1000)
KMC5 = kmeans(healthyVector, centers = 5, iter.max = 1000)
KMC6 = kmeans(healthyVector, centers = 6, iter.max = 1000)
KMC7 = kmeans(healthyVector, centers = 7, iter.max = 1000)
KMC8 = kmeans(healthyVector, centers = 8, iter.max = 1000)
KMC9 = kmeans(healthyVector, centers = 9, iter.max = 1000)
KMC10 = kmeans(healthyVector, centers = 10, iter.max = 1000)

#Then, we could generate the following plot:
        
        NumClusters = seq(2,10,1) #seq -> from 2 to 10, by 1

SumWithinss = c(sum(KMC2$withinss), sum(KMC3$withinss), sum(KMC4$withinss), sum(KMC5$withinss), sum(KMC6$withinss), sum(KMC7$withinss), sum(KMC8$withinss), sum(KMC9$withinss), sum(KMC10$withinss))

plot(NumClusters, SumWithinss, type="b")
 #PASTE IMAGE OF THE PLOT HERE
#To determine the best number of clusters using this plot, we want to look for a bend,
#or elbow, in the plot. This means that we want to find the number of clusters 
#for which increasing the number of clusters further does not significantly help 
#to reduce the within-cluster sum of squares. For this particular dataset, 
#it looks like 4 or 5 clusters is a good choice. Beyond 5, increasing the number
#of clusters does not really reduce the within-cluster sum of squares too much.

#You may have noticed it took a lot of typing to generate SumWithinss; 
#this is because we limited ourselves to R functions we've learned so far in the
#course. In fact, R has powerful functions for repeating tasks with a different input
#(in this case running kmeans with different cluster sizes). For instance, 
#we could generate SumWithinss with:
SumWithinss = sapply(2:10, function(x) sum(kmeans(healthyVector, centers=x, iter.max=1000)$withinss))

#MORE information of sapply: http://www.r-bloggers.com/using-apply-sapply-lapply-in-r/ 

# Video 6

# Apply to a test image
 
tumor = read.csv("tumor.csv", header=FALSE)
tumorMatrix = as.matrix(tumor)
tumorVector = as.vector(tumorMatrix)

#We will not run the k-means algorith againg -> we will apply the results on the tumor vector
# in other words, we treat hte healgy vector as the trianing set and the tumor vector as a testing set
#to do this we need to install a new packacke called flexclust
# Apply clusters from before to new image, using the flexclust package
install.packages("flexclust")
library(flexclust)

#flexclust has the KCCA, which stands for K-Centroids cluster analysis
#we need to convert the information from the clustering algorith to an object of the class LCCA
        #we as callin our new variable KMC.Kcca 
        # function kcca -> 1st argument original variable that stored information of the k-means
        # 2nd input -> the data we clusterd
KMC.kcca = as.kcca(KMC, healthyVector)
# now we can cluster the pixels in the tumorVector using the predict function
        tumorClusters = predict(KMC.kcca, newdata = tumorVector)
        #the tumorCluster is a vectors that assignas a value 1 through 5 to each 
        #of the intensity values in the tumorVector as predicted by the k-means algorithm

# Visualize the clusters ->1st convert into a matrix
dim(tumorClusters) = c(nrow(tumorMatrix), ncol(tumorMatrix))

image(tumorClusters, axes = FALSE, col=rainbow(k))

