# Unit 5 - Recitation


# Video 2

# Load the dataset
#When reading data set for text analytics specify stringsAsFactors = FALSE
emails = read.csv("energy_bids.csv", stringsAsFactors=FALSE)

str(emails)

# Look at emails

emails$email[1]
strwrap(emails$email[1]) #to make easier the reading. It chops the sentences down.
emails$responsive[1]
 
emails$email[2]
strwrap(emails$email[2])
emails$responsive[2]

# Responsive emails

table(emails$responsive)



# Video 3


# Load tm package

library(tm)


# Create corpus

corpus = Corpus(VectorSource(emails$email))

corpus[[1]]
strwrap(corpus[[1]])


# Pre-process data
corpus = tm_map(corpus, tolower)
strwrap(corpus[[1]])

# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after this video was recorded.
corpus = tm_map(corpus, PlainTextDocument)
strwrap(corpus[[1]])

corpus = tm_map(corpus, removePunctuation)
strwrap(corpus[[1]])

corpus = tm_map(corpus, removeWords, stopwords("english"))
strwrap(corpus[[1]])

corpus = tm_map(corpus, stemDocument)
strwrap(corpus[[1]])
# Look at first email
corpus[[1]]



# Video 4

# Create matrix

dtm = DocumentTermMatrix(corpus)
dtm #we see there are 27024 -> too many variables!

# Remove sparse terms
dtm = removeSparseTerms(dtm, 0.97) #any term that it si not at least at 3% of the doc
dtm #now we have 784 -> much better

# Create data frame
labeledTerms = as.data.frame(as.matrix(dtm))

# Add in the outcome variable
labeledTerms$responsive = emails$responsive

str(labeledTerms)



# Video 5


# Split the data

library(caTools)

set.seed(144)

spl = sample.split(labeledTerms$responsive, 0.7)

train = subset(labeledTerms, spl == TRUE)
test = subset(labeledTerms, spl == FALSE)

# Build a CART model

library(rpart)
library(rpart.plot)

emailCART = rpart(responsive~., data=train, method="class")

prp(emailCART) #if california appears at least 2 times, we are going to take the right part



# Video 6

# Make predictions on the test set

pred = predict(emailCART, newdata=test)
pred[1:10,] #left -> predicte probability of the doc being non responsive
#right -> predicted probabiity of the document being responsive
# left + right = 1
# we are interested in the probability of being responsive (right column)
pred.prob = pred[,2] #pred contains our test set probability

# Compute accuracy

table(test$responsive, pred.prob >= 0.5)

(195+25)/(195+25+17+20) # 0.8560311
# we have 20 false positives -> more cost of reviewing
# we have 17 false negatives -> we will miss the document
# -> therefore, we will assign a higher cost to false negatives (favor a model with hight sensitivity)

# Baseline model accuracy

table(test$responsive)
prop.table(table(test$responsive)) #0.8365759 -> so our model does a little better
215/(215+42)



# Video 7

# ROC curve

library(ROCR)

predROCR = prediction(pred.prob, test$responsive)

perfROCR = performance(predROCR, "tpr", "fpr")

plot(perfROCR, colorize=TRUE)
#we want a model with high sentitivity to avoid false positive rate
#

# Compute AUC

performance(predROCR, "auc")@y.values #0.7936323

#Nearly all cases today use tgus eDuscivert aooriacg,

