# Unit 5 - Twitter


# VIDEO 5

# Read in the data

tweets = read.csv("tweets.csv", stringsAsFactors=FALSE)

str(tweets)


# Create dependent variable

tweets$Negative = as.factor(tweets$Avg <= -1)

table(tweets$Negative)


# Install new packages

install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)


#corupus = collection of documents
# Create corpus -> we need to convert our tweets to a corpus for preprocessing
 
corpus = Corpus(VectorSource(tweets$Tweet)) 

# Look at corpus 
corpus #now the observations are documents

corpus[[1]] #I can't see it!!


# Convert to lower-case
#tm_map function has 2 arguments: 1st, name of the corpus, 2nd what we want to do

corpus = tm_map(corpus, tolower)

corpus[[1]] #Now I see the tweet!

# IMPORTANT NOTE: If you are using the latest version of the tm package, you will need to run the following line before continuing (it converts corpus to a Plain Text Document). This is a recent change having to do with the tolower function that occurred after this video was recorded.

corpus = tm_map(corpus, PlainTextDocument)


# Remove punctuation
#We also use tm_map function, but now, 2nd arg is removePunctuation

corpus = tm_map(corpus, removePunctuation)

corpus[[1]]

# Look at stop words 
stopwords("english")[1:10]

# Remove stopwords and apple
# we need a 3rd argument to say which words we want to remove

corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))

corpus[[1]]

# Stem document 

corpus = tm_map(corpus, stemDocument)

corpus[[1]]




# Video 6

# Create matrix
#DocumentTermMatrix generates a matrix:
# rows = documents (e.g. tweets)
# columns = words in those tweeds
#values = nº of times a word apperars in each document

frequencies = DocumentTermMatrix(corpus)

frequencies

# Look at matrix 

inspect(frequencies[1000:1005,505:515]) #there are lots of zeros

# Check for sparsity

findFreqTerms(frequencies, lowfreq=20)
#Why should we care?
# more termes -> more VI -> more time to build model
#ratio vi/observations will affect the inferrence (how good the model will generalize)


# Remove sparse terms

sparse = removeSparseTerms(frequencies, 0.995)
sparse

# Convert to a data frame

tweetsSparse = as.data.frame(as.matrix(sparse))

# Make all variable names R-friendly
# Make all variable names R-friendly -> i.e. they do not begint with a number
#you should do this ALWAYS YOU BUILD  a dataframe using tets analytics
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

# Add dependent variable

tweetsSparse$Negative = tweets$Negative

# Split the data

library(caTools)

set.seed(123)

split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)

trainSparse = subset(tweetsSparse, split==TRUE)
testSparse = subset(tweetsSparse, split==FALSE)



# Video 7

# Build a CART model

library(rpart)
library(rpart.plot)

tweetCART = rpart(Negative ~ ., data=trainSparse, method="class")

prp(tweetCART)
#if there is freak (>0.5) -> then true (negative sentiment)
#if no freek but hate -> negative sentiment true
#if not hate but wtf -> negative sentiemnt

# Evaluate the performance of the model
predictCART = predict(tweetCART, newdata=testSparse, type="class") #1st argument actual outcome, 2nd our predictions

table(testSparse$Negative, predictCART)

# Compute accuracy

(294+18)/(294+6+37+18) #0.8788732 I get different values
# (291 + 19)/ (291 + 9 + 36 + 19) 0.8732394

# Baseline accuracy 

table(testSparse$Negative)

300/(300+55) #$the same


# Random forest model

library(randomForest)
set.seed(123)

tweetRF = randomForest(Negative ~ ., data=trainSparse)
#???we have so many vi that it takes time

# Make predictions:
predictRF = predict(tweetRF, newdata=testSparse)

table(testSparse$Negative, predictRF) #0.88 It is a little better, but less interpretable

# Accuracy:
(293+21)/(293+7+34+21)
#My values
(284 + 27)/ (284 + 16 + 28 + 27) #0.8760563

#w could use cross-validation to pick the cp for the CART model, and the accuracy
# would be the same than the randmo forest

#So using a bag-of-words approach we can reasonably predict sentiment even with a relatively
#small data set of tweets

#Quick question: Build a logistic model
#Build a logistic regression model (using the training set) to predict "Negative"
#using all of the independent variables. You may get a warning message after 
#building your model - don't worry (we explain what it means in the explanation).
lr.mod <- glm(Negative ~ ., data = trainSparse, family = binomial)
summary(lr.mod)

#Now, make predictions using the logistic regression model:

predictions = predict(lr.mod, newdata=testSparse, type="response")


#You might also get a warning message after this command, but don't worry -
#it is due to the same problem as the previous warning message.

#Build a confusion matrix (with a threshold of 0.5) and compute the accuracy of the model.
#What is the accuracy? 

t1 <- table(testSparse$Negative, predictions > 0.5)
t1
accuracy <- sum(diag(t1))/margin.table(t1)
accuracy #0.8253521

#The accuracy is (254+37)/(254+46+18+37) = 0.8197183, which is worse than the baseline. 
#If you were to compute the accuracy on the training set instead, you would see 
#that the model does really well on the training set - this is an example of over-fitting. 
#The model fits the training set really well, but does not perform well on the test set.
#A logistic regression model with a large number of variables is particularly at risk for overfitting.

#Note that you might have gotten a different answer than us, because the glm function struggles with this many variables. The warning messages that you might have seen in this problem have to do with the number of variables, and the fact that the model is overfitting to the training set. We'll discuss this in more detail in the Homework Assignment.
