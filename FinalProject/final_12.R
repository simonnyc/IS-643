
library(recommenderlab)
library(cluster)

setwd("C:/CUNY/Courses/IS-643/Final/data")


users = read.csv("BX-Users.csv",sep = ";")
books = read.csv("BX-Books.csv",sep = ";")
rating = read.csv("Bx-Book-Ratings.csv",sep = ";",header = T)

View(head(users, 10))
View(head(books, 10))
View(head(rating, 10))

#############
#min(nchar(levels(books$ISBN)))

################



#################


length(users)

View(head(rating, 10))
#install.packages("recommenderlab")
#install.packages("cluster")
library(recommenderlab)
library(cluster)
ratings <- read.csv("C:/CUNY/Courses/IS-643/Final/data/Bx-Book-Ratings.csv", sep=";",header = T)
View(head(ratings, 2))

bookratings = merge(books,ratings, by = "ISBN")
View(head(bookratings,2))
names(bookratings)
bookratings$Image.URL.S = NULL
bookratings$Image.URL.L = NULL
bookratings$Image.URL.M = NULL
#bookratings$ISBN = NULL
bookratings$Book.Author = NULL
bookratings$Year.Of.Publication = NULL
bookratings$Publisher = NULL

names(bookratings)



ratings = as.data.frame(bookratings$User.ID)
ratings$BookTitle = bookratings$Book.Title
ratings$Rating = bookratings$Book.Rating
str(ratings)
# Removing the duplicate rating combination of books and users


ratings = unique(ratings)

# As per the book crossing business rules ratings of the book are considered between 1 and 10
# So, we are removing the users who have given no rating for the books

ratings = subset(ratings,ratings$Rating > 0)


# Many of the functions in "recommenderlab" package take realRatingMatrix object
# realRatingMatrix is a 2-D matrix with users in rows, books in columns and cells are ratings

RatingMatrix = as(ratings,"realRatingMatrix")

RatingMatrix
RatingMatrix2 <- RatingMatrix

# As per the book crossing business rules any given user should have read and rated atleast 10 movies
# So remove the users who have watched and rated less than 10 movies
RatingMatrix = RatingMatrix[rowCounts(RatingMatrix)>10,colCounts(RatingMatrix)>10]

str(RatingMatrix)

RatingMatrix

# Recommender function in recommendationlab takes realRatingMatrix object

RatingMatrix = as(ratings,"realRatingMatrix")
str(RatingMatrix)

# Sampling of data to create training and testing data

indexes = sample(1:nrow(RatingMatrix), size=0.7*nrow(RatingMatrix))
training = RatingMatrix[indexes,]
testing = RatingMatrix[-indexes,]


# Histogram of users and their ratings
# Here histogram gives us the frequency of the given ratings

hist(getRatings(RatingMatrix), breaks = 100)


hist(getRatings(normalize(RatingMatrix)), breaks=100)
hist(rowCounts(RatingMatrix), breaks=100)
hist(colMeans(RatingMatrix), breaks=20)

users_similarity = similarity(RatingMatrix, method = "euclidean",which = "users")

as.matrix(users_similarity)

# Heirarchical clustering
ratings$BookTitle = as.numeric(ratings$BookTitle)
distances = dist(ratings[1:20,], method = "euclidean")
clusterbooks = hclust(distances, method = "ward.D")
plot(clusterbooks)

# K-means clustering used in User Base Collaborative Clustering

kmeanscluster = kmeans(ratings,centers = 2)
clusplot(ratings,kmeanscluster$cluster, color = TRUE)

# RecommenderRegistry stores different types of recommendation methods
recommenderRegistry$get_entries(dataType = "realRatingMatrix")

# Building the recommender algorithm for popular books

recom_popular = Recommender(RatingMatrix[1:200], method = "POPULAR")

names(getModel(recom_popular))

# Get the Top - N list of books
# Here top 15 books are recommended for user 202
# Different books are recommended for different users

predictrating  = predict(recom_popular,RatingMatrix[202],n=15)

as(predictrating,"list")

# Predict the ratings using predict() function

ratingrecommendation = predict(recom_popular, RatingMatrix, type="ratings")
as(ratingrecommendation,"list")

# User based Collaborative filtering method

recom_ubcf = Recommender(RatingMatrix[1:200], method = "UBCF")

# getModel() function gives the details of the recommendation model

modeldetails = getModel(recom_ubcf)

names(modeldetails)

modeldetails$data

# Applying on testing data

n_users = 6

testing_ubcf = predict(object = recom_ubcf,newdata = RatingMatrix[201:400],n = n_users)

recc_matrix = sapply(testing_ubcf@items, function(x)
{
  colnames(RatingMatrix)[x]
})

recc_matrix


# Evaluation of Predicted Ratings

# Split the data into training/testing in the ratio of 70/30

tdata = evaluationScheme(RatingMatrix, method="split", train=0.7 ,given=0, goodRating=10)

result1 = Recommender(getData(tdata, "train"), "UBCF")
result1

prediction_1 = predict(result1, getData(tdata,"known"), type = "ratings")
prediction_1

dim(tdata)
# Error values gives Root Mean Square Error, Mean Square Error and Mean Absolute Error of the predicted ratings
errorvalues = calcPredictionAccuracy(prediction_1, getData(tdata, "unknown"))
errorvalues

