---
title: "Project_1"
author: "Shazia Khan"
date: "February 4, 2017"
output: pdf_document
---


## Project 1 : 

Requirements:

The goal of this assignment is to help you build your intuition about recommender systems, with a basic soup to nuts implementation coded "from scratch." 
 
Your task is to build a very basic recommender system, first by writing your own functions, then by replacing those functions with those provided in an R Package or a Python library (such as scikitlearn). 
 
 - You should very briefly first describe the recommender system that you're going to build out from a business perspective, e.g. "This system recommends movies to users." 
 
 - You can find a dataset, or build out your own toy dataset and load into (for example) an R or pandas dataframe, a Python dictionary or list of lists, (or other data structure of your choosing). 

 - You can use either collaborative filtering, or a hybrid of content management and collaborative filtering.   

 - You are encouraged to hand code at least your similarity function. 
 - After you have built out your own code base, create an alternate version using packages or libraries.  Compare the results and performance. 

 - You are also encouraged to think about how to best handle missing data. 
 
 - Your code should be turned in an RMarkdown file or a Jupyter notebook, and posted to Github. You may work in a small group (2 or 3 people) on this assignment.  While you're never discouraged from adding features or advanced capabilities such as regularization and matrix factorization methods, it is not expected at this point in the course.   

```{r}


u01 <- c(1,2,3,4,5,NA,1,2,1,3,1,4)
u02 <- c(5,4,3,2,1,1,2,1,2,3,1,3)
u03 <- c(1,NA,1,NA,1,2,NA,NA,1,1,2,3)
u04 <- c(1,1,1,1,1,1,1,1,1,1,1,1)
u05 <- c(1,1,1,2,2,2,3,3,3,4,4,4)
u06 <- c(1,2,3,4,5,1,2,3,4,5,1,2)
u07 <- c(NA,1,2,3,0,1,2,3,NA,1,2,3)
u08 <- c(5,NA,4,NA,3,NA,2,NA,1,NA,1,2)
u09 <- c(5,4,3,5,4,3,5,4,3,5,4,3)
u10 <- c(1,2,1,2,1,2,NA,1,2,NA,1,2)

ratings <- data.frame(userno=integer(),
                 item01=integer(),
                 item02=integer(),
                 item03=integer(),
                 item04=integer(),
                 item05=integer(),
                 item06=integer(),
                 item07=integer(),
                 item08=integer(),
                 item09=integer(),
                 item10=integer(),
                 item11=integer(),
                 item12=integer()
                 )

ratings[1,]  <-c(1,u02)
ratings[2,]  <-c(2,u02)
ratings[3,]  <-c(3,u03)
ratings[4,]  <-c(4,u04)
ratings[5,]  <-c(5,u05)
ratings[6,]  <-c(6,u06)
ratings[7,]  <-c(7,u07)
ratings[8,]  <-c(8,u08)
ratings[9,]  <-c(9,u09)
ratings[10,] <-c(10,u10)

```

#Ratings matrix

```{r}
ratings

```



Implementing Item based recommender systems, like user based collaborative filtering, requires two steps:

1. Calculating Item similarities
2. Predicting the targeted item rating for the targeted User.

Step1: Calculating Item Similarity:
we calculate the similarity between co-rated items. We use cosine similarity or pearson-similarity to compute the similarity between items. 
The output for step is similarity matrix between Items.


```{r}
#step 1: item-similarity calculation co-rated items are considered and similarity between two items
#are calculated using cosine similarity

#install.packages("lsa")
library(lsa)
x = ratings[,2:ncol(ratings)]
x[is.na(x)] = 0
itemSimil = cosine(as.matrix(x))
```

# Item similarity Matrix Using Cosine similarity

```{r}
itemSimil
```


Step2: Predicting the targeted item rating for the targeted User 

Recommending Top N items:
Once all the non rated movies are predicted we recommend top N movies to a user Code for Item based collaborative filtering in R:

```{r}
 
userRecmd = function(userno)
 {
   #extract all the movies not rated by CHAN
   userRatings = ratings[userno,]
   non_rated_movies = list()
   rated_movies = list()
   for(i in 2:ncol(userRatings)){
     if(is.na(userRatings[,i]))
     {
       non_rated_movies = c(non_rated_movies,colnames(userRatings)[i])
     }
     else
     {
       rated_movies = c(rated_movies,colnames(userRatings)[i])
     }
   }
   non_rated_movies = unlist(non_rated_movies)
   rated_movies = unlist(rated_movies)
   #create weighted similarity for all the rated movies by user
   non_rated_pred_score = list()
   for(j in 1:length(non_rated_movies)){
     temp_sum = 0
     df = itemSimil[which(rownames(itemSimil)==non_rated_movies[j]),]
     for(i in 1:length(rated_movies)){
       temp_sum = temp_sum+ df[which(names(df)==rated_movies[i])]
        }
     weight_mat = df*ratings[userno,2:7]
     non_rated_pred_score = c(non_rated_pred_score,rowSums(weight_mat,na.rm=T)/temp_sum)
     }
   pred_rat_mat = as.data.frame(non_rated_pred_score)
   names(pred_rat_mat) = non_rated_movies
   for(k in 1:ncol(pred_rat_mat)){
     ratings[userno,][which(names(ratings[userno,]) == names(pred_rat_mat)[k])] = pred_rat_mat[1,k]
   }
   return(ratings[userno,])
 }
```



# Recommend for User 10
Now we will use the above to find a rating for user 10


```{r}
userRecmd(10)


################################## Mohamed Changes ####################################
x = ratings[,2:ncol(ratings)]
#x[is.na(x)] = 0
x<- as.matrix(x)
x <- as(x, "realRatingMatrix")
as(x, "matrix")

rec=Recommender(x[1:nrow(x)],method="UBCF", param=list(normalize = "Z-score",method="Cosine", nn=5))
rec2 <- predict(rec, x[1:nrow(x)])
as(rec2, "matrix")['10',]

############################
rec=Recommender(x[1:nrow(x)],method="UBCF", param=list(normalize = "Z-score",method="Jaccard", nn=5))
rec2 <- predict(rec, x[1:nrow(x)])
as(rec2, "matrix")['10',]
############################
rec=Recommender(x[1:nrow(x)],method="UBCF", param=list(normalize = "Z-score",method="Jaccard"))
rec2 <- predict(rec, x[1:nrow(x)])
as(rec2, "matrix")['10',]

##################

  
```