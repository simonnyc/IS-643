install.packages('sparklyr')
install.packages('dplyr')
spark_install(version = "1.6.2")

library(sparklyr)
library(dplyr)
library(randomForest)
library(magrittr)
library(methods)

# Sys.setenv(SPARK_HOME='C:/spark-1.6.2-bin-hadoop2.6')
# 
# Sys.unsetenv('SPARK_HOME')


setwd("C:/Senthil/MSDataAnalytics/Semester5/643/Assignment4_Spark")

# LOCAL CONNECTION:
#spark_install()
#spark_disconnect_all()

sc <- spark_connect(master = "local")

ratings_df <- read.csv("C:/Senthil/MSDataAnalytics/Semester5/643/Assignment4_Spark/train_v3.csv", head=T)
#ratings_df <- ratings_df[1:100,2:4]
ratings_df <- ratings_df[,2:4]
colnames(ratings_df)[1:3] <- c("user","item","rating")
dim(ratings_df)
head(ratings_df)

#iris_tbl <- copy_to(sc, iris)
movie_rating <- copy_to(sc, ratings_df, overwrite = T)


# model <- ml_als_factorization(iris_tbl, iter.max = 25, regularization.parameter = 0.01, 
#                               implicit.preferences = TRUE, alpha = 1.0)

# model <- ml_als_factorization(movie_rating, iter.max = 25, regularization.parameter = 0.01, 
#                               implicit.preferences = TRUE, alpha = 1.0)

model <- ml_als_factorization(movie_rating, rating.column = "rating", user.column = "user",
                              item.column = "item", rank = 10L, regularization.parameter = 0.1,
                              iter.max = 10L, ml.options = ml_options())

summary(model)

predictions <- model$.model %>%
  invoke("transform", spark_dataframe(movie_rating)) %>%
  collect()

sqrt(mean(with(predictions, prediction-rating)^2))

spark_disconnect(sc)
