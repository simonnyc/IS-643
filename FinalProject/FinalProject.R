install.packages('sparklyr')
install.packages('dplyr')
#spark_install(version = "1.6.2")

library(sparklyr)
library(dplyr)
library(randomForest)
library(magrittr)
library(methods)

# Sys.setenv(SPARK_HOME='C:/spark-1.6.2-bin-hadoop2.6')
# Sys.unsetenv('SPARK_HOME')



sc <- spark_connect(master = "local")

ratings_df <- read.csv("C:/Senthil/MSDataAnalytics/Semester5/643/FinalProject/BX-CSV-Dump/Senthil1/BxBookRating.csv", head=T)
ratings_df <- ratings_df[,c(1,3,4)]
ratings_df <- ratings_df[,c('UserID','BookID','Rating')]
colnames(ratings_df)[1:3] <- c("user","item","rating")




book_rating <- copy_to(sc, ratings_df, overwrite = T)

# model <- ml_als_factorization(iris_tbl, iter.max = 25, regularization.parameter = 0.01, 
#                               implicit.preferences = TRUE, alpha = 1.0)

# model <- ml_als_factorization(book_rating, iter.max = 25, regularization.parameter = 0.01, 
#                               implicit.preferences = TRUE, alpha = 1.0)

model <- ml_als_factorization(book_rating, rating.column = "rating", user.column = "user",
                              item.column = "item", rank = 10L, regularization.parameter = 0.1,
                              iter.max = 10L, ml.options = ml_options())

summary(model)

predictions <- model$.model %>%
  invoke("transform", spark_dataframe(book_rating)) %>%
  collect()

sqrt(mean(with(predictions, prediction-rating)^2))

spark_disconnect(sc)
