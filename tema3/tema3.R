library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(e1071)
library(class)

train <- read.csv("input_files/train.csv")
test <- read.csv("input_files/test.csv")

knn_titanic <-knn(train[, -1], test[, -1], train[, 1], k = 5)
