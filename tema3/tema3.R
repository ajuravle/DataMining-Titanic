library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(e1071)
library(class)

train <- read.csv("../input_files/train.csv")
test <- read.csv("../input_files/test.csv")

# Remove Name, Ticket and Cabin columns
train <- train[,-c(4,9,11)]
test <- test[,-c(3,8,10)]

# Change Sex to 0 = male, 1 = female
train$Sex <- sapply(as.character(train$Sex), switch, 'male' = 0, 'female' = 1)
test$Sex <- sapply(as.character(test$Sex), switch, 'male' = 0, 'female' = 1)

# Change Embarked column to 0 = 'C', 1 = 'Q', 2 = 'S' and remove NAs
train$Embarked[train$Embarked == ''] <- 'S'
train$Embarked <- sapply(as.character(train$Embarked), switch, 'C' = 0, 'Q' = 1, 'S' = 2)
test$Embarked <- sapply(as.character(test$Embarked), switch, 'C' = 0, 'Q' = 1, 'S' = 2)

# Remove NAs from Age and Fare columns
train_age <- na.omit(train$Age)
train_age_avg <- mean(train_age)
train$Age[is.na(train$Age)] <- train_age_avg

test_age <- na.omit(test$Age)
test_age_avg <- mean(test_age)
test$Age[is.na(test$Age)] <- test_age_avg

test_fare <- na.omit(test$Fare)
test_fare_avg <- mean(test_fare)
test$Fare[is.na(test$Fare)] <- test_fare_avg

#random_forest <- randomForest(train[,-c(1,2)])
