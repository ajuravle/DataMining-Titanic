library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(e1071)
library(class)
library(randomForest)
library(xgboost)
library(neuralnet)

train <- read.csv("input_files/train.csv")
test <- read.csv("input_files/test.csv")

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
train$Age[is.na(train$Age)] <- mean(train$Age,na.rm=T)
train$Fare[is.na(train$Fare)] <- mean(train$Fare,na.rm=T)
test$Age[is.na(test$Age)] <- mean(test$Age,na.rm=T)
test$Fare[is.na(test$Fare)] <- mean(test$Fare,na.rm=T)

model <- model.matrix(~ Survived + Pclass + Sex+ Age + Fare + SibSp,data = train)
neural_network <- neuralnet( 
  Survived ~ Pclass + Sex+ Age + Fare + SibSp, data=model, hidden=2, threshold=0.01, linear.output = F)
plot(neural_network)

model_test <- model.matrix(~ Pclass + Sex+ Age + Fare + SibSp,data = test)
res <- neuralnet::compute(neural_network, model_test[,c("Pclass","Sexmale","Age", "Fare","SibSp")])