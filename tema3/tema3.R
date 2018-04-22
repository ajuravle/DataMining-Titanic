library(rattle)
library(rpart.plot)
library(RColorBrewer)

train <- read.csv("input_files/train.csv")
test <- read.csv("input_files/test.csv")

decision_tree <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class")
fancyRpartPlot(decision_tree)