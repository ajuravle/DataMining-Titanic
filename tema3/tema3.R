rez = c(150)
for (j in 1:150) {
  accuracies <- c()
  folds <- createFolds(train$Survived, k = 5, list = TRUE, returnTrain = TRUE)
  for (i in 1:5) {
    model <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + Fsize, 
                   data = train[folds[[i]],], method = "class", 
                   control=rpart.control(minsplit=124, cp=0.0078, maxcompete=113))
    prediction <- predict(object = model, newdata = train[-folds[[i]],], type = "class")
    accuracies <- c(accuracies, confusionMatrix(prediction, as.factor(train[-folds[[i]], ]$Survived))$overall[[1]])
  }
  rez[j] = mean(accuracies)
}
plot(rez)
which(rez==max(rez)) 
rez[which(rez==max(rez))]