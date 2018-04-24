library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(e1071)
library(class)
library(randomForest)
library(xgboost)
library('dplyr')

train <- read.csv("input_files/train.csv")
test <- read.csv("input_files/test.csv")

full  <- bind_rows(train, test)

# Extract title from name
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

# Extract Surname from Name
full$Surname <- sapply(full$Name,  
                       function(x) strsplit(as.character(x), split = '[,.]')[[1]][1])

# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1

# Create a family variable 
full$Family <- paste(full$Surname, full$Fsize, sep='_')

# Discretize family size
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

# Create a Deck variable. Get passenger deck A - F:
full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(as.character(x), NULL)[[1]][1]))

full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'

train <- full[1:891,]
test <- full[892:1309,]