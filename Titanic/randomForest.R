# Load packages
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)

# Import datasets
train = read.csv("Titanic/train.csv")
test = read.csv("Titanic/test.csv")

# Prepare train/test sets for feature engineering
test$Survived = NA
merged = rbind(train, test)

# Extract passenger title as new feature
merged$Name = as.character(merged$Name)
merged$Title = sapply(merged$Name, FUN = function(x) {strsplit(x, split = '[.,]')[[1]][2]})
merged$Title = sub(' ', '', merged$Title)
merged$Title[merged$Title == 'Mme'] = 'Mrs'
merged$Title[merged$Title %in% c('Mlle', 'Ms')] = 'Miss'
merged$Title[merged$Title %in% c('Capt', 'Don', 'Col', 'Major')] = 'Sir'
merged$Title[merged$Title %in% c('Dona', 'Jonkheer', 'the Countess')] = 'Lady'
merged$Title = factor(merged$Title)

# Merge SibSb and Parch to create FamilySize feature
merged$FamilySize = merged$SibSp + merged$Parch + 1

# Fill in missing Age values using anova
agePredict = rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FamilySize,
                   data = merged[!is.na(merged$Age),], 
                   method="anova")
merged$Age[is.na(merged$Age)] = predict(agePredict, merged[is.na(merged$Age),])

# Fill in blank Fare and Embarked values
merged$Embarked[which(merged$Embarked == '')] = "S"
merged$Embarked = factor(merged$Embarked)
merged$Fare[which(is.na(merged$Fare))] = median(merged$Fare, na.rm = TRUE)

# Split train and test sets
train = merged[1:891,]
test = merged[892:1309,]
test = subset(test, select= -Survived)

# Fit random forest
set.seed(1)
forest = randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                        Embarked + Title + FamilySize,
                      data = train,
                      importance = TRUE,
                      ntree = 2000)

# Make prediction
pred = predict(tree, newdata = test, type = "class")
submit = data.frame(PassengerId = test$PassengerId, Survived = pred)
write.csv(submit, file = "submission4.csv", row.names = FALSE)