# Load packages
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

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

# Split train and test sets
train = merged[1:891,]
test = merged[892:1309,]

# Build tree
tree = rpart(Survived ~ Pclass + Sex + Age + Fare + Embarked + Title + FamilySize,
             data = train,
             method = "class")

# Plot tree
fancyRpartPlot(tree)

# Make prediction
pred = predict(tree, newdata = test, type = "class")
submit = data.frame(PassengerId = test$PassengerId, Survived = pred)
write.csv(submit, file = "submission3.csv", row.names = FALSE)