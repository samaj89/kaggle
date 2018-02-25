# Load packages
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

# Import datasets
train = read.csv("Titanic/train.csv")
test = read.csv("Titanic/test.csv")

# Build tree
tree <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")

# Plot tree
fancyRpartPlot(tree)

# Make Prediction
pred = predict(tree, newdata = test, type = "class")
submit = data.frame(PassengerId = test$PassengerId, Survived = pred)
write.csv(submit, file = "submission2.csv", row.names = FALSE)