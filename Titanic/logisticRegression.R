# Import datasets
train = read.csv("Titanic/train.csv", header=T, na.strings=c(""))
test = read.csv("Titanic/test.csv", header=T, na.strings=c(""))

# Fill in NAs
train$Age[is.na(train$Age)] = mean(train$Age,na.rm=T)
train$Embarked[which(is.na(train$Embarked))] = "S"
test$Age[is.na(test$Age)] = mean(train$Age,na.rm=T)
test$Fare[is.na(test$Fare)] = median(train$Fare,na.rm=T)

# Fit model
model = glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
            family=binomial(link='logit'),data=train)

# Predict on test set
pred = predict(model, newdata=subset(test, select= -c(PassengerId, Name, Ticket, Cabin)), type='response')
test$Survived = 0
test$Survived[pred >= 0.5] = 1

submit = submit = data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "submission5.csv", row.names = FALSE)
