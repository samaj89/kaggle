# Import datasets
train = read.csv("Titanic/train.csv")
test = read.csv("Titanic/test.csv")

#Compare Sex with Survived in train
prop.table(table(train$Sex, train$Survived), 2)

#                0         1
# female 0.1475410 0.6812865
# male   0.8524590 0.3187135

# Set Survived column in test and set to 1 if female
test$Survived = rep(0, 418)
test$Survived[test$Sex == 'female'] = 1

# Prepare submission and download .csv
submit = data.frame(PassengerID = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "submission1.csv", row.names = FALSE)
