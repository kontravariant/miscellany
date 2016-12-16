#import test, train spreadsheets
train <- read.csv("~/Projects/titanic_kaggle/train.csv", stringsAsFactors = FALSE)
test <- read.csv("~/Projects/titanic_kaggle/test.csv", stringsAsFactors = FALSE)

library(rpart)
library(RGtk2)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
fit <- rpart(Survived ~ Pclass + Sex + Age + Fare, data=train, method="class")

fancyRpartPlot(fit)
text(fit)

Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)