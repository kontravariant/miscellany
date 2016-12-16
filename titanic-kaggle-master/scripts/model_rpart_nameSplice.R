#import test, train spreadsheets
train <- read.csv("~/Projects/titanic_kaggle/train.csv", stringsAsFactors = FALSE)
test <- read.csv("~/Projects/titanic_kaggle/test.csv", stringsAsFactors = FALSE)

library(rpart)
library(RGtk2)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(Hmisc)

#Feature Engineering carried out by batch combine and adjusting
#Blank suvived var in test set
test$Survived <- NA
#batch and unfactor name strings
batch = rbind(train,test)
batch$Name = as.character(batch$Name)
#Split all names to get title
batch$Title <- (sapply(batch$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]}))
batch$Title = trimws(batch$Title)
#batch common titles
cFrench = c('Mme','Mlle')
cSir = c('Capt', 'Don', 'Major', 'Sir', 'Rev','Col','Dr')
cLady = c('Dona', 'Lady', 'the Countess', 'Jonkheer')
batch$Title[which(batch$Title %in% cFrench)] = 'French'
batch$Title[which(batch$Title %in% cSir)] = 'Sir'
batch$Title[which(batch$Title %in% cLady)] = 'Lady'
batch$Title = as.factor(batch$Title)
#Family size var
batch$FamSize = batch$SibSp + batch$Parch

var.levels = levels(batch$Title)
for (v in var.levels) {
  ageWhich = which(batch$Title == v)
  batch$Age[ageWhich] = impute(batch$Age[which(batch$Title == v)])
}


#Splitting batch by Survived column
batch.na = which(is.na(batch$Survived)==TRUE)
test.batch = batch[batch.na,]
train.batch = batch[-batch.na,]

#Train
fit = rpart(Survived ~ Sex + Age + Pclass + Title, data=train.batch, method='class')

fancyRpartPlot(fit)


#submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
#write.csv(submit, file = "titlesplit.csv", row.names = FALSE)