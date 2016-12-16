setwd("~/Projects/titanic_kaggle/")
#input test, train spreadsheets
train.in <- read.csv("~/Projects/titanic_kaggle/train.csv", stringsAsFactors = FALSE)
test.in <- read.csv("~/Projects/titanic_kaggle/test.csv", stringsAsFactors = FALSE)

library(rpart)
library(RGtk2)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(Hmisc)

#Feature Engineering carried out by batch combine and adjusting
featEng = function(train, test) {
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
  test.eng = batch[batch.na,]
  train.eng = batch[-batch.na,]

  return( list(test.eng = test.eng, train.eng = train.eng) )
}


feat = featEng(train.in,test.in)
train.eng = feat$train.eng
test.eng  = feat$test.eng


#Train
fit = rpart(Survived ~ Sex + Age + Pclass + Title, data=train.eng, method='class')
#Predict
Prediction <- predict(fit, test.eng, type = "class")

fancyRpartPlot(fit)

sub.df = data.frame(PassengerId = test.eng$PassengerId, Survived = Prediction)
submit <- sub.df
write.csv(submit, file = "rpart2.csv", row.names = FALSE)