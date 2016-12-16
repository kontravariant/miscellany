setwd("~/Projects/titanic_kaggle/")
#input test, train spreadsheets
train.in <- read.csv("~/Projects/titanic_kaggle/train.csv", stringsAsFactors = FALSE)
test.in <- read.csv("~/Projects/titanic_kaggle/test.csv", stringsAsFactors = FALSE)

library(randomForest)
library(RGtk2)
library(rattle)
library(RColorBrewer)
library(Hmisc)
library(party)

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
  cSir = c('Capt', 'Don', 'Major', 'Sir', 'Rev','Col','Dr', 'Jonkheer')
  cLady = c('Dona', 'Lady', 'the Countess')
  batch$Title[which(batch$Title %in% cFrench)] = 'French'
  batch$Title[which(batch$Title %in% cSir)] = 'Sir'
  batch$Title[which(batch$Title %in% cLady)] = 'Lady'
  batch$Title = as.factor(batch$Title)
  #Family size var
  batch$FamSize = batch$SibSp + batch$Parch + 1
  #Family size integration
  batch$FamSize[batch$FamSize == 1] = 1
  batch$FamSize[batch$FamSize < 5 & batch$FamSize > 1] = 2
  batch$FamSize[batch$FamSize > 4] = 3
  batch$FamSize = as.factor(batch$FamSize)
  #Fare impute
  batch$Pclass = as.factor(batch$Pclass)
  class.levels = levels(batch$Pclass)
  for (c in class.levels) {
    classWhich = which(batch$Pclass == c)
    batch$Fare[classWhich] = impute(batch$Fare[which(batch$Pclass == c)])
  }
  
  title.levels = levels(batch$Title)
  for (t in title.levels) {
    ageWhich = which(batch$Title == t)
    batch$Age[ageWhich] = impute(batch$Age[which(batch$Title == t)])
  }
  
  batch$Sex = as.factor(batch$Sex)
  
  #Splitting batch by Survived column
  batch.na = which(is.na(batch$Survived)==TRUE)
  test.eng = batch[batch.na,]
  train.eng = batch[-batch.na,]
  
  return( list(batch = batch, test.eng = test.eng, train.eng = train.eng) )
}


feat = featEng(train.in,test.in)
batch = feat$batch
train.eng = feat$train.eng
test.eng  = feat$test.eng

set.seed(322)
fit = randomForest(as.factor(Survived) ~ Sex + Age + FamSize + Title + Pclass + Fare, data=train.eng, importance=TRUE, ntree=2000, na.action = na.roughfix)


Prediction <- predict(fit, test.eng)
submit = data.frame(PassengerId = test.eng$PassengerId)
rownames(submit) = rownames(test.eng)
submit$Survived = Prediction
write.csv(submit, file = "rf_model_1.csv", row.names = FALSE)
