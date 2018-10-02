#Load Libraries
library(rpart)
library(rpart.plot)

#DataSet
iris
str(iris)
head(iris)

# Classification Tree
set.seed(1)
trainIndex <- sample(1:nrow(iris),0.80*nrow(iris))
trainIndex1 <- sample(1:nrow(iris),trunc(0.80*nrow(iris)))
trainIndex
trainIndex1

# create dataset

Train <- iris[trainIndex,]
Test <- iris[-trainIndex,]

str(Train)
table(Train$Species)

#set.seed(1234)
ctree = rpart(Species ~ ., method='class', data=Train)
ctree
rpart.plot(ctree, main='Classification Tree', type=3, extra=104)
par(mfrow=c(1,1))
printcp(ctree)

ctreeprune = prune(ctree, cp=0.44)
ctreeprune
rpart.plot(ctreeprune, main='Classification Tree', nn=T, type=4, extra=104)

pred<-predict(ctree, newdata = Test, type= "class")
test_pred <- cbind(Test, pred, match = Test$Species==pred)

library(caret)

?confusionMatrix
confusionMatrix(test_pred$pred,test_pred$Species)

