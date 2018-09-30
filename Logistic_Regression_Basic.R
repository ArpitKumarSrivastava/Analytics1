# Logistic Regression
#data()  # datasets available for use in R

# Load the textbook R package
library(ISLR)   #install the package
?Default   # data set
# Load in the credit data
data("Default")
str(Default)
class(Default)
head(Default)
names(Default)
dim(Default)
summary(Default)
Default
View(Default)

# How many people actual default?
(tmp = table(Default$default))

333/10000
table(Default$default, Default$student)
table(Default$student)

?glm       #Multiple Logistic Regression like lm for linear regression

logit1 = glm(default ~ income + balance + student, family='binomial', data=Default)      #defining the logistic regression model || only 2 output (binary) that is why binomial family
# logit1 = glm(default ~ ., family='binomial', data=Default)   if we use . , it means we are using all the data in the table
summary(logit1)

# Similar to linear model table understanding except
# for income, the p value is >0.05, therefore it do not relate (not significant) to the model
# null deviance with low deviance is considered as good : with no variables
# residual deviance: is decreased when we add more variables

exp(coef(logit1))
#income is not significant - remove it

step(glm(default ~ ., family='binomial', data=Default))    #step function used || it will step wise show us the option to reduce the AIC value
# this shows student + balance is the suitable model

#using this model

logit2 = glm(default ~ balance + student, family='binomial', data=Default)
summary(logit2)
coef(logit2)
exp(coef(logit2))

anova(logit2, logit1)

# Predict : use sample values
head(Default)
seq(1, 10000,500)
Default[c(1,501),]
ndata1= Default[seq(1, 10000,500),]
ndata1
nrow(Default[seq(1, 10000,500),])
10000/500

library(dplyr)
(ndata = (slice(Default, seq(1,n(),500))))
ndata
slice(Default, seq(1,n(),1000))  # another way
head(ndata)

addmargins(prop.table(table(Default$default,Default$student)))
0.2817/0.9667; 0.0127/0.0333

options(digits=10)

# Predict on the test data || gives the probability
fitted.results = predict(logit2, newdata=ndata,type='response')
fitted.results
head(fitted.results)
fitted.results
cbind(ndata, fitted.results)

# Now with the help of the model, we are defining the threshold value 

p2 <- ndata %>% mutate(predictnew = ifelse(fitted.results < 0.5, 'No','Yes'))
cbind(p2,fitted.results)
fitted.results

ndata1
ndata1$pred <- ifelse(fitted.results<0.5,"No","Yes")

# model validation using ROC Curve

install.packages("InformationValue")
library(InformationValue)
plotROC(factor(ndata1$default,levels = c("No", "Yes"),labels = c("0", "1")), fitted.results)

## create model on insignificant variable and then create ROC

logit3 = glm(default ~ income , family='binomial', data=Default)

fitted.results3 = predict(logit3, newdata=ndata1,type='response')
fitted.results3

plotROC(factor(ndata1$default,levels = c("No", "Yes"),labels = c("0", "1")), fitted.results3)

# Identify the optimal cut-off value

library(InformationValue)
optCutOff <- optimalCutoff(factor(ndata1$default,levels = c("No", "Yes"),labels = c("0", "1")), fitted.results)

optCutOff

plotROC(factor(ndata$default ,levels = c("No", "Yes"),labels = c("0", "1")), ifelse(fitted.results3 < optCutOff, 0,1))

(ndata2 = data.frame(student=c('Yes','No'), balance=mean(Default$balance), income=mean(Default$income)))
(fitted.results2 <- predict(logit2, newdata=ndata2,type='response'))

#Accuracy of Model
#Run Model by creating Training and Test Dataset#

library(caret)
set.seed(3456)
str(Default)
trainIndex <- sample(1:nrow(Default),trunc(0.80*nrow(Default)))

Train <- Default[ trainIndex,]
Test  <- Default[-trainIndex,]
head(Train)
head(Test)
dim(Train)

# Logistic Regression Model
model = glm(default ~ balance + student, data=Default, family='binomial')

# Validate on test data

Test$model_prob <- predict(model, Test, type = 'response')
head(Test)

# Identify the optimal cut-off value

library(InformationValue)
library(dplyr)
optCutOff <- optimalCutoff(factor(Test$default,levels = c("No", "Yes"),labels = c("0", "1")), Test$model_prob)

# ROC Curve
plotROC(factor(Test$default,levels = c("No", "Yes"),labels = c("0", "1")), Test$model_prob)

Test <- Test  %>% mutate(default_pred = ifelse(model_prob > .425,'Yes','No'))
head(Test)

# create new column for checking the actual and predicted value

Test <- Test %>% mutate(accurate = 1*(default == default_pred))

sum(Test$accurate)/nrow(Test)

#97% Accuracy
