# Case Study on NPS
# Rating:
# distractor: 1-6
# : 7, 8
# Promotores: 9, 10
# NPA = %(rating>8)-%(rating<=6)

library("readxl")
library("dplyr")

?read_excel

# Import the xlsx data file
read_excel(path, sheet = NULL, range = NULL, col_names = TRUE,
           col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
           guess_max = min(1000, n_max))

NPS_Data <- read_excel(path="./data/NPS Data.xlsx", sheet = 'Sheet1',col_names = T)
View(NPS_Data)

# Operations: Create a binary column based on likert

NPS_Data$Recommend <- ifelse(NPS_Data$`Recommendation-Likert Scale`>8,1,0)

summary(NPS_Data)
table(NPS_Data$`Manages Account`)

NPS_Data$mang_account <- as.factor(NPS_Data$`Manages Account`)

NPS_Data$`Manages Account` <= NULL
NPS_Data

# Convert binary variables into factor

NPS_Data$mang_account<- factor(NPS_Data$mang_account, levels = c("No", "Yes"),labels = c("0", "1"))

NPS_Data$Recommend <- factor(NPS_Data$Recommend,levels = c("0", "1"),labels = c("0", "1"))
summary(NPS_Data)
View(NPS_Data)

# Missing Value Treatment || Replaced the NA in Complaints column to 0 as the frequency of 0 is maximum to 15

hist(NPS_Data$Complaints)
pie(table(NPS_Data$Complaints))

table(NPS_Data$Complaints, NPS_Data$Tenure)
complete.cases(NPS_Data)

NPS_Data[!complete.cases(NPS_Data),]
NPS_Data$Complaints <- ifelse(is.na(NPS_Data$Complaints) , 0, NPS_Data$Complaints)

colSums(is.na(NPS_Data))

# Remove Unnecessary columns

NPS_Data1 <- NPS_Data[,c(2:6,8:9)]

# Develop Logistic Regression Model

model = glm(Recommend ~., data=NPS_Data1, family='binomial')
model1 = glm(Recommend ~Age+Complaints+Tenure, data=NPS_Data1, family='binomial')
summary(model)
summary(model1 )

step(glm(Recommend ~., data=NPS_Data1, family='binomial'))

model2 <- glm(formula = Recommend ~ Age + Complaints + Tenure, family = "binomial",data = NPS_Data1)
summary(model2)

# Develop Linear Model on likert Scale || to develope the understanding and getting the value based on the linear regression

colnames(NPS_Data)[7] <- "likert"
NPS_Data_lm <- NPS_Data[-c(8)]
lmodel = lm(likert ~., data= NPS_Data_lm )

summary(lmodel)

step(lmodel)

lmodel1 = lm(likert ~Age + Complaints + Tenure, data= NPS_Data_lm )
summary(lmodel1)

# Add Predictive Values in column 

fitted(model2)

NPS_Data2<-cbind(NPS_Data1[,1:6], Pred_recommend=ifelse(fitted(model2)<0.5,0,1),prob=fitted(model2))
summary(NPS_Data2)

confusionMatrix(NPS_Data2$Recommend, factor(NPS_Data2$Pred_recommend
                                            ,levels = c("0", "1"),labels = c("0", "1")))





