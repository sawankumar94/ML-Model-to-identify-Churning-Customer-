rm(list=ls())
library(mice)
library(caret)
library(dplyr)
library(DBI)
library(car)
library(boot)
library(caret)
library(ROCR)
library(e1071)
library(rpart)
setwd("C:/Users/Sawan/Desktop/Unifynd/fwdmlproblems")

churn <- read.csv('churn.csv')

head(churn)  #To have a look on the columns

#The purpose of the below code is to make the following columns 'factor'
levels(churn$Area.Code) <- 0:3
levels(churn$Int.l.Plan) <- 0:2
levels(churn$VMail.Plan) <- 0:2
levels(churn$Churn.) <- 0:1

# Replacing '?' with NA 
churn2 <- as.matrix(churn)
y <- which(churn2== "?")         
churn2[y] <- NA

churn3 <- as.data.frame(churn2)

churn4 <- churn3[complete.cases(churn3),] #1737 so obviously we can't remove
#wherever we have NA and fit the model 

#to find the percentage of missing values in each column
Missing <- function(x){sum(is.na(x))/length(x)*100}
round(apply(churn3,2,Missing),2) 

churn3 <- churn3[,-c(1, 5)] #removing 'X' and 'Phone' from data

head(churn3) #to have a look on the table

churn3[, c(2,6:20)] <- sapply(churn3[, c(2,6:20)], as.numeric) #Except 'State' ,
# 'Area.Code, 'Int.l.Plan   & 'VMail.Plan make all other columns so that we
# can impute 


#when we convert 'Churn.' it to factor it takes values '1' and '2' 
#so converting it to '0' & '1'
churn3$Churn.[churn3$Churn. == 1] <- 0
churn3$Churn.[churn3$Churn. == 2] <- 1

##lets  impute the values in column which takes qantitative values


imp <- mice(churn3, m=5, seed = 23109)  #This code creates 5 imputed datasets. The multiple
# datasets are created to pool the regression estimates 
# if used for prediction.

head(imp$loggedEvents, 2) #to have a look on 'logs' generated , it gives warning 
#messages , here it says "Intl.Charge" column is not imputed because it is 
#collinear with other column


final_data1 <- complete(imp,1) #I am using first of the 5 created imputed datasets. So 'final_data'  
# is the complete data where missing values are imputed from the 
#first of the 5 imputed datasets.
round(apply(final_data1,2,Missing),2)

cor(churn3[6:19], use = "pairwise.complete.obs") #to see the correlation
#matrix , here we can see that "Intl.Charge"  is higly collinear with 
# 'Intl.Mins'

stripplot(imp, pch = 20, cex = 0.2) #to have a look on the distribution of imputed 
#values
 
#####

final_data <- final_data1[,-18] #so removing "Intl.Charge"
model <-  lm(Churn. ~ ., data = final_data)
print(vif(model)) # checked for multicollinearity and fpund
# 'Day.Mins' and 'Day.Charge' to be collinear      


#Removing 'Day.Charge'

final_data <- final_data[,-9]

final1 <- final_data

final1 <- final1[sample(nrow(final1)),]

#Create 10 equally size folds
folds <- cut(seq(1,nrow(final1)),breaks=10,labels=FALSE)


###Random Forest for Classification
final2 <- final1
final2$Churn. <- as.factor(final2$Churn.)

library(randomForest)

a <- numeric()
for(i in 1:10){
  
  val_Indexes <- which(folds==i,arr.ind=TRUE)
  val_Data <- final2[val_Indexes, ]
  val_Data_check <- val_Data[,1:18]
  val_Data_label <- val_Data$Churn. 
  trainData <- final2[-val_Indexes, ]
  
  rf = randomForest(Churn.~.,data= trainData, norm.votes = TRUE, proximity = TRUE, ntree= 100)
  glm.probs1 =predict(rf, val_Data_check, type = "prob" )
  p <- glm.probs1[,2]
  pr <- prediction(p, val_Data$Churn.)
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  a <- c(a,auc)
  #print(auc)
}  
print(mean(a))   #0.895

