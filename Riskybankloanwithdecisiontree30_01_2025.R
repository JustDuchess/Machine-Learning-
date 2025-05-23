##Machine Learning Jan 30th 2025
##Get work in Directory 
getwd()
#set your working directory
setwd("C:\\Users\\HP\\Desktop\\DA TERM 2")

## Example: Identifying risky bank loans----
##Step 2: Exploring and Preparing the the data ---
credit <- read.csv(file.choose(), stringsAsFactors = TRUE)
#view credit data
view(credit)
nrow(credit)
ncol(credit)
str(credit)
summary(credit)
#find out if your data has a missing value
sum(is.na(credit))
colnames(credit)

#look at the two characteristics of the applicant 

table(credit$checking_balance)
table(credit$savings_balance)

#look at the two characteristics of the loan
summary(credit$months_loan_duration)
summary(credit$amount)

#look at the class variables
credit$default
table(credit$default)

#use set.seed to use the same random number sequence as the tutor
set.seed(123)
train_sample <- sample(1000,900)

str(train_sample)

#split the train_sample vector into
#90 percent training and 
#10 percent test data sets:

#12/2/25
credit_train <- credit[train_sample, ]
#view(credit_train)

credit_test <- credit[-train_sample, ]

#check the proportion of class variables 
#credit_train$default
#table(credit_train$default)
table(credit_train$default)
prop.table(table(credit_test$default))
#credit test
#credit_test$default
table(credit_test$default)
prop.table(table(credit_test$default))


#step 3: Training a model on the data---
#build the simplest decision tree
library(c50)

#build the decision tree with c50 algorithm 
credit_model <- c5.0(credit_train[-17],credit_train$default)
credit_model <- c5

colnames(credit_train)

