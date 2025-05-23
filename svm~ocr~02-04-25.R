getwd()
setwd("C:\\Users\\AMAKA\\OneDrive\\Desktop\\NIIT TERM 2 SLIDES")
 

### get your current working directory

## Step 2: Exploring and preparing the data ----
# read in data and examine structure
letters  <- read.csv(file.choose(), stringsAsFactors = TRUE)

letters <- read.csv(file.choose())
letters
head(letters)

### divide into training and testing data
# create training and test data
letters_train <- letters[1:16000, ]
letters_test <- letters[16001:20000, ]


## ## Step 3: Training a model on the data ----
# begin by training a simple linear SVM
library(kernlab)

letter_classifier <- ksvm(letter ~ ., data = letters_train,
                          kernel = "vanilladot")

letter_classifier

## Step 4: Evaluating model performance ----
# predictions on testing dataset
letter_predictions <- predict(letter_classifier, letters_test)

head(letter_predictions)

table(letter_predictions, letters_test$letter)

# look only at agreement vs. non-agreement
# construct a vector of TRUE/FALSE indicating correct/incorrect predictions
agreement <- letter_predictions == letters_test$letter
table(agreement)
prop.table(table(agreement))

## Step 5: Improving model performance ----

letter_classifier_rbf <- ksvm(letter ~ ., data = letters_train,
                                 kernel = "rbfdot")
 letter_classifier_rbf
 
 
 letter_predictions_rbf <- predict(letter_classifier_rbf,
                                   letters_test)
 
 head(lettr)

 ### comparing the accuracy 
 