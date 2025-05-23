#10th/4/25

getwd()
setwd('C:\\Users\\HP\\Desktop\\DA TERM 2')
teens <-read.csv(file.choose(), stringsAsFactors = TRUE)

## Example: Finding Teen Market Segments ----
## Step 2: Exploring and preparing the data ----
#teens <- read.csv("snsdata.csv", stringsAsFactors = TRUE)
str(teens)

# look at missing data for female variable
table(teens$gender)
table(teens$gender, useNA = "ifany")

# look at missing data for age variable
summary(teens$age)

# eliminate age outliers
teens$age <- ifelse(teens$age >= 13 & teens$age < 20,
                    teens$age, NA)

summary(teens$age)

View(teens)

na_counts <- colSums(is.na(teens[, !names(teens) %in% "gender"]))
na_counts

na_counts <- colSums(is.na(teens[, !names(teens) %in% "age"]))
na_counts

#teens[(teens$age > 20), "age"]
teens[(teens$age > 20), 'age']

#teens[(teens$age > 20), "age"]
#teens[(teens$age > 20), 'gender']

# eliminate age outliers
teens$age <- ifelse(teens$age >= 13 & teens$age < 20,
                    teens$age, NA)
summary(teens$age)

# reassign missing gender values to "unknown"
teens$female <- ifelse(teens$gender == "F" &
                         !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1, 0)

# check our re-coding work
table(teens$gender, useNA = "ifany")
table(teens$female, useNA = "ifany")
table(teens$no_gender, useNA = "ifany")


#STOP

#11th of April 2025

# finding the mean age by cohort
mean(teens$age) # doesn't work
mean(teens$age, na.rm = TRUE) # works

#table(teens$gradyear)
#age by cohort
aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)

#aggregate(data = teens, gradyear ~ age, mean, na.rm = TRUE)

#create a vector with the average age for each gradyear, reapeted
ave_age <- ave(teens$age, teens$gradyear,
               FUN = function(x) mean(x, na.rm = TRUE))
               
ave_age               

#View(teens)
#teens$age

teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)

summary(teens$age)

ncol(teens)


#create a z-score standardization data frame for easier interpretation 
interests <- teens[5:40]
interests_z <- as.data.frame(lapply(interests, scale))
#interests_z <- as.data.frame(lapply(interests, scale))
#as_tibble(interests_z)

#cluster means to group features

colnames(interests)

#compare  the data before and after the transformation 
summary(interests$basketball)
summary(interests_z$basketball)

# create the clusters using k-means
RNGversion("3.5.2") # use an older random number generator to match the book
set.seed(2345)
teen_clusters <- kmeans(interests_z, 5)

teen_clusters

## Step 4: Evaluating model performance ----
# look at the size of the clusters

teen_clusters$size

# look at the cluster centers
summary(teen_clusters$centers)
teen_clusters$centers

## Step 5: Improving model performance ----
# apply the cluster IDs to the original data frame
teens$cluster <- teen_clusters$cluster

# look at the first five records
teens[1:5, c("cluster", "gender", "age", "friends")]
