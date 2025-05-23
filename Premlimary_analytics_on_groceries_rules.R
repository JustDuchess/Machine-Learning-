getwd()
setwd("C:\\Users\\HP\\Desktop\\DA TERM 2")
##Premlimary analytics
#groceryitems <- ("groceries.csv", sep = ',' )


#load the groceries data
library(arules)
groceries <-read.transactions("groceries.csv", sep = ',' )

summary(groceries)

#lookat the first five transaction
inspect(groceries[1:5])

# examine the frequency of items
itemFrequency(groceries[, 1:3])

# plot the frequency of items
itemFrequencyPlot(groceries, support = 0.1)
itemFrequencyPlot(groceries, topN = 20)

# a visualization of the sparse matrix for the first five transactions
image(groceries[1:5])

image(groceries)

# visualization of a random sample of 100 transactions
image(sample(groceries, 100))

## inspect(groceries)

## Step 3: Training a model on the data ----
library(arules)

# default settings result in zero rules learned
apriori(groceries)

#using default parameters
# set better support and confidence levels to learn more rules
groceryrules <- apriori(groceries, parameter = list(support =
                                                      0.006, confidence = 0.25, minlen = 2))
inspect(groceryrules[1:3])
    groceries
groceryrules    
apriori(groceries)

## Step 4: Evaluating model performance ----
# summary of grocery association rules

summary(groceryrules)

# sorting grocery rules by lift
inspect(sort(groceryrules, by = "lift")[1:5])

# look at the first three rules
inspect(groceryrules[1:3])


# finding subsets of rules containing any berry items
berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)


inspect(sort(groceryrules, by = "lift")[1:10])

View

# writing the rules to a CSV file
write(groceryrules, file = "groceryrules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)

# converting the rule set to a data frame
groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)

groceryrules_df
#as.tibble(groceryrules_df)
#as.table(groceryrules_df)
#as_tibble(groceryrules_df)

#as.table(groceryrules_df$rules)


