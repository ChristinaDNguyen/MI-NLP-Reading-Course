#CH 7: CLASSIFICATION ===========================================================

#There's a difference between the method we learn in ch 6 and 7. Ch 6 dealt with
#"regression models" which predict a numeric or continuous value. Ch 7 will
#classify (yeah, that's why it's called classification), by predicting
#a class label or group membership.

#We will use the data of complaints submitted to US CFPB

library(tidyverse)
complaints <- read_csv("C://Users//chris//Downloads//complaints.csv")
glimpse(complaints)

#The goal of this exercise is to ***BUILD A CLASSIFICATION MODEL  TO PREDICT
#WHAT TYPE OF FINANCIAL PRODUCT THE COMPALINTS ARE REFERRING TO***

#First, a binary model ---------------------------------------------------------

head(complaints$consumer_complaint_narrative)


complaints$consumer_complaint_narrative %>%
  str_extract_all("\\{\\$[0-9\\.]*\\}") %>%
  compact() %>%
  head()

#We will use the consumer_complaint_narrative as a way to create a bunch of 
#labels for our dataset

#We make a factor outcome variable called 'product' with two levels, 'credit' 
#and 'other.' Then we split the data into TRAINING and TESTING data sets. 

library(tidymodels)
set.seed(1234)
complaints2class <- complaints %>%
#This says "put/paste the column called 'product' in, and the values of this column can be "credit reporting" ... )
  mutate(product = factor(if_else(Product == paste("Credit reporting, credit repair services,","or other personal consumer reports"),"Credit", "Other")))
complaints_split <- initial_split(complaints2class, strata = product)
complaints_train <- training(complaints_split)
complaints_test <- testing(complaints_split)

dim(complaints_train)
dim(complaints_test)
#Preprocess this text now by turnint it into numeric features for ML. We use
#the recipes package. Just like before when we were 'baking'

complaints_rec <- recipe(product ~ consumer_complaint_narrative, data = complaints_train)

library(textrecipes)
complaints_rec <- complaints_rec %>%
  step_tokenize(consumer_complaint_narrative) %>% #tokenized
  step_tokenfilter(consumer_complaint_narrative, max_tokens = 1e3) %>%
  step_tfidf(consumer_complaint_narrative) #calculated tf-idf


