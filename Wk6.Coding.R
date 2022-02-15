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
#Now we can build upa  workflow to bundle together our modelling components
complaint_wf <- workflow() %>%
  add_recipe(complaints_rec)


#Start with a Naive Bayes model! Yay! NB models are really good at dealing with a large 
#number of features (tokens), even though here we only use 1000 tokens. Good to know
#for future if we want to use more than 1000 tokens. (I wonder why it mathematically
#works to use NB for loads of tokens...)

library(discrim)
nb_spec <- naive_Bayes() %>%
  set_mode("classification") %>%
  set_engine("naivebayes")
nb_spec

nb_fit <- complaint_wf %>%
  add_model(nb_spec) %>%
  fit(data = complaints_train)

#Evaluate our model - NOT by using the test set because that's valuable data
#saved for the very end. Instead we evaluate by using sampling, just as we did
#in chapter 6. 10 fold cross-validation sets.

set.seed(234)
complaints_folds <- vfold_cv(complaints_train)
complaints_folds


nb_wf <- workflow() %>%
  add_recipe(complaints_rec) %>%
  add_model(nb_spec)
nb_wf

nb_rs <- fit_resamples( nb_wf, complaints_folds, control = control_resamples(save_pred = TRUE))

nb_rs_metrics <- collect_metrics(nb_rs)
nb_rs_predictions <- collect_predictions(nb_rs)

nb_rs_metrics

nb_rs_metrics

nb_rs_predictions %>%
  group_by(id) %>%
  roc_curve(truth = product, .pred_Credit) %>%
  autoplot() +
  labs(
    color = NULL,
    title = "ROC curve for US Consumer Finance Complaints",
    subtitle = "Each resample fold is shown in a different color")

#Compare with null model
conf_mat_resampled(nb_rs, tidy = FALSE) %>%
  autoplot(type = "heatmap")

null_classification <- null_model() %>%
  set_engine("parsnip") %>%
  set_mode("classification")
null_rs <- workflow() %>%
  add_recipe(complaints_rec) %>%
  add_model(null_classification) %>%
  fit_resamples(
    complaints_folds )


null_rs %>%
  collect_metrics()

#COmpare with lasso class. model which is just a cool linear regression model 
#that also performs variable selection
lasso_spec <- logistic_reg(penalty = 0.01, mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")
lasso_spec

lasso_wf <- workflow() %>%
  add_recipe(complaints_rec) %>%
  add_model(lasso_spec)
lasso_wf
lasso_rs_predictions %>%
  group_by(id) %>%
  roc_curve(truth = product, .pred_Credit) %>%
  autoplot() +
  labs(
    color = NULL,
    title = "ROC curve for US Consumer Finance Complaints",
    subtitle = "Each resample fold is shown in a different color")

conf_mat_resampled(lasso_rs, tidy = FALSE) %>%
  autoplot(type = "heatmap")
#Tuning this lasso hyperparam.

tune_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")
tune_spec
lambda_grid <- grid_regular(penalty(), levels = 30)
lambda_grid

tune_wf <- workflow() %>%
  add_recipe(complaints_rec) %>%
  add_model(tune_spec)
set.seed(2020)
tune_rs <- tune_grid(
  tune_wf,
  complaints_folds,
  grid = lambda_grid,
  control = control_resamples(save_pred = TRUE)
)
tune_rs
collect_metrics(tune_rs)
autoplot(tune_rs) +
  labs( title = "Lasso model performance across regularization penalties", subtitle = "Performance metrics can be used to identity the best penalty")


tune_rs %>%
  show_best("roc_auc"
)
chosen_auc <- tune_rs %>%
  select_by_one_std_err(metric = "roc_auc", -penalty)
chosen_auc
final_lasso <- finalize_workflow(tune_wf, chosen_auc)
final_lasso

final_lasso <- finalize_workflow(tune_wf, chosen_auc)
final_lasso

fitted_lasso %>%
  pull_workflow_fit() %>%
  tidy() %>%
  arrange(-estimate)
fitted_lasso %>%
  pull_workflow_fit() %>%
  tidy() %>%
  arrange(estimate)

#7.5 case study: sparse encoding --------------------------------------------


