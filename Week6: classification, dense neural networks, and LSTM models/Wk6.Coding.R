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

library(hardhat)
sparse_bp <- default_recipe_blueprint(composition = "dgCMatrix")

sparse_wf <- workflow() %>%
  add_recipe(complaints_rec, blueprint = sparse_bp) %>%
  add_model(tune_spec)
sparse_wf

#20 levels
smaller_lambda <- grid_regular(penalty(range = c(-5, 0)), levels = 20)
smaller_lambda

#EACH possible reg param is tested on EACH resampling fold (that's a lot)

set.seed(2020)
sparse_rs <- tune_grid(
  sparse_wf,
  complaints_folds,
  grid = smaller_lambda
)
sparse_rs

sparse_rs %>%
  show_best("roc_auc")
#Because we use a sparse method, the model fitting this time around was much
#faster by 10%. This is important to consider when developing own projects.

#7.6 Two-class or multiclass? (binary Y/N or multiclass) ----------------------

#Yeah let's ramp it up! 

set.seed(1234)
multicomplaints_split <- initial_split(complaints, strata = product)
multicomplaints_train <- training(multicomplaints_split)
multicomplaints_test <- testing(multicomplaints_split)

multicomplaints_train %>%
  count(product, sort = TRUE) %>%
  select(n, product)

multicomplaints_train %>%
  count(product, sort = TRUE) %>%
  select(n, product)
step_tokenize(consumer_complaint_narrative) %>%
  step_tokenfilter(consumer_complaint_narrative, max_tokens =1e3) %>%
  step_tfidf(consumer_complaint_narrative) %>%
  step_downsample(product)

#new crossvalidation
multicomplaints_folds <- vfold_cv(multicomplaints_train)

#can't use same lasso method from binary since that only works for binary
#need new tuning method

multi_spec <- multinom_reg(penalty = tune(), mixture = 1) %>%
  set_mode("classification") %>%
  set_engine("glmnet")
multi_spec

multi_lasso_wf <- workflow() %>%
  add_recipe(multicomplaints_rec, blueprint = sparse_bp) %>%
  add_model(multi_spec)
multi_lasso_wf

#output performance values to see how well it fit
multi_lasso_rs <- tune_grid(
  multi_lasso_wf,
  multicomplaints_folds,
  grid = smaller_lambda,
  control = control_resamples(save_pred = TRUE)
)
multi_lasso_rs #best fit is?
best_acc <- multi_lasso_rs %>%
  show_best("accuracy")
best_acc #in binary class, there's one right and one wrong, here there's one right and 8 wrong
#confused about what a confusion matrix is? watch https://www.youtube.com/watch?v=Kdsp6soqA7o&ab_channel=StatQuestwithJoshStarmer

#sorry about that, future tina, i know it's hard

#7.7 case study: including NON-TEXT data --------------------------------------
#read in data

#clean it by removeing the original date variable, convert teh new month and 
#DOTW into indicator variables using step_dummy()

more_vars_rec <-
  recipe(product ~ date_received + tags + consumer_complaint_narrative,
         data = complaints_train)
more_vars_rec <-
  recipe(product ~ date_received + tags + consumer_complaint_narrative,
         data = complaints_train)
#account for missing data
more_vars_rec <- more_vars_rec %>%
  step_unknown(tags) %>%
  step_dummy(tags)
#make a recipe

more_vars_rec <- more_vars_rec %>%
  step_tokenize(consumer_complaint_narrative) %>%
  step_tokenfilter(consumer_complaint_narrative, max_tokens = 1e3) %>%
  step_tfidf(consumer_complaint_narrative)

more_vars_wf <- workflow() %>%
  add_recipe(more_vars_rec, blueprint = sparse_bp) %>%
  add_model(tune_spec)
more_vars_wf
#tune workflow with resampled data sets, find value for reg penalty, estimate performance
more_vars_wf <- workflow() %>%
  add_recipe(more_vars_rec, blueprint = sparse_bp) %>%
  add_model(tune_spec)
more_vars_wf

more_vars_rs %>%
  show_best("roc_auc")
finalize_workflow(more_vars_wf,
                  select_best(more_vars_rs, "roc_auc")) %>%
  fit(complaints_train) %>%
  pull_workflow_fit() %>%
  tidy() %>%
  arrange(-abs(estimate)) %>%
  mutate(term_rank = row_number()) %>%
  filter(!str_detect(term, "tfidf"))

#7.8 case study: data censoring
library(tidytext)
complaints_train %>%
  slice(1:1000) %>%
  196 7 Classification
unnest_tokens(trigrams,
              consumer_complaint_narrative, token = "ngrams",
              collapse = NULL) %>%
  count(trigrams, sort = TRUE) %>%
  mutate(censored = str_detect(trigrams, "xx")) %>%
  slice(1:20) %>%
  ggplot(aes(n, reorder(trigrams, n), fill = censored)) +
  geom_col() +
  scale_fill_manual(values = c("grey40", "firebrick")) +
  labs(y = "Trigrams", x = "Count")

top_censored_trigrams <- complaints_train %>%
  slice(1:1000) %>%
  unnest_tokens(trigrams,
                consumer_complaint_narrative, token = "ngrams",
                collapse = NULL) %>%
  count(trigrams, sort = TRUE) %>%
  filter(str_detect(trigrams, "xx")) %>%
  slice(1:25)
plot_data <- complaints_train %>%
  unnest_tokens(trigrams,
                consumer_complaint_narrative, token = "ngrams",
                collapse = NULL) %>%
  right_join(top_censored_trigrams, by = "trigrams") %>%
  count(trigrams, product, .drop = FALSE)
plot_data %>%
  ggplot(aes(n, trigrams, fill = product)) +
  geom_col(position = "fill")

#locate all places we had xx

uncensor <- function(n) {
  as.character(sample(seq(10 ^ (n - 1), 10 ^ n - 1), 1))
}
uncensor_vec <- function(x) {
  locs <- str_locate_all(x, "XX")
  map2_chr(x, locs, ~ {
    for (i in seq_len(nrow(.y))) {
      str_sub(.x, .y[i, 1], .y[i, 2]) <- uncensor(2)
    }
    .x
  })
}
uncensor_vec("In XX/XX/XXXX I leased a XXXX vehicle")

#visualize
complaints_train %>%
  slice(1:1000) %>%
  mutate(text = uncensor_vec(consumer_complaint_narrative)) %>%
  unnest_tokens(trigrams, text, token = "ngrams",
                collapse = NULL) %>%
  count(trigrams, sort = TRUE) %>%
  mutate(censored = str_detect(trigrams, "xx")) %>%
  slice(1:20) %>%
  ggplot(aes(n, reorder(trigrams, n), fill = censored)) +
  geom_col() +
  scale_fill_manual(values = c("grey40", "firebrick")) +
  labs(y = "Trigrams", x = "Count")

#looking for social security numbers in terms of aaa-bb-ccc format *

mutate(censored = str_detect(trigrams, "xx")) %>%
  slice(1:20) %>%
  ggplot(aes(n, reorder(trigrams, n), fill = censored)) +
  geom_col() +
  scale_fill_manual(values = c("grey40", "firebrick")) +
  labs(y = "Trigrams", x = "Count")

#7.9 case study: custom features

credit_cards <- c("my XXXX XXXX XXXX XXXX balance, and XXXX XXXX XXXX XXXX.",
                  "card with number XXXX XXXX XXXX XXXX.",
                  "at XX/XX 2019 my first",
                  "live at XXXX XXXX XXXX XXXX XXXX SC")
str_detect(credit_cards, "XXXX XXXX XXXX XXXX")

str_detect(credit_cards, "[^X] XXXX XXXX XXXX XXXX [^X]") 
#reminder gto self: find false positives in your own data
str_detect(credit_cards, "[^X] +XXXX XXXX XXXX XXXX(\\.| [^X])")

creditcard_indicator <- function(x) {
  str_detect(x, "[^X] +XXXX XXXX XXXX XXXX(\\.| [^X])")
}
creditcard_count <- function(x) {
  str_count(x, "[^X] +XXXX XXXX XXXX XXXX(\\.| [^X])")
}
creditcard_indicator(credit_cards) #t t f f 
creditcard_count(credit_cards)
#just read the next section on credit cards

#7.10 WHAT EVALUATION METRICS ARE APPROPRRIATE? -------------------------------------
#set up new tuning grid for two new classification metrics
nb_rs <- fit_resamples(nb_wf,  complaints_folds,
  metrics = metric_set(recall, precision))

nb_rs_predictions %>%
  recall(product, .pred_class)
nb_rs_predictions %>%
  group_by(id) %>%
  recall(product, .pred_class)
conf_mat_resampled(nb_rs, tidy = FALSE)

 #7.11 The full game of classification ----------------------------------------
complaints_rec_v2 <-
  recipe(product ~ consumer_complaint_narrative, data = complaints_train)
#take out the funs
extract_funs <- list(creditcard_count = creditcard_count,
                     percent_censoring = percent_censoring,
                     max_money = max_money)
complaints_rec_v2 <- complaints_rec_v2 %>%
  step_mutate(narrative_copy = consumer_complaint_narrative) %>%
  step_textfeature(narrative_copy, extract_functions = extract_funs
                   
                   complaints_rec_v2 <- complaints_rec_v2 %>%
                     step_tokenize(consumer_complaint_narrative) %>%
                     step_tokenfilter(consumer_complaint_narrative,
                                      max_tokens = tune(), min_times = 100) %>%
                     step_tfidf(consumer_complaint_narrative)
                   
#specify the model
#reuse some bits
                   sparse_wf_v2 <- sparse_wf %>%
                     update_recipe(complaints_rec_v2, blueprint = sparse_bp)
                   sparse_wf_v2

#combo
                   final_grid <- grid_regular(
                     penalty(range = c(-4, 0)),
                     max_tokens(range = c(1e3, 3e3)),
                     levels = c(penalty = 20, max_tokens = 3)
                   )
                   final_grid
  #tuning grid
                   set.seed(2020)
                   tune_rs <- tune_grid(
                     sparse_wf_v2,
                     complaints_folds,
                     grid = final_grid,
                     metrics = metric_set(accuracy, sensitivity, specificity)
                   )
                   
                   
#Finally we evaluate
                   autoplot(tune_rs) +
                     labs(
                       color = "Number of tokens",
                       title = "Model performance across regularization penalties and tokens",
                       subtitle = paste("We can choose a simpler model with higher regularization"))
                   
                   choose_acc <- tune_rs %>%
                     select_by_pct_loss(metric = "accuracy", -penalty)
                   choose_acc
                   final_wf <- finalize_workflow(sparse_wf_v2, choose_acc)
                   final_wf
                   
                   
        #fit it
           final_fitted <- last_fit(final_wf, complaints_split)
collect_metrics(final_fitted)

#conf table
collect_predictions(final_fitted) %>%
  conf_mat(truth = product, estimate = .pred_class) %>%
  autoplot(type = "heatmap")
collect_predictions(final_fitted) %>%
  roc_curve(truth = product, .pred_Credit) %>%
  autoplot() +
  labs(color = NULL, title = "ROC curve for US Consumer Finance Complaints",
    subtitle = "With final tuned lasso regularized classifier on the test set")

library(vip)
complaints_imp <- pull_workflow_fit(final_fitted$.workflow[[1]]) %>%
  vi(lambda = choose_acc$penalty)
complaints_imp %>%
  mutate(
    Sign = case_when(Sign == "POS" ~ "Less about credit reporting",
                     Sign == "NEG" ~ "More about credit reporting"),
    Importance = abs(Importance),
    Variable = str_remove_all(Variable, "tfidf_consumer_complaint_narrative_"),
    Variable = str_remove_all(Variable, "textfeature_narrative_copy_")
  ) %>%
  group_by(Sign) %>%
  top_n(20, Importance) %>%
  ungroup %>%
  ggplot(aes(x = Importance,
             y = fct_reorder(Variable, Importance),
             fill = Sign)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  facet_wrap(~Sign, scales = "free") +
  labs(
    y = NULL,
    title = "Variable importance for predicting the topic of a CFPB complaint",
    subtitle = paste0("These features are the most important in predicting\n",
                      "whether a complaint is about credit or not")
  )

complaints_bind <- collect_predictions(final_fitted) %>%
  bind_cols(complaints_test %>% select(-product))
complaints_bind %>%
  filter(product == "Credit", .pred_Credit < 0.2) %>%
  select(consumer_complaint_narrative) %>%
  slice_sample(n = 10)
complaints_bind %>%
  filter(product == "Other", .pred_Credit > 0.8) %>%
  select(consumer_complaint_narrative) %>%
  slice_sample(n = 10)

#CHAPTER 8: DENSE NEURAL NETWORKS =====================================================

#This is the first chapter of 'deep learning methods.' In the previous ML
#chapters, we used algorithms to predict outcomes. DL models approach to same 
#tasks and have the same goals, but the algorithms used are different. They 
#have MULTIPLE LAYERS to learn how to map from INPUT to OUTPUT. The previous
#two chapters were SHALLOW learning (1 layer). 

#Bringing in the data from Kickstarter

library(tidyverse)
kickstarter <- read_csv("C://Users//chris//Downloads//kickstarter.csv.gz")
kickstarter

#What is the distribution of characters?

kickstarter %>%
  ggplot(aes(nchar(blurb))) +
  geom_histogram(binwidth = 1, alpha = 0.8) +
  labs(x="Number of characters per campaign blurb", y = "Number of campaign blurb")
#Ignore the warning messages about removing two rows containing non-finite values, that's OK

kickstarter %>%
  count(nchar(blurb), sort = TRUE)


#Draw a few random blurbs. Truncated at 135 chars?
set.seed(1)
kickstarter %>%
  filter(nchar(blurb) == 135) %>%
  slice_sample(n = 5) %>%
  pull(blurb) #Nope, it wasn't truncated, that's weird.

#Look at blurbs more than 135 chars... still checking why out data has a weird distribution like we 
#saw in that graph.

set.seed(1)
kickstarter %>%
  filter(nchar(blurb) > 135) %>%
  slice_sample(n=5) %>%
  pull(blurb)



#Create a haeatmap of the length of blurbs and the time the campaign was posted.

kickstarter %>%
  ggplot(aes(created_at, nchar(blurb))) +
  geom_bin2d()+
  labs(x= "Year campaign was posted", y = "Number of characters per campaign blurb")
#This explains why the first graph looked ood. At the end of 2010 there was a 
#policy change in the blurb length that shortens it from 150 characters
#down to 135 ONLY. We confirm that by checking the year-month-date number:

kickstarter %>%
  filter(nchar(blurb)>135) %>%
  summarise(max(created_at))

#8.2 a first deep learning model -------------------------------------------
#Split our data into training and testing sets, as usual
library(tidymodels)
set.seed(1234)
kickstarter_split <- kickstarter %>%
  filter(nchar(blurb) >= 15) %>%
  initial_split()
kickstarter_train <- training(kickstarter_split)
kickstarter_test <- testing(kickstarter_split)



#Preprocessing



kickstarter_train %>%
  mutate(n_words = tokenizers::count_words(blurb)) %>%
  ggplot(aes(n_words)) +
  geom_bar() +
  labs(x = "Number of words per campaign blurb",
       y = "Number of campaign blurbs")
#put in 20k words for our covab and our threshhold max_length will be 30 words
library(textrecipes)
max_words <- 2e4
max_length <- 30
kick_rec <- recipe(~ blurb, data = kickstarter_train) %>%
  step_tokenize(blurb) %>%
  step_tokenfilter(blurb, max_tokens = max_words) %>%
  step_sequence_onehot(blurb, sequence_length = max_length)
kick_rec #the output of "hot encoding" means the tokenization/preprocessing
#works for deep learning methods

#Sidetrack to understand how the tokenization works in this DL one-hot
#thing that makes it process into a numerical variable which DL understands 
#(as opposed to shallow learning which was just a bag of words)

small_data <- tibble(text = c("Adventure Dice Game", "Spooky Dice Game",
           "Illustrated Book of Monsters",
           "Monsters, Ghosts, Goblins, Me, Myself and I"))
small_spec <- recipe(~ text, data = small_data) %>%
  step_tokenize(text) %>%
  step_sequence_onehot(text, sequence_length = 6, prefix = "")
prep(small_spec)

prep(small_spec) %>%
  tidy(2)
prep(small_spec) %>%
  bake(new_data = NULL, composition = "matrix")


recipe(~ text, data = small_data) %>%
  step_tokenize(text) %>%
  step_sequence_onehot(text, sequence_length = 6, prefix = "",
                       padding = "pre", truncating = "post") %>%
  prep() %>%
  bake(new_data = NULL, composition = "matrix")

#leftalign

recipe(~ text, data = small_data) %>%
  step_tokenize(text) %>%
  step_sequence_onehot(text, sequence_length = 6, prefix = "",
                       padding = "post", truncating = "post") %>%
  prep() %>%
  bake(new_data = NULL, composition = "matrix")


kick_prep <- prep(kick_rec)
kick_train <- bake(kick_prep, new_data = NULL, composition = "matrix")
dim(kick_train)
#Now we can finnaly use the keras package to make our very first model!


library(keras) 
#WAIT, before you go on, you have to open the windows
#command prompt, and use pip to install the tensorflow package into PYTHON
#because this package is an r package that depends on PYTHON.
#https://www.youtube.com/watch?v=OJnxX8K8vbY&ab_channel=ProgrammingFever
#i know life is confusing

install.packages("tensorflow")


dense_model <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_words + 1,
                  output_dim = 12,
                  input_length = max_length) %>%
  layer_flatten() %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")
dense_model

dense_model %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)
dense_history <- dense_model %>%
  fit(
    x = kick_train,
    y = kickstarter_train$state,
    batch_size = 512,
    epochs = 20,
    validation_split = 0.25,
    verbose = FALSE
  )


plot(dense_history)

set.seed(234)
kick_val <- validation_split(kickstarter_train, strata = state)
kick_val

kick_analysis <- bake(kick_prep, new_data = analysis(kick_val$splits[[1]]),
                      composition = "matrix")
dim(kick_analysis)
kick_assess <- bake(kick_prep, new_data = assessment(kick_val$splits[[1]]),
                    composition = "matrix")
dim(kick_assess)

state_analysis <- analysis(kick_val$splits[[1]]) %>% pull(state)
state_assess <- assessment(kick_val$splits[[1]]) %>% pull(state)
dense_model <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_words + 1,
                  output_dim = 12,
                  input_length = max_length) %>%
  layer_flatten() %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")
dense_model %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy", metrics = c("accuracy"))


val_history <- dense_model %>%
  fit(
    x = kick_analysis,
    y = state_analysis,
    batch_size = 512,
    epochs = 10,
    validation_data = list(kick_assess, state_assess),
    verbose = FALSE)
val_history

plot(val_history)

library(dplyr)
keras_predict <- function(model, baked_data, response) {
  predictions <- predict(model, baked_data)[, 1]
  tibble(
    .pred_1 = predictions,
    .pred_class = if_else(.pred_1 < 0.5, 0, 1),
    
    state = response  ) %>%
    mutate(across(c(state, .pred_class), ## create factors
                  ~ factor(.x, levels = c(1, 0)))) ## with matching levels
}
val_res <- keras_predict(dense_model, kick_assess, state_assess)
val_res
metrics(val_res, state, .pred_class)


val_res %>%
  conf_mat(state, .pred_class) %>%
  autoplot(type = "heatmap")


val_res %>%
  roc_curve(truth = state, .pred_1) %>%
  autoplot() +
  labs(   title = "Receiver operator curve for Kickstarter blurbs" ) #no xy apparently

#Bag-of-words

kick_bow_rec <- recipe(~ blurb, data = kickstarter_train) %>%
  step_tokenize(blurb) %>%
  step_stopwords(blurb) %>%
  
  step_tokenfilter(blurb, max_tokens = 1e3) %>%
  step_tf(blurb)

kick_bow_prep <- prep(kick_bow_rec)
kick_bow_analysis <- bake(kick_bow_prep,
                          new_data = analysis(kick_val$splits[[1]]),
                          composition = "matrix")
kick_bow_assess <- bake(kick_bow_prep,
                        new_data = assessment(kick_val$splits[[1]]),
                        composition = "matrix")
bow_model <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = "relu", input_shape = c(1e3)) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")
bow_model %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)
bow_history <- bow_model %>%
  fit(
    x = kick_bow_analysis,
    y = state_analysis,
    batch_size = 512,
    epochs = 10,
    validation_data = list(kick_bow_assess, state_assess),
    verbose = FALSE
  )
bow_history

bow_res <- keras_predict(bow_model, kick_bow_assess, state_assess)
metrics(bow_res, state, .pred_class)
library(textdata)
glove6b <- embedding_glove6b(dimensions = 50) %>% select(1:13)
glove6b
tidy(kick_prep)
tidy(kick_prep)
tidy(kick_prep, number = 3)

#account for out-of-vocab words
glove6b_matrix <- tidy(kick_prep, 3) %>%
  select(token) %>%
  left_join(glove6b, by = "token") %>%
  mutate_all(replace_na, 0) %>%
  select(-token) %>%
  as.matrix() %>%
  rbind(0, .)
dense_model_pte <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_words + 1,
                  output_dim = ncol(glove6b_matrix),
                  input_length = max_length) %>%
  layer_flatten() %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

dense_model_pte %>%
  get_layer(index = 1) %>%
  set_weights(list(glove6b_matrix)) %>%
  freeze_weights()
dense_model_pte %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)
dense_pte_history <- dense_model_pte %>%
  fit(
    x = kick_analysis,
    y = state_analysis,
    batch_size = 512,
    epochs = 20,
    validation_data = list(kick_assess, state_assess),   verbose = FALSE )
dense_pte_history

pte_res <- keras_predict(dense_model_pte, kick_assess, state_assess)
metrics(pte_res, state, .pred_class)

dense_model_pte2 <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_words + 1,
                  output_dim = ncol(glove6b_matrix),
                  input_length = max_length) %>%
  layer_flatten() %>%
  layer_dense(units = 32, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

dense_model_pte2 %>%
  get_layer(index = 1) %>%
  set_weights(list(glove6b_matrix))

dense_model_pte2 %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)
dense_pte2_history <- dense_model_pte2 %>% fit(
  x = kick_analysis,
  y = state_analysis,
  batch_size = 512,
  epochs = 20,
  validation_data = list(kick_assess, state_assess),
  verbose = FALSE
)

#evaluate
pte2_res <- keras_predict(dense_model_pte2, kick_assess, state_assess)
metrics(pte2_res, state, .pred_class)
#cross-validate (is it 10 times?)
set.seed(345)
kick_folds <- vfold_cv(kickstarter_train, v = 5)
kick_folds

fit_split <- function(split, prepped_rec) {
  ## preprocessing
  x_train <- bake(prepped_rec, new_data = analysis(split),
                  composition = "matrix")
  x_val <- bake(prepped_rec, new_data = assessment(split),
                composition = "matrix")
  ## create model
  y_train <- analysis(split) %>% pull(state)
  y_val <- assessment(split) %>% pull(state)
  mod <- keras_model_sequential() %>%
    layer_embedding(input_dim = max_words + 1,
                    output_dim = 12,
                    input_length = max_length) %>%
    layer_flatten() %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = 1, activation = "sigmoid") %>% compile(
      optimizer = "adam",
      loss = "binary_crossentropy",
      metrics = c("accuracy")
    )
  ## fit model
  mod %>%
    fit(
      x_train,
      y_train,
      epochs = 10,
      validation_data = list(x_val, y_val),
      batch_size = 512,
      verbose = FALSE
    )
  ## evaluate model
  keras_predict(mod, x_val, y_val) %>%
    metrics(state, .pred_class, .pred_1)
}
cv_fitted <- kick_folds %>%
  mutate(validation = map(splits, fit_split, kick_prep))
cv_fitted
cv_fitted %>%
  unnest(validation)
cv_fitted %>%
  unnest(validation) %>%
  group_by(.metric) %>%
  summarize(
    mean = mean(.estimate),
    n = n(),
    std_err = sd(.estimate) / sqrt(n)
  )
#8.4 compare and evaluate all dnn models

all_dense_model_res <- bind_rows(
  val_res %>% mutate(model = "dense"),
  pte_res %>% mutate(model = "pte (locked weights)"),
  pte2_res %>% mutate(model = "pte (not locked weights)"))

all_dense_model_res %>%
  group_by(model) %>%
  metrics(state, .pred_class)

all_dense_model_res %>%
  group_by(model) %>%
  roc_curve(truth = state, .pred_1) %>%
  autoplot() +
  labs(
    title = "Receiver operator curve for Kickstarter blurbs"
  )
kick_test <- bake(kick_prep, new_data = kickstarter_test,
                  composition = "matrix")
final_res <- keras_predict(dense_model, kick_test, kickstarter_test$state)
final_res %>% metrics(state, .pred_class, .pred_1)
kickstarter_bind <- final_res %>%
  bind_cols(kickstarter_test %>% select(-state))
kickstarter_bind %>%
  filter(state == 1, .pred_1 < 0.2) %>%
  select(blurb) %>%
  slice_sample(n = 10)

kickstarter_bind %>%
  filter(state == 0, .pred_1 > 0.8) %>%
  select(blurb) %>%
  slice_sample(n = 10)

#Chapter 9: long short-term memory networks ------------------------------
#Split the data into training and testing sets, as usual.
library(tidyverse)
kickstarter <- read_csv("C://Users//chris//Downloads//kickstarter.csv.gz")
kickstarter

library(tidymodels)
#remember that a seed makes a random model that you can use over and over
set.set(1234)
kickstarter_split <- kickstarter %>%
  filter(nchar(blurb) >= 15) %>%
  mutate(state = as.integer(state)) %>%
  initial_split()
kickstarter_train <- training(kickstarter_split)
kickstarter_test <- testing(kickstarter_split)
#Do the preprocessing, which again is different from the shallow learning
#preprocessing we did in ch.s 6 and 7. It's more similar to what we just
#did in ch. 8
library(textrecipes)
max_words <- 2e4
max_length <- 30
kick_rec <- recipe(~ blurb, data = kickstarter_train) %>%
  step_tokenize(blurb) %>%
  step_tokenfilter(blurb, max_tokens = max_words) %>%
  step_sequence_onehot(blurb, sequence_length = max_length)

kick_prep <- prep(kick_rec)
kick_train<- bake(kick_prep, new_data = NULL, composition = "matrix")
dim(kick_train) #output gives us 202092, 30
#Also remember that keras uses matrices (dfm?) rather than dataframes or tibbles
#(but I guess it's still tidy, or works tidily, even though it doesn't use tibbles)

library(keras)
lstm_mod <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_words + 1, output_dim = 32) %>%
  layer_lstm(units = 32) %>%
  layer_dense(units = 1, activation = "sigmoid")
lstm_mod
lstm_mod %>%
  compile(
    optimizer = "adam",
    loss = "binary_crossentropy",
    metrics = c("accuracy"))
#fit the model. later we evaluate th emodel.
lstm_history <- lstm_mod %>%
  fit(
    kick_train,
    kickstarter_train$state,
    epochs = 10,
    validation_split = 0.25,batch_size = 512, verbose = FALSE )
lstm_history

#Evaluate now

set.seed(234)
kick_val <- validation_split(kickstarter_train, strata = state)
kick_val
kick_analysis <- bake(kick_prep, new_data = analysis(kick_val$splits[[1]]),
                      composition = "matrix")
dim(kick_analysis)
kick_assess <- bake(kick_prep, new_data = assessment(kick_val$splits[[1]]),
                    composition = "matrix")
dim(kick_assess)
state_analysis <- analysis(kick_val$splits[[1]]) %>% pull(state)
state_assess <- assessment(kick_val$splits[[1]]) %>% pull(state)
lstm_mod <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_words + 1, output_dim = 32) %>%
  layer_lstm(units = 32, dropout = 0.4, recurrent_dropout = 0.4) %>%
  layer_dense(units = 1, activation = "sigmoid")
lstm_mod %>%
  compile(
    optimizer = "adam",
    loss = "binary_crossentropy",
    metrics = c("accuracy")
  )
val_history <- lstm_mod %>%
  fit(
    kick_analysis,
    state_analysis,
    epochs = 10,
    validation_data = list(kick_assess, state_assess),
    batch_size = 512,
    verbose = FALSE
  )
val_history

plot(val_history)


#make a tibble instead of matrix
val_res <- keras_predict(lstm_mod, kick_assess, state_assess)
val_res %>% metrics(state, .pred_class, .pred_1)
val_res %>%
  roc_curve(state, .pred_1) %>%
  autoplot()

rnn_mod <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_words + 1, output_dim = 32) %>%
  layer_simple_rnn(units = 32, dropout = 0.4, recurrent_dropout = 0.4) %>%
  layer_dense(units = 1, activation = "sigmoid")
rnn_mod %>%
  compile(
    optimizer = "adam",
    loss = "binary_crossentropy",
    metrics = c("accuracy")
  )
rnn_history <- rnn_mod %>%
  fit(
    kick_analysis,
    state_analysis,
    epochs = 10,
    validation_data = list(kick_assess, state_assess),
    batch_size = 512,
    verbose = FALSE
  )
rnn_history
plot(rnn_history)

#Case study with bidirectional
bilstm_mod <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_words + 1, output_dim = 32) %>%
  bidirectional(layer_lstm(units = 32, dropout = 0.4,
                           recurrent_dropout = 0.4)) %>%
  layer_dense(units = 1, activation = "sigmoid")
bilstm_mod %>%
  compile(
    optimizer = "adam",
    loss = "binary_crossentropy",
    metrics = c("accuracy")
  )
bilstm_history <- bilstm_mod %>%
  fit(kick_analysis, state_analysis, epochs = 10, validation_data = list(kick_assess, state_assess),
    batch_size = 512,
    verbose = FALSE )
bilstm_history

bilstm_res <- keras_predict(bilstm_mod, kick_assess, state_assess)
bilstm_res %>% metrics(state, .pred_class, .pred_1)
#another case study: stacking layers of lstm

stacked_mod <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_words + 1, output_dim = 32) %>%
  layer_lstm(units = 32, dropout = 0.4, recurrent_dropout = 0.4,
             return_sequences = TRUE) %>%
  layer_lstm(units = 32, dropout = 0.4, recurrent_dropout = 0.4) %>%
  layer_dense(units = 1, activation = "sigmoid")
stacked_mod %>%
  compile(
    optimizer = "adam",
    loss = "binary_crossentropy",
    metrics = c("accuracy")
  )
stacked_history <- stacked_mod %>%
  fit(
    kick_analysis,
    state_analysis,
    epochs = 10,
    validation_data = list(kick_assess, state_assess),
    batch_size = 512,
    verbose = FALSE
  )
stacked_history
stacked_res <- keras_predict(stacked_mod, kick_assess, state_assess)
stacked_res %>% metrics(state, .pred_class, .pred_1)

#another case study with padding
padding_rec <- recipe(~ blurb, data = kickstarter_train) %>%
  step_tokenize(blurb) %>%
  step_tokenfilter(blurb, max_tokens = max_words) %>%
  step_sequence_onehot(blurb, sequence_length = max_length, padding = "post")
padding_prep <- prep(padding_rec)
padding_matrix <- bake(padding_prep, new_data = NULL, composition = "matrix")
dim(padding_matrix)
pad_analysis <- bake(padding_prep, new_data = analysis(kick_val$splits[[1]]),
                     composition = "matrix")
pad_assess <- bake(padding_prep, new_data = assessment(kick_val$splits[[1]]),
                   composition = "matrix")

padding_mod <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_words + 1, output_dim = 32) %>%
  layer_lstm(units = 32, dropout = 0.4, recurrent_dropout = 0.4) %>%
  layer_dense(units = 1, activation = "sigmoid")
padding_mod %>%
  compile(
    optimizer = "adam",
    loss = "binary_crossentropy",
    metrics = c("accuracy")
  )
padding_history <- padding_mod %>%
  fit(
    pad_analysis,
    state_analysis,
    epochs = 10,
    validation_data = list(pad_assess, state_assess),
    batch_size = 512,
    verbose = FALSE
  )
padding_history

#case study: training a regression model (ooooh! very interesting and usedful for 
#future research projects)

library(scotus)
set.seed(1234)
scotus_split <- scotus_filtered %>%
  mutate(
    year = (as.numeric(year) - 1920) / 50,
    text = str_remove_all(text, "'")
  ) %>%
  initial_split(strata = year)
scotus_train <- training(scotus_split)
scotus_test <- testing(scotus_split)
max_words <- 2e4
max_length <- 1e3
scotus_rec <- recipe(~ text, data = scotus_train) %>%
  step_tokenize(text) %>%
  step_tokenfilter(text, max_tokens = max_words) %>%
  step_sequence_onehot(text, sequence_length = max_length)
scotus_prep <- prep(scotus_rec)
scotus_train_baked <- bake(scotus_prep,
                           new_data = scotus_train,
                           composition = "matrix")
scotus_test_baked <- bake(scotus_prep,
                          new_data = scotus_test,
                          composition = "matrix"
                          dim(scotus_train_baked)

                          scotus_mod <- keras_model_sequential() %>%
                            layer_embedding(input_dim = max_words + 1, output_dim = 64) %>%
                            layer_lstm(units = 32, dropout = 0.4, recurrent_dropout = 0.4) %>%
                            layer_dense(units = 1)
                          scotus_mod %>%
                            compile(
                              optimizer = "adam",
                              loss = "mse",
                              metrics = c("mean_squared_error")
                            )
                          scotus_history <- scotus_mod %>%
                            fit(
                              scotus_train_baked,
                              scotus_train$year,
                              epochs = 10,
                              validation_split = 0.25,
                              verbose = FALSE
                            )
#check
 scotus_res <- tibble(year = scotus_test$year,
                                               .pred = predict(scotus_mod, scotus_test_baked)[, 1]) %>%
 mutate(across(everything(), ~ . * 50 + 1920))
                          scotus_res %>% metrics(year, .pred)
                          
#case study: vocab size - very large
                          max_words <- 1e4 #ie4 meaning 1000, the scientific notation
                          max_length <- 30
                          smaller_rec <- recipe(~ blurb, data = kickstarter_train) %>%
                            step_tokenize(blurb) %>%
                            step_tokenfilter(blurb, max_tokens = max_words) %>%
                            step_sequence_onehot(blurb, sequence_length = max_length)
                          kick_prep <- prep(smaller_rec)
                          kick_analysis <- bake(kick_prep, new_data = analysis(kick_val$splits[[1]]),
                                                composition = "matrix")
                          kick_assess <- bake(kick_prep, new_data = assessment(kick_val$splits[[1]]),
                                              composition = "matrix")
                          smaller_mod <- keras_model_sequential() %>%
                            layer_embedding(input_dim = max_words + 1, output_dim = 32) %>%
                            layer_lstm(units = 32, dropout = 0.4, recurrent_dropout = 0.4) %>%
                            layer_dense(units = 1, activation = "sigmoid")
                          smaller_mod %>%
                            compile(
                              optimizer = "adam",
                              loss = "binary_crossentropy",
                              metrics = c("accuracy")
                            )
                          smaller_history <- smaller_mod %>%
                            fit(
                              kick_analysis,
                              state_analysis,
                              epochs = 10,
                              validation_data = list(kick_assess, state_assess),
                              batch_size = 512,
                              verbose = FALSE
                            )
                          smaller_history
                          smaller_res <- keras_predict(smaller_mod, kick_assess, state_assess)
                          smaller_res %>% metrics(state, .pred_class, .pred_1)
#The full game of lstm
                          max_words <- 2e4
                          max_length <- 30
                          kick_rec <- recipe(~ blurb, data = kickstarter_train) %>%
                            step_tokenize(blurb) %>%
                            step_tokenfilter(blurb, max_tokens = max_words) %>%
                            step_sequence_onehot(blurb, sequence_length = max_length)
                          kick_prep <- prep(kick_rec)
                          kick_train <- bake(kick_prep, new_data = NULL, composition = "matrix")
                          dim(kick_train)
                          
                          final_mod <- keras_model_sequential() %>%
                            layer_embedding(input_dim = max_words + 1, output_dim = 32) %>%
                            bidirectional(layer_lstm(
                              units = 32, dropout = 0.4, recurrent_dropout = 0.4,
                              return_sequences = TRUE
                            )) %>%
                            bidirectional(layer_lstm(
                              units = 32, dropout = 0.4, recurrent_dropout = 0.4,
                              return_sequences = TRUE
                            )) %>%
                            bidirectional(layer_lstm(
                              units = 32, dropout = 0.4, recurrent_dropout = 0.4
                            )) %>%
                            layer_dense(units = 1, activation = "sigmoid")
                          final_mod %>%
                            compile(
                              optimizer = "adam",
                              loss = "binary_crossentropy",
                              metrics = c("accuracy")
                            )
                          final_history <- final_mod %>%
                            fit(
                              kick_train,
                              kickstarter_train$state,
                              epochs = 10,
                              validation_split = 0.1,
                              batch_size = 512,
                              verbose = FALSE
                            )
                          final_history
                          
                          #roc curve again with TEST data
                          final_res %>%
                            roc_curve(state, .pred_1) %>%
                            autoplot()
                          final_res %>%
                            conf_mat(state, .pred_class) %>%
                            autoplot(type = "heatmap") #heatmap aka confusion matrix?
