#========================== Chapter 4: Stemming ===============================

#Often documents contain different versions of the same base word, called here
#a "stem." (E.g. infinitive is 'manger', remove the -er ending to get 'mang-' 
#as the stem). For example, "The Fir-Tree" has more than one version (called
#"inflected form") of the word "Tree."

library(hcandersenr)
library(tidyverse)
library(tidytext)

#Read in the story.
fir_tree <- hca_fairytales() %>%
  filter(book == "The fir tree", language == "English")

#Tidy the data before processing.
tidy_fir_tree <- fir_tree %>%
  unnest_tokens(word,text) %>%
  anti_join(get_stopwords())

#Count how many times "tree" occurs
tidy_fir_tree %>%
count(word, sort = TRUE) %>%
  filter(str_detect(word, "^tree"))

#The output above tells us that "tree" occurs 76 times, "trees" occurs 12 times,
#and "tree's" occurs once. If we wanted to study only the stem, though ("tree")
#and remove any conjugations, we have to do the action called "stemming."
#There are two major ways to do this. We will try them both. Each has its own
#benefits and weaknesses.

#Way 1 of stemming: Porter (SnowballC) method ---------------------------------

library(SnowballC)

tidy_fir_tree %>%
  mutate(stem = wordStem(word)) %>%
  count(stem, sort = TRUE)
#Now we see that we have 88 incidences of "tree", i.e. 76+12+1 (?).

#Let's try Porter stemming for other languages. It supports danish, english,
#french, german, and spanish

#Heh, the trouble with tribbles is that they multiply so fast... 

stopword_df <- tribble(~language, ~two_letter,
                       "danish", "da",
                       "english", "en",
                       "french", "fr",
                       "german", "de",
                       "spanish", "es")

#Then we import all the Hans Christen Andersen fairytales in those multiple
#languages (da, en, fr, de, es).

tidy_by_lang <- hca_fairytales() %>%
  filter(book == "The fir tree") %>%
  select(text, language) %>%
  mutate(language = str_to_lower(language)) %>%
  unnest_tokens(word, text) %>%
  nest(data = word)

#Remove stopwords for all languages and stem with the language-specific Porter
#algorithm. Then we see what the top 20 stems are of "The Fir-Tree" story in all 
#5 languages.

tidy_by_lang %>%
  inner_join(stopword_df) %>%
  mutate(data = map2(data, two_letter, ~ anti_join(.x, get_stopwords(language = .y)))  ) %>%
  unnest(data) %>%
  mutate(stem = wordStem(word, language = language)) %>%
  group_by(language) %>%
  count(stem) %>%
  top_n(20,n) %>%
  ungroup %>%
  ggplot(aes(n, fct_reorder(stem, n), fill = language)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~language, scales = "free_y", ncol = 2) +
  labs(x = "Frequency", y = NULL)

#So the top 20 for French include "sapin-" "petit-" "tout-" "plus-" etc.
#The top 20 for German include "baum-" "klein" "tannenbaum-" etc. That's fairly
#interesting because tannenbaum IS a baum, so there's a bit of morphology
#biases when using German, since German likes complex multi-word words. Also,
#like the textbook says, French has contractions stopwords (e.g. le/la being 
#shortened when attached to words, like "l'abre"), so you don't actually 
#remove all the stopwords! You still have the le/la in there.

#Way #2 of stemming: the Hunspell dictionary method ---------------------------

#This algorithm was initially written to handle Hungarian, but has since
#been expanded to handle many languages with compound words (thank goodness! says
#all the German compound nouns) and complicated morphology.

library(hunspell)
tidy_fir_tree %>%
  mutate(stem = hunspell_stem(word)) %>%
  unnest(stem) %>%
  count(stem, sort = TRUE)

hunspell_stem("discontented")
#We have two stems, apparently, because this stemmer works differently. Not so 
#great for applications I have in mind, but OK. Good to know.

#Why use stemming? An ex from Supreme Court ------------------------------------

#Stemming reduces the feature space of text data. Now we use an example from the 
#US Supreme Court which exists in the scotus package.

#First remove standard stopwords, and see how large the lexicon still is. Too big.

install.packages("remotes")
remotes::install_github("EmilHvitfeldt/scotus")

library(scotus)
tidy_scotus <- scotus_filtered %>%
  unnest_tokens(word, text) %>%
  anti_join(get_stopwords())
tidy_scotus %>%
  count(word, sort = TRUE)

#Let's actually cast the data into a matrix so we can see sparsity

tidy_scotus %>%
  count(case_name, word) %>%
  cast_dfm(case_name, word, n)

#The output takes a few minutes to run (? like soooo long? at least 5 minutes).
#It tells us that the dfm of all the 9000+
#documents has a sparsity of 99.49%. That's sooooo dense, not sparse at all.

#If we stem all these words, it'll make the text less dense and more sparse/
#workable.

tidy_scotus %>%
  mutate(stem = wordStem(word)) %>%
  count(case_name, stem) %>%
  cast_dfm(case_name, stem, n)

#Also, why doesn't it count "tree's" as "tree"?

fir_tree_counts <- fir_tree %>%
  unnest_tokens(word, text, token = "regex", pattern = "\\s+|[[:punct:]]+") %>%
  anti_join(get_stopwords()) %>%
  mutate(stem = wordStem(word)) %>%
  count(stem, sort = TRUE)
fir_tree_counts

  #Much better!
  
#Compare some stemming options -------------------------------------------------


stemming <- tidy_fir_tree %>%
  select(-book, -language) %>%
  mutate(`Remove S` = str_remove(word, "s$"),
         `Plural endings` = case_when(str_detect(word, "[^e|aies$]ies$") ~
                                        str_replace(word, "ies$", "y"),
                                      str_detect(word, "[^e|a|oes$]es$") ~
                                        str_replace(word, "es$", "e"),
                                      str_detect(word, "[^ss$|us$]s$") ~
                                        str_remove(word, "s$"),
                                      TRUE ~ word),
         `Porter stemming` = wordStem(word)) %>%
  rename(`Original word` = word)

stemming %>%
  gather(Type, Result, `Remove S`:`Porter stemming`) %>%
  mutate(Type = fct_inorder(Type)) %>%
  count(Type, Result) %>%
  group_by(Type) %>%
  top_n(20, n) %>%
  ungroup %>%
  ggplot(aes(fct_reorder(Result, n),
             n, fill = Type)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Type, scales = "free_y") +
  coord_flip() +
  labs(x = NULL, y = "Frequency")

stemming %>%
  filter(`Remove S` != `Plural endings`) %>%
  distinct(`Remove S`, `Plural endings`, .keep_all = TRUE)


stemming %>%
  gather(Type, Result, `Remove S`:`Porter stemming`) %>%
  filter(Result %in% c("come", "coming")) %>%
  distinct(`Original word`, Type, Result)

#4.5 Lemmatization instead of stemming ----------------------------------------- THIS SECTION PREVIOUSLY HAD A MAJOR PROBLEM SOLVED BY ROD. IT'S OK NOW.

#We'll use the package called spacyr to lemmatize 'The Fir-Tree'

# spaCy offers pre-trained models in different sizes (complexity / features) for almost any language. 
#Note to future self: I had to download and install Python and Anaconda first
#Basically spacy is a package originally designed for Python, not R,
#so we have to install Python, and install spacy on Python, before
#we can do anything with spacyr. Here's a video on how to install spacy on 
#Python using the pip command 
#https://www.youtube.com/watch?v=MkSc_ElzL6s&ab_channel=ArtificialintelligenceWithDave

#Yeah, it's confusing, but deal with it, Future Me who's reading these notes
#and freaking out. Also, don't worry too much. Everything's gonna be fine if you
#follow the steps.

#In the terminal (NOT console), here's what it looks like C:\Users\chris\Documents>python -m spacy download en


library(spacyr)


spacy_initialize(entity = FALSE)
fir_tree %>%
  mutate(doc_id = paste0("doc", row_number())) %>%
  select(doc_id, everything()) %>%
  spacy_parse() %>%
  anti_join(get_stopwords(), by = c("lemma" = "word")) %>%
  count(lemma, sort = TRUE) %>%
  top_n(20, n) %>%
  ggplot(aes(n, fct_reorder(lemma, n))) +
  geom_col() + labs(x = "Frequency", y = NULL)
  
#4.7 Stemming and stop words -----------------------------------------------

#Process: tokenize -> remove stop words -> stem/lemmatize

library(stopwords)
#Return the words that don't have a stemmed version on the list/package. These
#will usually be your stop words.

not_stemmed_in <- function(x) {  x[!SnowballC::wordStem(x) %in% x]}
not_stemmed_in(stopwords(source = "snowball"))


#======================= Chapter 5: Word embeddings ============================

#What kind of data structure might work well for typical text data?
#We explore using csv file first
library(tidyverse)
library(tidytext)
library(SnowballC)
#Read in a file from my comptuer folder
complaints <- read_csv("C://Users//chris//Downloads//complaints.csv")
#This above command takes a few minutes to load
complaints

complaints %>%
  #We know that in the csv file (excel) there's a column called Consumer Complaint Narrative, so we put that in
  unnest_tokens(word, consumer_complaint_narrative) %>%
  anti_join(get_stopwords(), by = "word") %>%
  mutate(stem = wordStem(word)) %>%
  count(complaint_id, stem) %>%
  cast_dfm(complaint_id, stem, n)
#Can also do weighted, instead of word count alone
complaints %>%
  unnest_tokens(word, consumer_complaint_narrative) %>%
  anti_join(get_stopwords(), by = "word") %>%
  mutate(stem = wordStem(word)) %>%
  count(complaint_id, stem) %>%
  bind_tf_idf(stem, complaint_id, n) %>%
  cast_dfm(complaint_id, stem, tf_idf)

library(lobstr)
get_dfm <- function(frac) { complaints %>%
    sample_frac(frac) %>%
    unnest_tokens(word, consumer_complaint_narrative) %>%
    anti_join(get_stopwords(), by = "word") %>%
    mutate(stem = wordStem(word)) %>%
    count(complaint_id, stem) %>%
    cast_dfm(complaint_id, stem, n)
}
set.seed(123)
tibble(frac = 2 ^ seq(-16, -6, 2)) %>%
  mutate(dfm = map(frac, get_dfm), words = map_dbl(dfm, quanteda::nfeat),  sparsity = map_dbl(dfm, quanteda::sparsity),
         `RAM (in bytes)` = map_dbl(dfm, lobstr::obj_size)) %>%
  pivot_longer(sparsity:`RAM (in bytes)`, names_to = "measure") %>%
  ggplot(aes(words, value, color = measure)) +
  geom_line(size = 1.5, alpha = 0.5) +
  geom_point(size = 2) +
  facet_wrap(~measure, scales = "free_y") +
  scale_x_log10(labels = scales::label_comma()) +
  scale_y_continuous(labels = scales::label_comma()) +
  theme(legend.position = "none") +
  labs(x = "Number of unique words in corpus (log scale)",
       y = NULL)

#5.2 Understanding word embeddings by finding them yourself --------------------
#We'll use a method called matrix factorization, along with word counts, to 
#create a vector for a text (a word embedding vector). Very statisticy.

#Step 1, we will filter out words that are used rarely in thie data set, and
#create a nested dataframe (i.e. one complaint per row)


tidy_complaints <- complaints %>%
  select(complaint_id, consumer_complaint_narrative) %>%
  unnest_tokens(word, consumer_complaint_narrative) %>%
  add_count(word) %>%
  filter(n >= 50) %>%
  select(-n)
nested_words <- tidy_complaints %>%
  nest(words = c(word))
nested_words

slide_windows <-function(tbl, window_size)
  {
  skipgrams <- slider::slide(
    tbl,
    ~.x,
    .after = window_size - 1,
    .step = 1,
    .complete =TRUE
  )
  safe_mutate <- safely(mutate)
  out <- map2(skipgrams,
           1:length(skipgrams),
          ~ safe_mutate(.x, window_id = .y))
  out %>%
    transpose() %>%
    pluck("result") %>%
    compact() %>%
    bind_rows()
}


library(widyr)
library(furrr)
plan(multisession) ## for parallel processing, which means that it's running several windows at once, much faster
tidy_pmi <- nested_words %>%
  mutate(words = future_map(words, slide_windows, 4L))%>%
  unnest(words) %>%
  unite(window_id, complaint_id, window_id) %>%
  pairwise_pmi(word, window_id)
tidy_pmi

tidy_word_vectors <- tidy_pmi %>%
  widely_svd(
    item1, item2, pmi,
    nv = 100, maxit = 1000
  )
tidy_word_vectors

#OK so we get a very long tibble that has the words and their probability values

#5.3 Exploring CFPB word embeddings ---------------------------------------

#nearest words to each other? fuzzy mactching later perhaps?
nearest_neighbors <- function(df, token) 
  {
  df %>%
    widely(
      ~ {
        y <- .[rep(token, nrow(.)), ]
        res <- rowSums(. * y) /
          (sqrt(rowSums(. ^ 2)) * sqrt(sum(.[token, ] ^ 2)))
        matrix(res, ncol = 1, dimnames = list(x = names(res)))
      },
      sort = TRUE
    )(item1, dimension, value) %>%
    select(-item2)
}

tidy_word_vectors %>%
  nearest_neighbors("error")
tidy_word_vectors %>%
  nearest_neighbors("month")
tidy_word_vectors %>%
  filter(dimension <= 24) %>%
  group_by(dimension) %>%
  top_n(12, abs(value)) %>%
  ungroup %>%
  mutate(item1 = reorder_within(item1, value, dimension)) %>%
  ggplot(aes(item1, value, fill = dimension)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~dimension, scales = "free_y", ncol = 4) +
  scale_x_reordered() +
  coord_flip() +
  labs(
    x = NULL,
    y = "Value",
    title = "First 24 principal components for text of CFPB complaints",
    subtitle = paste("Top words contributing to the components that explain",
                     "the most variation")
  )


word_matrix <- tidy_complaints %>%
  count(complaint_id, word) %>%
  cast_sparse(complaint_id, word, n)
embedding_matrix <- tidy_word_vectors %>%
  cast_sparse(item1, dimension, value)
doc_matrix <- word_matrix %*% embedding_matrix
dim(doc_matrix)

#5.4 Use pre-trained word embeddings ---------------------------------------

#If data set is small, you cannot reliably train word embeddings. How small
#is too small? Complicated answer, but generally less than a million words
#or tokens is too small (i.e. one million observations)

#In the cases where the dataset is too small to train, we can still use word 
#embeddings to do analysis/modelling, but we just can't use embeddings
#that we determine ourselves from our down data set. Instead, we use ones that
#other people have already made from bigger datasets. These are called "pre-
#trained" embeddings.
#In this case we will use the GloVe word vectors trained on 6 billion tokens
#from Wikipedia and news sources. We use 100 dimensions.

library(textdata)

glove6b<-embedding_glove6b(dimensions = 100)
glove6b

#transform these word embeddings into a more tidy format, using pivot_longer
#from tidyr; we will also give this tidied version the same
#column names as tidy_word_vectors for convienience (that's what the 'rename' means)

tidy_glove <- glove6b %>%
  pivot_longer(contains("d"), names_to = "dimension") %>%
  rename(item1 = token)
tidy_glove

#Since matrices we are dealing with here are very large, we need to make one
#change to our nearest_neighbors() command and add maximum_size = NULL (i.e. no
#maximum size at all) 

nearest_neighbors <- function(df, token) {
  df %>%
    widely(
      ~ {
        y <- .[rep(token, nrow(.)), ]
        res <- rowSums(. * y) /
          (sqrt(rowSums(. ^ 2)) * sqrt(sum(.[token, ] ^ 2)))
        matrix(res, ncol = 1, dimnames = list(x = names(res)))
      },
      sort = TRUE,
      maximum_size = NULL
    )(item1, dimension, value) %>%
    select(-item2)
}
#Find words that are usually beside the word "error"
tidy_glove %>%
  nearest_neighbors("error")

#Above we spotted some potential sources of error, since sports has a term 
#called "error" which mixes up our accuraacy. This type of test can't
#tell difference between different meanings of the word "error."
#If we design a rigorous research method, we have to account for this source
#of data bias/error.

#Next word we'll try to find  immediate neighbors for is "month"

tidy_glove %>%
  nearest_neighbors("month")

#Next word we'll try to find immediate neighbors for is "fee"

tidy_glove %>%
  nearest_neighbors("fee")


#So a fully worked example of using pre-trained word embeddings in analysis
#looks like....
word_matrix <- tidy_complaints %>%
  inner_join(by = "word",
             tidy_glove %>%
               distinct(item1) %>%
               rename(word = item1)) %>%
  count(complaint_id, word) %>%
  cast_sparse(complaint_id, word, n)
glove_matrix <- tidy_glove %>%
  inner_join(by = "item1",
             tidy_complaints %>%
               distinct(word) %>%
               rename(item1 = word)) %>%
  cast_sparse(item1, dimension, value)
doc_matrix <- word_matrix %*% glove_matrix
dim(doc_matrix)

#5.5 Fairness and word embeddings

#Oh, rest of chapter is non-coding, so I'll stop here.

#Chapter 6: Regression ---------------------------------------------------------
#Let's start by creating a continuos and numeric outcome from the same US 
#Supreme Court data we've used before.

library(tidyverse)
library(scotus)
scotus_filtered %>%
  as_tibble()
#Check how many opinions we have PER DECADE 
scotus_filtered %>%
  mutate(year = as.numeric(year),
         year = 10 * (year %/% 10)) %>%
  count(year) %>%
  ggplot(aes(year, n)) +
  geom_col() +
  labs(x = "Year", y = "Number of opinions per decade")
#Welp, that tells us a lot about bias - some years have a lot more opinoins
#than others, so we have to be careful when interpreting our data, some values
#weigh more heavily than others

#Now we split our data into training AND testing sets. 
#Also, convert the year into a numeric value since it was originally stored
#as a character value
library(tidymodels)
set.seed(1234)
scotus_split <- scotus_filtered %>%
  mutate(year = as.numeric(year),
         text = str_remove_all(text, "'")) %>%
  initial_split()
scotus_train <- training(scotus_split)
scotus_test <- testing(scotus_split)

#Let's preprocess/clean the data a bit. There's several steps here.

library(textrecipes)
scotus_rec <- recipe(year ~ text, data = scotus_train) %>%
  step_tokenize(text) %>%
  step_tokenfilter(text, max_tokens = 1e3) %>%
  step_tfidf(text) %>%
  step_normalize(all_predictors())
scotus_rec
#Now estimate the necessary parameters for each step using the training data and apply
#the steps to the data. This next step with the bake will take 15+ minutes to run...
#I know, it sucks, but stick with it, Future Tina.

scotus_prep <- prep(scotus_rec)
memory.limit()
memory.limit(30000)
scotus_bake <- bake(scotus_prep, new_data = NULL)
dim(scotus_bake)
#To keep things organized, let's create a workflow() to bundle together with
#our recipe with any model specifications we may want to create later. The first
#step in this is to create an empty workflow() and then add the scotus_rec() to it

scotus_wf <- workflow() %>%
  add_recipe(scotus_rec)
scotus_wf

svm_spec <- svm_linear() %>%
  set_mode("regression") %>%
  set_engine("LiblineaR")
svm_fit <- scotus_wf %>%
  add_model(svm_spec) %>%
  fit(data = scotus_train)

svm_fit %>%
  pull_workflow_fit() %>%
  tidy() %>%
  arrange(-estimate)
svm_fit %>%
  pull_workflow_fit() %>%
  tidy() %>%
  arrange(estimate)

#10 fold validation

set.seed(123)
scotus_folds <- vfold_cv(scotus_train)
scotus_fold

set.seed(123) #seed just creates a unique set of random numbers, remember? 
svm_rs <- fit_resamples(
  scotus_wf %>% add_model(svm_spec),
  scotus_folds,
  control = control_resamples(save_pred = TRUE)
)
svm_rs
collect_metrics(svm_rs)

#HEre I marked in the textbook what the regression values mean. E.g. r^2.
#Future Me should check this if Future Me is confused about the numbers.

svm_rs %>%
  collect_predictions() %>%
  ggplot(aes(year, .pred, color = id)) +
  116 6 Regression
geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_point(alpha = 0.3) +
  labs(
    x = "Truth",
    y = "Predicted year",
    color = NULL,
    title = "Predicted and true yrs for SC opinions",
    subtitle = "Each cross-validation fold is shown in a diff colour"  )

#Another way (way 2 of 3, because way three is the forest model) of testing the model is comparing it agianst the null model. We
#wil do that here
null_regression <- null_model() %>%
  set_engine("parsnip") %>%
  set_mode("regression")
null_rs <- fit_resamples(
  scotus_wf %>% add_model(null_regression),
  scotus_folds,
  metrics = metric_set(rmse)
)
null_rs
collect_metrics(null_rs)
#Well bleepbleepbleep, this model sucks bleep. It's not very accurate
#and as the textbook says, even our first model was better

#Another way (3 of 3) is the random forest model

rf_spec <- rand_forest(trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("regression")
rf_spec
rf_rs <- fit_resamples(
  scotus_wf %>% add_model(rf_spec),
  scotus_folds,
  control = control_resamples(save_pred = TRUE)
)

collect_metrics(rf_rs) #ask it to tell us the performance values
#looks good so let suse this

collect_predictions(rf_rs) %>%
  ggplot(aes(year, .pred, color = id)) +
  geom_abline(lty = 2, color = "gray80", size = 1.5) +
  geom_point(alpha = 0.3) +
  labs(
    x = "Truth",
    y = "Predicted year",
    color = NULL,
    title = paste("Predicted and true years for SC opinions using",
                  "a Random Foreast model", sep = "\n"),
    subtitle = "Each cross-validation fold is shown in a diff colour" )

#6.4 Removing stop words ------------------------------------------------------
#Well poop, is every variation of any given exercise in this book going to be
#about removing stop words? :) So it is, and so it shall be!
#I just love removing stop words!

stopword_rec <- function(stopword_name) {
  recipe(year ~ text, data = scotus_train) %>%
    step_tokenize(text) %>%
    step_stopwords(text, stopword_source = stopword_name) %>%
    step_tokenfilter(text, max_tokens = 1e3) %>%
    step_tfidf(text) %>%
    step_normalize(all_predictors())
  stopword_rec("snowball")
  
  svm_wf <- workflow() %>%
    add_model(svm_spec)
  svm_wf
  
  #more cross-validation as before using multiple random seeds
  set.seed(123)
  snowball_rs <- fit_resamples( svm_wf %>% add_recipe(stopword_rec("snowball")),scotus_folds
  )
  set.seed(234)
  smart_rs <- fit_resamples(
    svm_wf %>% add_recipe(stopword_rec("smart")),
    scotus_folds
  )
  set.seed(345)
  stopwords_iso_rs <- fit_resamples(
    svm_wf %>% add_recipe(stopword_rec("stopwords-iso")),
    scotus_folds
  )
  collect_metrics(smart_rs)
  library(ggplot2)
  
  word_counts <- tibble(name = c("snowball", "smart", "stopwords-iso")) %>%
    mutate(words = map_int(name, ~length(stopwords::stopwords(source = .))))
  list(snowball = snowball_rs,
       smart = smart_rs,
       `stopwords-iso` = stopwords_iso_rs) %>%
    map_dfr(show_best, "rmse", .id = "name") %>%
    left_join(word_counts, by = "name") %>%
    mutate(name = paste0(name, " (", words, " words)"),
           name = fct_reorder(name, words)) %>%
    ggplot(aes(name, mean, color = name)) +
    geom_crossbar(aes(ymin = mean - std_err, ymax = mean + std_err), alpha = 0.6) +
    geom_point(size = 3, alpha = 0.8) +
    theme(legend.position = "none") +
    labs(x = NULL, y = "RMSE",
         title = "Model performance for three stop word lexicons",
         subtitle = "For this data set, the Snowball lexicon performed best")
  
  #6.5 Varying n-grams case study ---------------------------------------
  
    