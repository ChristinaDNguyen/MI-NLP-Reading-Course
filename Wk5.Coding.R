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
#and "tree's" occurs once.

