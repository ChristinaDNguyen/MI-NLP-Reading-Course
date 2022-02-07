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

fir_tree_counts







