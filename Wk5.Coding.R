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

#4.5 Lemmatization instead of stemming ----------------------------------------- THIS SECTION IS INCOMPLETE, ASKED CLASSMATES FOR HELP. SPACY WON'T RUN.

#We'll use the package called spacyr to lemmatize 'The Fir-Tree'

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
  


