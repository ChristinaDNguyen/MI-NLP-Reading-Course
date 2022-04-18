#============================================================================= #
#'A mere maiden': Exploring Lúthien Tinúviel's  relationship with dance and song
#with tf-idf scores and fuzzy matching 
#============================================================================= #

# We are using the tidytext format.

#Section 1: preliminary word frequency tests ---------------------------------------

library(dplyr)
library(tidytext)

taleoftinuviel <- scan("C://Users//chris//Downloads//TaleofTinuviel.txt", what = "character", sep = " ")
taleoftinuviel 
taleoftinuviel_df <- tibble(line = 1:4927, text = taleoftinuviel)
taleoftinuviel_df #This is the step 1 of the tokenized text, where each word is in a neat table's cell.
taleoftinuviel_df %>%
  unnest_tokens(word, text) #This is the step 2 (last step) of the tokenized text.

#Rename the df as "tidy"
taleoftinuviel_tidy  <- taleoftinuviel_df 

#Ask it to sort by each word that is in the column of data called "text"
taleoftinuviel_tidy %>% 
  count(text, sort = TRUE)

#Graph results
library(ggplot2)
taleoftinuviel_tidy %>%
  count(text, sort=TRUE) %>%
  filter(n>30) %>%
  mutate(text = reorder(text, n)) %>%
  ggplot(aes(text, n)) +
  geom_col() + 
  xlab(NULL) +
  coord_flip() +
  ggtitle("Most frequently occuring words in TOLT") + 
  labs(y = "word frequency") +
  labs(x = "word") 
 
#We see that (aside from the stopwords), the most frequently occurring words, and potentially
#themes, are Tinuviel and Beren. 


#Section 2: tf-idf scoring ----------------------------------------------------


#Section 3: fuzzy matching (ngrams) -------------------------------------------