#======================== Chapter 2: Tokenization =============================

#2.1 Importing the story -------------------------------------------------------

#Load in the necessary packages for analyzing Hans Christian Andersen
#and import the story called "The Fir-Tree." We put the story into a 
#variable called "the_fir_tree." For the importing command(s), we should type
#each of the three lines and hit enter those three times, and then at the end
#we go back to the top of the three and hit "enter" again to run all of them
#together (otherwise we will get an error). The "%>%" just means that all 
#of the commands are tied together.

library(tokenizers)
library(tidyverse)
library(tidytext)
library(hcandersenr)

the_fir_tree <- hcandersen_en %>%
  filter(book == "The fir tree") %>%
  pull(text)

#Now we ask R to pull out the "head" or the "top" 9 lines from the story.

head(the_fir_tree, 9)

#Now we ask R to tokenize the first two lines of "The Fir Tree" story.
#That means we use some regex to split the lines *by any character that is not
#in the alphabet or the numbers.* That will mean to R: "look for any character 
#that is not abc... or 123... and make the split there." Usually this makes the
#split at space " " characters.

strsplit(the_fir_tree[1:2], "[^a-zA-Z0-9]+")

#There was some unwanted splitting in this case. For example, the "fir-tree"
#would have been better if it was considered one word, rather than "fir" "tree,"
#since it is the hero of our story. So instead, we use the library 
#package called "tokenizers" which is better at consistently splitting the way 
#we want it to. It will tokenize in this case the first 2 sentences again.

library(tokenizers)
tokenize_words(the_fir_tree[1:2])

#Sadly "fir-tree" is still two words, not one. But the textbook doesn't seem to
#care about our poor friend the fir-tree. Anyways, in this part of the code,
#the textbook is explaining how tokenizers define what a "word" is and how
#it decides where to split up text.

#What this shows though, is that your choice of tokenizer will influence your
#results (how the words end up being tokenized), so don't be afraid
#to experiment with different tokenizers or, if necessary, to write your own
#to solve your problems.

#2.2 Types of tokens ----------------------------------------------------------

#Now we will explore using the tokenizer package to tokenize even more. We'll
#manually feed it a couple of phrases to work on.

sample_vector <- c("Far down in the forest", "grew a pretty little fir-tree")
sample_tibble <- tibble(text = sample_vector)
tokenize_words(sample_vector)
#Or another way to tokenize it to words is using the tidytext package's functions
#instead of the tokenizer package.

sample_tibble %>%
  unnest_tokens(word, text, token = "words")

#You can also make the tidytext tokens speak to the tokenizer package's tokens
#by doing:

sample_tibble %>%
  unnest_tokens(word, text, token = "words", strip_punct = FALSE)

#We can also try to split the text into characters, if that's what our research
#question requires. We are telling it, tokenize into characters, use the story called
#"The Fir-Tree", make everything lowercase, remove all non-alphanumeric characters,
#and don't simplify (e.g. if c occurs twice, don't simplify it into one c)

tft_token_characters <- tokenize_characters(x = the_fir_tree, lowercase = TRUE, strip_non_alphanum = TRUE, simplify = FALSE)

#Let's print it out to see how it tokenized into chars.
head(tft_token_characters) %>%
  glimpse()

#We can obviously change the arguments around and ask the tokenize_characters 
#command to lowercase = FALSE (meaning we keep the capital letters), or change
#the other arguments around too.

#As said before, we can tokenize by words

tft_token_words <- tokenize_words(x= the_fir_tree, lowercase = TRUE, stopwords =NULL, strip_punct =TRUE, strip_numeric = FALSE)
head(tft_token_words) %>%
  glimpse()

#OK, let's use the tidytext method to study the most commonly occurrng words in
#two texts, "The Fir-Tree" and "The Little Mermaid."

hcandersen_en %>%
  filter(book %in% c("The fir tree", "The little mermaid")) %>%
  unnest_tokens(word, text) %>%
  count(book, word) %>%
  group_by(book) %>%
  arrange(desc(n)) %>%
  slice(1:5)
#The output tells us that the top 5 words for each story are ... Most of the words
#it tells us are stop words (the, and, it, a, the, and, of, she, to). We will 
#look more into stopwords in Ch.3.

#Now we will split the text "The Fir Tree" into ngrams using the function 
#tokenize_ngram()

tft_token_ngram <- tokenize_ngrams(x = the_fir_tree, lowercase = TRUE, n = 3L, n_min = 3L, stopwords = character(), ngram_delim= " ", simplify = FALSE)

#Now we ask it to do the ngram command on just the first line of "The Fir-Tree."
tft_token_ngram[[1]]

#We can also ask R to find both bigrams and unigrams in the same command, simply
#by saying "go from 1L to 2L" as the boundaries.

tft_token_ngram <- tokenize_ngrams(x= the_fir_tree, n =2L, n_min = 1L)
tft_token_ngram[[1]]




#2.3 Where does tokenization break down?


