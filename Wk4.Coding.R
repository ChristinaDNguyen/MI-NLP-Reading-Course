#======================== Chapter 2: Tokenization =============================

#1. Load in the necessary packages for analyzing Hans Christian Andersen
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


