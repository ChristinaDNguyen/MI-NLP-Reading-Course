#Read in the text. Print it to check.

text <- c("Because I could not stop for Death -", "He kindly stopped for me -", "The carriage held but just Ourselves -", "and Immortality")
text

#Turn it into a tidy text dataset, by putting it into a dataframe called "text_df"
#then print it out to check that it works

library(dplyr)
text_df <- data_frame(line = 1:4, text = text)
text_df

#Yes, it did put it into a dataframe successfully. It says that it has "a tibble" that is 
#4x2 (4 rows and 2 columns), meaning that it is a modern class of data frame within R. 
#It is available in  the dplyr and tibble packages, offering a convenient print
# method. Tibbles are good for tidy tools.

#We still need to convert this so that we have one token per document per row 
#(i.e. we need each row to be tokenized, to have one word so we can filter out 
#words or count which occur most frequently, v.v.) This is where the 
#"unnest" command comes in, saying "make each WORD a token in the TEXT".

library(tidytext)
text_df %>%
  unnest_tokens(word, text)
#Yes, so now each word in the poem is on its own line, i.e. the text is tokenized.


#______________________________________________________________________

#Let's tidy the works of Jane Austin, a larger corpus. Create a new object 
#called "original_books" and book all her books into that. Use "mutate"
#to change variables types into another. Use regular expressions too.

library(janeaustenr)
library(dplyr)
library(stringr)

original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(), chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup()
original_books

#Now tokenize for one word per row/line

library(tidytext)
tidy_books <- original_books %>%
  unnest_tokens(word, text)
tidy_books

#Remove boring stop words in the text using "anti_join"

data(stop_words)
tidy_books <- tidy_books %>%
  anti_join(stop_words)

#Find most common words in all 6 books as a whole (remember that we removed
#stopwords, so words like "the" and "a" should not show up)

tidy_books %>%
  count(word, sort = TRUE)

#So we see "miss, time, fanny, dear, lady, sir"... Now we use the "ggplot2" package
#to make a graph of the most common words.

library(ggplot2)

tidy_books %>%
  count(word, sort=TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL)+
  coord_flip()

#OK so a graph showed on the right-hand-side of RStudio with all the horizontal bars.
#The bars show the occurrences of each of the top 10 or so words across
#all 6 books.

#______________________________________________________________________

#Now let's introduce the "gutenbergr" package. We will look at word frequencies
#across more than one text. We will use "The Time Machine," "The War of the 
#Worlds," "The Invisible Man," and "The Island of Doctor Moreau" COMBINED
#INTO A SINGLE MEGA FILE (i.e. we're not comparing from book
#to book, only within the megacorpus). Use "gutenberg_download()"
#and the project Gutenberg ID numbers for each novel to read it into R.


library(gutenbergr)

hgwells <- gutenberg_download(c(35, 36, 5230, 159))

tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#Most common words in Wells?

tidy_hgwells %>%
  count(word, sort = TRUE)
#... We see that the top 10 words are "time, people, door, heard, black..."

#Now we use some works from the Bronte sisters: "Jane Eyre, Wuthering Heights, The Tenant
#of Wildfell Hall, Villete, and Agnes Grey." 

bronte <- gutenberg_download(c(1260, 768, 969, 9182, 767))
tidy_bronte <- bronte %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_bronte

#_______________________________________________________________

#Now we compare all three corpuses - Jane Austen, the Brontes, and Wells. We
#bind the data frames together (into a megacorpus, so to speak). We will use
#the "spread" and "gather" commands from "tidyr" package to do this
#binding the three datasets together.

library(tidyr)
frequency <- bind_rows(mutate(tidy_bronte, author = "Bronte sisters"), mutate(tidy_hgwells, author = "H.G. Wells"), mutate(tidy_books, author = "Jane Austen")) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author)%>%
  mutate(proportion = n/sum(n)) %>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, 'Bronte sisters':'H.G. Wells')

#Now we plot it

library(scales)
ggplot(frequency, aes(x = proportion, y = `Jane Austen`, 
                      color = abs(`Jane Austen` - proportion))) +
                  geom_abline(color = "gray40", lty = 2) + 
                    geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
                  geom_text(aes(label= word), check_overlap=TRUE, vjust = 1.5) +
                  scale_x_log10(labels = percent_format()) + 
                  scale_y_log10(labels = percent_format()) + 
              scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75")+
                  facet_wrap(~author, ncol = 2)+
                  theme(legend.position="none")+
                labs(y = "Jane Austen", x=NULL)
  





  
  
                              




