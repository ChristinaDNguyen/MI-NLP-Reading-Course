
#=============================== CHAPTER 1 ====================================

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
  gather(author, proportion, `Bronte sisters`:`H.G. Wells`)

#Now we plot it

library(scales)

ggplot(frequency, aes(x = proportion, y = `Jane Austen`,
                      color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001),
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)

#Now we quantify how similar and different these sets of words frequencies are using a 
#correlation test. "How correlated are the word freqs between Austen and the Bronte sisters,
#and between Austen and Wells?"

cor.test(data=frequency[frequency$author == "Bronte sisters",], ~proportion + `Jane Austen`)

#That outputted a correlation of 0.7375659

cor.test(data=frequency[frequency$author == "H.G. Wells",], ~proportion + `Jane Austen`)





#=============================== CHAPTER 2 ====================================

  library(tidytext)
sentiments

#We use three lexicons (or "dictionaries of sentiment", as it were). Each one can
#do different functions to identify sentiment. "AFINN" gives a score from 
#-5 to +5; "Bing" gives a POSTIIVE/NEGATIVE; "NRC" rates it according to a particular
#emotion (sad, angry, upset, etc.)

get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

#OK, let's look for words scored as "joy" in Jane Austen's "Emma." First, make the text 
#tidy. Second, set up more columns to keep track of these values: which line of the book
#each word comes from, and which chapter of the book each word comes from. To do this
#second step, we use "group_by" and "mutate."

library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number() , chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

#Now that it's all tidy, we do sentiment analysis with the NRC lexicon (since
#that one gives us emotional rankings of words)

nrcjoy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")
tidy_books %>%
  filter(book == "Emma")%>%
  inner_join(nrcjoy) %>%
  count(word, sort = TRUE)


#Or we could do a different type of sentiment analysis and study how sentiment 
#changes throughout each novel. First, we find a sentiment score for each word with Bing.
#Then we count how many positive and negative words there are in defined
#sections of each book.

library(tidyr)

janeaustensentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(book, index = linenumber %% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#Now plot these sent scores across the plot trajectory of each novel (i.e. agaisnt
#plot time, or narrative time)

library(ggplot2)

ggplot(janeaustensentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) + 
  facet_wrap(~book, ncol =2, scales = "free_x")

#Let's compare the three sentiment dictionaries

pride_prejudice <- tidy_books %>%
  filter(book == "Pride & Prejudice")

pride_prejudice

#Now we use inner_join() to calculate the sentiment in different ways
#And we will use integer division to define larger sections of text that span multiple lines...

library(tidyr)

afinn <- pride_prejudice %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(index = linenumber %/% 80) %>%
  summarise(sentiment = sum(score)) %>%
  mutate(method = "AFINN")

bing_and_nrc <- bind_rows(
  pride_prejudice %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  pride_prejudice %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive",
                                         "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_Wrap(~method, ncol = 1, scales = "free_y")

#Hypothetical: why is the result fo rthe NRC lexicon biased so high in sentiment 
#compared to the Bing et al lexicon?

get_sentiments("nrc") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(sentiment)

get_sentiments("bing") %>%
                 count(sentiment)


#Most common positive and negative words (we want to see which words
#had the most impact, the most weight, on our total sentiment calculation)

bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing"))%>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

#Graph it!
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

#We see that "miss" is actually a problematic part of this analysis
#since "miss" is coded as a negative here yet the stories actually
#use "miss" to mean an unmarried young woman. So we might add "miss"
#as a stopword to remove it altogether from our analysis

custom_stop_words <- bind_rows(data_frame(word = c("miss"),
                                          lexicon = c("custom")),
                               stop_words)
custom_stop_words

#Wordcloud time!

library(wordcloud)
tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
 with(wordcloud(word, n, max.words = 100))

#We can shape it too, so that it goes from negative to positive

library(reshape2)

tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)

PandP_sentences <- data_frame(text = prideprejudice) %>%
  unnest_tokens(sentence, text, token = "sentences")
PandP_sentences$sentence[4]

austen_chapters <- austen_books() %>%
  group_by(book) %>%
  unnest_tokens(chapter, text, token = "regex",
                pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
  ungroup()
austen_chapters %>%
  group_by(book) %>%
  summarise(chapters = n())

#Let's get ratios though - easier to compare

bingnegative <- get_sentiments("bing") %>%
  filter(sentiment == "negative")
wordcounts <- tidy_books %>%
  group_by(book, chapter) %>%
  summarize(words = n())
tidy_books %>%
  semi_join(bingnegative) %>%
  group_by(book, chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("book", "chapter")) %>%
  mutate(ratio = negativewords/words) %>%
  filter(chapter != 0) %>%
  top_n(1) %>%
  ungroup()


#=============================== CHAPTER 3 ====================================

#Start by looking at term frequency in Jane Austen's novels.
library(dplyr)
library(janeaustenr)
library(tidytext)

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort=TRUE) %>%
  ungroup()
total_words <- book_words%>%
  group_by(book)%>%
  summarize(total = sum(n))

book_words <-left_join(book_words, total_words)

book_words

library(ggplot2)
ggplot(book_words, aes(n/total, fill = book))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.0009)+
  facet_wrap(~book, ncol = 2, scales = "free_y")

#Zipf's law

freq_by_rank <- book_words %>%
  group_by(book) %>%
  mutate(rank=row_number(), `term frequency` = n/total)
freq_by_rank

#Plot Zipf's

freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = book)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

#Let's see what the exponent of the power law is for the middle section of the rank range.

rank_subset <- freq_by_rank %>%
  filter(rank < 500, rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)
#We have in fact gotten a slope close to -1 here. Let's plot this fitted
#power law to see how it looks

freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype =2)+
  geom_line(size = 1.1, alpha = 0.8, show.legend =FALSE) +
  scale_x_log10()+
  scale_y_log10()

#OK so above we just fitted an exponent for Zipf's law with Austen's novels. What 
#that means is that the corpus of Austen's novels fits almost perfectly
#to Zipf's law. This kind of analysis can be extended to compare
#authors, or to compare any other collections of text.

#Now we will use the bind_tf_idf function to mix both tf and idf together
#as originally planned at the beginning of the chapter. 

book_words <- book_words %>%
  bind_tf_idf(word, book, n)
book_words

book_words %>%
  select(-total)%>%
  arrange(desc(tf_idf))

#Let's visualize these high tf-idf words 
book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(book) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()
  

#Now let's work with a corpus of physics texts
library(gutenbergr)
physics <- gutenberg_download(c(37729, 14725, 13476, 5001),
                              meta_fields = "author")

physics_words <- physics %>%
  
unnest_tokens(word, text) %>%
  count(author, word, sort=TRUE) %>%
  ungroup()
physics_words


plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  arrange(desc(tf_idf))%>%
  mutate(word=factor(word, levels = rev(unique(word)))) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo", "Huygens, Christiaan", "Tesla, Nikola", "Einstein, Albert")))

#Now plot like an evil villain plots!

plot_physics %>%
  group_by(author) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()

#Right so I'm missing one graph - the Einstein one - because I don't
 #have the data for it.
library(stringr)
physics %>%
  filter(str_detect(text, "eq\\.")) %>%
  select(text)

#It's probably lumpy like that because I'm missing data...
#I also can't do the next chunk of code because the Einstein text
#isn't there.



#=============================== CHAPTER 4 ====================================


library(dplyr)
library(tidytext)
library(janeaustenr)

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n =2)

austen_bigrams

austen_bigrams %>%
  count(bigram, sort = TRUE)

#remove stop words, this is a bit more complex than before since
#we're using bigrams. Seperate the bigram into two chunks so we can
#get rid of stopwords before continuing 

library(tidyr)
bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2", sep=" "))

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#new bigram counts

bigram_counts <-bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

bigram_counts

#well we might want to recombind the words back into a bigram as
#normal instead of two chunks

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

#You can repeat the same techniques with trigrams (we'll skip that, it's
#the same commands over, just set n =3)

#OK let's analyze bigrams to see which streets occur the most 

bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)

bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

#Provide context using bigrams in sentiment analysis

bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)


#now try sentiemnts
AFINN <- get_sentiments("afinn")

not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word"))%>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

#We can plot, but let's skip for now. Good to know.

#Visualizing a network of bigrams with ggraph

library(igraph)

bigram_counts
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph

library(ggraph)
set.seed(2017)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

#OK so how about using bigrams on other texts?

library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)

count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}
visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

#Now we try to visualize the KJV Bible 

library(gutenbergr)
kjv <- gutenberg_download(10)
library(stringr)
kjv_bigrams <- kjv %>%
  count_bigrams()
kjv_bigrams %>%
  filter(n>40, !str_detect(word1, "\\d"), !str_detect(word2, "\\d")) %>%
  visualize_bigrams()
#For some reason I'm missing the edges (we can work that out later)

#Widyr package

austen_section_words <- austen_books() %>%
  filter(book == "Pride & Prejudice") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

austen_section_words

library(widyr)
word_pairs <-austen_section_words %>%
  pairwise_count(word, section, sort = TRUE)
word_pairs

word_pairs %>%
  filter(item1 == "darcy")

word_cors <- austen_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors

word_cors %>%
  filter(item1 == "pounds")

word_cors %>%
  filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

set.seed(2016)
word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
  








 
  


  
  










  
                              




