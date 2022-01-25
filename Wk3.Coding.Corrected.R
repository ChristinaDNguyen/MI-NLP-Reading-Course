
# ======== Chapter 9 - Case study: analyzing usenet text================

#That is sooooo much data! I won't run it, that will take ages.

# Pre-processing =====================================================

# Reading in every single message from the 20news-bydate folder,
# which contains sub-folders, where each message has its own file.
# (Maybe that's why the file is so huge?!)

library(dplyr)
library(tidyr)
library(purrr)
library(readr)

#Note to future self: Here I can't copy the code exactly from the textbook, since the
#textbook's code tells me where the authors stored their tar.gz file. 
#I needed to go to my downloads folder (where I downloaded the tar.gz file,
#then shift+right click the tar.gz file, then click "copy file path". Also, when
#I pasted the file path, I had to change all \ to \\ since the \ means something
#different to R.)


training_folder <- ("C:\\Users\\chris\\Downloads\\20news-bydate.tar.gz")
training_folder


# Create a function to read ALL files from the above folder into a data frame
read_folder <- function(infolder) {
  data_frame(file = dir(infolder, full.names = TRUE)) %>%
    mutate(text = map(file, read_lines)) %>%
    transmute(id = basename(file), text) %>%
    unnest(text)
}
#Phew, that ran OK.... I can see this will take ages.

# Use unnest() and map() to apply read_folder to each subfolder
raw_text <- data_frame(folder = dir(training_folder, full.names = TRUE)) %>%
  unnest(map(folder, read_folder)) %>%
  transmute(newsgroup = basename(folder), id, text)


raw_text

# ID "newsgroups" and the # of messages that each contains
library(ggplot2)

raw_text %>%
  group_by(newsgroup) %>%
  summarize(messages = n_distinct(id)) %>%
  ggplot(aes(newsgroup, messages)) +
  geom_col() +
  coord_flip()

# ===================== Pre-processing text ===============

#Use stringr package with its cleaning commands

library(stringr)

cleaned_text <- raw_text %>%
  group_by(newsgroup, id) %>%
  filter(cumsum(text == "") > 0,
         cumsum(str_detect(text, "^--")) == 0) %>%
  ungroup()

#more cleaning, non0text messages, quotes, vv


cleaned_text <- cleaned_text%>%
  filter(str_detect(text, "^[^>]+[A-Za-z\\d]") | text == "",
      !str_detect(text, "writes(:|\\.\\.\\.)$"),
        !str_detect(text, "^In article <"),
    !id %in% c(9704, 9985))

# tokenize, remove stopwords
library(tidytext)

usenet_words <- cleaned_text %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word, "[a-z']$"),
   !word %in% stop_words$word)

# Words in newsgroups 

# ID  commonest words in all the sub-folder/all the messages
usenet_words %>%
  count(word, sort = TRUE)

# Now in each newsgroup
words_by_newsgroup <- usenet_words %>%
  count(newsgroup, word, sort = TRUE) %>%
  ungroup()

words_by_newsgroup

# Finding tf-idf value for each newsgroup

tf_idf <- words_by_newsgroup %>%
  bind_tf_idf(word, newsgroup, n) %>%
  arrange(desc(tf_idf))

tf_idf



tf_idf %>%
  filter(str_detect(newsgroup, "^sci\\.")) %>%
  group_by(newsgroup) %>%
  top_n(12, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = newsgroup)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ newsgroup, scales = "free") +
  ylab("tf-idf") +
    coord_flip()

#Pairwise cor. of word frequencies inside each newsgroup using pairwise_cor() (widyr package, that is)
#which tells us "is group a" most like "group b" of messages

library(widyr)

newsgroup_cors <- words_by_newsgroup %>%
  pairwise_cor(newsgroup, word, n, sort = TRUE)

newsgroup_cors

#Visualize as network graph

library(ggraph)
library(igraph)
set.seed(2017)

newsgroup_cors %>%
  filter(correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") + 
  geom_edge_link(aes(alpha = correlation, width = correlation)) +
  geom_node_point(size = 6, color = "lightblue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

# ============== Topic modeling ==========================

# words that happen 50+ times only, less is not significant (how do we decide this, by the way?)
word_sci_newsgroups <- usenet_words %>%
  filter(str_detect(newsgroup, "^sci")) %>%
  group_by(word) %>%
  mutate(word_total = n()) %>%
  ungroup() %>%
  filter(word_total > 50)

#turn into dfm
sci_dtm <- word_sci_newsgroups %>%
  unite(document, newsgroup, id) %>%
  count(document, word) %>%
  cast_dtm(document, word, n)

library(topicmodels)

sci_lda <- LDA(sci_dtm, k = 4, control = list(seed = 2016))

# Visualize 

sci_lda %>%
  tidy()%>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  ungroup()%>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip()

#check topic's gammas, same as with nasa dataset

sci_lda %>%
  tidy(matrix = "gamma") %>%
  separate(document, c("newsgroup", "id"), sep = "_") %>%
  mutate(newsgroup = reorder(newsgroup, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ newsgroup) +
  labs(x = "Topic",
       y = "number of messages where this was the highest % topic")

# ============== Sentiment analysis=========================

#positivity scores, as usual

newsgroup_sentiments <- words_by_newsgroup %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(newsgroup) %>%
  summarize(score = sum(score * n) / sum(n))

newsgroup_sentiments %>%
  mutate(newsgroup = reorder(newsgroup, score)) %>%
  ggplot(aes(newsgroup, score, fill = score > 0)) + # I like this
    geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("Average sentiment score")

# scoring by word

#What's the weight of each word's contribution to the score?

contributions <- usenet_words %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(occurences = n(),
            contribution = sum(score))

contributions

contributions %>%
  top_n(25, abs(contribution)) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, contribution, fill = contribution > 0))+
  geom_col(show.legend = FALSE)+
  coord_flip()

top_sentiment_words <- words_by_newsgroup %>%
  inner_join(get_sentiments("afinn"), by = "word")%>%
  mutate(contribution = score*n / sum(n))

top_sentiment_words



# Categorize by "id" not "newsgroup:

sentiment_messages <- usenet_words %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(newsgroup, id) %>%
     summarize(sentiment = mean(score),
            words = n()) %>%
  ungroup() %>%
  filter(words >= 5) #this gets rid of the unimportant background stuff,
#less background noise as it were


sentiment_messages %>%
  arrange(desc(sentiment))



print_message <- function(group, message_id) {
  result <- cleaned_text %>%
    filter(newsgroup == group, id == message_id, text != "")
  
  cat(result$text, sep = "\n")
}

print_message("rec.sport.hockey", 53560)


# Most negative 
sentiment_messages %>%
  arrange(sentiment)

print_message("rec.sport.hockey", 53907)

# N-grams, looking at multiple words at once

# Explore n-grams for use of negative sentiments (words beside each other
#like "not happy" where sentiment lies beside each other, not individual
#words alone -- from a  literary perspective, this is pretty useful
#in avoiding biases)

usenet_bigrams <- cleaned_text %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

usenet_bigram_counts <- usenet_bigrams %>%
  count(newsgroup, bigram, sort = TRUE) %>%
  ungroup() %>%
  separate(bigram, c("word1", "word2"), sep = " ")


usenet_bigram_counts %>%
  filter(word1 %in% negate_words) %>%
  count(word1, word2, wt = n, sort = TRUE) %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  mutate(contribution = score * nn) %>%
  group_by(word1) %>%
  top_n(10, abs(contribution)) %>%
  ungroup() %>%
  mutate(word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>%
  ggplot(aes(word2, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free", nrow = 3) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  xlab("Words preceded by a negation") +
  ylab("Sentiment score * # of occurrences") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()


