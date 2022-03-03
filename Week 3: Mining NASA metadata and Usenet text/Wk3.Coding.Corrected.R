

#========== Chapter 8 -  NASA metadata =======================================

library(jsonlite)
metadata <- fromJSON("https://data.nasa.gov/data.json")
names(metadata$dataset)

# Check out the title, description, and keywords 

class(metadata$dataset$title)

class(metadata$dataset$description)

class(metadata$dataset$keyword) 

# Wrangling and tidying the data-


library(dplyr)

nasa_title <- data_frame(id = metadata$dataset$`_id`$`$oid`,
                         title = metadata$dataset$title)

nasa_title

nasa_desc <- data_frame(id = metadata$dataset$`_id`$`$oid`, 
                        desc = metadata$dataset$description)

nasa_desc %>% 
  select(desc) %>% 
  sample_n(5) # nice - samples n rows from a table

# for the keywords, need unnest() bc they're in a list-column

library(tidyr)

nasa_keyword <- data_frame(id = metadata$dataset$`_id`$`$oid`, 
                           keyword = metadata$dataset$keyword) %>%
  unnest(keyword)

nasa_keyword

#Now make it one word per row (i.e. not "rosetta stone mission" by "rosetta" "stone" "mission")

library(tidytext)

nasa_title <- nasa_title %>% 
  unnest_tokens(word, title) %>% 
  anti_join(stop_words)

nasa_desc <- nasa_desc %>% 
  unnest_tokens(word, desc) %>% 
  anti_join(stop_words)

nasa_title

nasa_desc

#Heck yeah, it's all TIDY. Let's Marie Kondo the crap out of everything!

# Some initial exploration ------------------------------------------------

# Most common words in the NASA dataset titles

nasa_title %>% 
  count(word, sort = TRUE)

# Now descriptions

nasa_desc %>% 
  count(word, sort = TRUE)

#Yay. No //major// errors yet, thank GOODNESS.


# Create a custom stop words list to remove some less meaningful
## text (e.g., digits)

my_stopwords <- data_frame(word = c(as.character(1:10),
                                    "v1", "v03", "l2", "l3", "l4", "v5.2.0", 
                                    "v003", "v004", "v005", "v006", "v7"))


nasa_title <- nasa_title %>% 
  anti_join(my_stopwords)
nasa_desc <- nasa_desc %>% 
  anti_join(my_stopwords)

# Most common keywords

nasa_keyword %>% 
  group_by(keyword) %>% 
  count(sort = TRUE)
#That is a weird-looking 10th keyword... churymov-gerasimenko

# Change all keywords to upper case to avoid dups

nasa_keyword <- nasa_keyword %>% 
  mutate(keyword = toupper(keyword))


# Word co-occurrences and correlations -------------------------------------

# First examining words that commonly occur togetehr, then
## examine word networks to determine which datasets are
## related to each other

# Networks of descriptions and title words --------------------------------

# Use pairwise_count() from widyr to count how many times
## pairs of words occur together in title or desc fields

library(widyr)

#Since "id" column (we got a 1*n) doesn't exist anymore... We'll just type
#the code anyways but the "id" argument won't do anything. (Emailed Rod
#about this, it really sucks to affect so much code)

title_word_pairs <- nasa_title %>% 
  pairwise_count(word, id, sort = TRUE, upper = FALSE)

title_word_pairs

desc_word_pairs <- nasa_desc %>% 
  pairwise_count(word, id, ==sort = TRUE, upper = FALSE)

desc_word_pairs

# Plot networks of co-occurring words using ggraph... Well, we can't since 
#id doesn't exist, neither does the "title_word_pairs" nor "desc_word_pairs"
#_______________________________ ARGH FRUSTRATION _____________________________

library(ggplot2)
library(igraph)
library(ggraph)

set.seed(1234)
title_word_pairs %>%
  filter(n >= 250) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()

#See? There are some pretty obvious clusters

set.seed(1234)
desc_word_pairs %>%
  filter(n >= 5000) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "darkred") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

# Networks of keywords ----------------------------------------------------

# See which keywords commonly occur together in the same
## datasets... still not keyword_pairs since we're missing the 
##bloody ID thing, but OK, we'll pretend.

keyword_pairs <- nasa_keyword %>% 
  pairwise_count(keyword, id, sort = TRUE, upper = FALSE)

keyword_pairs

set.seed(1234)
keyword_pairs %>%
  filter(n >= 700) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

# i.e. we just checked correlations among
## keywords, to see those that are more likely to occur together
## than with other keywords in a description field (i.e. a closer to b than a is to 
#d)

keyword_cors <- nasa_keyword %>% 
  group_by(keyword) %>%
  filter(n() >= 50) %>%
  pairwise_cor(keyword, id, sort = TRUE, upper = FALSE)

keyword_cors


# Visualiz the correlations between the keywords

set.seed(1234)
keyword_cors %>%
  filter(correlation > .6) %>% 
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void() #I wonder how we





# --- Calculate tf-idf for the description fields ---------------------------


# What is tf-idf for desc

desc_tf_idf <- nasa_desc %>% 
  count(id, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, id, n)

# Highest tf-idf wds

desc_tf_idf %>% 
  arrange(-tf_idf)


# ----- connecting the description fields to keywords -------------------------------


desc_tf_idf <- full_join(desc_tf_idf, nasa_keyword, by = "id")


desc_tf_idf %>% 
  filter(!near(tf, 1)) %>%
  filter(keyword %in% c("solar activity", "seismology", "clouds" "budget", "human health", "astrophysics")) %>%
                      
  arrange(desc(tf_idf)) %>%
  group_by(keyword) %>%
  distinct(word, keyword, .keep_all = TRUE) %>%
  top_n(15, tf_idf) %>% 
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  ggplot(aes(word, tf_idf, fill = keyword)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~keyword, ncol = 3, scales = "free") +
  coord_flip() +
  labs(title = "Highest tf-idf words in NASA metdata description fields",
       caption = "NASA metdata from https://data.nasa.gov/data.json",
       x = NULL, y = "tf-idf")

# ---------------------------------- Topic modeling -------------------


my_stop_words <- bind_rows(stop_words, 
                   data_frame(word = c("nbsp", "amp", "gt", "lt",
             "timesnewromanpsmt", "font",
                 "td", "li", "br", "tr", "quot",
               "st", "img", "src", "strong",
                  "http", "file", "files",
                  as.character(1:12)), 
                                      lexicon = rep("custom", 30)))

word_counts <- nasa_desc %>%
  anti_join(my_stop_words) %>%
  count(id, word, sort = TRUE) %>%
  ungroup()

word_counts


desc_dtm <- word_counts %>%
  cast_dtm(id, word, n)

desc_dtm




library(topicmodels)

desc_lda <- LDA(desc_dtm, k = 24, control = list(seed = 1234))
desc_lda


tidy_lda <- tidy(desc_lda)

tidy_lda

top_terms <- tidy_lda %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

# Visualize
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  mutate(term = factor(paste(term, topic, sep = "__"), 
                 levels = rev(paste(term, topic, sep = "__")))) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  labs(title = "Top 10 terms in each LDA topic",
       x = NULL, y = expression(beta)) +
  facet_wrap(~ topic, ncol = 4, scales = "free")

#  what topics are associated with what description

lda_gamma <- tidy(desc_lda, matrix = "gamma")

lda_gamma

# Visualize how the p(x)s are distributed
ggplot(lda_gamma, aes(gamma)) +
  geom_histogram() +
  scale_y_log10() +
  labs(title = "Distribution of probabilities for all topics",
       y = "Number of documents", x = expression(gamma))



ggplot(lda_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 4) +
  scale_y_log10() +
  labs(title = "Distribution of probability for each topic",
       y = "Number of documents", x = expression(gamma))



lda_gamma <- full_join(lda_gamma, nasa_keyword, by = c("document" = "id"))

lda_gamma

top_keywords <- lda_gamma %>% 
  filter(gamma > 0.9) %>% 
  count(topic, keyword, sort = TRUE)

top_keywords


top_keywords %>%
  group_by(topic) %>%
  top_n(5, n) %>%
  group_by(topic, keyword) %>%
  arrange(desc(n)) %>%  
  ungroup() %>%
  mutate(keyword = factor(paste(keyword, topic, sep = "__"), 
                          levels = rev(paste(keyword, topic, sep = "__")))) %>%
  ggplot(aes(keyword, n, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  labs(title = "Top keywords for each LDA topic",
       x = NULL, y = "Number of documents") +
  coord_flip() +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  facet_wrap(~ topic, ncol = 4, scales = "free")
#_________________


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


