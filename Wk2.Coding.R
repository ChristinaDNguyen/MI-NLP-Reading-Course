#==================== CONVERTING BETWEEN DIFFERENT TM FORMATS =================

#Example of "tidying a DocumentTermMatrix Object"
#We use the collection of Associated Press newspaper articles, which
#lies in the topicmodels package. It's in a DTM format, so we 
#want to tidy it up

library(tm)
data("AssociatedPress", package = "topicmodels")
AssociatedPress

#We see that 99% of terms in here are sparse
#We can access the terms in the document with the Terms() function

terms <- Terms(AssociatedPress)
head(terms)

#Now we have to turn it into a tidy data frame, because right now there
#ISN'T one token per document per row (see, one row has six words across it, not good)

library(dplyr)
library(tidytext)
ap_td <- tidy(AssociatedPress)
ap_td

#It's also great to tidy because we got rid of all the zero values. I.e. the nontidy
#format would print out all the words that had a value of 0; the tidy version doesn't.

#Because it's tidy, of course now we can use it with dplyr, tidytext, and ggplot2 packages.
#Eg we do the sentiment analysis that we learned in ch 2.

ap_sentiments <- ap_td %>%
  inner_join(get_sentiments("bing"), by = c(term="word"))
ap_sentiments

library(ggplot2)
ap_sentiments %>%
  count(sentiment, term, wt=count) %>%
  ungroup() %>%
  filter(n >= 200) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  

  
  ggplot(aes(term, n, fill =sentiment)) + geom_bar(stat="identity")+
  ylab("Contibution to sentiment") + 
  coord_flip()


#Tidying Document FRAME matrices, e.g. from quanteda package

library(methods)
data("data_corpus_inaugural", package = "quanteda")

inaug_dfm <- quanteda::dfm(data_corpus_inaugural, verbose= FALSE)
inaug_dfm

inaug_td <- tidy(inaug_dfm)
inaug_td

#If we want to find out the words most specific to each of the inaug speeches...
#we can use tf-idf of each term-speech pair and use bind_tf_idf() like in ch. 3

inaug_tf_idf <- inaug_td %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

inaug_tf_idf

#We can throw shade on quanteda a bit because it parses "?" as a term
#when the unnest_tokens does not


#We can also extract the year and computer the total number of words within each 
#year

library(tidyr)
year_term_counts <-inaug_td %>%
  extract(document, "year", "(\\d+)", convert = TRUE) %>%
  complete(year, term, fill = list(count = 0)) %>%
  group_by(year) %>%
  mutate(year_total = sum(count))

year_term_counts %>%filter(term %in% c("god", "america", "foreign", "union", "constitution", "freedom")) %>%
  ggplot(aes(year, count / year_total)) + 
  geom_point()+
  geom_smooth()+
  facet_wrap(~ term, scales = "free_y")+
  scale_y_continuous(labels = scales::percent_format())+
  ylab("% freq of word in inaugural address")


#Casting tidytextdata into a matrix

#Sometimes you need the input data to be a matrix. So you can use tidytext's
#cast_verbs to convert from tidy to matrix. In this ex we use the tidied data
#that we made for the AP and make it back intoa  matrix (e.g. we want to use
#it for quanteda)

ap_td %>%
  cast_dtm(document, term, count)
#Similarly, we could cast the table from a dfm object from
#quanteda's dfm with cast_dfm

ap_td %>%
  cast_dfm(term, document, count)
library(Matrix)
m <- ap_td %>%
  cast_sparse(document, term, count)

class(m)
dim(m)

#But what if there's metadata to tidy up? Here we tidy corpus objects
#with metadata


data("acq")
acq
acq[[1]]


acq_td <- tidy(acq)
acq_td

acq_tokens <- acq_td %>%
  select(-places) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

acq_tokens %>%
  count(word, sort = TRUE)

acq_tokens %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))

#Now we can try web mining financial articles

library(tm.plugin.webmining)
library(purrr)
company <- c("Microsoft", "Apple", "Google", "Amazon", "Facebook",
             "Twitter", "IBM", "Yahoo", "Netflix")
symbol <- c("MSFT", "AAPL", "GOOG", "AMZN", "FB", "TWTR", "IBM", "YHOO", "NFLX")
download_articles <- function(symbol) {
  WebCorpus(GoogleFinanceSource(paste0("NASDAQ:", symbol)))
}
stock_articles <- data_frame(company = company,
                             symbol = symbol) %>%
  mutate(corpus = map(symbol, download_articles))


#The rest of this plugin.webmining section is left out since the code just won't run
#"Error: package or namespace load failed for 'tm.plugin.webmining':
#.onLoad failed in loadNamespace() for 'rJava', details:
 # call: fun(libname, pkgname)
#error: JAVA_HOME cannot be determined from the Registry""

#=====================================CHAPTER 6, TOPIC MODELLING===========

#OK nous utilisons ce packet, "topicmodels" et nous lui disons d'utiliser
#les doneess "AP"

library(topicmodels)
data("AssociatedPress")
AssociatedPress

#And we tell it to create a TWO-TOPIC LDA model by setting k =2 (of course
#we can adjust this later if we want more topics)

ap_lda <- LDA(AssociatedPress,  k = 2, control = list(seed =1234))
ap_lda

#And now we do word-topic probabilities

library(tidytext)
ap_topics <-tidy(ap_lda, matrix = "beta")
ap_topics

library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

library(tidyr)
beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  
  mutate(log_ratio = log2(topic2 / topic1))
beta_spread

#We can also check the document-topic probabilities (e.g. p that a certain word
#in a text will be about a certain topic, either topic 1 or 2)

ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

tidy(AssociatedPress) %>%
  filter(document ==6) %>%
  arrange(desc(count))

#example
titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
            "Pride and Prejudice", "Great Expectations")
library(gutenbergr)
books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

library(stringr)
# divide into documents, each document is ONE CHAPTER (not one book, since
#that's what we are looking for)
reg <- regex("^chapter ", ignore_case = TRUE)
by_chapter <- books %>%
  group_by(title) %>%
  
  
  mutate(chapter = cumsum(str_detect(text, reg))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)
# split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)
# get the document-word counts
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()


word_counts
chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)
chapters_dtm


chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))
chapters_lda

chapter_topics <- tidy(chapters_lda, matrix = "beta")
chapter_topics

top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms
library(ggplot2)
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()



chapters_gamma <- tidy(chapters_lda, matrix = "gamma")
chapters_gamma
chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)
chapters_gamma
chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)

chapter_classifications <- chapters_gamma %>%
  group_by(title, chapter) %>%
  top_n(1, gamma) %>%

ungroup()
chapter_classifications

book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  group_by(title) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = title, topic)
chapter_classifications %>%
  inner_join(book_topics, by = "topic") %>%
  filter(title != consensus)



#By-Word assignments: augment
#Now we assign each word in a document to a topic (not manually, thank goodness)
#The more words that are assigned to a topic means a higher weight (gamme) for that dtc

assignments <- augment(chapters_lda, data = chapters_dtm)
assignments
assignments <- assignments %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>%
  inner_join(book_topics, by = c(".topic" = "topic"))
assignments

assignments %>%
  count(title, consensus, wt = count) %>%
  group_by(title) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(consensus, title, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "red", label = percent_format())+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust= 1),
        panel.grid = element_blank()) +
  labs(x = "book words were assigned to",
       y = "book words came from",
       fill = "% of assignments")


#We could also use the MALLET format to do the LDA if we wanted. 

library(mallet)

collapsed <- by_chapter_word %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_replace(word, "'", "")) %>%
  group_by(document) %>%
  summarize(text = paste(word, collapse = " "))
# create an empty file of "stop words"
file.create(empty_file <- tempfile())
docs <- mallet.import(collapsed$document, collapsed$text, empty_file)
mallet_model <- MalletLDA(num.topics = 4)
mallet_model$loadDocuments(docs)
mallet_model$train(100)
# word-topic pairs

tidy(mallet_model)
# document-topic pairs
tidy(mallet_model, matrix = "gamma")
# call the column "term" in order to be able to "augment"
term_counts <- rename(word_counts, term = word)
augment(mallet_model, term_counts)

#OMG MALLET makes so much more sense than the previous one











 

