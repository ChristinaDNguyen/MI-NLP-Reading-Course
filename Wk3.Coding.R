#======================CHAPTER 8: MINING NASA METADATA=======================

#NASA's metadata lets us understand the relationships between the NASA
#datasets (e.g. about its planetary exploration missions). Metadata is a term
#that refers to data that gives information (like context) to other data -
#recall that this is also a LIS term that you learned about in 
#'Information Experience' course and 'Communities and Values' course.

#NASA's metadata here includes information in categories like "title of dataset,"  
#"description field," vv. First we have to check what those categories are.

library(jsonlite)
metadata <- fromJSON("https://data.nasa.gov/data.json")
names(metadata$dataset)
#This above line takes a minute to load, since there are a lot of "names" (i.e. "categories of metadata")
#in this dataset. The results here reading from L to R and T to D are "accessLevel",
#"landingPage", "bureauCode", "issued", ...

#Let's check out the title, description, and keywords to draw connections.

class(metadata$dataset$title)
class(metadata$dataset$description)
class(metadata$dataset$keyword)


#Let's tidy the data by creating separate data frames for each of the 
#three categories we chose. We should also keep the dataset ID for each
#so we can use them later in the analysis if needed.

library(dplyr)
nasa_title <- data_frame(id = metadata$dataset$`_id`$`$oid`, title = metadata$dataset$title)
nasa_title

#I am missing the "id number" column: I only have the title column.
# I suspect that the metadata has been updated since my code does not give an
#ID number column. I tried to check the JSON itself to see how the ID
#numbers were sorted (so I could try to change the search regex)
#, but the JSON took forever to load, so I couldn't. This is a minor
#problem that will affect how the next steps go. We'll just have
#one column of metadata so I remove all the first args in the dataframe
#(i.e. in the textbook the line that has "id" in it)

nasa_desc <- data_frame(desc=metadata$dataset$description)

nasa_desc %>%
  select(desc) %>%
  sample_n(5)

library(tidyr)
nasa_keyword <- data_frame(keyword = metadata$dataset$keyword) %>%
unnest(keyword)

nasa_keyword
#This is now tidied because we have one row for each keywords.
#Now we unnest the title and description fields so we can do the 
#text analsysi. We also remove stop words from the titles and descriptions. Not 
#from keywords.

library(tidytext)
nasa_title <- nasa_title %>%
  unnest_tokens(word, title) %>$
  anti_join(stop_words)

nasa_desc <- nasa_desc %>%
  unnest_tokens(word, desc) %>$
  anti_join(stop_words)

#Error above - let's move on for now

nasa_title
#Well, it IS one word per row, in a chr format, so it can't be tooooo bad.
nasa_desc

#That's not so good - it hasn't done one word per row, it's done ~1 sentence per 
#row... We need to check this again later to see how to fix it.



#Initial exploration

nasa_title %>%
  count(word, sort = TRUE)

#We'll type this one for conssitency but because the description
#doesn't have single words (as we noted in the error above, it won't work)
nasa_desc %>%
  count(word, sort= TRUE)

my_stopwords <- data_frame(word = c(as.character(1:10), "v1", "v03", "12", "13", "14", "v5.2.0", "v003", "v004", "v005", "v006", "v7"))
nasa_title <- nasa_title %>%
  anti_join(my_stopwords)

nasa_keyword %>%
  group_by(keyword) %>%
  count(sort=TRUE)

nasa_keyword <- nasa_keyword %>%
  mutate(keyword = toupper(keyword))

#We now study co-occurances and correlations.

#Just read this section - it has way too many errors that I need
#to ask about in class.

keywords_cors <- nasa_keyword %>%
  group_by(keyword ) %>%
  filter(n() >= 50)
pairwise_cor(keyword, id, sort = TRUE, upper=FALSE)
#Why is object "keyword" not found?

keywords_cors

library(ggplot2)
set.seed(1234)
keyword_cors %>%
  filter(correlation > .6) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation),
    edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

desc_tf_idf <- nasa_desc %>%
  count(id, word, sort =TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, id, n)


  desc_tf_idf %>%
  arrange(-tf_idf) %>%
  select(-id)



  desc_tf_idf <- full_join(desc_tf_idf, nasa_keyword, by = "id")
  desc_tf_idf %>%
    filter(!near(tf, 1)) %>%
    filter(keyword %in% c("SOLAR ACTIVITY", "CLOUDS",
                          "SEISMOLOGY", "ASTROPHYSICS",
                          "HUMAN HEALTH", "BUDGET")) %>%
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
    labs(title = "Highest tf-idf words in NASA metadata description fields",
         caption = "NASA metadata from https://data.nasa.gov/data.json",
       x = NULL, y = "tf-idf")
  
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
  # time intensive
  desc_lda <- LDA(desc_dtm, k = 24, control = list(seed = 1234))
  desc_lda
  
  
  tidy_lda <- tidy(desc_lda)
  tidy_lda
  
  top_terms <- tidy_lda %>%
    group_by(topic) %>%
    top_n(10, beta)%>%
    ungroup() %>%
    arrange(topic, -beta)
  
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
    facet_wrap(~ topic, ncol = 3, scales = "free")
  
  lda_gamma <- tidy(desc_lda, matrix = "gamma")
  lda_gamma
  
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
  top_keywords <- lda_gamma %>%
    filter(gamma > 0.9) %>%
    count(topic, keyword, sort = TRUE)
  top_keywords
  
  # top keywords for each topic are below
  
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
    facet_wrap(~ topic, ncol = 3, scales = "free")


