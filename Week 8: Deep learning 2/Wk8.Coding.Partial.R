samples <- c("The cat sat on the mat.", "The dog ate my homework.")

#index all tokens in dataset
token_index <- list()
for (sample in samples)
#tokenize (i guess we don't clean it?!)
  for (word in strsplit(sample, " ")[[1]])
      if (!word %in% names(token_index))
    token_index[[word]] <- length(token_index) + 2 
#Vectorize
max_length <- 10
results <- array(0, dim = c(length(samples), 
    max_length, 
    max(as.integer(token_index))))
for (i in 1:length(samples)) {sample <- samples[[i]]
  words <- head(strsplit(sample, " ")[[1]], n = max_length)
  for (j in 1:length(words)) {index <- token_index[[words[[j]]]]
    results[[i, j, index]] <- 1
  }
}

samples <- c("The cat sat on the mat. ", "The dog ate my homework.")
ascii_tokens <- c("", sapply(as.raw(c(32:126)), rawToChar))
token_index <- c(1:(length(ascii_tokens)))
names(token_index) <- ascii_tokens
max_length <- 50
results <- array(0, dim = c(length(samples), max_length, length(token_index)))
for (i in 1:length(samples)) {
  sample <- samples[[i]]
  characters <- strsplit(sample, "")[[1]]
  for (j in 1:length(characters)) {
    character <- characters[[j]]
    results[i, j, token_index[[character]]] <- 1
  }
}

library(keras)
samples <- c("The cat sat on the mat", "The dog ate my homework")

tokenizer <-text_tokenizer(num_words = 1000) %>%
  fit_text_tokenizer(samples)

sequences <- texts_to_sequences(tokenizer, samples)
one_hot_results <- texts_to_matrix(tokenizer, samples, mode = "binary")
#find/resave the word index that was computed
word_index <- tokenizer$word_index
cat("Found", length(word_index), "unique tokens.\n")


library(hashFunction)
samples <- c("The cat sat on the mat", "The dog ate my homework") #ARRRGH SPELLING MISTAKES I HATE THEM

dimensionality<- 1000
max_length <- 10
results <- array(0, dim = c(length(samples), max_length, dimensionality))
for (i in 1:length(samples)) {
  sample <- samples[[i]]
  words <- head(strsplit(sample, " ")[[1]], n = max_length)
  for (j in 1:length(words)) {

    index <- abs(spooky.32(words[[i]])) %% dimensionality
    results[[i, j, index]] <- 1
  }}
