#installing tensorflow:https://www.youtube.com/watch?v=cIUg11mAmK4&t=233s&ab_channel=LiquidBrain

library(keras)
imdb <- dataset_imdb(num_words = 10000) #only keep the top 10,000 words in the training data (discard rare words)
c(c(train_data, train_labels), c(test_data, test_labels)) %<-% 
  #that operator at the end of called the multi-assignment operator

  #quickly decode one of the reviews back to English words
  
  word_index <- dataset_imdb_word_index()
reverse_word_index <- names(word_index)
names(reverse_word_index) <- word_index
decoded_review <-sapply(train_data[[1]]), function(index{word<- if(index>=3) reverse_word_index[[as.character(index - 3)]]
if(!is.null(word)) word else "?"
})
  
vectorize_sequences <- function(sequences, dimension = 10000) {
  results <- matrix(0, nrow = length(sequences), ncol = dimension)
  for (i(in 1:length(sequences))
       results[i, sequences[[i]]] <- 1
       results
}

x_train <- vectorize_sequences(train_data)
x_test <- vectorize_sequences(test_data)


y_train <- as.numeric(train_labels)
y_test <- as.numeric(test_labels)

library(keras)
model <- kera_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape - c(10000)) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")
  
model %>% compile(
  optimizer = "rmsprop", loss = "binary_crossentropy",
  metrics = c("accuracy")
  
)

#configure the optimizer

model %>% compile(
  optimizer = optimizer_rmsprop(lr=0.0001), loss = "binary_crossentropy", metrics = c("accuracy")
)


#using custom losses and metrics
model %>% compile(optimizer = optimizer_rmsprop(lr=0.001),
                  loss = loss_binary_crossentropy, metrics = metric_binary_accuracy)

#validate the approach
val_indices <- 1:10000
x_val <- x_train[val_indices,]
partial_x_train <- x_train[-val_indices,]
y_val <- y_train[val_indices]
partial_y_train <- y_train[-val_indices]

#train over 20 epochs

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)
history <- model %>% fit( partial_x_train, partial_y_train,
  epochs = 20,
  batch_size = 512,
  validation_data = list(x_val, y_val)
)
#new model
model <- keras_model_sequential() %>% 
  layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>% 
  layer_dense(units = 16, activation = "relu") %>% 
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)
model %>% fit(x_train, y_train, epochs = 4, batch_size = 512)
results <- model %>% evaluate(x_test, y_test)
results

model %>% predict(x_test[1:10,])