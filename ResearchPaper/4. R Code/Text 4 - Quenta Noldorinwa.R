#============================================================================= #
#'A mere maiden': Exploring L�thien Tin�viel's  relationship with dance and song
#with tf-idf scores and fuzzy matching 
#Text 4 only - Quenta Noldorinwa
#============================================================================= #

#Section 1: term frequency basics --------------------------------------------------
#1a counting sing vs dance ........................................................
text_v <- scan("C://Users//chris//Downloads//QuentaNoldorinwa.txt", what= "character")
text_v
text_v <- tolower(text_v)
text_v

loweredtext <- strsplit(text_v, "\\W")
str(loweredtext)
loweredtext

#How many words are "dance"
dance_hits_v <- length(loweredtext[which(loweredtext=="dance")])
dance_hits_v #0 hits
#How many words are "dancing"
dancing_hits_v <- length(loweredtext[which(loweredtext=="dancing")])
dancing_hits_v #1 hits
#How many words are "danced"
danced_hits_v <- length(loweredtext[which(loweredtext=="danced")])
danced_hits_v #1
#How many words are "dances"
dances_hits_v <- length(loweredtext[which(loweredtext=="dances")])
dances_hits_v #0

#Therefore total number of all iterations of danc- is 1
totaldancehits_v = 1
total_words_v <- length(loweredtext)
totaldancehits_v/total_words_v #which tells us that "danc-" makes up 0.00032 of the whole Noldorinwa



#How many words are "song"
song_hits_v <- length(loweredtext[which(loweredtext=="song")])
song_hits_v #3
#How many words are "sing"
sing_hits_v <- length(loweredtext[which(loweredtext=="sing")])
sing_hits_v #1
#How many words are "singing"
singing_hits_v <- length(loweredtext[which(loweredtext=="singing")])
singing_hits_v #1
#How many words are "sang"
sang_hits_v <- length(loweredtext[which(loweredtext=="sang")])
sang_hits_v#3
#How many words are "sings"
sings_hits_v <- length(loweredtext[which(loweredtext=="sings")])
sings_hits_v #0
#How many words are "sung"
sung_hits_v <- length(loweredtext[which(loweredtext=="sung")])
sung_hits_v #1

#Therefore total number of all iterations of sing- is 3+1+1+3+1=9
totalsinghits_v = 9
totalsinghits_v/total_words_v #which tells us that "sing-"'s iterations makes up 0.000018 of the whole Noldorinwa


#1b dispersion plots of sing and dance ...................................................

#create novelistic time index

n_time_v <- seq(from = 1, to = length(loweredtext))

#1b1 identify at which index points the word "dance" occurs
dance_v <- which(loweredtext == "dance") 
dance_count_v <- rep(NA, times = length(n_time_v))
dance_count_v[dance_v] <- 1

plot(dance_count_v, main = "Dispersion plot of 'dance' in QN", xlab = "novelistic time", ylab = "dance", type = "h", ylim = c(0,1), yaxt='n')

#1b2 identify at which index points the word "dancing" occurs
dancing_v <- which(loweredtext == "dancing")
dancing_count_v <- rep(NA, times = length(n_time_v))
dancing_count_v[dancing_v] <- 1

plot(dancing_count_v, main = "Dispersion plot of 'dancing' in QN", xlab = "novelistic time", ylab = "dancing", type = "h", ylim = c(0,1), yaxt = 'n')

#1b3 identify at which index points the word "danced" occurs
danced_v <- which(loweredtext == "danced")
danced_count_v <- rep(NA, times = length(n_time_v))
danced_count_v[danced_v] <- 1

plot(danced_count_v, main = "Dispersion plot of 'danced' in QN", xlab = "novelistic time", ylab = "danced", type = "h", ylim = c(0,1), yaxt = 'n')

#1b4 identify at which index points the word "dances" occurs
dances_v <- which(loweredtext == "dances")
dances_count_v <- rep(NA, times = length(n_time_v))
dances_count_v[dances_v] <- 1

plot(dances_count_v, main = "Dispersion plot of 'dances' in QN", xlab = "novelistic time", ylab = "dances", type = "h", ylim = c(0,1), yaxt='n')

#overlay/combine all the above danc- words together (in Photoshop)

#identify at which index points the word "singing" occurs
singing_v <- which(loweredtext == "singing")
singing_count_v <- rep(NA, times = length(n_time_v))
singing_count_v[singing_v] <- 1
plot(singing_count_v, main = "Dispersion plot of 'singing' in QN", xlab = "novelistic time", ylab = "singing", type = "h", ylim = c(0,1), yaxt= 'n')

#identify at which index points the word "song" occurs
song_v <- which(loweredtext == "song")
song_count_v <- rep(NA, times = length(n_time_v))
song_count_v[song_v] <- 1
plot(song_count_v, main = "Dispersion plot of 'song' in QN", xlab = "novelistic time", ylab = "song", type = 'h', ylim = c(0,1), yaxt = 'n')

#identify at which index points the word "sing" occurs
sing_v <- which(loweredtext == "sing")
sing_count_v <- rep(NA, times = length(n_time_v))
sing_count_v[sing_v] <- 1
plot(sing_count_v, main = "Dispersion plot of 'sing' in QN", xlab = "novelistic time", ylab = "sing", type = 'h', ylim = c(0,1), yaxt = 'n')


#identify at which index points the word "sings" occurs
sings_v <- which(loweredtext == "sings")
sings_count_v <- rep(NA, times = length(n_time_v))
sings_count_v[sings_v] <- 1
plot(sings_count_v, main = "Dispersion plot of 'sings' in QN", xlab = "novelistic time", ylab = "sing", type = 'h', ylim = c(0,1), yaxt = 'n')

#identify at which index points the word "sang" occurs
sang_v <- which(loweredtext == "sang")
sang_count_v <- rep(NA, times = length(n_time_v))
sang_count_v[sang_v] <- 1
plot(sang_count_v, main = "Dispersion plot of 'sang' in QN", xlab = "novelistic time", ylab = "sang", type = 'h', ylim = c(0,1), yaxt = 'n')

#identify at which index points the word "sung" occurs
sung_v <- which(loweredtext == "sung")
sung_count_v <-rep(NA, times = length(n_time_v))
sung_count_v[sung_v] <- 1
plot(sung_count_v, main = "Dispersion plot of 'sung' in QN", xlab = "novelistic time", ylab = "sung", type = 'h', ylim = c(0,1), yaxt = 'n')




#Section 2: correlation tests ----------------------------------------------------------------

#Split into chunks of 200 words each
library(tokenizers)
library(tidytext)
library(tibble)
library(readtext)
text_v <- readtext("C://Users//chris//Downloads//QuentaNoldorinwa.txt")
text_v
text_v <- tolower(text_v)
text_v

chunky_text <- text_v %>%
  chunk_text(200) #chunked sucessfully!
chunky_text
#turn the list into a matrix so it
#is easy to manipulate
chunky_text <- matrix(unlist(chunky_text), ncol =1, nrow =16)
chunky_text

#occurances of song variants and dance variants in each chunk
library(stringr)
song_temp <- str_count(chunky_text, pattern = "song")
sing_temp <- str_count(chunky_text, pattern = "sing")
sang_temp <- str_count(chunky_text, pattern = "sang")
sung_temp <- str_count(chunky_text, pattern = "sung")
singing_temp <- str_count(chunky_text, pattern = "singing")
sings_temp <- str_count(chunky_text, pattern = "sings")

dance_temp <- str_count(chunky_text, pattern = "dance")
dances_temp <- str_count(chunky_text, pattern = "dances")
dancing_temp <- str_count(chunky_text, pattern = "dancing")
danced_temp <- str_count(chunky_text, pattern = "danced")

song_variants_count <- song_temp+sing_temp+sang_temp+sung_temp+singing_temp+sings_temp 
song_variants_count

song_variants_count <- data.matrix(song_variants_count) 
song_variants_count

dance_variants_count <- dance_temp+dances_temp+dancing_temp+danced_temp
dance_variants_count
dance_variants_count <- data.matrix(dance_variants_count)
dance_variants_count

bound_both_variants <- cbind(song_variants_count, dance_variants_count) #both variants are bound together
head(bound_both_variants)


#Correlation test
cor(bound_both_variants)
