#============================================================================= #
#'A mere maiden': Exploring Lúthien Tinúviel's  relationship with dance and song
#with tf-idf scores and fuzzy matching 
#============================================================================= #


#Section 1: term frequency basics --------------------------------------------------

text_v <- scan("C://Users//chris//Downloads//TaleofTinuviel.txt", what= "character")
text_v
text_v[966]
text_v <- tolower(text_v)
text_v

loweredtext <- strsplit(text_v, "\\W")
loweredtext
str(loweredtext)

#How many words are "dance"
dance_hits_v <- length(loweredtext[which(loweredtext=="dance")])
dance_hits_v #9 hits
#How many words are "dancing"
dancing_hits_v <- length(loweredtext[which(loweredtext=="dancing")])
dancing_hits_v #8 hits
#How many words are "danced"
danced_hits_v <- length(loweredtext[which(loweredtext=="danced")])
danced_hits_v #11
#How many words are "dances"
dances_hits_v <- length(loweredtext[which(loweredtext=="dances")])
dances_hits_v #2

#Therefore total number of all iterations of danc- is 9+8+11+2 = 30
totaldancehits_v = 30
total_words_v <- length(loweredtext)
totaldancehits_v/total_words_v #which tells us that "danc-" makes up 0.0023 of the whole Tale of Tinuviel

#How many words are "song"
song_hits_v <- length(loweredtext[which(loweredtext=="song")])
song_hits_v #12
#How many words are "sing"
sing_hits_v <- length(loweredtext[which(loweredtext=="sing")])
sing_hits_v #1
#How many words are "singing"
singing_hits_v <- length(loweredtext[which(loweredtext=="singing")])
singing_hits_v #2
#How many words are "sang"
sang_hits_v <- length(loweredtext[which(loweredtext=="sang")])
sang_hits_v#7
#How many words are "sings"
sings_hits_v <- length(loweredtext[which(loweredtext=="sings")])
sings_hits_v #1
#How many words are "sung"
sung_hits_v <- length(loweredtext[which(loweredtext=="sung")])
sung_hits_v #1

#Therefore total number of all iterations of sing- is 12+1+2+7+1+1 = 24
totalsinghits_v = 24
totalsinghits_v/total_words_v #which tells us that "sing-"'s iterations makes up 0.0018 of the whole Tale of Tinuviel



#Section 2: correlation tests ----------------------------------------------------------------

#Section 3: fuzzy matching (ngrams) -------------------------------------------