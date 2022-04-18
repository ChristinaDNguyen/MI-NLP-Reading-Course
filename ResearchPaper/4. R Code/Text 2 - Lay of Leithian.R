#============================================================================= #
#'A mere maiden': Exploring Lúthien Tinúviel's  relationship with dance and song
#with tf-idf scores and fuzzy matching 
#Text 2 only - Lay of Leithian
#============================================================================= #

#Section 1: term frequency basics --------------------------------------------------
#1a counting sing vs dance ........................................................
text_v <- scan("C://Users//chris//Downloads//LayofLeithian.txt", what= "character")
text_v
text_v[966]
text_v <- tolower(text_v)
text_v

loweredtext <- strsplit(text_v, "\\W")
str(loweredtext)
loweredtext

#How many words are "dance"
dance_hits_v <- length(loweredtext[which(loweredtext=="dance")])
dance_hits_v #6 hits
#How many words are "dancing"
dancing_hits_v <- length(loweredtext[which(loweredtext=="dancing")])
dancing_hits_v #11 hits
#How many words are "danced"
danced_hits_v <- length(loweredtext[which(loweredtext=="danced")])
danced_hits_v #3
#How many words are "dances"
dances_hits_v <- length(loweredtext[which(loweredtext=="dances")])
dances_hits_v #2

#Therefore total number of all iterations of danc- is 6+11+3+2=22
totaldancehits_v = 22
total_words_v <- length(loweredtext)
totaldancehits_v/total_words_v #which tells us that "danc-" makes up 0.0010 of the whole Lay of Leithian

#How many words are "song"
song_hits_v <- length(loweredtext[which(loweredtext=="song")])
song_hits_v #26
#How many words are "sing"
sing_hits_v <- length(loweredtext[which(loweredtext=="sing")])
sing_hits_v #12
#How many words are "singing"
singing_hits_v <- length(loweredtext[which(loweredtext=="singing")])
singing_hits_v #17
#How many words are "sang"
sang_hits_v <- length(loweredtext[which(loweredtext=="sang")])
sang_hits_v#19
#How many words are "sings"
sings_hits_v <- length(loweredtext[which(loweredtext=="sings")])
sings_hits_v #3
#How many words are "sung"
sung_hits_v <- length(loweredtext[which(loweredtext=="sung")])
sung_hits_v #3

#Therefore total number of all iterations of sing- is 26+12+17+19+3+3=80
totalsinghits_v = 80
totalsinghits_v/total_words_v #which tells us that "sing-"'s iterations makes up 0.0037 of the whole Lay of Leithian