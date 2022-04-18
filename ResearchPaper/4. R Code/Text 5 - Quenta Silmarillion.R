#============================================================================= #
#'A mere maiden': Exploring Lúthien Tinúviel's  relationship with dance and song
#with tf-idf scores and fuzzy matching 
#Text 5 only - Quenta Silmarillion
#============================================================================= #

#Section 1: term frequency basics --------------------------------------------------
#1a counting sing vs dance ........................................................
text_v <- scan("C://Users//chris//Downloads//QuentaSilmarillion.txt", what= "character")
text_v
text_v <- tolower(text_v)
text_v

loweredtext <- strsplit(text_v, "\\W")
str(loweredtext)
loweredtext

#How many words are "dance"
dance_hits_v <- length(loweredtext[which(loweredtext=="dance")])
dance_hits_v #1 hits
#How many words are "dancing"
dancing_hits_v <- length(loweredtext[which(loweredtext=="dancing")])
dancing_hits_v #0 hits
#How many words are "danced"
danced_hits_v <- length(loweredtext[which(loweredtext=="danced")])
danced_hits_v #2 hits
#How many words are "dances"
dances_hits_v <- length(loweredtext[which(loweredtext=="dances")])
dances_hits_v #0

#Therefore total number of all iterations of danc- is 3
totaldancehits_v = 3
total_words_v <- length(loweredtext)
totaldancehits_v/total_words_v #which tells us that "danc-" makes up 8.1e-05  of the whole Silmarillion



#How many words are "song"
song_hits_v <- length(loweredtext[which(loweredtext=="song")])
song_hits_v #21
#How many words are "sing"
sing_hits_v <- length(loweredtext[which(loweredtext=="sing")])
sing_hits_v #2
#How many words are "singing"
singing_hits_v <- length(loweredtext[which(loweredtext=="singing")])
singing_hits_v #3
#How many words are "sang"
sang_hits_v <- length(loweredtext[which(loweredtext=="sang")])
sang_hits_v#9
#How many words are "sings"
sings_hits_v <- length(loweredtext[which(loweredtext=="sings")])
sings_hits_v #0
#How many words are "sung"
sung_hits_v <- length(loweredtext[which(loweredtext=="sung")])
sung_hits_v #1

#Therefore total number of all iterations of sing- is 21+2+3+9+1 = 36
totalsinghits_v = 36
totalsinghits_v/total_words_v #which tells us that "sing-"'s iterations makes up 0.0007 of the whole Silmarillion