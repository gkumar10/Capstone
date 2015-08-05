options(java.parameters = "-Xmx6g", stringsAsFactors = FALSE)
require(tm)
#require(RWeka)
require(stringr)
setwd("~/Coursera/Capstone")

save(df1, file="~/Coursera/CapstoneData/ngram1df.rdata")
save(df2, file="~/Coursera/CapstoneData/ngram2df.rdata")
save(df3, file="~/Coursera/CapstoneData/ngram3df.rdata")
save(txt, file="~/Coursera/CapstoneData/enUS.rdata")

#load rdata file
load("~/Coursera/CapstoneData/ngram1df.rdata")
load("~/Coursera/CapstoneData/ngram2df.rdata")
load("~/Coursera/CapstoneData/ngram3df.rdata")
load("~/Coursera/CapstoneData/ngram2dfNoStopWords.rdata")
load("~/Coursera/CapstoneData/ngram3dfNoStopWords.rdata")
load("~/Coursera/CapstoneData/enUS.rdata")

df1$whichngram <- c("unigram")
df2$whichngram <- c("bigram")
df3$whichngram <- c("trigram")
df2n$whichngram <- c("bigram")
df3n$whichngram <- c("trigram")

#sapply(strsplit(str1, " "), length)
#df3$ngram <- sapply(df3$ngram, as.character)

#code to grep last 1-2-3 words from sentence -> query against n-gram dfs -> return words with probability/frequency listed
txt1 <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
txt2 <- "You're the reason why I smile everyday. Can you follow me please? It would mean the"
txt3 <- "Hey sunshine, can you follow me and make me the"
txt4 <- "Very early observations on the Bills game: Offense still struggling but the" 
txt5 <- "Go on a romantic date at the"
txt6 <- "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
txt7 <- "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"
txt8 <- "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
txt9 <- "Be grateful for the good times and keep the faith during the"
txt10 <- "If this isn't the cutest thing you've ever seen, then you must be"

txt11 <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
txt12 <- "xxxGuy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
txt13 <- "xxxI'd give anything to see arctic monkeys this"
txt14 <- "Talking to your mom has the same effect as a hug and helps reduce your"
txt15 <- "xxxWhen you were in Holland you were like 1 inch away from me but you hadn't time to take a"
txt16 <- "xxxI'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
txt17 <- "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"
txt18 <- "Every inch of you is perfect from the bottom to the"
txt19 <- "I’m thankful my childhood was filled with imagination and bruises from playing"
txt20 <- "I like how the same people are in almost all of Adam Sandler's"

sentence <- "I am the"
func1(sentence)

#code to grep last 1-2-3 words from sentence -> query against n-gram dfs -> return words with probability/frequency listed
func1 <- function(sentence)
{

  #get last 1-3 words = n-1 number of ngrams available. 5 grams are available.
  sentence <- gsub("[[:punct:]]", "", sentence)
  sentence <- gsub("[[:digit:]]", "", sentence)
  sentence <- tolower(sentence)
  sentence <- gsub("^ ", "", sentence) #remove blank space in beginning
  sentence <- gsub(" $", "", sentence) #remove blank space in end
  
  last2 <- paste(as.character(sapply(strsplit(sentence, " "), tail, 2)), collapse=" ") #returns a character vector of length 2.
  last1 <- paste(as.character(sapply(strsplit(sentence, " "), tail, 1)), collapse=" ") #returns a character vector of length 1.
  
  i3 <- df3[grep(paste("^", last2, " ", sep=""), df3$ngram),] #returns rows with similar characters
  if ((nrow(i3) > 0) & (sapply(strsplit(last2, " "), length) > 1)) {
    i3 <- i3[order(i3$freq, decreasing = TRUE),] #order using frequency
    i3$prob <- i3$freq/sum(i3$freq)
  } else {
    i3 <- data.frame(ngram=0, freq=0, prob=0)
    i3 <- i3[FALSE,]
  }


  i2 <- df2[grep(paste("^", last1, " ", sep=""), df2$ngram),] #returns rows with similar characters
  if (nrow(i2) > 0) {
    i2 <- i2[order(i2$freq, decreasing = TRUE),] #order them using frequency
    i2$prob <- i2$freq/sum(i2$freq)
  } else {
    i2 <- data.frame(ngram=0, freq=0, prob=0)
    i2 <- i2[FALSE,]
  }

  i <- rbind(i3[order(i3$freq, decreasing=TRUE),], i2[order(i2$freq, decreasing=TRUE),])
  i <- i[order(i$prob, decreasing=TRUE),]

  if (nrow(i)==0) {
    print(df1$ngram[sample(nrow(df1), 4)])
  } else {
    i$nextword <- word(i$ngram, -1)
    print(head(unique(i$nextword), 4))
    print(tail(unique(i$nextword), 4))
  }
  rm(i, i2, i3)
}

discount <- 0.1
kbo <- function(discount) {
  if ((i3$prob - discount) < 0) {
    i3$kbo == 0  
  } else {
    i3$kbo <- i3$prob - discount
  }
  i3$kbo <- sapply(i3$kbo < 0, i3$kbo==0)
  missingmass <- 1 - sum(i3$kbo)
}