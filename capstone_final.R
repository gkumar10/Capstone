options(java.parameters = "-Xmx6g", stringsAsFactors = FALSE)
require(tm)
require(RWeka)
require(stringr)
require(stylo)
require(wordnet)
require(qdap)
require(openNLP)
setwd("~/Coursera/Capstone")

save(df1, file="~/Coursera/CapstoneData/ngram1df.rdata")
save(df2, file="~/Coursera/CapstoneData/ngram2df.rdata")
save(df3, file="~/Coursera/CapstoneData/ngram3df.rdata")
save(txt, file="~/Coursera/CapstoneData/enUS.rdata")

#load rdata file
load("~/Coursera/CapstoneData/ngram1df.rdata")
load("~/Coursera/CapstoneData/ngram2df.rdata")
load("~/Coursera/CapstoneData/ngram3df.rdata")
load("~/Coursera/CapstoneData/enUS.rdata")

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
txt11 <- "13. I see beauty everywhere I."
txt12 <- "And also surprised that that quote is so famous when a few pages later we have Toad"
txt13 <- "lol its"

sentence <- "zyrtec"

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
  
  last3 <- paste(as.character(sapply(strsplit(sentence, " "), tail, 3)), collapse=" ") #returns a character vector of length 3.
  last2 <- paste(as.character(sapply(strsplit(sentence, " "), tail, 2)), collapse=" ") #returns a character vector of length 2.
  last1 <- paste(as.character(sapply(strsplit(sentence, " "), tail, 1)), collapse=" ") #returns a character vector of length 1.
  
  i3 <- df3[grep(paste("^", last2, " ", sep=""), df3$ngram),] #returns rows with similar characters
    if (nrow(i3) > 0) {
      i3 <- i3[order(i3$freq, decreasing = TRUE),] #order using frequency
    } else {
#      print("no 2 word match in 3-gram df")
    }
  
  i2 <- df2[grep(paste("^", last1, " ", sep=""), df2$ngram),] #returns rows with similar characters
    if (nrow(i2) > 0) {
      i2 <- i2[order(i2$freq, decreasing = TRUE),] #order them using frequency
    } else {
#      print("no match to first word in 2-gram df")
    }
  
  i <- rbind(i3[order(i3$freq, decreasing=TRUE),], i2[order(i2$freq, decreasing=TRUE),])

  if (nrow(i)==0) {
    print(head(df1$ngram, c(2:4)))
  } else {
    i$nextword <- word(i$ngram, -1)
    print(head(unique(i$nextword), 4))
  }
}
