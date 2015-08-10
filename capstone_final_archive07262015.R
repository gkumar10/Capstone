options(java.parameters = "-Xmx6g", stringsAsFactors = FALSE)
require(tm)
require(RWeka)
require(stringr)
require(stylo)
require(wordnet)
require(qdap)
require(openNLP)
setwd("~/Coursera/Capstone")

#read in complete dataset. reads as character class
blogs <- readLines("~/Coursera/CapstoneData/final/en_US/en_US.blogs.txt", skipNul=TRUE)
news <- readLines("~/Coursera/CapstoneData/final/en_US/en_US.news.txt", skipNul=TRUE)
twitter <- readLines("~/Coursera/CapstoneData/final/en_US/en_US.twitter.txt", skipNul=TRUE)

save(df1, file="~/Coursera/CapstoneData/ngram1df.rdata")
save(df2, file="~/Coursera/CapstoneData/ngram2df.rdata")
save(df2n, file="~/Coursera/CapstoneData/ngram2dfNoStopWords.rdata")
save(df3, file="~/Coursera/CapstoneData/ngram3df.rdata")
save(df3n, file="~/Coursera/CapstoneData/ngram3dfNoStopWords.rdata")
save(txtf, file="~/Coursera/CapstoneData/enUSFullLines.rdata")
save(txtn, file="~/Coursera/CapstoneData/enUSNoStopWords.rdata")
save(txt, file="~/Coursera/CapstoneData/enUSAllWords.rdata")

#load rdata file
load("~/Coursera/CapstoneData/ngram1df.rdata")
load("~/Coursera/CapstoneData/ngram2df.rdata")
load("~/Coursera/CapstoneData/ngram3df.rdata")
load("~/Coursera/CapstoneData/enUSAllWords.rdata")
load("~/Coursera/CapstoneData/enUS.rdata")
load("~/Coursera/CapstoneData/enUSFullLines.rdata")
load("~/Coursera/CapstoneData/ngram2dfNoStopWords.rdata")
load("~/Coursera/CapstoneData/enUSNoStopWords.rdata")
load("~/Coursera/CapstoneData/enUSFullLines.rdata")

sapply(strsplit(str1, " "), length)
df3n$ngram <- sapply(df3n$ngram, as.character)

#create sample data set for faster processing
blogs_sample <- blogs[rbinom(length(blogs)*.01, length(blogs), .5)]
news_sample <- news[rbinom(length(news)*.01, length(news), .5)]
twitter_sample <- twitter[rbinom(length(twitter)*.01, length(twitter), .5)]

#trying to do this w/o creating a corpus
txt <- c(blogs_sample, news_sample, twitter_sample)
txt <- c(blogs, news, twitter)
txt <- gsub("[^[:alnum:][:space:]']", "", txt)
txt <- gsub("[[:digit:]]", "", txt)
txtf <- tolower(txt)
txt <- unlist(strsplit(txt, " "))

#create n-gram df
ng2 <- make.ngrams(txt, ngram.size=2)
ng2 <- gsub("^ ", "", ng2)
ng2 <- gsub(" $", "", ng2)
df2 <- data.frame(table(ng2), stringsAsFactors = FALSE)
names(df2) <- c("ngram", "freq")
df2 <- df2[order(df2$freq, decreasing=TRUE), ]
df2 <- df2[df2$freq >= 25, ]
df2 <- df2[nchar(as.character(df2$ngram)) > 3, ]

ng3 <- make.ngrams(txt, ngram.size=3)
ng3 <- gsub("^ ", "", ng3)
ng3 <- gsub(" $", "", ng3)
df3 <- data.frame(table(ng3), stringsAsFactors = FALSE)
rm(ng3)
names(df3) <- c("x", "freq")
df3n$ngram <- sapply(df3n$x, as.character)
df3n <- df3n[order(df3n$freq, decreasing=TRUE), ]
df3n <- df3[df3$freq >= 25, ]
df3n <- df3n[nchar(df3n$ngram) > 3, ]


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

func1 <- function(sentence){

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

#print second word
# i <- i[order(i$freq, decreasing=TRUE),]  
# x <- sapply(strsplit(as.character(i$words), " "), head, 2) #get first 2 words
# x <- head(x[2,], 4)
# print(x) #prints 4 choices for 2nd word
#}

a <- "[game] [offense] [struggling]"
x <- regexpr(pattern=a, txt[grep(a, txt)])
y <- substr(txt[grep(a, txt)], start=x+nchar(a), stop=x+nchar(a)+20)
y <- gsub("^ ", "", y)
z <- word(y, 1)
head(z[order(table(z), decreasing = TRUE)],20)

# ngram2_5 <- NGramTokenizer(txt[1:500000], Weka_control(min=2, max=5, delimiters=(" .,;:?!"), M=50))
# ngram2_5df <- data.frame(table(ngram2_5)); names(ngram2_5df) <- c("words", "freq")
# 
# ngram_master <- ngram2_df
# ngram_master <- rbind(ngram_master, ngram2_df)
# 
# blogs1 <- NGramTokenizer(txt, Weka_control(min=2, max=5, delimiters=(" .,;:?!")))
# blogs1df <- data.frame(table(blogs1)); names(blogs1) <- c("words", "freq")
# write.csv(blogs1df, "blogs1df.csv", row.names = FALSE)
# 
# news1 <- NGramTokenizer(txt, Weka_control(min=2, max=5, delimiters=(" .,;:?!")))
# news1df <- data.frame(table(news1)); names(news1) <- c("words", "freq")
# write.csv(news1df, "news1df.csv", row.names = FALSE)
# 
# twitter1 <- NGramTokenizer(txt, Weka_control(min=2, max=5, delimiters=(" .,;:?!")))
# twitter1df <- data.frame(table(twitter1)); names(twitter1) <- c("words", "freq")
# write.csv(twitter1df, "twitter1df.csv", row.names = FALSE)
# 
# ngram2 <- NGramTokenizer(txt, Weka_control(min=2, max=5, M=25))
# ngram2df <- data.frame(table(ngram2)); names(ngram2df) <- c("words", "freq")
# ngram3 <- NGramTokenizer(txt, Weka_control(min=3, max=3))
# ngram3df <- data.frame(table(ngram3)); names(ngram3df) <- c("words", "freq")
# ngram4 <- NGramTokenizer(txt, Weka_control(min=4, max=4))
# ngram4df <- data.frame(table(ngram4)); names(ngram4df) <- c("words", "freq")
# ngram5 <- NGramTokenizer(txt, Weka_control(min=5, max=5))
# ngram5df <- data.frame(table(ngram5)); names(ngram5df) <- c("words", "freq")
# #ngramdf <- rbind(ngram1df, ngram2df, ngram3df, ngram4df, ngram5df)
# 
# #write to file. to be read later in Corpus creating
# # write.csv(blogs_sample, file = "~/Coursera/CapstoneData/sample/en_US.blogs.csv", row.names = FALSE, col.names=FALSE)
# # write.csv(news_sample, file = "~/Coursera/CapstoneData/sample/en_US.news.csv", row.names = FALSE, col.names=FALSE)
# # write.csv(twitter_sample, file = "~/Coursera/CapstoneData/sample/en_US.twitter.csv", row.names = FALSE, col.names=FALSE)
# write.csv(blogs, file = "~/Coursera/CapstoneData/sample/en_US.blogs.csv", row.names = FALSE, col.names=FALSE)
# write.csv(news, file = "~/Coursera/CapstoneData/sample/en_US.news.csv", row.names = FALSE, col.names=FALSE)
# write.csv(twitter, file = "~/Coursera/CapstoneData/sample/en_US.twitter.csv", row.names = FALSE, col.names=FALSE)
# 
# #create corpus
# #mycorpus <- Corpus(DirSource("~/Coursera/CapstoneData/sample/"), readerControl=list(language="en_US"))
# mycorpus <- Corpus(VectorSource(txt), readerControl=list(language="en_US"))
# mycorpus <- tm_map(mycorpus, removePunctuation)
# mycorpus <- tm_map(mycorpus, removeNumbers)
# mycorpus <- tm_map(mycorpus, content_transformer(tolower))
# mycorpus <- tm_map(mycorpus, removeWords, c("=", " =", " = ", "= "))
# mycorpusTDM <- TermDocumentMatrix(mycorpus)
# 
# #create n-gram df
# # ngram1 <- NGramTokenizer(mcyorpusUS, Weka_control(min=1, max=1))
# # ngram1df <- data.frame(table(ngram1))
# ngram2 <- NGramTokenizer(mycorpus, Weka_control(min=2, max=2))
# ngram2df <- data.frame(table(ngram2))
# ngram3 <- NGramTokenizer(mycorpus, Weka_control(min=3, max=3))
# ngram3df <- data.frame(table(ngram3))
# ngram4 <- NGramTokenizer(mycorpus, Weka_control(min=4, max=4))
# ngram4df <- data.frame(table(ngram4))
# ngram5 <- NGramTokenizer(mycorpus, Weka_control(min=5, max=5))
# ngram5df <- data.frame(table(ngram5))

