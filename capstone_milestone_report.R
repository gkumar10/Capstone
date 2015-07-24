require(tm)
require(RWeka)
setwd("~/Coursera/Capstone")

#read in complete dataset. reads as character class
blogs <- readLines("~/Coursera/CapstoneData/final/en_US/en_US.blogs.txt", skipNul=TRUE, n=15000)
news <- readLines("~/Coursera/CapstoneData/final/en_US/en_US.news.txt", skipNul=TRUE, n=15000)
twitter <- readLines("~/Coursera/CapstoneData/final/en_US/en_US.twitter.txt", skipNul=TRUE, n=15000)

#create sample data set for faster processing
blogs_sample <- sample(blogs, size=10)
news_sample <- sample(news, size=10)
twitter_sample <- sample(twitter, size=10)

#word count
sum(sapply(strsplit(blogs_sample, " "), length))
sum(sapply(strsplit(news_sample, " "), length))
sum(sapply(strsplit(twitter_sample, " "), length))

write.csv(blogs_sample, file = "~/Coursera/CapstoneData/sample/en_US.blogs.csv", row.names = FALSE, col.names=FALSE)
write.csv(news_sample, file = "~/Coursera/CapstoneData/sample/en_US.news.csv", row.names = FALSE, col.names=FALSE)
write.csv(twitter_sample, file = "~/Coursera/CapstoneData/sample/en_US.twitter.csv", row.names = FALSE, col.names=FALSE)

corpusUS <- Corpus(DirSource("~/Coursera/CapstoneData/sample/"), readerControl=list(language="en_US"))
corpusUS <- tm_map(corpusUS, removePunctuation)
corpusUS <- tm_map(corpusUS, removeNumbers)
corpusUS <- tm_map(corpusUS, content_transformer(tolower))
corpusUS <- tm_map(corpusUS, removeWords, c("="))
corpusUS1TDM <- TermDocumentMatrix(corpusUS1)

#corpusUS <- Corpus(VectorSource(c(twitter_sample, blogs_sample, news_sample)), readerControl=list(language="en_US"))
corpusUS <- Corpus(VectorSource(blogs_sample), readerControl=list(reader=readPlain, language="en_US"))
dtm <- DocumentTermMatrix(corpusUS)
dtm1 <- as.matrix(dtm)
f <- colSums(dtm1)
f1 <- sort(f, decreasing = TRUE)
head(f1)
plot(dtm, terms=findFreqTerms(tdm)[1:25])
temp <- inspect(dtm)
FreqMat <- data.frame(word = rownames(temp), freq = rowSums(temp))

for (i in 1:30000) {sapply(sapply(strsplit(as.character(corpusUS[[i]][1]), " "), length), sum)}

#combine sample dataset into a Corpus. for final submission, i will replace sample data with complete dataset
#corpusUS <- Corpus(VectorSource(c(twitter_sample, blogs_sample, news_sample)), readerControl=list(language="en_US", 
#                                                                                                  removePunctuation=TRUE,
#                                                                                                  stopwords=TRUE,
#                                                                                                  removeNumbers=TRUE,
#                                                                                                  content_transformer(tolower),
#                                                                                                  stripWhitespace=TRUE))


gram1 <- NGramTokenizer(corpusUS, Weka_control(min=1, max=1))
freqgram <- data.frame(table(gram1))
sort.freqgram <- freqgram[order(freqgram$Freq,decreasing = TRUE),]
f_top10<-head(freqgram,10)
barplot(f_top10$Freq, names.arg = f_top10$gram2, border=NA, las=2, main="Top 10 Most Frequent TriGrams", cex.main=2)

gram2 <- NGramTokenizer(corpusUS, Weka_control(min=2, max=2))
gram2 <- data.frame(table(gram2))
gram2 <- gram2[order(gram2$Freq, decreasing=TRUE),]
barplot(head(gram2$Freq, 10), 
        names.arg = head(gram2$gram2,10),
        main="Top 10 Most Frequent BIGrams")


#convert to lower. need to use content_transformer to retain PlainTextDocument vector
#a <- tm_map(corpusUS, content_transformer(tolower))

#remove some vectors to save on space
rm(blogs, news, twitter, blogs_sample, news_sample, twitter_sample)

#data pre-processing
x <- tm_map(corpusUS, removeNumbers)
a <- TermDocumentMatrix(corpusUS, control=list())
findFreqTerms(a, 0, 100)

b <- DocumentTermMatrix(corpusUS, control=list())

#extra code. reads file as data frame class
USb <- read.table("~/Coursera/CapstoneData/final/en_US/en_US.blogs.txt", sep=" ", strip.white=TRUE, fill=TRUE, skipNul=TRUE, quote="")
USn <- read.table("~/Coursera/CapstoneData/final/en_US/en_US.news.txt", sep=" ", strip.white=TRUE, fill=TRUE, skipNul=TRUE, quote="")
USt <- read.table("~/Coursera/CapstoneData/final/en_US/en_US.twitter.txt", sep=" ", strip.white=TRUE, fill=TRUE, skipNul=TRUE, quote="")

#Task 4: Modeling
