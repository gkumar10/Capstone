if (!exists("df1")) {
  load("ngram1df.rdata")
}

if (!exists("df2")) {
  load("ngram2df.rdata")
}

if (!exists("df3")) {
  load("ngram3df.rdata")
}

if (!exists("strings")) {
  require(stringr)
}

nextword <- function(sentence)
{
  if (sentence=="") {
    stop()
  }
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
    nextword <- unique(goodturing(last1))
  } else {
    i$nextword <- word(i$ngram, -1)
    nextword <- head(unique(i$nextword), 6)
  }
}

goodturing <- function(lastword){
  r <- as.numeric(df1$freq[grep(paste("^", lastword, "$", sep=""), df1$ngram)])
  nr1 <- ifelse(nrow(df1[df1$freq==(r+1),]) == 0, (2.3 + (-0.17 * 0)), nrow(df1[df1$freq==(r+1),]))
  rstar <- (r + 1) * nr1/nrow(df1[df1$freq==r,])
  rstar <- ifelse(rstar >=0, rstar, rstar * -1)
  goodturingprob <- rstar/sum(df1$freq)
  i <- df1[df1$prob >= goodturingprob,]
  charset <- c("is", "in", "for", "as", "at", "the", "of", "and", "a", "to")
  goodturing <- if (nrow(i) >= 1) head(i$ngram, 6) else sample(charset, 6)
}