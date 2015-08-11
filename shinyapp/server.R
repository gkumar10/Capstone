library(shiny)

#load datasets

if (as.logical((exists("df1") & exists("df2") & exists("df3"))) == "FALSE") {

  load("~/Coursera/CapstoneData/ngram1df.rdata")
  load("~/Coursera/CapstoneData/ngram2df.rdata")
  load("~/Coursera/CapstoneData/ngram3df.rdata")
  
}

shinyServer(
  
  function(input, output) {
    #define output text
    output$nextword <- renderUI({
      HTML('You entered: ', input$sentence)
      print(nextword(input$sentence))
   })
})

nextword <- function(sentence)
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
    df1$prob <- df1$freq/sum(df1$freq)
    kbo(0.2)
    addone(last1)
    goodturing(last1)
    #    print(df1$ngram[sample(nrow(df1), 4)])
  } else {
    i$nextword <- word(i$ngram, -1)
    print(head(unique(i$nextword), 4))
    #print(tail(unique(i$nextword), 4))
  }
  rm(i, i2, i3)
}

#Katz Back Off
discount <- 0.2
kbo <- function(discount) {
  i2$kbo <- ifelse((i2$freq - discount)/sum(i2$freq) > 0, (i2$freq - discount)/sum(i2$freq), 0)
  missingmass <- 1 - sum(i2$kbo)
  df1$kbo <- missingmass * df1$prob/(nrow(df1) + df1$prob)
  df1 <- df1[order(df1$kbo, decreasing=TRUE),]
  print(head(df1$ngram, 4))
}

#Add-One Smoothing http://www.cs.sfu.ca/~anoop/teaching/CMPT-413-Spring-2014/smooth.pdf (slide 8,9)
addone <- function(lastword){
  addoneprob <- (1 + nrow(i3))/(nrow(df1) + df1$freq[grep(paste("^", lastword, "$", sep=""), df1$ngram)])
  df1$addone <- addoneprob * df1$prob
  df1 <- df1[order(df1$addone, decreasing=TRUE),]
  print(head(df1$ngram, 4))
}

#Good-Turing Smoothing http://www.cs.sfu.ca/~anoop/teaching/CMPT-413-Spring-2014/smooth.pdf (slide 11-14)
goodturing <- function(lastword){
  r <- df1$freq[grep(paste("^", lastword, "$", sep=""), df1$ngram)]
  nr1 <- ifelse(nrow(df1[df1$freq==(r+1),]) == 0, (2.3 + (-0.17 * r)), nrow(df1[df1$freq==(r+1),]))
  rstar <- (r + 1) * nr1/nrow(df1[df1$freq==r,])
  rstar <- ifelse(rstar >=0, rstar, rstar * -1)
  goodturingprob <- rstar/sum(df1$freq)
  print(goodturingprob)
}