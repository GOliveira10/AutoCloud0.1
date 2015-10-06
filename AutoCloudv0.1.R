install.packages("tm")
install.packages("tau")
install.packages("wordcloud")
library(tau)
library(tm)
library(wordcloud)
## run this function and write the name of the file in your working directory 
## (with its extension!) in quotations between the parentheses

wordCloud <- function(x){
        fileName <- paste("./", x , sep="")
        txt = readLines(fileName, warn=FALSE)
        txt = paste(txt, collapse = " ") 
        myCorpus = Corpus(VectorSource(txt))
        myCorpus = tm_map(myCorpus, tolower)
        myCorpus = tm_map(myCorpus, removePunctuation)
        myCorpus = tm_map(myCorpus, removeNumbers)
        myCorpus = tm_map(myCorpus, removeWords, stopwords("english"))
        myCorpus <- tm_map(myCorpus, PlainTextDocument)
        
        myDTM = TermDocumentMatrix(myCorpus, control = 
                                        list(minWordLength = 1))
        
        m = as.matrix(myDTM)
        
        v = sort(rowSums(m), decreasing = TRUE)
        
        set.seed(42)
        cloudName <- gsub(".txt", "", x)
        png(paste(cloudName,"CLOUD",'.png',sep=""))
        wordcloud(names(v), v, min.freq = 5)
        dev.off()
}  
