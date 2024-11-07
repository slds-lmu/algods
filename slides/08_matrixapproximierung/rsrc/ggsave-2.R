

library(RCurl)
library(XML)
library(tm)
library(NMF)
library(microbenchmark)
library(ggplot2)
library(wordcloud)



options(digits = 2)
  

tdm = readRDS("rsrc/tdm")
      
head(tdm)



set.seed(1)
res = nmf(tdm, 2, "Frobenius")

  
wordmatrix = as.data.frame(basis(res)) # topic-word-matrix
    
wordmatrix$word = rownames(wordmatrix)
colnames(wordmatrix) = c("topic1", "topic2", "word")
    
head(wordmatrix)[1:2]

df = wordmatrix[order(- wordmatrix[, 1]), ]
df = as.data.frame(df[1:30, ])

wordcloud(df$word,df$topic1)




df = wordmatrix[order(- wordmatrix[, 2]), ]
df = as.data.frame(df[1:30, ])

wordcloud(df$word,df$topic2)