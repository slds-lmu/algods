

library(RCurl)
library(XML)
library(tm)
library(NMF)
library(microbenchmark)
library(ggplot2)
library(wordcloud)

X = t(matrix(c(5, NA, 3, NA,
               5, 4, 3, 3,
               2, NA, 5, NA,
               5, 5, 3, 1,
               1, 2, 5, 5,
               1, 2, 4, 5), ncol = 6))

movies = c("Die Hard", "Top Gun", "Titanic", "Notting Hill")
colnames(X) = movies

users = c("User 1", "User 2", "User 3", "User 4", "User 5", "User 6")
rownames(X) = users

X = ifelse(is.na(X), rowMeans(X, na.rm = TRUE), unlist(X))



set.seed(1)
res = nmf(X, rank = 2)

W = res@fit@W
H = res@fit@H


colnames(W) = c("Action", "Romance")

rownames(H) = c("Action", "Romance")


W.df = as.data.frame(W)
W.df$Group = as.factor(c(1, 1, 2, 1, 2, 2))

ggplot(data = W.df, aes(x = Action, y = Romance, col = Group, label = rownames(W.df))) + geom_point(size = 6) + geom_text(vjust = 2) + theme_bw()



H.df = as.data.frame(t(H))

ggplot(data = H.df, aes(x = Action, y = Romance, p, label = rownames(H.df))) + geom_point(size = 5) + geom_text(vjust = 2) + theme_bw()

