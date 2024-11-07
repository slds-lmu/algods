library(RCurl)
library(XML)
library(tm)
library(ggplot2)
library(NMF)

# URLs for analysis
urls = c("https://www.nytimes.com/2017/11/05/technology/machine-learning-artificial-intelligence-ai.html?rref=collection%2Fsectioncollection%2Ftechnology&action=click&contentCollection=technology&region=rank&module=package&version=highlights&contentPlacement=2&pgtype=sectionfront",
         "https://www.nytimes.com/2017/09/10/business/warehouse-robots-learning.html?action=click&contentCollection=Technology&module=RelatedCoverage&region=Marginalia&pgtype=article",
         "https://www.nytimes.com/2017/09/10/technology/amazon-robots-workers.html?action=click&contentCollection=Business%20Day&module=RelatedCoverage&region=Marginalia&pgtype=article"
  )

for (i in 1:length(urls)) {
  html = getURL(urls[i], followlocation = TRUE)
  doc = htmlParse(html, asText = TRUE)
  plain.text = xpathSApply(doc, "//p", xmlValue)
  write.table(plain.text, paste("rsrc/txt/doc", i, ".txt", sep = ""), sep = "\t", fileEncoding = "ASCII")
}

cname = file.path("rsrc/txt")
length(dir(cname))

docs = Corpus(DirSource(cname))

toSpace = content_transformer(function(x, pattern) gsub(pattern, " ", x))

docs = tm_map(docs, toSpace, "/|@|\\|")
docs = tm_map(docs, content_transformer(tolower))
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, removeWords, c(stopwords("english"), "can", "like", "will", "building"))
docs = tm_map(docs, stripWhitespace)
tdm = TermDocumentMatrix(docs)


tdm = as.data.frame(as.matrix(tdm))
colnames(tdm) = c("doc1", "doc2", "doc3")

saveRDS("rsrc/tdm", object = tdm)

