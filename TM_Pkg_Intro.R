rm(list=ls(all=T))

setwd("C:/Users/jeevan/Desktop/Day02")

## ----load_packages-------------------------------------------------------
library(tm)              # Framework for text mining.
library(dplyr)           # Data wrangling, pipe operator %>%().
library(magrittr)         
library(ggplot2)
library(RColorBrewer)    # Generate palette of colours for plots.
library(wordcloud)

## location of text documents
cname <- file.path(getwd(), "corpus", "txt")
cname

cname
dir(cname)

##loading the corpus
# Corpus, representing a collection of text documents. 

docs <- Corpus(DirSource(cname))   
class(docs)
class(docs[[1]])
summary(docs)
inspect(docs)

# A character representation of a document is available via as.character()
writeLines(as.character(docs[[1]]))


## ------------------------------------------------------------------------
viewDocs <- function(d, n) {d %>% extract2(n) %>% as.character() %>% writeLines()}
viewDocs(docs, 16)

## ------------------------------------------------------------------------
getTransformations()

#[1] "removeNumbers"     "removePunctuation" "removeWords"      
#[4] "stemDocument"      "stripWhitespace" 

## ----transformatioms-----------------------------------------------------
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

## ------------------------------------------------------------------------
## docs <- tm_map(docs, toSpace, "/|@|\\|")
rm(toSpace)

viewDocs(docs, 16)

## ------------------------------------------------------------------------
docs <- tm_map(docs, content_transformer(tolower))

viewDocs(docs, 16)

## ------------------------------------------------------------------------
docs <- tm_map(docs, removeNumbers)

viewDocs(docs, 16)

## ------------------------------------------------------------------------
docs <- tm_map(docs, removePunctuation)

viewDocs(docs, 16)

## ------------------------------------------------------------------------
docs <- tm_map(docs, removeWords, stopwords("english"))

viewDocs(docs, 16)

## ----list_stopwords------------------------------------------------------
length(stopwords("english"))
stopwords("english")

## ----remove_own_stopwords------------------------------------------------
# docs <- tm_map(docs, removeWords, c("department", "email"))
# viewDocs(docs, 16)

## ------------------------------------------------------------------------
docs <- tm_map(docs, stripWhitespace)

viewDocs(docs, 16)

## ----specific_transforms-------------------------------------------------
toString <- content_transformer(function(x, from, to) gsub(from, to, x))

docs <- tm_map(docs, toString, "america", "USA")
rm(toString)
viewDocs(docs, 16)

## ------------------------------------------------------------------------
docs <- tm_map(docs, stemDocument)

viewDocs(docs, 16)


## ----create_document_term_matrix, out.lines=20---------------------------
dtm <- DocumentTermMatrix(docs)

dtm

## ----inspect_dtm---------------------------------------------------------
inspect(dtm[1:5, 1000:1005])

## ----dtm_matrix----------------------------------------------------------
class(dtm)
dim(dtm)

## ----create_term_document_matrix, out.lines=20---------------------------
tdm <- TermDocumentMatrix(docs)
tdm

rm(tdm)

## ------------------------------------------------------------------------
freq <- colSums(as.matrix(dtm))
length(freq)

## ----out.lines=10--------------------------------------------------------
ord <- order(-freq)

# Most frequent terms.
freq[head(ord)]

## ------------------------------------------------------------------------
# Least frequent terms.
freq[tail(ord)]

# Frequency of frequencies.
head(table(freq), 15)
tail(table(freq), 15)

## ----dtm_to_m------------------------------------------------------------
m <- as.matrix(dtm)
dim(m)

## ----remove_sparse_terms-------------------------------------------------
dim(dtm)
dtms <- removeSparseTerms(dtm, 0.1)
#(1-0.1)*24 = 21. Retains words with frequency > 21
# as.matrix(removeSparseTerms(myTdm, .01))
# as.matrix(removeSparseTerms(myTdm, .99))
# as.matrix(removeSparseTerms(myTdm, .5))


dim(dtms)

## ------------------------------------------------------------------------
inspect(dtms)

## ------------------------------------------------------------------------
freq <- colSums(as.matrix(dtms))
freq
table(freq)

## ----freq_terms_1000-----------------------------------------------------
findFreqTerms(dtms, lowfreq=100)

## ----wordcloud, echo=FALSE-----------------------------------------------

set.seed(123)
wordcloud(names(freq), freq, min.freq=100)

## ----wordcloud_max_words-------------------------------------------------
set.seed(142)
wordcloud(names(freq), freq, max.words=100)

## ----wordcloud_colour----------------------------------------------------    
set.seed(142)
wordcloud(names(freq), freq, min.freq=100, colors=brewer.pal(6, "Dark2"))