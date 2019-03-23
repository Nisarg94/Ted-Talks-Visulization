suppressPackageStartupMessages(library(anytime))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(RJSONIO))
suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(igraph))
suppressPackageStartupMessages(library(reshape))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(SnowballC))
suppressPackageStartupMessages(library(wordcloud))
library(shiny)
setwd("C:/Users/nisar/OneDrive/Desktop/Depaul/CSC-465_DataVisulization/Project_TedTalks");
getwd();
df <- read.csv("ted_main_edited.csv")


df$film_date <- as.POSIXct(df$film_date, origin="1970-01-01",tz ="GMT")
df$published_date <- as.POSIXct(df$published_date, origin="1970-01-01",tz ="GMT")

month_order = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
day_order = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')
df$month <- month_order[month(df$film_date)]
df$month <- factor(df$month, levels = month_order)
df$year <- year(df$film_date)

####################

# Text as visualization (Word Cloud)

####################


df1 <- read.csv("transcripts.csv",stringsAsFactors = FALSE)
df2 <- base::merge(df,df1,by ="url")
texts <- df2$transcript
docs <- Corpus(VectorSource(texts))
docs <- tm_map(docs, PlainTextDocument)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeWords, stopwords('english'))
docs <- tm_map(docs, stemDocument)
docs <- tm_map(docs, removeWords, c("and", "this", "there","â€”","can","one","like","peopl","now","new","year","thing","get","know")) 
docs <- Corpus(VectorSource(docs))
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
d <- d[-which(d$word %in% c("and","this","that","there","â€”","can","one","like","peopl","now","new","year","thing","get","know")),]
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, max.freq = 2000,
          max.words=300 , random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

