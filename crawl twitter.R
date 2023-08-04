

library(twitteR)
library(ROAuth)
library(RCurl)

download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
api_key <- "XXXXXXXXXXXXXXX"
api_secret <- "XXXXXXXXXXXXXXX"
access_token <- "XXXXXXXXXXXXXXX"
access_secret <- "XXXXXXXXXXXXXXX"
setup_twitter_oauth(api_key, api_secret, access_token, access_secret)

search.string <- "permendikbud"
no.of.tweets <- 10000

permendikbud.Tweets <- searchTwitter(search.string, n=no.of.tweets,lang="id")
df_permendikbud <- do.call("rbind", lapply(permendikbud.Tweets, as.data.frame))
View(df_permendikbud)

write.csv(df_permendikbud, file='D:/permendikbud.csv',row.names=F)

#library
library(tm)

#load data
tweets.df <-read.csv('D:\\KULIAH\\SKRIPSI\\DATA\\permendikbud.csv')
tweets.df

#cleaning data
myCorpus <- Corpus(VectorSource(tweets.df$text))
myCorpus <- sapply(myCorpus,function(row) iconv(row, "latin1", 
                                                "ASCII", sub="byte"))
myCorpus<- Corpus(VectorSource(myCorpus))
removeURL<-function(x) gsub ("http[^[[:space:]]*","",x)
myCorpus<-tm_map(myCorpus,content_transformer(removeURL))
removeNL <- function(x) gsub("\n", " ", x)
myCorpus <- tm_map(myCorpus, removeNL)
replacecomma <- function(x) gsub(",", "", x)
myCorpus <- tm_map(myCorpus, replacecomma)
removeRT <- function(x) gsub("RT ", "", x)
myCorpus <- tm_map(myCorpus, removeRT)
removetitik2 <- function(x) gsub(":", "", x)
myCorpus <- tm_map(myCorpus, removetitik2)
removetitikkoma <- function(x) gsub(";", " ", x)
myCorpus <- tm_map(myCorpus, removetitikkoma)
removeamp <- function(x) gsub("&amp;", "", x)
myCorpus <- tm_map(myCorpus, removeamp)
removeUN <- function(x) gsub("@\\w+", "", x)
myCorpus <- tm_map(myCorpus, removeUN)
removeNumFuct<-function(x) gsub("[^[:alpha:][:space:]]*","",x)
myCorpus<-tm_map(myCorpus,content_transformer(removeNumFuct))

myCorpus = tm_map(myCorpus, PlainTextDocument)
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus<-tm_map(myCorpus,content_transformer(tolower))
inspect(myCorpus)

#stop word
id_stopwords<- readLines('D://KULIAH//SKRIPSI//DATA//stopword.csv')
myCorpus<-tm_map(myCorpus,removeWords,id_stopwords)
inspect(myCorpus)

#menghapus ekstraksi angka
myCorpus<-tm_map(myCorpus, stripWhitespace)
myCorpus <- tm_map(myCorpus, removeNumbers)
myCorpusCopy<-myCorpus
dtm = DocumentTermMatrix(myCorpus)
tdm = TermDocumentMatrix(myCorpus)
tdm

##Frekuensi data dan asosiasi(hubungan)
idx <- which(dimnames(tdm)$Terms == "permendikbud")
inspect(tdm[idx + (1:5), 91:100])
(freq.terms <- findFreqTerms(tdm, lowfreq = 15))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 15)
df <- data.frame(term = names(term.freq), freq = term.freq)
View(df)
library(wordcloud)
library(RColorBrewer)
m<-as.matrix(tdm)

#tfidf
View(m)
tf<-data.frame(m)
write.csv(tf,file = 'D:\\KULIAH\\SKRIPSI\\DATA\\permendikbud_tfidf.csv')

word.freq<-sort(rowSums(m),decreasing = T)
pal<-brewer.pal(5, "BuGn")[-(1:4)]
windows()
wordcloud(words = names(word.freq),freq = word.freq, min.freq = 50, 
          scale=c(3.5,0.50),random.order = F, colors = brewer.pal(2, "Dark2"))
library(ggplot2)
term.freq <- rowSums(as.matrix(tdm))
term.freq<-subset(term.freq,term.freq>=500)
df<-data.frame(term=names(term.freq),freq=term.freq)
View(df)


## save data
dataframe<-data.frame(text=unlist(sapply(myCorpusCopy, `[`)), 
                      stringsAsFactors=F)
View(dataframe)
write.csv(dataframe,file = 'D:\\KULIAH\\SKRIPSI\\DATA\\Data permendikbud.csv')


