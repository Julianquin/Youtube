
library(twitteR)
library(RCurl)
library(tm)
library(wordcloud)
library(stringr)
#library(ROAuth)
#library(httr)

setup_twitter_oauth("uj7Vm9SsAAURaFK6Sv2kXofHb",
                    "fJFci7FWFShUIauKf22qrp6wzG0aBFi0k0sSnn0yumGHbr8Qo0", 
                    "865788214707138560-Lj7aSLC8020HspFL8CdLpj13LHiBoYO",
                    "iNkNYWwFDiQjS19YeD9U1BDtSExOCXDqv7n48nizeTJzQ")
troya <- searchTwitter("iphone",n=1000,lang = "en" )# ,locale = "colombia" , resultType="popular" resultType="recent", geocode='42.375,-71.1061111,10mi')

class(troya)

troya_texto <- sapply(troya,function(x) x$getText())
str(troya_texto)

troya_corpus <- Corpus(VectorSource(troya_texto))
troya_corpus <- tm_map(troya_corpus, PlainTextDocument)
troya_limpio <- tolower(troya_corpus)
troya_limpio <- tm_map(troya_corpus,removePunctuation)
troya_limpio <- tm_map(troya_limpio,removeWords,stopwords("en"))
troya_limpio <- tm_map(troya_limpio,removeNumbers)
troya_limpio <- tm_map(troya_limpio, stripWhitespace)
# troya_limpio <- tm_map(troya_limpio,removeWords, c("xxx","yyy"))
x11();wordcloud(troya_limpio,random.order = F)
x11();wordcloud(troya_limpio,random.order = F,col=rainbow(50),max.words = 80)
