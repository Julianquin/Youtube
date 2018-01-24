library(twitteR)

setup_twitter_oauth("uj7Vm9SsAAURaFK6Sv2kXofHb", # "your_api_key"
                    "fJFci7FWFShUIauKf22qrp6wzG0aBFi0k0sSnn0yumGHbr8Qo0",  #"your_api_secret"
                    "865788214707138560-Lj7aSLC8020HspFL8CdLpj13LHiBoYO", # "your_access_token"
                    "iNkNYWwFDiQjS19YeD9U1BDtSExOCXDqv7n48nizeTJzQ") # "your_access_token_secret"
source("authenticate.r")



EarthQuakeTweets = searchTwitter("petro", since='2014-09-29')
str(EarthQuakeTweets[1])
Locs <- availableTrendLocations()
LocsIndia <- subset(Locs, country == "Colombia")
woeidDelhi <- subset(LocsIndia, name == "Cali")$woeid
trends <- getTrends(woeid=woeidDelhi)


head(trends)

head(searchTwitter("petro",
                   lang = "es",
                   locale = "colombia",
                   resultType="popular"))

Meru_tweets <- searchTwitter("BarackObama", n=2000, lang="en")
Ola_tweets <- searchTwitter("narendramodi", n=2000, lang="en")
TaxiForSure_tweets <-  searchTwitter("realDonaldTrump", n=2000, lang="en")
Uber_tweets = searchTwitter("Oprah", n=2000, lang="en")


head(Meru_tweets)
MeruTweets <- sapply(Meru_tweets, function(x) x$getText())
OlaTweets <- sapply(Ola_tweets, function(x) x$getText())
TaxiForSureTweets <- sapply(TaxiForSure_tweets,function(x) x$getText())
UberTweets <- sapply(Uber_tweets, function(x) x$getText())


catch.error = function(x)
{
  y = NA
  catch_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(catch_error, "error"))
    y = tolower(x)
  return(y)
}


cleanTweets<- function(tweet){
  # eliminar enlaces html, que no son necesarios para el análisis de sentimiento
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  # Primero eliminaremos las entidades de retweet de  los tweets almacenados (texto)
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  # Then remove all "#Hashtag"
  tweet = gsub("#\\w+", " ", tweet)
  # Then remove all "@people"
  tweet = gsub("@\\w+", " ", tweet)
  # Then remove all the punctuation
  tweet = gsub("[[:punct:]]", " ", tweet)
  # Then remove numbers, we need only text for analytics
  tweet = gsub("[[:digit:]]", " ", tweet)
  # finally, we remove unnecessary spaces (white spaces, tabs etc)
  tweet = gsub("[ \t]{2,}", " ", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
  # si algo más, sientes, deberías eliminar, puedes.   Por ejemplo, "palabras de argot", etc. usando la función y los métodos anteriores.
  # A continuación, convertiremos todas las palabras en minúsculas.   This makes uniform pattern.
  tweet = catch.error(tweet)
  tweet
}


cleanTweetsAndRemoveNAs<- function(Tweets) {
  TweetsCleaned = sapply(Tweets, cleanTweets)
  # Remove the "NA" tweets from this tweet list
  TweetsCleaned = TweetsCleaned[!is.na(TweetsCleaned)]
  names(TweetsCleaned) = NULL
  # Remove the repetitive tweets from this tweet list
  TweetsCleaned = unique(TweetsCleaned)
  TweetsCleaned
}


MeruTweetsCleaned = cleanTweetsAndRemoveNAs(MeruTweets)
OlaTweetsCleaned = cleanTweetsAndRemoveNAs(OlaTweets)
TaxiForSureTweetsCleaned <-  cleanTweetsAndRemoveNAs(TaxiForSureTweets)
UberTweetsCleaned = cleanTweetsAndRemoveNAs(UberTweets)


opinion.lexicon.pos =  scan("palabras positivas.txt", what='character', comment.char=';')
opinion.lexicon.neg =  scan('palabras negativas.txt', what='character', comment.char=';')
head(opinion.lexicon.pos)



getSentimentScore = function(sentences, words.positive,
                             words.negative, .progress='none'){
  require(plyr)
  require(stringr)
  scores = laply(sentences,
                 function(sentence, words.positive, words.negative) {
                   # Primero elimine el dígito, el carácter de puntuación y los caracteres de control:
                   sentence = gsub('[[:cntrl:]]', '', gsub('[[:punct:]]', '',
                                                           gsub('\\d+', '', sentence)))
                   # Luego, convierta todas las frases a mayúsculas y minúsculas:
                   sentence = tolower(sentence)
                   # Ahora vamos a dividir cada oración por el delimitador de espacio
                   words = unlist(str_split(sentence, '\\s+'))
                   # Obtenga el emparejamiento booleano de cada palabra con el léxico de opinión positivo y negativo
                   pos.matches = !is.na(match(words, words.positive))
                   neg.matches = !is.na(match(words, words.negative))
                   # Ahora obtenga el puntaje como sentimiento positivo total menos el total de negativos
                   score = sum(pos.matches) - sum(neg.matches)
                   return(score)
                 }, words.positive, words.negative, .progress=.progress )
  # Devuelve un marco de datos con la oración respectiva y el puntaje
  return(data.frame(text=sentences, score=scores))
}

auron <- getSentimentScore(MeruTweetsCleaned,opinion.lexicon.pos,opinion.lexicon.neg)
wismi <- getSentimentScore(OlaTweetsCleaned,opinion.lexicon.pos,opinion.lexicon.neg)
dross <- getSentimentScore(TaxiForSureTweetsCleaned,opinion.lexicon.pos,opinion.lexicon.neg)
villalob <- getSentimentScore(UberTweetsCleaned,opinion.lexicon.pos,opinion.lexicon.neg)

MeruTweetsClassEmo = classify_emotion(MeruTweetsCleaned,algorithm="bayes", prior=1.0)
OlaTweetsClassEmo = classify_emotion(OlaTweetsCleaned,algorithm="bayes", prior=1.0)
TaxiForSureTweetsClassEmo =  classify_emotion(TaxiForSureTweetsCleaned, algorithm="bayes",prior=1.0)
UberTweetsClassEmo = classify_emotion(UberTweetsCleaned, algorithm="bayes", prior=1.0)


MeruEmotion = MeruTweetsClassEmo[,7]
OlaEmotion = OlaTweetsClassEmo[,7]
TaxiForSureEmotion = TaxiForSureTweetsClassEmo[,7]
UberEmotion = UberTweetsClassEmo[,7]


MeruEmotion[is.na(MeruEmotion)] = "unknown"
OlaEmotion[is.na(OlaEmotion)] = "unknown"
TaxiForSureEmotion[is.na(TaxiForSureEmotion)] = "unknown"
UberEmotion[is.na(UberEmotion)] = "unknown"

MeruTweetsClassPol = classify_polarity(MeruTweetsCleaned,algorithm="bayes")
OlaTweetsClassPol = classify_polarity(OlaTweetsCleaned,algorithm="bayes")
TaxiForSureTweetsClassPol = classify_polarity(TaxiForSureTweetsCleaned, algorithm="bayes")
UberTweetsClassPol = classify_polarity(UberTweetsCleaned,algorithm="bayes")

MeruPol = MeruTweetsClassPol[,4]
OlaPol = OlaTweetsClassPol[,4]
TaxiForSurePol = TaxiForSureTweetsClassPol[,4]
UberPol = UberTweetsClassPol[,4]




MeruSentimentDataFrame = data.frame(text=MeruTweetsCleaned,
                                    emotion=MeruEmotion, polarity=MeruPol, 
                                    stringsAsFactors=FALSE)
OlaSentimentDataFrame = data.frame(text=OlaTweetsCleaned,
                                   emotion=OlaEmotion, polarity=OlaPol, 
                                   stringsAsFactors=FALSE)
TaxiForSureSentimentDataFrame =  data.frame(text=TaxiForSureTweetsCleaned,
                                  emotion=TaxiForSureEmotion, 
                                  polarity=TaxiForSurePol,
                                  stringsAsFactors=FALSE)
UberSentimentDataFrame = data.frame(text=UberTweetsCleaned,
                                    emotion=UberEmotion, 
                                    polarity=UberPol, 
                                    stringsAsFactors=FALSE)



# rearrange data inside the frame by sorting it
MeruSentimentDataFrame = within(MeruSentimentDataFrame, emotion <-
                                  factor(emotion, levels=names(sort(table(emotion),
                                                                    decreasing=TRUE))))
OlaSentimentDataFrame = within(OlaSentimentDataFrame, emotion <-
                                 factor(emotion, levels=names(sort(table(emotion),
                                                                   decreasing=TRUE))))
TaxiForSureSentimentDataFrame =  within(TaxiForSureSentimentDataFrame, emotion <- 
                                factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
UberSentimentDataFrame = within(UberSentimentDataFrame, emotion <-
                                  factor(emotion, levels=names(sort(table(emotion),
                                                                    decreasing=TRUE))))
plotSentiments1<- function (sentiment_dataframe,title) {
  library(ggplot2)
  ggplot(sentiment_dataframe, aes(x=emotion)) +
    geom_bar(aes(y=..count.., fill=emotion)) +
    scale_fill_brewer(palette="Dark2") +
    ggtitle(title) +
    theme(legend.position='right') + ylab('Number of Tweets') +
    xlab('Emotion Categories')
}


plotSentiments1(MeruSentimentDataFrame, 'Sentiment Analysis of
Tweets on Twitter about MeruCabs')
plotSentiments1(OlaSentimentDataFrame, 'Sentiment Analysis of
Tweets on Twitter about OlaCabs')
plotSentiments1(TaxiForSureSentimentDataFrame, 'Sentiment Analysis
of Tweets on Twitter about TaxiForSure')
plotSentiments1(UberSentimentDataFrame, 'Sentiment Analysis of
Tweets on Twitter about UberIndia')



# Similarly we will plot distribution of polarity in the tweets
plotSentiments2 <- function (sentiment_dataframe,title) {
  library(ggplot2)
  ggplot(sentiment_dataframe, aes(x=polarity)) +
    geom_bar(aes(y=..count.., fill=polarity)) +
    scale_fill_brewer(palette="RdGy") +
    ggtitle(title) +
    theme(legend.position='right') + ylab('Number of Tweets') +
    xlab('Polarity Categories')
}
plotSentiments2(MeruSentimentDataFrame, 'Polarity Analysis of
Tweets on Twitter about MeruCabs')
plotSentiments2(OlaSentimentDataFrame, 'Polarity Analysis of
Tweets on Twitter about OlaCabs')
plotSentiments2(TaxiForSureSentimentDataFrame, 'Polarity Analysis
of Tweets on Twitter about TaxiForSure')
plotSentiments2(UberSentimentDataFrame, 'Polarity Analysis of
Tweets on Twitter about UberIndia')


removeCustomeWords <- function (TweetsCleaned) {
  for(i in 1:length(TweetsCleaned)){
    TweetsCleaned[i] <- tryCatch({
      TweetsCleaned[i] = removeWords(TweetsCleaned[i],
                                     c(stopwords("english"), "care", "guys", "can", "dis", "didn",
                                       "guy" ,"booked", "plz"))
      TweetsCleaned[i]
    }, error=function(cond) {
      TweetsCleaned[i]
    }, warning=function(cond) {
      TweetsCleaned[i]
    })
  }
  return(TweetsCleaned)
}

getWordCloud <- function(sentiment_dataframe, TweetsCleaned, Emotion) {
  emos = levels(factor(sentiment_dataframe$emotion))
  n_emos = length(emos)
  emo.docs = rep("", n_emos)
  TweetsCleaned = removeCustomeWords(TweetsCleaned)
  for (i in 1:n_emos){
    emo.docs[i] = paste(TweetsCleaned[Emotion ==
                                        emos[i]], collapse=" ")
  }
  corpus = Corpus(VectorSource(emo.docs))
  tdm = TermDocumentMatrix(corpus)
  tdm = as.matrix(tdm)
  colnames(tdm) = emos
  require(wordcloud)
  suppressWarnings(comparison.cloud(tdm, colors =
                                      brewer.pal(n_emos, "Dark2"), scale = c(3,.5), random.order =
                                      FALSE, title.size = 1.5))
}
x11();getWordCloud(MeruSentimentDataFrame, MeruTweetsCleaned, MeruEmotion)
x11();getWordCloud(OlaSentimentDataFrame, OlaTweetsCleaned, OlaEmotion)
x11();getWordCloud(TaxiForSureSentimentDataFrame, TaxiForSureTweetsCleaned, TaxiForSureEmotion)
x11();getWordCloud(UberSentimentDataFrame, UberTweetsCleaned, UberEmotion)
