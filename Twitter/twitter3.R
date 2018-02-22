library(twitteR)
setwd("D:/Tematicas/Youtube/Youtube/Twitter")
setup_twitter_oauth("uj7Vm9SsAAURaFK6Sv2kXofHb", # "your_api_key"
                    "fJFci7FWFShUIauKf22qrp6wzG0aBFi0k0sSnn0yumGHbr8Qo0",  #"your_api_secret"
                    "865788214707138560-Lj7aSLC8020HspFL8CdLpj13LHiBoYO", # "your_access_token"
                    "iNkNYWwFDiQjS19YeD9U1BDtSExOCXDqv7n48nizeTJzQ") # "your_access_token_secret"
#---------------------------------------------------------------------------------
# Expandirá una URL procesada por un acortador de enlaces (por ejemplo, bit.ly). 
# Proporcionado como una función de conveniencia para los usuarios que pueden realizar esta operación.

# decode_short_url("http://bit.ly/2G8kbGt")

#---------------------------------------------------------------------------------
# Proporciona un modelo que representa los mensajes directos (DM) de Twitter

dm <- dmFactory$new(text='foo', recipientSN='blah')
dm$getText()
## assume 'json' is the return from a Twitter call
dm <- dmFactory$new(json)
dm$getSenderID()

#---------------------------------------------------------------------------------
# Estas funciones le permiten interactuar, enviar y eliminar mensajes directos (DM) en Twitter.

dms <- dmGet(n=10)
dms
## delete the first one
dms[[1]]$destroy()
dmDestroy(dms[[2]])
## send a DM
dmSend('Mensaje de prueba', 'julianquin01')

#---------------------------------------------------------------------------------
# Devuelve los n últimos tweets favoritos del usuario especificado.

favs <- favorites("sergio_fajardo", n=200)

#---------------------------------------------------------------------------------
# Esta función aceptará una lista de otros usuarios de Twitter y detallará si te siguen y / o los siguen.
amistad <- friendships(c("sergio_fajardo","julianquin01"))

#---------------------------------------------------------------------------------
# Recuperará la información de limite de velocidad actual para el usuario autenticado, 
# que se muestra como un marco de datos que muestra información espec??fica para cada recurso de Twitter

getCurRateLimitInfo()

#---------------------------------------------------------------------------------
# Estas funciones te permitirán interactuar con la  tendencia de la API de Twitter
# availableTrendLocations(...)
# closestTrendLocations(lat, long, ...)
# getTrends(woeid, exclude=NULL, ...)

Locs <- availableTrendLocations()
LocsCol <- subset(Locs, country == "Colombia")
woeidCol <- subset(LocsCol, name == "Colombia")$woeid
woeidCali <- subset(LocsCol, name == "Cali")$woeid
trends <- getTrends(woeid=woeidCali)

#---------------------------------------------------------------------------------
# Estas funciones le permiten interactuar con información sobre un usuario de Twitter, 
# recuperando su información base, lista de amigos, lista de seguidores y una l??nea de tiempo actualizada.
usuario.petro <- getUser("petrogustavo")
# followers.petro <- usuario.petro$getFollowerIDs()
followers.petro <- usuario.petro$getFollowers()# Se limita 
favs.us <- usuario$getFavorites(n=200)
follow <-usuario$getFollowers() #se limita
data.frame <- usuario$toDataFrame()


users <- lookupUsers(c('JERobledo', 'petrogustavo'))
followers <- users$petrogustavo$getFollowerIDs() # Se limita igual al comando anterior


# Dado un back-end de base de datos registrado que contiene una tabla de tweets, 
# devolver? el ID del tweet m?s reciente almacenado en esa tabla
#register_sqlite_backend("sqlit_file")
#get_latest_tweet_id("rstats_tweets")

#---------------------------------------------------------------------------------
# Funciones dise?adas para importar datos en objetos twitteR de una variedad de fuentes de datos. 
# Actualmente, solo se admite JSON, y toda esta rama de funcionalidad debe considerarse experimental
# y en desarrollo.
#status_list = import_statuses(list_of_status_json)   

#---------------------------------------------------------------------------------
# Estas funciones permiten a un usuario almacenar datos basados en twitteR en un back-end de base de datos, 
# as? como recuperar datos previamente almacenados
# tweets = searchTwitter("#scala")
# store_tweets_db(tweets)
# from_db = load_tweets_db()

#---------------------------------------------------------------------------------
# twitteR puede tener un backend de base de datos registrado desde donde almacenar y cargar tweets y 
# datos de usuario. Estas funciones proporcionan mecanismos para configurar la conexi?n dentro de twitteR
# register_db_backend 

#---------------------------------------------------------------------------------
# Estas funciones se pueden usar para devolver retweets o usuarios que retuitearon un tweet
tweet <-users[["JERobledo"]][[".->lastStatus"]][[".->id"]]
x <- retweets(tweet)
st <- showStatus(tweet)
y <- retweeters(st$getId())
st$retweetCount
st$text

#---------------------------------------------------------------------------------
# Esta funci?n emitir? una b?squeda de Twitter basada en una cadena de b?squeda proporcionada.

x1 <- searchTwitter("el Cali+America", n=2000, lang="es")
x2 <- sapply(x1, function(x) x$getText())

catch.error = function(x){
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
  # si algo más, sientes, deber??as eliminar, puedes.   Por ejemplo, "palabras de argot", etc. usando la función y los métodos anteriores.
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

x3 <- cleanTweetsAndRemoveNAs(x2)
library(tm)
x4 <- Corpus(VectorSource(x3))
x4 <- tm_map(x4, PlainTextDocument)
x4 <- tm_map(x4,removePunctuation)
x4 <- tm_map(x4,removeWords,stopwords("es"))
x4 <- tm_map(x4,removeNumbers)
x4 <- tm_map(x4, stripWhitespace)
x4 <- tolower(x4)
# x4 <- tm_map(x4,removeWords, c("xxx","yyy"))
# Opcion 1
library(wordcloud)
x11();wordcloud(x4,random.order = F)
# Opcion 2
getSentimentScore = function(sentences, words.positive,words.negative, .progress='none'){
  require(plyr)
  require(stringr)
  scores = laply(sentences,
                 function(sentence, words.positive, words.negative) {
                   # Primero elimine el d??gito, el carácter de puntuación y los caracteres de control:
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

opinion.lexicon.pos =  scan("palabras positivas.txt", what='character', comment.char=';')
opinion.lexicon.neg =  scan('palabras negativas.txt', what='character', comment.char=';')


x5 <- getSentimentScore(x3,opinion.lexicon.pos,opinion.lexicon.neg)

#---------------------------------------------------------------------------------
# Una funci?n de conveniencia dise?ada para envolver el proceso de ejecutar una b?squeda de Twitter 
# y enviar los resultados a una base de datos. Si se llama m?s de una vez, la b?squeda comenzar? con
# el tweet m?s reciente ya almacenado.
# register_sqlite_backend("sqlit_file")
# n = search_twitter_and_store("#rstats", "rstats_tweets")

#---------------------------------------------------------------------------------
# Esta funci?n ajusta las funciones de handshake de autenticaci?n OAuth del paquete httr para una sesi?n twitteR
setup_twitter_oauth("uj7Vm9SsAAURaFK6Sv2kXofHb", # "your_api_key"
                    "fJFci7FWFShUIauKf22qrp6wzG0aBFi0k0sSnn0yumGHbr8Qo0",  #"your_api_secret"
                    "865788214707138560-Lj7aSLC8020HspFL8CdLpj13LHiBoYO", # "your_access_token"
                    "iNkNYWwFDiQjS19YeD9U1BDtSExOCXDqv7n48nizeTJzQ") # "your_access_token_secret"

#---------------------------------------------------------------------------------
# Estas funciones se pueden usar para recuperar tweets espec?ficos del servidor
x <- showStatus(tweet)
x <- lookup_statuses(c(tweet))

#---------------------------------------------------------------------------------
# Contenedor para mensajes de estado de Twitter, incluido el texto y la informaci?n b?sica
st <- statusFactory$new(screenName="test", text="test message")
st$getScreenName()
st$getText()
## Assume 'json' is the return from a Twitter call
st <- statusFactory$new(json)
st$getScreenName()


#---------------------------------------------------------------------------------
# Dada una lista de objetos de estado, eliminar? retweets de la lista para proporcionar 
# un conjunto "puro" de tweets.
tweets <- searchTwitter("cali")
no_retweets <- strip_retweets(tweets)

#---------------------------------------------------------------------------------
# Esta funci?n ejecutar? una expresi?n R y enviar? un mensaje directo a un usuario 
# especificado en caso de ?xito o fracaso.
# taskStatus(z<-5, "JERobledo", session=sess)

#---------------------------------------------------------------------------------
# Estas funciones le permitir?n recuperar varias l?neas de tiempo dentro del universo de Twitter
ut <- userTimeline("JERobledo", n=100) # tweets de un usuario
ut <- homeTimeline(n=25) # tweets en mi pagina de inicio
ut <- mentions(n=20) # tweets en los que me mencionan
ut <- retweetsOfMe(n=20) # retweets que me han hecho

#---------------------------------------------------------------------------------
# Esta funci?n tomar? una lista de objetos de una ?nica clase twitteR y devolver? una versi?n de data.frame 
# de los miembros
zz <- searchTwitter("Cali")
x <- twListToDF(zz)

#---------------------------------------------------------------------------------
# Estas funciones se pueden usar para configurar o eliminar el estado de Twitter de un usuario
ns <- updateStatus("this is my new status message")
## ooops, we want to remove it!
deleteStatus(ns)

#---------------------------------------------------------------------------------
# Esta clase est? dise?ada para representar a un usuario en Twitter, modelando informaci?n disponible
# us <- userFactory$new(screenName="test", name="Joe Smith")
# us$getScreenName()
# us$getName()
## Not run:
## Assume 'json' is the return from a Twitter call
# us <- userFactory$new(json)
# us$getScreenName()

#---------------------------------------------------------------------------------
# Esta funci?n usa un Token Oatuth httr existente en la sesi?n de Twitter

use_oauth_token(twitter_token)

#---------------------------------------------------------------------------------
