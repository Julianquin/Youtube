
library(tuber)
client.id <- "180929964582-9f2irgbuaad83gms40fpc16jsc2ja93f.apps.googleusercontent.com"
client.secret <- "Z-IPCqy43-cfolpdgJhXyAm5"
token <- yt_oauth(client.id, client.secret)

# Lenguages 
lengua <- list_langs()
regiones <- list_regions()

#################################################
#-------------------- VIDEOS -------------------#
#################################################


# Obtener estadÃ???sticas de un video
vid.stats <- get_stats(video_id="GHw5BBPNiN8")

# Obtenga detalles tales como cuándo se publicó el video, el título, la descripción, las miniaturas, la categoría, etc.
vid.detalles <- get_video_details(video_id="GHw5BBPNiN8")

# Obtener comentarios de un video
coment <- get_comment_threads(c(video_id="GHw5BBPNiN8"),max_results = 101)

# Toma una identificacion de video y devuelve videos relacionados
relacionados <- get_related_videos(video_id = "GHw5BBPNiN8")

# Lista de categor1as que pueden asociarse con videos
vid.category <- list_videocats(c(region_code = "ES"))

# Lista (mas popular) Videos
vid.pop <- list_videos(hl="es",region_code = "CO",max_results = 50,video_category_id =10)

# Busque videos, canales y listas de reproducciÃ³n. (De manera predeterminada, la funciÃ³n busca videos).
busqueda <- yt_search(term = "geopolitica", published_before = "2018-01-22T00:00:00Z",
                      published_after = "2017-01-22T00:00:00Z",max_results = 50)



f.video <- function(video_id){
  require(tuber)
  stats <- get_stats(video_id=video_id)
  detalles <- get_video_details(video_id=video_id)
  
  
  id <- stats[["id"]]
  title <- detalles[["items"]][[1]][["snippet"]][["title"]]
  descrip <- detalles[["items"]][[1]][["snippet"]][["description"]]
  views <- stats[["viewCount"]]   
  like <- stats[["likeCount"]]
  dislike <- stats[["dislikeCount"]]
  fav <- stats[["favoriteCount"]]
  coment <- stats[["commentCount"]]
  id.categ <- detalles[["items"]][[1]][["snippet"]][["categoryId"]]
  canal <- detalles[["items"]][[1]][["snippet"]][["channelTitle"]]
  idcanal <- detalles[["items"]][[1]][["snippet"]][["channelId"]]
  feacha.pub <- detalles[["items"]][[1]][["snippet"]][["publishedAt"]]
  legua <- detalles[["items"]][[1]][["snippet"]][["defaultAudioLanguage"]]
  
  video <- cbind(id,title,descrip,views,like,dislike,fav,coment,id.categ,canal,idcanal,feacha.pub,legua)
  video <- as.data.frame(video)
  tags <- detalles[["items"]][[1]][["snippet"]][["tags"]]
  coment <- get_comment_threads(c(video_id=video_id),max_results = 101)
  relacionados <- get_related_videos(video_id = video_id)
  list(video,tags,coment,relacionados)
}

x <- f.video("GHw5BBPNiN8")



#################################################
#-------------------- CANALES ------------------#
#################################################


# Obtener estadisticas de un canal
can.stats <- get_channel_stats(channel_id="UC-lHJZR3Gqxm24_Vd_AJ5Yw")

# obtener listas de reproduccion
can.playlist <- get_playlists(filter=c(channel_id="UC-lHJZR3Gqxm24_Vd_AJ5Yw"))

# Devuelve una lista de eventos de canal que coinciden con los criterios de solicitud. Uso
actividad <- list_channel_activities(filter = c(channel_id = "UCJQQVLyM6wtPleV4wFBK06g"),
                                     published_before = "2018-01-01T00:00:00Z",
                                     published_after = "2017-01-01T00:00:00Z")

# Obtener una lista de categorias que pueden asociarse con los canales de YouTube
can.category <- list_guidecats(c(region_code = "CO"),hl="es")


f.canales <- function(channel_id){
  require(tuber)
  stats <- get_channel_stats(channel_id=channel_id)
  id <- stats[["id"]]
  title <- stats[["snippet"]][["title"]]
  descrip <- stats[["snippet"]][["description"]]
  fecha.inicio <- stats[["snippet"]][["publishedAt"]]
  views <- stats[["statistics"]][["viewCount"]] 
  coment <- stats[["statistics"]][["commentCount"]]
  subs <- stats[["statistics"]][["subscriberCount"]]
  videos <- stats[["statistics"]][["videoCount"]]
  canal <- cbind(id,title,descrip,fecha.inicio,views,coment,subs,videos)
  as.data.frame(canal)
}

pew <- f.canales("UC-lHJZR3Gqxm24_Vd_AJ5Yw")
bieber <- f.canales("UCHkj014U2CQ2Nv0UZeYpE_A")
h.german <- f.canales("UCZJ7m7EnCNodqnu5SAtg8eQ")
t.series <- f.canales("UCq-Fj5jknLsUf-MWSy4_brA")
taylor <- f.canales("UCANLZYMidaCbLQFWXBC95Jg")
rubius <- f.canales("UCXazgXDIYyWH-yXLAkcrFxw")
rihana <- f.canales("UC2xskkQVFEpLcGFnNSLQY0A")
ed.shar <- f.canales("UC0C-w0YjGpqDXGB8IHb662A")
whinderssonnunes <- f.canales("UC3KQ5GWANYF8lChqjZpXsQw")
KatyPerry <- f.canales("UC-8Q-hLdECwQmaWNwXitYDw")
Fernanfloo <- f.canales("UCV4xOVpbcV8SdueDCOxLXtQ")
KondZilla <- f.canales("UCffDXn7ycAzwL2LDlbyWOTw")
Dude.Perfect <- f.canales("UCRijo3ddMTht_IHyNSNXpNQ")
eminen <- f.canales("UC20vb-R_px4CguHzzBPhoyQ")
JuegaGerman <- f.canales("UCYiGq8XF7YQD00x7wAd62Zg")
OneDirectionVEVO <- f.canales("UCbW18JZRgko_mOGm5er8Yzg")
Smosh <- f.canales("UCY30JRSgfhYXA6i6xX1erWg")
Theellenshow <- f.canales("UCp0hYYBW6IMayGgR-WeoCvQ")
VanossGaming <- f.canales("UCKqH_9mk1waLgBiL2vT5b9g")
WWE <- f.canales("UCJ5v_MCY6GNUBTO8-D3XoAg")
vegetta777 <- f.canales("UCam8T03EOFBsNdR0thrFHdQ")
nigahiga <- f.canales("UCSAUGyc_xA8uYzaIVG6MESQ")
yuya <- f.canales("UCBNs31xysxpAGMheg8OrngA")
Spinnin <- f.canales("UCpDJl2EmP7Oh90Vylx0dZtA")
Markiplier <- f.canales("UC7_YxT-KID8kRbqZo7MyscQ")
bruno <- f.canales("UCoUM-UJ7rirJYP8CQ0EIaHA")
ariana.grande <- f.canales("UC0VOyT2OCBKdQhF3BAbZ-1g")
shakira <- f.canales("UCGnjeahCJW1AF34HBmQTJ-Q")

popular <- rbind(pew,bieber,h.german,t.series,taylor,rubius,rihana,ed.shar,
      whinderssonnunes,KatyPerry,Fernanfloo,KondZilla,Dude.Perfect,eminen,
      JuegaGerman,OneDirectionVEVO,Smosh,Theellenshow,VanossGaming,WWE,
      vegetta777,nigahiga,yuya,Spinnin,Markiplier,bruno,ariana.grande,shakira)



popular[,5] <- as.numeric(levels(popular[,5]))[popular[,5]]
popular[,6] <- as.numeric(levels(popular[,6]))[popular[,6]]
popular[,7] <- as.numeric(levels(popular[,7]))[popular[,7]]
popular[,8] <- as.numeric(levels(popular[,8]))[popular[,8]]

hist(popular$views)


#################################################
#-------------------- LISTAS--------------------#
#################################################


# Obtener items de una lista de reproduccion
playlist_items <- get_playlist_items(filter = c(playlist_id = can.playlist[["items"]][[1]][["id"]]),max_results = 51)

















