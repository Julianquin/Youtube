# No me sirve en el momento


# Obtener subtitulos de un video  Falla!!!!!!
# get_captions(id="TVDjcP3DO7334Z31mnq2jb7CbNp7DkJp")
# get_captions(id = "y3ElXcEME3lSISz6izkWVT5GvxjPu8pA")
# Obtener suscripciones Falla!!
# get_subscriptions(filter = c(channel_id = "UCyQqzYXQBUWgBTn4pw_fFSQ"))

# Listar subtÃ�tulos de un video 
subs <- list_caption_tracks(video_id = "ZoRHJBvFHEQ")
# Enumera las razones que pueden usarse para informar videos abusivos
list_abuse_report_reasons()
# Obtenga estadÃ�sticas de todos los videos en un canal
list_channel_resources(filter = c(channel_id = "UCECJDeK0MNapZbpaOzxrUPA"), part="id")
# Devuelve la lista de secciones de canal a las que pertenece el ID de canal. 
secciones <- list_channel_sections(c(channel_id = "UCyQqzYXQBUWgBTn4pw_fFSQ"))

# Itere a travÃ©s del nÃºmero mÃ¡ximo de listas de reproducciÃ³n en el canal y obtenga los videos para cada una de las listas de reproducciÃ³n.
list <- list_channel_videos(channel_id = "UCJQQVLyM6wtPleV4wFBK06g", max_results = 10)
# Buscar en YouTube por tema Utiliza la lista de temas de Freebase Paila!!!!
yt_topic_search(topic = "geopolitica")