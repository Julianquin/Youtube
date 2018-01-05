########################################################################################################
#######                                                                                        #########
#######                     ANALISIS DE TEXTO DE LOS ACUERDOS FINALES                          #########
#######                                                                                        #########
########################################################################################################

# mas info en "http://www.gutenberg.org/ebooks/49836?msg=welcome_stranger"

#################################################
#setwd("C:/Users/emartigo/Desktop/Eduard Fernando Martinez Gonzalez/R")
setwd("C:/Users/Usuario/Desktop/acuerdo/")
rm(list =ls()) 
#################################################


#SE NECESITAN LOS PAQUETES 
#install.packages("tm")
#install.packages("SnowballC")
#install.packages("wordcloud")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("readr")
#install.packages("cluster")
#install.packages("NLP")
#install.packages("RColorBrewer")
#**************************
library(tm) # específico para minería de textos. 
library(SnowballC)  
library(wordcloud) #para graficar nubes de palabras  
library(ggplot2) #una gramática de gráficas que expande las funciones base de R. 
library(dplyr) # con funciones auxiliares para manipular y transformar datos. En particular, el operador %>% permite escribir funciones más legibles para seres humanos.
library(readr) # facilitará leer y escribir documentos. 
library(cluster) # con funciones para realizar análisis de grupos. 
library(NLP) 
library(RColorBrewer) 




################################
acuerdos <- read_lines("acuerdo final.txt", skip = 566, n_max = 33503-566 ) #leyendo el documento 49836-0.txt desde la linea 419 hasta la linea 8313
str(acuerdos)  #El objeto acuerdo que obtuvimos es uno de tipo character, con 32937 elementos.


diez <- rep(1:ceiling(length(acuerdos)/10), each = 10)

# sep='\t'  esto quiere decir que esta separado por tabulaciones

#De este vector, nos quedamos con un número de elementos igual al número de renglones del objeto nov_raw (length(nov_raw)), para facilitar combinarlos.

diez <- diez[1:length(acuerdos)]


#Combinamos diez con now_raw y los asignamos al objeto nov_text. Así tenemos una columna con los renglones de texto y otra con un número que identifica a qué grupo de diez renglones pertenece.
#Además, convertimos a data.frame para que las columnas estén identificadas con un nombre, lo cual será útil en los siguientes pasos.
nov_text <- cbind(diez, acuerdos) %>% data.frame()

#Usamos aggregate para concatenar los renglones (FUN = paste, con collapse = " " para preservar el espacio entre palabras), agrupados por diez (formula = nov_raw ~ diez).
nov_text <- aggregate(formula = acuerdos ~ diez,
                      data = nov_text,
                      FUN = paste,
                      collapse = " ")

#Como sólo necesitamos la columna con los ahora párrafos de texto, con eso nos quedamos. Aprovechamos para transformar nov_text en una matrix, pues esto nos facilitará los pasos siguientes.
nov_text <- nov_text %>% select(acuerdos) %>% as.matrix
dim(nov_text)




nov_text <- gsub("[[:cntrl:]]", " ", nov_text)
nov_text <- tolower(nov_text) #convirtiendo todo a minusculas
nov_text <- removeWords(nov_text, words = stopwords("spanish")) #eliminar palabras vacias, tales como algunas preposiciones y muletillas.
nov_text <- removePunctuation(nov_text) #se  deshace de la puntuación, puesto que fin y fin. son identificadas como palabras diferentes, lo cual no deseamos.
nov_text <- removeNumbers(nov_text) #En este caso, removemos los números, pues en Niebla no hay fechas y otras cantidades que deseemos conservar.
nov_text <- stripWhitespace(nov_text) #Por último eliminamos los espacios vacios excesivos, muchos de ellos introducidos por las transformaciones anteriores.


###############          Analisis de CORPUS       ########################
#Con nuestro documento preparado, procedemos a crear nuestro Corpus, es decir, esto es nuestro acervo de documentos a analizar.
#En nuestro caso, nuestro Corpus se compone de todos los parrafos del acuerdo de paz y los asignaremos al objeto nov_corpus 
#usando las funciones VectorSource y Corpus.
nov_corpus <- Corpus(VectorSource(nov_text))
nov_corpus


########################        Nube de palabras      ########################
#Mapearemos nuestro Corpus como un documento de texto plano usando las funciones tm_map y PlainTextDocument).
nov_ptd <- tm_map(nov_corpus, PlainTextDocument)
# Con nuestro Corpus mapeado de esta manera, podemos crear fácilmente una nube de palabras (wordcloud de la librería del mismo nombre) que nos muestro los términos más frecuentes en Niebla.
wordcloud(nov_ptd, max.words = 250, random.order = F, colors = brewer.pal(name = "Dark2", n = 8))


########################        Mas depuración      ########################
nov_text <- removeWords(nov_text, words = c("así", "dentro" , "nuevo" , "deberá" , "sala" , "nivel" , "acuerdo" , "ptn" , "acuerdo final" ,"farcep" , "deben" , "ser" , "uso" , "cada", "mmv" , "zvtn", "día", "cfhbd"))
nov_corpus <- nov_text %>% VectorSource() %>% Corpus()
nov_ptd <- nov_corpus %>% tm_map(PlainTextDocument)




########################          Term Document Matrix        ######################## 
#Mapearemos nuestro Corpus indicando que es una matriz de términos, de esta manera podremos hacer realizar operaciones como identificar asociaciones entre palabras.
# Usaremos la función TermDocumentMatrix en nuestro Corpus y asignaremos el resultado al objeto nov_tdm.
nov_tdm <- TermDocumentMatrix(nov_corpus)
nov_tdm




########################         Frecuencia de palabras              ##########################
#Aunque una nube de palabras nos muestra de manera visual la frecuencia de las palabras en nuestro Corpus, no nos devuelve cantidades.
#Para obtenerlas, primero transformaremos nuestro objeto nov_tdm en un objeto de clase matrix, que de nuevo tendrá un número de renglones igual al número de palabras distintas de nuestro Corpus y número de columnas igual a su número de documentos.
nov_mat <- as.matrix(nov_tdm)
dim(nov_mat)

# Obtenemos las sumas de renglones (rowSums) odenadas de mayor a menor (sort con decreasing = TRUE)para conocer la frecuencia de cada palabra y después transformamos 
#los resultados a objeto de clase data.frame de dos columnas, palabra y frec, que nos permitirá graficar fácilmente su contenido.
nov_mat <- nov_mat %>% rowSums() %>% sort(decreasing = TRUE)
nov_mat <- data.frame(palabra = names(nov_mat), frec = nov_mat)

# Graficando este nuevo objeto
wordcloud(
  words = nov_mat$palabra, 
  freq = nov_mat$frec, 
  max.words = 250, 
  random.order = F, 
  colors=brewer.pal(name = "Dark2", n = 8)
)

nov_mat[1:100,]
repetidas <- data.frame(nov_mat[1:100,])
#install.packages("xlsx")
#library(xlsx)
write_excel_csv(repetidas, "repetidas.csv")





