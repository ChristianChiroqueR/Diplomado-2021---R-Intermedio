library(rtweet)
library(tidyverse)
library(twitteR)
library(ROAuth)
library(httr)
library(tm)
library(SnowballC)

############################################################################
####1. CONSEGUIR LA DATA
############################################################################

##############################################
####### 1.1 UTILIZAR LA API DE TWITTER Y ESTABLECER CONEXIÓN
##############################################

#api_key <- "TU API KEY"
#api_secret <- "TU API SECRET"
#access_token <- "TU ACCESS TOKEN"
#access_token_secret <- "TU ACCESS TOKEN SECRET"

api_key <- "..."
api_secret <- "..."
access_token <- "..."
access_token_secret <- "..."

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

##############################################
####### 1.2 EXTRAER LA DATA DE UN USUARIO DE TWITTER
##############################################

sagasti <- get_timeline("@FSagasti", 1000)
sagasti[1,5]
# Qué tipo de objeto devuelve el R?


##############################################
####### 1.3 GUARDO LOS TWEETS EN UNA DATA
##############################################

tweets.sagasti <- tweets_data(sagasti)
head(tweets.sagasti)

library(rio)
export(tweets.sagasti,"sagasti.csv",row.names = F)

############################################################################
####2. PREPROCESAR
############################################################################

# Si ya lo tienes en archivo csv lo podrías abrir como cualquier archivo. 

max(tweets.sagasti$created_at); min(tweets.sagasti$created_at)
head(tweets.sagasti$text)

# Esta parte consiste en varios pasos de limpieza de la data para poder analizarla. 

##############################################
####### 2.1 CREACIÓN DE CORPUS
##############################################

textolimpio <- tweets.sagasti$text
textolimpio[1]

# Creando un corpus, leer los tweets dentro de un corpus para poder manipularlos
library(tm)
?Corpus
micorpus <- Corpus(VectorSource(textolimpio))
length(micorpus)
content(micorpus[1])


##############################################
####### 2.2 TRANSFORMACIÓN DE TEXTO
##############################################

# Convertir a minusculas
content(micorpus[1])
micorpus <- tm_map(micorpus,content_transformer(tolower)) 
content(micorpus[1])

#Convertir a mayusculas
#content(micorpus[10])
#micorpus <- tm_map(micorpus,content_transformer(toupper)) 
#content(micorpus[10])

#remueve URLs
content(micorpus[1])
removerURL <- function(x) gsub("http[^[:space:]]*", "", x)
micorpus <- tm_map(micorpus, content_transformer(removerURL))
content(micorpus[1])

#eliminar tildes
content(micorpus[1])
removerTILDE <- function(x) chartr('áéíóú','aeiou',x)
micorpus <- tm_map(micorpus, removerTILDE)
content(micorpus[1])

#eliminar ?s
content(micorpus[1])
remover <- function(x) chartr('?','n',x)
micorpus <- tm_map(micorpus, remover)
content(micorpus[1])

#remover @otros usuarios
content(micorpus[1])
removerUSUARIOS <- function(x) gsub("@\\w+", "", x)
micorpus <- tm_map(micorpus, removerUSUARIOS)
content(micorpus[1])

#Remover retweets: Esta era la forma antigua. Ahora se puede filtrar al inicio los retweets
# removerRETWEETS <- function(x) gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", x)
# micorpus <- tm_map(micorpus, removerRETWEETS)
# content(micorpus[1])

# Quitar la puntuación
content(micorpus[1])
micorpus <- tm_map(micorpus, removePunctuation)
content(micorpus[1])

#quitar números
content(micorpus[23])
micorpus <- tm_map(micorpus, removeNumbers)
content(micorpus[23])


##############################################
####### 2.3 REMOVER STOPWORD
##############################################

# Ya hay una lista predeterminada. Hay una lista por cada idioma. 
stopwords("spanish")[1:20]
stopwords("english")[1:20]

content(micorpus[1])
micorpus <- tm_map(micorpus, removeWords,c(stopwords("spanish")))
content(micorpus[1])

#Quitar las stopwords específicas (si es que lo amerita)
#content(micorpus[1])
#micorpus <- tm_map(micorpus, removeWords,c("?","email","hola"))
#content(micorpus[1])


##############################################
####### 2.4 ÚLTIMOS PASOS
##############################################

#Eliminar espacios en blanco, varios espacios en blanco se contraen en uno solo
content(micorpus[1])
micorpus <- tm_map(micorpus, stripWhitespace)
content(micorpus[1])

#Reducir las palabras subraiz, cortarlas
#content(micorpus[10])
#micorpus <- tm_map(micorpus, stemDocument)
#content(micorpus[10])

###### Sería más fácil usando stringr?

# limpiar_texto<- function(x){
#   x<-str_to_lower(x)
#   x<- str_replace_all(x, "http[^[:space:]]*", "") # Direcciones
#   x<- str_replace_all(x, "á", "a") # Tildes
#   x<- str_replace_all(x, "é", "e") # Tildes
#   x<- str_replace_all(x, "í", "i") # Tildes
#   x<- str_replace_all(x, "ó", "o") # Tildes
#   x<- str_replace_all(x, "ú", "u") # Tildes
#   x<- str_replace_all(x, "\\?","") # Signo de interrogación
#   x<- str_replace_all(x, "(?<=\\s)\\@[:alpha:]{1,15}","") # Usuarios de twitter
#   x<- str_replace_all(x, "\\_|\\)|\\(|\\/|\\.|\\,|\\;","") # Signos de puntuación
#   x<- str_replace_all(x, "\\d","")
#   x<- Corpus(VectorSource(x))
#  x<- tm_map(x, removeWords,c(stopwords("spanish")))
# }
# textolimpio2<-limpiar_texto(textolimpio)
# content(textolimpio2[1:10])
# textolimpio2 <- tm_map(textolimpio2, stripWhitespace)
# content(textolimpio2[1:10])

#Dato: Puedes poner varias líneas como comentario seleccionándolas y apretando
# las teclas Ctrl+Shift+C

############################################################################
####3. EXPLORACIÓN
############################################################################

##############################################
####### 3.1 CREAR UNA MATRIZ DE TÉRMINOS
##############################################

?TermDocumentMatrix
term <- TermDocumentMatrix(micorpus)
term

inspect(term)

##############################################
####### 3.2 ANALIZAR LA FRECUENCIA DE LAS PALABRAS
##############################################

findFreqTerms(term,lowfreq = 20) # al menos 20 veces 

##Top Frequent Terms#

# Convierte a una matriz
m <- as.matrix(term)
head(m)

# Conteo de palabras en orden decreciente
wf <- sort(rowSums(m),decreasing=TRUE)
wf
head(wf)

##############################################
####### 3.3 CREAR UN DATA FRAME CON LAS PALABRAS Y SUS FRECUENCIAS
##############################################

dm <- data.frame(word = names(wf), freq=wf)
head(dm)

dm1 <- dm
hist(dm1$freq)
dm1 <- subset(dm1, dm1$freq >= 10)
dm1

##############################################
####### 3.4 GRÁFICOS
##############################################

## BARRAS

ggplot(dm1, aes( x= word, y=freq )) + geom_bar(stat="identity") +
  xlab("Terminos") + ylab("Frecuencia") + coord_flip() +
  theme(axis.text=element_text(size=7))

barplot(dm1[1:20,]$freq, las = 2, names.arg = dm1[1:20,]$word,
        col ="lightgreen", main ="Top 5 palabras más frecuentes",
        ylab = "Palabras más frecuentes")

## WORDCLOUD
#install.packages("wordcloud")
library(wordcloud)
head(dm)
termcount <- data.frame(freq = apply(term,1,sum))
head(termcount)

wordcloud(dm$word, dm$freq, random.order=FALSE, min.freq=2, colors=brewer.pal(8, "Dark2"))


############################################################################
####4. MODELAMIENTO
############################################################################

##############################################
####### 4.1 ASOCIACIÓN ENTRE PALABRAS
##############################################

# Qué palabras esta asociada con "ciencia" con asociación mayor que 0.3. (Va de 0 a 1)
?findAssocs
findAssocs(term, c("ciencia"), c(0.50))


##############################################
####### 4.2 CLUSTER DE TEXTOS
##############################################

#Quitando las palabras menos frecuentes
#Para eliminar las tweets que aparecen una vez o pocas

tdm2 <- removeSparseTerms(term, sparse = 0.95)
m2 <- as.matrix(tdm2)
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D")
plot(fit)
rect.hclust(fit, k = 3)


##############################################
####### 4.3 ANÁLISIS DE SENTIMIENTOS
##############################################

#install.packages("SentimentAnalysis")
library(SentimentAnalysis)

sentimientos <- analyzeSentiment(textolimpio,language = "spanish")

sentimientos_final <- data.frame(textolimpio,
                               sentiment = convertToDirection(sentimientos$SentimentGI))
head(sentimientos_final)
str(sentimientos_final)
table(sentimientos_final$sentiment)

sentimientos_final$score <- 0
sentimientos_final$score[sentimientos_final$sentiment == "positive"] <- 1
sentimientos_final$score[sentimientos_final$sentiment == "negative"] <- -1

head(sentimientos_final)
table(sentimientos_final$score)
