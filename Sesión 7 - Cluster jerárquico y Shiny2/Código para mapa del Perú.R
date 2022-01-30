########### Te recomiendo revisar este link:

# https://geocompr.robinlovelace.net/index.html

########### LIBRERÍAS

library(geojsonio)
library(sf)
library(ggrepel)
library(tidyverse)
library(rio)

########### OPCIONAL: SI TIENES UN SHAPEFILE PROPIO PUEDES SUBIRLO A LA SIGUIENTE PÁGINA
########### ESTO TE SERVIRÁ PARA DESPUÉS "LLAMARLO" EN LA APP

# https://mapshaper.org/

########### SE DESCARGA EL MAPA

link_mapa = "https://github.com/ChristianChiroqueR/Diplomado-2021---R-Intermedio/raw/main/Sesi%C3%B3n%207%20-%20Cluster%20jer%C3%A1rquico%20y%20Shiny2/DEPARTAMENTOS.json"
spdf <- geojson_read(link_mapa,  what = "sp") #Lo tenemos que leer como archivo json
peru<-st_as_sf(spdf) # Lo convertimos a tipo sf para mayor facilidad

########### PRIMERA EXPLORACIÓN DEL MAPA

ggplot(data = peru) + geom_sf()


# Lo visualizamos
ggplot(data = peru) +
  geom_sf(fill="skyblue", color="black")  #Se le agrega un relleno celeste y bordes negros

########### OBTENER LA DATA

insumo<-import("https://github.com/ChristianChiroqueR/Diplomado-2021---R-Intermedio/raw/main/Sesi%C3%B3n%207%20-%20Cluster%20jer%C3%A1rquico%20y%20Shiny2/lista.xlsx")


########### CREAMOS UNA NUEVA DATA QUE SERÁ EL MERGE DE LAS ANTERIORES

data_mapa <- peru  |> 
  left_join(insumo)


########### GRÁFICO

ggplot(data_mapa) +
  geom_sf(aes(fill = as.factor(CLUSTER)))+
  labs(title = "Mapa del Perú según cluster de pobreza",
       caption = "Fuente: INEI, JNE, ETC
       Elaboración: Grupo 6",
       x="Longitud",
       y="Latitud") +
  scale_fill_discrete(name = "Clusters")

