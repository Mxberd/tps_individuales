#TRABAJO PRACTICO 02

#Proponemos realizar un analisis de las obras iniciadas en el barrio de Palermo, CABA.
#Descargamos datos del acumulado historico de la ciudad, sobre el que ejecutaremos algunas operaciones.

#Para empezar, llamamos a nuestras librerias.

library(tidyverse)
library(lubridate)
library(datos)
#Importamos la base de datos.

Historico_00 <- read.csv("Datos/registro-acumulado-historico-de-obras-iniciadas.csv", stringsAsFactors = TRUE)

#La funcion as_tibble()nos permitira hacer una mejor visualizacion de nuestros datos. Alli detectamos
#Que la fecha no esta ingresada como fecha!

Historico_V <- as_tibble(Historico_00)
head(Historico_V)


#Como primera accion, proponemos filtrar solamente los barrios de Palermo y Villa Crespo
#Solamente mantendremos los casos de demolicion total y obra nueva, no asi las refacciones

Historico_01 <- Historico_00 %>%
  filter( barrio == "Palermo" | barrio == "Villa Crespo" , tipo_obra != "AMP., MODIF. Y DEMOLICION PARCIAL") 
Historico_01 <- Historico_00 %>%
  select(periodo,long,lat,calle,calle_nro,seccion,manzana,parcela,tipo_obra,destino,barrio,comuna)

#Corregimos la columna de periodo para que represente correctamente año-mes y poder asi analizar mejor los datos.

Historico_01 <- Historico_01 %>% 
  mutate(periodo = ym(periodo))

git push -f origin master
  


