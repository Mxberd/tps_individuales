#TRABAJO PRACTICO 02

#Proponemos realizar un analisis de las obras iniciadas en el barrio de Palermo, CABA.
#Descargamos datos del acumulado historico de la ciudad, sobre el que ejecutaremos algunas operaciones.

#Para empezar, llamamos a nuestras librerias.

library(tidyverse)
library(lubridate)
library(datos)
library(janitor)
library(skimr)
#Importamos la base de datos.

Historico_00 <- read.csv("Datos/registro-acumulado-historico-de-obras-iniciadas.csv")

#La funcion as_tibble()nos permitira hacer una mejor visualizacion de nuestros datos. Alli detectamos
#Que la fecha no esta ingresada como fecha!

Historico_V <- as_tibble(Historico_00)
head(Historico_V)

#Tambien probamos el paquete Skimr para conocer la estructura de nuestos datos.

skim(Historico_00)


#Como primera accion, proponemos filtrar solamente los barrios de Palermo y Villa Crespo
#Solamente mantendremos los casos de demolicion total y obra nueva, no asi las refacciones
#Hacemos un poco de limpieza

Historico_VI  <-  Historico_00 %>% 
  str_detect("PARCIAL") #Asi buscamos todas las que pudieran referir a ampliaciones
#que no nos interesan.

Historico_01 <- Historico_00 %>%
  filter( !barrio %in% c("Comuna 1","Comuna 2","Comuna 3","Comuna 4",
  "Comuna 5","Comuna 6", "Comuna 7","Comuna 8" ,"Comuna 9", "Comuna 10",
  "Comuna 11","Comuna 12","Comuna 13","Comuna 14","Comuna 15")) %>% 
  filter( tipo_obra!= "AMP., MODIF. Y DEMOLICION PARCIAL") %>%
  filter( tipo_obra!= "PARCIAL") %>%
  filter( tipo_obra!= "MODIFICACION Y AMPLIACION") %>%
  filter( tipo_obra!= "DEMOLICION PARCIAL Y AMPLIACION DE OBRA") %>%
  filter( tipo_obra!= "AMPLIACION Y/O MODIFICACION C/DEMOLICION PARCIAL") %>%
  filter( tipo_obra!= "STOCK AMPLIACION Y/O MODIFICACION C/DEMOLICION PARCIAL") %>%
  clean_names()

#Otra opcion hubiera sido usar str_detect para detectar palabras

Historico_01 <-Historico_01 %>% 
  mutate(tipo_obra=case_when(str_detect(string = tipo_obra, pattern = "VACIO")
~ "Sin datos", TRUE ~ tipo_obra))

Historico_01 <-Historico_01 %>% 
  mutate(tipo_obra=case_when(is.na(tipo_obra) 
  ~ "Sin datos", TRUE ~ tipo_obra))


Historico_01 <- Historico_01 %>% 
  mutate(tipo_obra= "Demolicion y/o Obra Nueva")



#Eliminamos los NA con na.omit como funcion

Historico_01 <- Historico_01 %>% 
  na.omit()

Historico_01 <- Historico_01 %>%
  select(periodo,long,lat,calle,calle_nro,seccion,manzana,parcela,tipo_obra,destino,barrio,comuna)


#Corregimos la columna de periodo para que represente correctamente año-mes y poder asi analizar mejor los datos.

Historico_01 <- Historico_01 %>% 
  mutate(periodo = ym(periodo))


#Empiezamos a crear nuestros propios dataframes extraidos del original

Historico_02 <- Historico_01 %>% 
  select(barrio)%>%
  group_by(barrio) %>% 
  summarise(cantidad_registrada=n()) 



Historico_02 <- Historico_02%>% 
  na.omit(barrio)


Historico_02 <- Historico_02 %>%
  mutate(barrio = ifelse( barrio == "NuÃ±ez","Nuñez", barrio)) 
Historico_02 <- Historico_02 %>%
  mutate(barrio = ifelse(barrio == "Nu\u00f1ez","Nuñez", barrio))
  




 