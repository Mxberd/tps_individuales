#TRABAJO PRACTICO 02

#Para empezar, llamamos a nuestras librerias.

library(tidyverse)
library(lubridate)
library(datos)
library(janitor)
library(skimr)
library(sf)
#Tambien acomodamos la visualizacion de los valores numericos elimando notacion cientifica.
options(scipen = 999)

#Estaremos trabajando con el arbolado urbano de CABA. Los datos son de BADATA.

#Importamos la base de datos.


base_00 <- read.csv("Datos/arbolado-publico-lineal-2017-2018.csv", 
  encoding = "UTF-8", dec = ".")


#La funcion as_tibble()nos permitira hacer una mejor visualizacion de nuestros datos. 

base_0A <- as_tibble(base_00)

#Tambien probamos el paquete Skimr para conocer la estructura de nuestos datos.

skim(base_00)

#Hacemos un poco de limpieza

base_00 <- base_00 %>% 
  na.omit()

#Por otro lado con "Unique" eliminamos las cargas repetidas

base_00 <- base_00 %>% 
  unique()

#Hacemos algunas correciones de como se presentan nuestros datos. Renombramos columnas

base_00 <- base_00 %>%
  select( -nro_registro,-manzana,-calle_altura,-direccion_normalizada,-tipo_activ,-ubicacion) %>% 
  rename("Calle"=calle_nombre, "Altura"=calle_chapa, "Tipo"=nombre_cientifico, "ancho_vereda"=ancho_acera)


#str_detect Es una buena herramienta para hacerle preguntasa los datos. Por ejemplo:
#Hay bananos en el arbolado de alineacion? lo buscamos por su nombre cientifico "Musa x paradisiaca"!

Banano  <- 
  str_detect( string = base_0A, pattern = "Musa x paradisiaca") 

#La respuesta "FALSE" en todas las categorias nos da entender que no habria bananos, al menos
#espresados por su nombre cientifico.

#Vamos a eliminar de la lista aquellas especies que tuvieran escasa representatividad. Para ello debemos
#Contar las repeticiones de cada especie!

base_01 <- base_00 %>% 
  select(Tipo)%>%
  group_by(Tipo) %>% 
  summarise(cantidad_registrada=n())

#Ponemos 100 ejemplares como el piso a sobrepasar para considerarse especie signficativa
#Para ello usamos case_when
base_01 <-base_01 %>% 
  mutate(significatividad = case_when(cantidad_registrada<100
  ~ "No Significativa", TRUE ~ "Significativa"))

base_01 %>% count(significatividad)
  
#Detectamos que hay 333 especies No significativas, altener muy baja representatividad. 
# Por otro lad 95 si tienen buena representatividad.

#Vamos a llevar estos datos a nuestra base_00

base_02 <- base_01 %>% 
  select(-cantidad_registrada)

base_03 <- base_00 %>% 
  right_join(base_02)

#Ahora incorporamos el dato de que especies tienen poca representatividad en el conjunto.

#Proponemos hacer algunos graficos
#Tramos la subdivision administrativa de CABA para usar luego!

barrios <- st_read("Datos/barrios.geojson")

#Proponemos ver las primeras 25 mas significativas, para ello usamos
#arrage y slice

base_0C <- base_01 %>% 
  arrange(desc(cantidad_registrada)) %>% 
  slice(1:25)


ggplot(base_0C )+
  geom_bar(aes(x=reorder(Tipo,-cantidad_registrada), weight=cantidad_registrada,fill="cantidad de ejemplares"),
  fill = "darkolivegreen4")+
  coord_flip()+
  theme(legend.position="top")+  
  labs(title ="Arbolado - CABA", subtitle="Primera Aproximacion", fill="cantidades detectadas", x="especies", y="cantidad", caption= "Nota: fuente, BA data")+
  theme_light()+
  theme_classic()

#reorder nos permite presentar nuestros datos ordenadamente.

#Obervamos que algunas especies como el Fraxinus estan sobrerepresentadas.

#Nos gustaria identificar a laS especieS mas representativaS de cada barrio. 
#Para ello debemos cruzar datos.

#Primero transformamos las coordenadas en geometria.



#Ahora podemos ejutar la funcion


#Estudiemos la distribucion de las primeras 3
#Platanos, Fraxinus y Ficus.

base_05 <- base_03 %>% 
  filter (Tipo %in% c("Fraxinus pennsylvanica","Platanus x acerifolia",
  "Ficus benjamina"))

#Descubrimos que estas 3 especies practicamente representan la mitad de los ejemplares








 