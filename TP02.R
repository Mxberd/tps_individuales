#TRABAJO PRACTICO 02

#Para empezar, llamamos a nuestras librerias.

library(tidyverse)
library(datos)
library(janitor)
library(sf)
library(zip)
library(skimr)

#Tambien acomodamos la visualizacion de los valores numericos elimando notacion cientifica.
options(scipen = 999)

#Estaremos trabajando con el arbolado urbano de CABA. Los datos son de BADATA.

#Importamos la base de datos.


base_00<- read.csv("Datos/arbolado-publico-lineal-2017-2018.csv", 
                   encoding = "UTF-8", dec = ".")

#Si hubiera algun problema de lectura, tambien se puede cargar el zip que esta incluido en "datos"

#Podemos continuar!

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

#Estudiemos la distribucion de las primeras 3
#Platanos, Fraxinus y Ficus.

base_05 <- base_03 %>% 
  filter (Tipo %in% c("Fraxinus pennsylvanica","Platanus x acerifolia",
                      "Ficus benjamina"))

#Descubrimos que estas 3 especies practicamente representan la mitad de los ejemplares

#Queremos averiguar cual es el barrio con mas ficus.

barrios_01 <- barrios %>% 
  select(barrio,geometry)

base_06 <- base_05 %>%
  filter(Tipo=="Ficus benjamina") %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  select(Tipo,geometry) %>% 
  st_join(barrios_01)

#Habiendo unido con st_join la informacion de los barrios con la 
#respectiva ubicacion de los arboles, estamos listos para hacer un mapa coropletico

base_0D <- base_06 %>% 
  select(barrio)%>%
  group_by(barrio) %>% 
  summarise(cantidad_registrada=n())

#Con st_set_geometry(NULL) elimino los datos espaciales que contenian los puntos de los arboles

base_0E<-base_0D %>% 
  st_set_geometry(NULL) 

#hago una nueva union y vuelvo a aplicar st_as_sf.

base_0E <- base_0E %>% 
  right_join(barrios_01) %>% 
  st_as_sf()


ggplot(base_0E)+
  geom_sf(aes(fill=cantidad_registrada), color= NA)+
  scale_fill_viridis_c(breaks=c(0,200,400,600,800,1000,1200))+
  geom_sf_text(data=base_0E, aes(label = barrio), size=1.5)+
  labs(title = "Mapa coropletico arbolado",
       subtitle = "Visualizacion especie Ficus Benjamina",
       fill = "arboles/barrio",
       caption= "Fuente: BADATA, elaboracion propia")+
  theme_light()+
  theme_void()

#PALERMO, VILLA URQUIZA, CABALLITOS,FLORES Y MATADEROS
#SON LOS BARRIOS DONDE MAS FICUS HAN ESTADO PLANTANDO (SIN DUDA) LOS VECINOS-

#HEMOS TERMINADO EL EJERCICIO!


 