# =========================================================== #
# Elaborado por: Gabriela Navarro (201821568)                 #
#                Daniela Quintero (201821754)                 #
#                Maria Alejandra Saavedra (201815221)         #                                                               
# Fecha de elaboración: 15/05/2021                            #
# Ultima modificación: 04/06/2021                             #
# =========================================================== #


### TASK 3 - TALLER A ###

# initial configuration
rm(list = ls()) # limpia el entorno de R
pacman::p_load(tidyverse,sf,raster,leaflet,skimr, here, viridis, gapmindere) # cargar y/o instalar paquetes a usar


### PUNTO 1 - DATOS ESPACIALES ###
# 1.1 - Importar Datos Espaciales
 
  # 1.1.1
# Dado que solo piden importar el shapefile sin ninguna especificacion, se usa
via = st_read("data/input/VIAS.shp")
puntos = st_read("data/input/MGN_URB_TOPONIMIA.shp")

  # 1.1.2 
# Se crea un objeto que contiene la variable CSIMBOL igual a los valores indicados. 
# Para ello se hace uso de 'subset'que permite hacer uso de condicionales.
c_medico = subset(puntos,CSIMBOL %in% c('021001','021002','021003'))

  # 1.1.3
# Se importan los objetos que se encuentran en RDS. 
c_poblado = readRDS("data/input/c poblado (2017).rds")
depto = readRDS("data/input/dp deptos (2017).rds") %>% subset(name_dpto == "NORTE DE SANTANDER")
mapmuse = readRDS("data/input/victimas_map-muse.rds") %>% subset(cod_mpio >= 54001) %>% subset(cod_mpio < 55000)

# 1.2 - Atributos de los objetos

# Con el objetivo de explorar los objetos cargados, se usa 'skim' que da una amplia descripción de los data frame.
skim(via)
skim(c_medico)
skim(c_poblado)
skim(depto)
skim(mapmuse)

# 1.3 - Geometrias del objeto

  # 1.3.1
# Para obtener las coordenadas (st_bbox) y el CRS (st_crs) de cada objeto se realiza lo siguiente. 

st_bbox(via)
st_crs(via)

st_bbox(c_medico)
st_crs(c_medico)

st_bbox(c_poblado)
st_crs(c_poblado)

st_bbox(depto)
st_crs(depto)

st_bbox(mapmuse)
st_crs(mapmuse)

  # 1.3.2
# Se proyecta el CRS de todos los objetos. Para ello se usa 'st_transform' que transforma las coordenadas.  
puntos = st_transform(puntos, "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
via = st_transform(via, "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
c_medico = st_transform(c_medico, "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
c_poblado = st_transform(c_poblado, "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
depto = st_transform(depto, "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
mapmuse = st_transform(mapmuse, "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")

# 1.4 - Operaciones geometricas

  # 1.4.1 
# Para este punto se hace clipping. Esto quiere decir que, se recortan datos espaciales basados en diferentes objetos espaciales.
mapmuse = mapmuse[depto,]
mapmuse = st_intersection(mapmuse,depto)

#usando atributos
ggplot() + geom_sf(data = mapmuse) +
  geom_sf(data = depto,col="red") + theme_bw() # Queda delimitado Norte de Santander usando atributos

#usando geometria
ggplot() + geom_sf(data = mapmuse[depto,],col="red") +
  geom_sf(data = depto,fill=NA,col="blue") + theme_bw() #Queda delimitado Norte de Santander usando geometria
 
  # 1.4.2 Vias
clip_vias = st_intersection(vias,c_poblados %>% subset(cod_dane == 54001)) %>% st_length()
sample(c_poblado$codmpio,1) # Resulto en 05380

# 1.5 - Pintar Mapas
 
 # 1.5.1 
# Se usa el comando 'leaflet' para crear un mapa interactivo con el polígono de Norte de Santander y demas caracteristicas. 
map = leaflet() %>% addTiles() %>% addCircleMarkers(data = puntos %>% st_transform(.,4326))
map

  # 1.5.2 

c_medico = mutate(c_medico, Sitio = factor(c('Centro medico'))) #Identifica un centro medico
c_poblado = mutate(c_poblado, Sitio = factor(c('Centro poblado'))) #Identifica un centro poblado

map2 = ggplot(puntos %>% st_transform(.,4326), col="blue") + geom_sf(fill="red" , alpha=0.2 , colour="red" , size=1) + 
  geom_sf(data = depto,fill=NA,col="blue")+
  theme_bw()+ north(puntos%>% st_transform(.,4326)) +
  scale_fill_continuous(low = "#fff7ec", high = "#7F0000")
map2 #Se crea un mapa con ggplot() delimitado en Norte de Santander con la estrella del norte y la barra de escalas.

ggsave(plot = map2, file = "views/Mapa.pdf") 

### PUNTO 2 - REGRESIONES ###

# initial configuration
rm(list = ls()) # limpia el entorno de R
pacman::p_load(tidyverse,lfe,plm,AER,margins,stargazer,outreg,broom,estimatr, ggplot2) # cargar y/o instalar paquetes a usar

  # 2.1
df = readRDS("data/output/f_mapmuse.rds")
colnames(df)
ols = lm(formula= fallecido~tipo_accidente+ year+ month+ condicion + genero + actividad + cod_mpio + dist_hospi + dist_cpoblado + dist_vias, data=df) #Se estima un modelo de Probabilidad Lineal

  # 2.2 
source("https://www.r-statistics.com/wp-content/uploads/2010/07/coefplot.r.txt")
coefplot(ols) # Se exportan los graficos con los coeficientes 

  # 2.3
logit= glm(formula= fallecido~tipo_accidente+ year+ month+ condicion + genero + actividad + cod_mpio + dist_hospi + dist_cpoblado + dist_vias, data=df, family= binomial(link="logit")) # Estimación por logit
probit= glm(formula= fallecido~tipo_accidente+ year+ month+ condicion + genero + actividad + cod_mpio + dist_hospi + dist_cpoblado + dist_vias, data=df, family= binomial(link="probit")) # Estimación por probit

  #2.4 
list_modelos= list(ols, logit, probit)
r_outreg = outreg(list_modelos) 
cat(r_outreg, file = "outreg.txt")

  #2.5 

### PUNTO 3 - WEB SCRAPING ###
# Initial configuration
rm(list = ls())
pacman::p_load(tidyverse,data.table,plyr,XML,rvest,xml2)

  # 3.1
myurl = "https://es.wikipedia.org/wiki/Departamentos_de_Colombia"
myhtml = read_html(myurl)
class(myhtml)

  # 3.2
myhtml %>% html_nodes(xpath = '//*[@id="firstHeading"]')
texto = myhtml %>% html_nodes(xpath = '//*[@id="firstHeading"]') %>% 
html_text() # Convertir en texto
texto

  # 3.3
parse = myhtml %>% htmlParse()
tablas = parse %>% readHTMLTable(header = T)
tablas[[4]] %>% class()
tablas[[4]]
deptos = tablas[[4]]