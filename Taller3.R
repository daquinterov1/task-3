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
pacman::p_load(tidyverse,sf,raster,leaflet,skimr) # cargar y/o instalar paquetes a usar
   # HELLO GENTE QUE LEA ESTO  NOS FALTA AGREGAR ESTAS ??   here,viridis,gapminder,

### PUNTO 1 - DATOS ESPACIALES ###
# 1.1 - Importar Datos Espaciales
 
  # 1.1.1
# Dado que solo piden importar el shapefile sin ninguna especificaci?n, se usa
via = st_read("data/input/VIAS.shp")
puntos = st_read("data/input/MGN_URB_TOPONIMIA.shp")

  # 1.1.2 
c_medico = subset(puntos,CSIMBOL %in% c('021001','021002','021003'))

  # 1.1.3
c_poblado = readRDS("data/input/c poblado (2017).rds")
depto = readRDS("data/input/dp deptos (2017).rds") %>% subset(name_dpto == "NORTE DE SANTANDER")
mapmuse = readRDS("data/input/victimas_map-muse.rds") %>% subset(cod_mpio >= 54001) %>% subset(cod_mpio < 55000)

# 1.2 - Atributos de los objetos

skim(via)
skim(c_medico)
skim(c_poblado)
skim(depto)
skim(mapmuse)

# 1.3 - Geometrias del objeto

  # 1.3.1
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
puntos = st_transform(puntos, "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
via = st_transform(via, "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
c_medico = st_transform(c_medico, "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
c_poblado = st_transform(c_poblado, "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
depto = st_transform(depto, "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
mapmuse = st_transform(mapmuse, "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")

# 1.4 - Operaciones geometricas

  # 1.4.1 
mapmuse = mapmuse[depto,]
mapmuse = st_intersection(mapmuse,depto)
#usando atributos
ggplot() + geom_sf(data = mapmuse) +
  geom_sf(data = depto,col="red") + theme_bw()
#usando geometria
ggplot() + geom_sf(data = mapmuse[depto,],col="red") +
  geom_sf(data = depto,fill=NA,col="blue") + theme_bw()
 
  # 1.4.2 Vias
clip_vias = st_intersection(vias,c_poblados %>% subset(cod_dane == 54001)) %>% st_length()
sample(c_poblado$codmpio,1) #resulto en 05380

# 1.5 - Pintar Mapas
  # 1.5.1 
leaflet() %>% addTiles() %>% addCircleMarkers(data = puntos %>% st_transform(.,4326))
  
  # 1.5.2 
## !FALTA!

### PUNTO 2 - REGRESIONES ###
# initial configuration
rm(list = ls()) # limpia el entorno de R
pacman::p_load(tidyverse,lfe,plm,AER,margins,stargazer,outreg,broom,estimatr, ggplot2) # cargar y/o instalar paquetes a usar

  # 2.1
df = readRDS("data/output/f_mapmuse.rds")
colnames(df)
ols = lm(formula= fallecido~tipo_accidente+ year+ month+ condicion + genero + actividad + cod_mpio + dist_hospi + dist_cpoblado + dist_vias, data=df)

  # 2.2 
## No estoy segura?????
source("https://www.r-statistics.com/wp-content/uploads/2010/07/coefplot.r.txt")
coefplot(ols)

  # 2.3
logit= glm(formula= fallecido~tipo_accidente+ year+ month+ condicion + genero + actividad + cod_mpio + dist_hospi + dist_cpoblado + dist_vias, data=df, family= binomial(link="logit"))
probit= glm(formula= fallecido~tipo_accidente+ year+ month+ condicion + genero + actividad + cod_mpio + dist_hospi + dist_cpoblado + dist_vias, data=df, family= binomial(link="probit"))

list_modelos= list(ols, logit, probit)
outreg(list_modelos)
r_outreg = outreg(setNames(list_modelos, c('ols', 'logit', 'probit')))
r_outreg
#ni idea como exportarlo

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