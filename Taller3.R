
# intial configuration
rm(list = ls()) # limpia el entorno de R
pacman::p_load(tidyverse,sf,raster,leaflet,skimr) # cargar y/o instalar paquetes a usar

#1.1 Importar datos
#Dado que solo piden importar el shapefile sin ninguna especificación, se usa
via = st_read("data/input/VIAS.shp")
puntos = st_read("data/input/MGN_URB_TOPONIMIA.shp")


#1.1.2 nuevo dato
c_medico= subset(puntos,CSIMBOL %in% c('021001','021002','021003'))

#1.1.3 nuevo dato
c_poblado=readRDS("data/input/c poblado (2017).rds")
depto=readRDS("data/input/dp deptos (2017).rds") %>% subset(name_dpto=="NORTE DE SANTANDER")
mapmuse=readRDS("data/input/victimas_map-muse.rds") %>% subset(cod_mpio>=54001) %>% subset(cod_mpio<55000)

#1.2 skim

skim(via)
skim(c_medico)
skim(c_poblado)
skim(depto)
skim(mapmuse)

#1.3 Geometrias del objeto
#1.3.1
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

#1.3.2
via= st_transform(via, "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
c_medico= st_transform(c_medico, "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
c_poblado= st_transform(c_poblado, "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
depto= st_transform(depto, "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")
mapmuse=st_transform(mapmuse, "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs")

#1.4.1 Clipping
#usando atributos
ggplot() + geom_sf(data = mapmuse) +
  geom_sf(data = depto,col="red") + theme_bw()
#usando geometria
ggplot() + geom_sf(data = mapmuse[depto,],col="red") +
  geom_sf(data = depto,fill=NA,col="blue") + theme_bw()
#1.4.2 Vias
sample(c_poblado$codmpio,1) #resulto en 05380
#????????????

#2
rm(list = ls()) # limpia el entorno de R
pacman::p_load(tidyverse,lfe,plm,AER,margins,stargazer,outreg,broom,estimatr, ggplot2) # cargar y/o instalar paquetes a usar

#2.1
df= readRDS("data/output/f_mapmuse.rds")
colnames(df)
ols= lm(formula= fallecido~tipo_accidente+ year+ month+ condicion + genero + actividad + cod_mpio + dist_hospi + dist_cpoblado + dist_vias, data=df)

#2.2 No estoy segura?????
source("https://www.r-statistics.com/wp-content/uploads/2010/07/coefplot.r.txt")
coefplot(ols)

#2.3
logit= glm(formula= fallecido~tipo_accidente+ year+ month+ condicion + genero + actividad + cod_mpio + dist_hospi + dist_cpoblado + dist_vias, data=df, family= binomial(link="logit"))
probit= glm(formula= fallecido~tipo_accidente+ year+ month+ condicion + genero + actividad + cod_mpio + dist_hospi + dist_cpoblado + dist_vias, data=df, family= binomial(link="probit"))

list_modelos= list(ols, logit, probit)
outreg(list_modelos)
r_outreg = outreg(setNames(list_modelos, c('ols', 'logit', 'probit')))
r_outreg
#ni idea como exportarlo

#3 Web Scraping
rm(list = ls())
pacman::p_load(tidyverse,data.table,plyr,XML,rvest,xml2)

#3.1
myurl = "https://es.wikipedia.org/wiki/Departamentos_de_Colombia"
myhtml = read_html(myurl)
class(myhtml)

#3.2
myhtml %>% html_nodes(xpath = '//*[@id="firstHeading"]')
texto = myhtml %>% html_nodes(xpath = '//*[@id="firstHeading"]') %>% 
html_text() # Convertir en texto
texto

#3.3
parse = myhtml %>% htmlParse()
tablas = parse %>% readHTMLTable(header = T)
tablas[[4]] %>% class()
tablas[[4]]
deptos = tablas[[4]]