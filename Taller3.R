
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