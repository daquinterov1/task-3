
# intial configuration
rm(list = ls()) # limpia el entorno de R
pacman::p_load(tidyverse,sf,raster,leaflet) # cargar y/o instalar paquetes a usar

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
