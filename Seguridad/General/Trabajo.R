#Preparación del entorno de trabajo----
rm(list=ls())
carpetaRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/Uniandes - RAS - SDM"
gitRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/proBikePolicies"
pacman::p_load(sf)
pacman::p_load(tmap)

zats_loca<-st_read(paste0(carpetaRAS,"/RESULTADOS/GENERAL/GEO-DATA/ZATs_Localidad.shp"))

manzanas<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/IDECA_0318.gdb"),layer = "Manz")

m<-tm_shape(zats_loca)+tm_polygons()

tmap_leaflet(m)

#El número de Zat es ZAT_num_n3 -> Por verificación directa de un par de encuestas contra el mapa
#El factor de expansión es el Ponderador Calibrado de acuerdo a David Gonzalez de la Secretaria de Movilidad


load("/Users/germancarvajal/OneDrive - Universidad de Los Andes/UNIANDES - RAS - SDM/RESULTADOS/SEGURIDAD/Bases de datos/AccidentesCoords.Rdata")
zat_loca<-st_read("/Users/germancarvajal/OneDrive - Universidad de Los Andes/UNIANDES - RAS - SDM/RESULTADOS/GENERAL/GEO-DATA/ZATS_Localidad.shp")
AccidentesBici<-AccidentesBici[!is.na(AccidentesBici$lat),]
AccidentesBici<-st_as_sf(AccidentesBici,coords=c("lon","lat"),crs = 4326)
library(tmap)

m<-tm_shape(zat_loca)+tm_polygons(col="lightgray")+tm_shape(AccidentesBici)+tm_dots(col="red")
tmap_leaflet(m)
