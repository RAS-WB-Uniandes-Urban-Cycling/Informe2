#Preparación del entorno de trabajo----
rm(list=ls())
pacman::p_load(osrm)
pacman::p_load(sf)
pacman::p_load(cartography)
pacman::p_load(rjson)
pacman::p_load(bitops)
pacman::p_load(sp)
pacman::p_load(leaflet)
pacman::p_load(stplanr)
pacman::p_load(dplyr)
pacman::p_load(tidyr)

options(osrm.profile = "cycling")
options(osrm.server = "http://0.0.0.0:5000/")

osrmTable(m)

route=osrmRoute(src=m[2,],dst = m[3,],sp=TRUE)

zat_loca<-st_read("/Users/germancarvajal/OneDrive - Universidad de Los Andes/UNIANDES - RAS - SDM/RESULTADOS/GENERAL/GEO-DATA/ZATS_Localidad.shp")
zat_loca<-st_centroid(zat_loca)
m<-as.data.frame(st_coordinates(zat_loca))
names(m)<-c("lon","lat")
m$zat_id<-zat_loca$Zn_Nm_N
m<-m[1:5,c("zat_id","lon","lat")]

h4<-expand.grid(m$zat_id,m$zat_id) %>% 
  select(zat_src=Var1,zat_dst=Var2) %>%  
  filter(zat_src!=zat_dst) %>% 
  left_join(m,by=c("zat_src"="zat_id")) %>% 
  left_join(m,by=c("zat_dst"="zat_id")) %>% 
  rowwise() %>% 
  mutate(geometry = st_sfc(st_linestring(rbind(c(lon.x,lat.x),c(lon.y,lat.y))))) %>% 
  unite("ID",c("zat_src","zat_dst"),remove = F) %>% 
  st_as_sf(crs = 4326)

bike<-line2route(h4,route_fun = "route_osrm",l_id = "ID",osrmurl = "http://0.0.0.0:5000/",profile = "cycling")


#El número de Zat es ZAT_num_n -> Por verificación directa de un par de encuestas contra el mapa
#El factor de expansión es el Ponderador Calibrado de acuerdo a David Gonzalez de la Secretaria de Movilidad
load("/Users/germancarvajal/OneDrive - Universidad de Los Andes/UNIANDES - RAS - SDM/RESULTADOS/SEGURIDAD/Bases de datos/AccidentesCoords.Rdata")
AccidentesBici<-AccidentesBici[!is.na(AccidentesBici$lat),]
AccidentesBici<-st_as_sf(AccidentesBici,coords=c("lon","lat"),crs = 4326)
library(tmap)

w<-tm_shape(zat_loca[1:5,])+tm_dots(col="lightgray")+tm_shape(bike)+tm_lines(col="id",style="cat",lwd = 3)#+tm_shape(AccidentesBici)+tm_dots(col="red")+tm_shape(bike)+tm_lines()
tmap_leaflet(w)

h2<-st_as_sf(route)

viajes<-st_read("/Users/germancarvajal/OneDrive - Universidad de Los Andes/UNIANDES - RAS - SDM/RESULTADOS/PROPENSION/GEO-DATA",layer = "LineasViajes")

