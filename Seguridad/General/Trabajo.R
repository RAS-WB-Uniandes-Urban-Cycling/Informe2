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

install.packages("TSA")
library(TSA)
p<-periodogram(TotalAccidentesDias$x)
dd = data.frame(freq=p$freq, spec=p$spec)
order = dd[order(-dd$spec),]
top2 = head(order, 2)
top2
time = 1/top2$f
time

# El número de Zat es ZAT_num_n -> Por verificación directa de un par de encuestas contra el mapa
# El factor de expansión es el Ponderador Calibrado de acuerdo a David Gonzalez de la Secretaria de Movilidad
# Para ejecutar OSRM: en una consola de terminal dirigirse al folder osrm-backend: ejecutar: osrm-routed --algorithm=MLD map.xml.osrm
