#Preparación del entorno de trabajo----
rm(list=ls())
carpetaRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/UNIANDES - RAS - SDM"
gitRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/proBikePolicies"
require(pacman)
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
pacman::p_load(tmap)

#Configuración del servidor de ruteo local----
options(osrm.profile = "cycling")
options(osrm.server = "http://0.0.0.0:5000/")

#Lectura de la capa de ZAT y extracciones de los centroides----
zat_loca<-st_read(paste0(carpetaRAS,"/BASES DE DATOs/ZATs"))
zat_loca_ctr<-st_centroid(zat_loca)
zat_coord<-as.data.frame(st_coordinates(zat_loca_ctr))
names(zat_coord)<-c("lon","lat")
zat_coord$zat_id<-zat_loca$Zona_Num_N
zat_coord<-zat_coord[,c("zat_id","lon","lat")]

#Creación de las combinaciones de ZAT origen-destino por pares----
zat2zat<-expand.grid(zat_coord$zat_id,zat_coord$zat_id) %>% 
  select(zat_src=Var1,zat_dst=Var2) %>%  
  filter(zat_src!=zat_dst) %>% 
  left_join(zat_coord,by=c("zat_src"="zat_id")) %>% 
  left_join(zat_coord,by=c("zat_dst"="zat_id")) %>% 
  rowwise() %>% 
  mutate(geometry = st_sfc(st_linestring(rbind(c(lon.x,lat.x),c(lon.y,lat.y))))) %>% 
  unite("ID",c("zat_src","zat_dst"),remove = F) %>% 
  st_as_sf(crs = 4326) %>% 
  filter(!duplicated(.))

#Computo de las rutas de viaje en bicicleta más cortas entre pares de ZATs----
bike_dist_zat2zat<-line2route(zat2zat[1:1000,],route_fun = "route_osrm",l_id = "ID",osrmurl = "http://0.0.0.0:5000/",profile = "cycling")
for(i in 1:8){
  print(i)
  bike_dist_zat2zat2<-line2route(zat2zat[(1000*i+1):(1000*(i+1)),],route_fun = "route_osrm",l_id = "ID",osrmurl = "http://0.0.0.0:5000/",profile = "cycling")
  bike_dist_zat2zat<-rbind(bike_dist_zat2zat,bike_dist_zat2zat2)
  if(i%%10==0){
    save(zat2zat,bike_dist_zat2zat,file=paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/8. DistZat",(i+1)*1000,".Rdata"))
  }
}
bike_dist_zat2zat2<-line2route(zat2zat[9001:9694,],route_fun = "route_osrm",l_id = "ID",osrmurl = "http://0.0.0.0:5000/",profile = "cycling")
bike_dist_zat2zat<-rbind(bike_dist_zat2zat,bike_dist_zat2zat2)
save(bike_dist_zat2zat,file=paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/9. DistZat2Zat.Rdata"))
