library(tidyverse)
library(lubridate)
library(sf)

EncuestaM <- Encuesta %>% mutate(id_manzana=as.character(id_manzana),SecCodigo = paste0("00",str_sub(id_manzana,7,10)),ManCodigo = paste0(SecCodigo,0,str_sub(id_manzana,13,14)))

Data <- Personas %>% select(moviliza_bicicleta,id_encuesta,numero_persona,sexo,edad,nivel_educativo,actividad_principal,actividad_economica,licenciaconduccion1) %>%
  left_join(select(EncuestaM,id_encuesta,estrato,id_manzana,SecCodigo,ManCodigo,zat_hogar,vehic_personaidonea)) %>% left_join(group_by(Vehiculos,id_encuesta) %>% summarise(NumVehiculosM=n())) %>% 
  mutate(NumVehiculosM = ifelse(is.na(NumVehiculosM),0,NumVehiculosM),NumVehiculosNM=pmax(0,vehic_personaidonea-NumVehiculosM)) %>% filter(!is.na(moviliza_bicicleta))

ruta <- "C:/Users/pa.uriza274/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/BASES DE DATOS/Mapas de Referencia IDECA/MR"
manz2014 <- st_read(paste0(ruta,"1214.gdb"),layer = "Manz",stringsAsFactors = F,quiet = T) %>% st_cast("MULTIPOLYGON") %>% st_transform(4326) %>% st_centroid(.) %>% cbind(st_coordinates(.)) %>% select(ManCodigo,SecCodigo,LongMan=X,LatMan=Y)%>% st_set_geometry(NULL)

ruta2 <- "C:/Users/pa.uriza274/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/BASES DE DATOS/Marco Geoestadistico Nacional - DANE/2012/11_BOGOTA/MGN/"
MNG_BOG <- st_read(paste0(ruta2,"MGN_Manzana.shp"),stringsAsFactors = F,quiet = T) %>% st_transform(4326) %>% st_centroid(.) %>% cbind(st_coordinates(.)) %>% transmute(SecCodigo = paste0("00",SECU_SET_1),ManCodigo = paste0(SecCodigo,0,MANZ_CCDGO),LongMan=X,LatMan=Y)%>% st_set_geometry(NULL)
MNG_BOG <- st_read(paste0(ruta2,"MGN_Manzana.shp"),stringsAsFactors = F,quiet = T) %>% st_transform(4326) %>% st_centroid(.) %>% cbind(st_coordinates(.)) %>% transmute(id_manzana = paste0(SETR_CLSE_,SECR_SETR_,CPOB_SECR_,SECU_SET_1,SECU_SECU_,MANZ_CCDGO),LongMan=X,LatMan=Y)%>% st_set_geometry(NULL)

ruta2 <- "C:/Users/pa.uriza274/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/BASES DE DATOS/Marco Geoestadistico Nacional - DANE/2017/11_BOGOTA/URBANO/"
MNG_BOG <- st_read(paste0(ruta2,"MGN_URB_MANZANA.shp"),stringsAsFactors = F,quiet = T) %>% st_transform(4326) %>% st_centroid(.) %>% cbind(st_coordinates(.)) %>% transmute(id_manzana = paste0(MPIO_CCDGO,CLAS_CCDGO,SETU_CCDGO,SECU_CCDGO,MANZ_CCDGO),LongMan=X,LatMan=Y)%>% st_set_geometry(NULL)


ruta3 <- "C:/Users/pa.uriza274/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/BASES DE DATOS/Marco Geoestadistico Nacional - DANE/2012/25_CUNDINAMARCA/MGN/"
MNG_CUN <- st_read(paste0(ruta3,"MGN_Manzana.shp"),stringsAsFactors = F,quiet = T) %>% st_transform(4326) %>% st_centroid(.) %>% cbind(st_coordinates(.)) %>% transmute(SecCodigo = paste0("00",SECU_SET_1),ManCodigo = paste0(SecCodigo,0,MANZ_CCDGO),LongMan=X,LatMan=Y)%>% st_set_geometry(NULL)
MNG_CUN<- st_read(paste0(ruta3,"MGN_Manzana.shp"),stringsAsFactors = F,quiet = T) %>% st_transform(4326) %>% st_centroid(.) %>% cbind(st_coordinates(.)) %>% transmute(id_manzana = paste0(SETR_CLSE_,SECR_SETR_,CPOB_SECR_,SECU_SET_1,SECU_SECU_,MANZ_CCDGO),LongMan=X,LatMan=Y)%>% st_set_geometry(NULL)
MNG_CUN_SEC <- st_read(paste0(ruta3,"MGN_Seccion_urbana.shp"),stringsAsFactors = F,quiet = T) %>% st_transform(4326) %>% st_centroid(.) %>% cbind(st_coordinates(.)) %>% transmute(id_manzana = paste0(SETR_CLSE_,SECR_SETR_,CPOB_SECR_,SECU_SET_1,SECU_SECU_,MANZ_CCDGO),LongMan=X,LatMan=Y)%>% st_set_geometry(NULL)


Viajes <- read_csv2("../../Bases de datos/EncuestaMovilidad2015/Encuesta/encuesta 2015 - viajes.csv",locale = locale(encoding = stringi::stri_enc_get())) %>% `names<-`(str_to_lower(names(.)))

Orig <- Viajes %>% filter(!is.na(latitud_origen),latitud_origen>0) %>% st_as_sf(coords = c("longitud_origen","latitud_origen"),crs=4326)

zats <- st_read("../../Bases de datos/Bases.gdb",layer = "ZATs",stringsAsFactors = FALSE,quiet = TRUE) %>% st_transform(4326) %>% transmute(id = as.numeric(id),zat_hogar = as.numeric(zona_num_n)) %>% filter(!st_is_empty(.))

OrigenViajes <- Orig %>% st_join(zats,join = st_within)

tmap_mode("view")

mapO <- tm_shape(name = "Bogotá",osmdata::getbb("Bogotá", format_out = "sf_polygon")) + 
  tm_borders(col = "black") + tm_shape(name = "Proporcion",zatsMB) +
  tm_polygons(col = "Total", palette = "YlOrRd", style = "jenks",n = 5, id = "zat_hogar", title = "Proporción")+ 
  tm_shape(name = "Origen",filter(b,`n()`==2))+tm_dots(col = "black") + tm_legend(legend.position = c("left", "bottom")) + tm_layout(title = "Total encuestados por zat", basemaps = c( leaflet::providers$Stamen.TonerHybrid),basemaps.alpha = 0.3)

c <- OrigenViajes %>% right_join(Data) %>% group_by(id_encuesta,numero_persona)

Vp <- head(Viajes,2) %>% rowwise()%>% 
  mutate(geometry = list(rbind(c(longitud_origen,latitud_origen),c(longitud_destino,latitud_destino))))

Vp2 <- Vp %>% mutate(A = st_linestring(matrix(unlist(geometry),ncol = 2)))

Vp2 <- Vp %>% mutate(A = list(st_linestring(geometry)))

Vp2 <- Vp %>% mutate(A = st_sfc(st_linestring(geometry)))

l <- (lapply(Vp$geometry,FUN = st_linestring))

st_linestring(matrix(unlist(Vp$geometry),ncol = 2))

lineasDeseo <- Viajes %>% unite("id",c("id_encuesta","numero_persona","numero_viaje"),remove = F) %>% 
  filter(!is.na(latitud_origen),latitud_origen>0,!is.na(longitud_origen),longitud_origen>-80,
         !is.na(latitud_destino),latitud_destino>0,!is.na(longitud_destino),longitud_destino>-80) %>% 
  rowwise() %>% mutate(geometry = st_sfc(st_linestring(rbind(c(longitud_origen,latitud_origen),c(longitud_destino,latitud_destino))))) %>% 
  st_as_sf(crs = 4326) 

st_write(lineasDeseo,"C:/Users/pa.uriza274/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/RESULTADOS/PROPENSION/GEO-DATA/LineasViajes.shp")

lineasDeseoManhattan <- Viajes %>% unite("id",c("id_encuesta","numero_persona","numero_viaje"),remove = F) %>% 
  filter(!is.na(latitud_origen),latitud_origen>0,!is.na(longitud_origen),longitud_origen>-80,
         !is.na(latitud_destino),latitud_destino>0,!is.na(longitud_destino),longitud_destino>-80) %>% 
  rowwise() %>% mutate(geometry = st_sfc(st_linestring(rbind(c(longitud_origen,latitud_origen),c(longitud_destino,latitud_origen),c(longitud_destino,latitud_destino))))) %>% 
  st_as_sf(crs = 4326) %>% ungroup() %>%  mutate(Distancia = as.numeric(st_length(.)))

st_write(lineasDeseoManhattan,"C:/Users/pa.uriza274/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/RESULTADOS/PROPENSION/GEO-DATA/LineasViajesManhattan.shp")


for (i in 8:14){
rutas <- line2route(lineasDeseo[(10000*(i-1)+1):(10000*i),],"route_osrm",l_id = "id")
st_write(rutas,paste0("C:/Users/pa.uriza274/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/RESULTADOS/PROPENSION/GEO-DATA/rutas",i,".shp"))
}

#### Lectura lineas viajes Manhattan
ViajesL1 <- st_read("C:/Users/pa.uriza274/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/RESULTADOS/PROPENSION/GEO-DATA/LineasViajesManhattan.shp",stringsAsFactors = FALSE,quiet = TRUE) %>% st_set_geometry(NULL)
names(ViajesL1) <- c("id","id_encuesta","numero_persona","numero_viaje","motivoviaje","municipio_destino","departamento_destino","tiempo_camino","hora_inicio",
                     "hora_fin","medio_predominante","zat_destino","zat_origen","municipio_origen","departamento_origen","latitud_origen","latitud_destino",
                     "longitud_origen","longitud_destino","diferencia_horas","factor_ajuste","ponderador_calibrado","dia_habil","dia_nohabil","pico_habil",
                     "pico_nohabil","valle_nohabil","valle_habil","pi_k_i","pi_k_ii","pi_k_iii","fe_total","factor_ajuste_transmilenio","ponderador_calibrado_viajes",
                     "Distancia","geometry")

Data <- Personas %>% select(moviliza_bicicleta,id_encuesta,numero_persona,sexo,edad,nivel_educativo,actividad_principal,actividad_economica,licenciaconduccion1) %>%
  left_join(select(Encuesta,id_encuesta,estrato,zat_hogar,vehic_personaidonea)) %>% left_join(group_by(Vehiculos,id_encuesta) %>% summarise(NumVehiculosM=n())) %>% 
  mutate(NumVehiculosM = ifelse(is.na(NumVehiculosM),0,NumVehiculosM),NumVehiculosNM=pmax(0,vehic_personaidonea-NumVehiculosM)) %>% filter(!is.na(moviliza_bicicleta)) %>% 
  inner_join((lineasDeseoManhattan %>% st_set_geometry(NULL) %>% filter(Distancia>0) %>% group_by(id_encuesta,numero_persona,numero_viaje,zat_origen) %>% transmute(longitud_origen,latitud_origen,DistProm = mean(Distancia,na.rm = T))),
            by = c("id_encuesta","numero_persona","zat_hogar" = "zat_origen")) %>% group_by(id_encuesta,numero_persona) %>% top_n(1,wt = numero_viaje) %>%
  rowwise() %>% mutate(geometry = st_sfc(st_point(c(longitud_origen,latitud_origen)))) %>% st_as_sf(crs = 4326)

mapO <- tm_shape(name = "Bogotá",osmdata::getbb("Bogotá", format_out = "sf_polygon")) + 
  tm_borders(col = "black") + tm_shape(name = "Proporcion",zatsMB) +
  tm_polygons(col = "Total", palette = "YlOrRd", style = "jenks",n = 5, id = "zat_hogar", title = "Proporción")+ 
  tm_shape(name = "Origen",Data)+tm_dots(col = "black",alpha = 0.2) + tm_legend(legend.position = c("left", "bottom")) + tm_layout(title = "Total encuestados por zat", basemaps = c( leaflet::providers$Stamen.TonerHybrid),basemaps.alpha = 0.3)
mapO


mapL <- tm_shape(name = "Bogotá",osmdata::getbb("Bogotá", format_out = "sf_polygon")) + 
  tm_borders(col = "black") + tm_shape(name = "Proporcion",zatsMB) +
  tm_polygons(col = "Total", palette = "YlOrRd", style = "jenks",n = 5, id = "zat_hogar", title = "Proporción")+ 
  tm_shape(name = "Origen",head(lineasDeseoManhattan,50))+tm_lines(col = "black") + tm_legend(legend.position = c("left", "bottom")) + tm_layout(title = "Total encuestados por zat", basemaps = c( leaflet::providers$Stamen.TonerHybrid),basemaps.alpha = 0.3)
mapL
