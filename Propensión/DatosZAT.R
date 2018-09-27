library(tidyverse)
library(sf)
library(lwgeom)

# Ruta relativa a la base de datos
path <- "C:/Users/pa.uriza274/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/"

# Lectura capas base
cicloRuta <- read_sf(paste0(path,"BASES DE DATOS/Mapas de Referencia IDECA/MR0318.gdb"),layer = "RBic",stringsAsFactors = FALSE,type = 5) %>% st_transform(4326) %>% st_cast("LINESTRING",do_split = TRUE)
zats <- read_sf(paste0(path,"BASES DE DATOS/ZATs/ZATs_2012_MAG.shp"),stringsAsFactors = FALSE,quiet = TRUE) %>% st_transform(4326) %>% select(Zona_Num_N)
localidad <- read_sf(paste0(path,"BASES DE DATOS/Mapas de Referencia IDECA/MR0318.gdb"),layer = "Loca",stringsAsFactors = FALSE,quiet = TRUE) %>% st_transform(4326) %>% select(LocNombre,LocCodigo)
safetipin <- read_sf(paste0(path,"BASES DE DATOS/Safetipin.gdb"),layer = "Seguridad")

# Capa zats Bogotá
zats_Bogota <- zats %>% filter(st_centroid(.) %>% st_within(st_union(localidad %>% filter(LocNombre != "SUMAPAZ")),sparse = FALSE)) %>% st_join(localidad %>% filter(LocNombre != "SUMAPAZ"),largest = TRUE)
write_sf(zats_Bogota,paste0(path,"RESULTADOS/PROPENSION/GEO-DATA/zatsBogota.shp"),update = TRUE)

# Capa Cicloruta cortada por zats
cicloRuta2 <- cicloRuta %>% st_split(st_combine(zats_Bogota))
cicloRutaZat <- cicloRuta2 %>% st_collection_extract(type = c("LINESTRING")) %>% st_join(zats) %>% select(-SHAPE_Length)

write_sf(cicloRutaZat,paste0(path,"RESULTADOS/PROPENSION/GEO-DATA/CicloRutaZat.shp"),update = TRUE)

distcicloRutaZat <- cicloRutaZat %>% mutate(LongitudCicloRuta = as.numeric(st_length(.))) %>% group_by(Zona_Num_N) %>% summarise(LongitudCicloRuta = sum(LongitudCicloRuta)) %>% st_set_geometry(NULL)

write_csv(distcicloRutaZat,paste0(path,"RESULTADOS/PROPENSION/TABLAS/distCicloRutaZat.csv"))

# Capa calificaciones Safetipin por zats
safetipinZat <- safetipin %>% st_join(zats_Bogota,largest = TRUE)

puntajesSafetipinZat <- safetipinZat %>% group_by(Zona_Num_N) %>% summarise_if(is.numeric,mean) %>% st_set_geometry(NULL)

write_csv(puntajesSafetipinZat,paste0(path,"RESULTADOS/PROPENSION/TABLAS/puntajesSafetipinZat.csv"))

# Capa accidentes por zats
load(paste0(path,"RESULTADOS/SEGURIDAD/Bases de datos/AccidentesCoords.Rdata"))
NAccidentesZat <- AccidentesBici[57:59] %>% filter(!is.na(lon)) %>% st_as_sf(coords = c("lon","lat"),crs = 4326) %>% st_join(zats_Bogota,largest = TRUE) %>% 
  group_by(Zona_Num_N) %>% summarise(CantidadAccidentes =  n())%>% st_set_geometry(NULL)

write_csv(NAccidentesZat,paste0(path,"RESULTADOS/PROPENSION/TABLAS/NAccidentesZat.csv"))
