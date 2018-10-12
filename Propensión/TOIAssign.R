library(tidyverse)
library(sf)

path <- "C:/Users/pa.uriza274/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/"

TOI <- readxl::read_xlsx(paste0(path,"BASES DE DATOS/TOI/TOI_UPZ_Localidad.xlsx"))
TOI<- TOI %>% expand(orig,dest) %>% left_join(select(TOI,orig,dest,toi)) %>% mutate(orig=paste0("UPZ",orig),dest=paste0("UPZ",dest)) %>% group_by(orig) %>% 
  mutate(toi = if_else(orig==dest,max(toi,na.rm = T),toi)) %>% ungroup()

UPZ <- read_sf(paste0(path,"BASES DE DATOS/Mapas de Referencia IDECA/MR0318.gdb"),layer = "UPLA",stringsAsFactors = FALSE) %>% st_transform(4326) %>% filter(UPlTipo == 1) %>% select(UPlCodigo)

UPZ_origen <- st_centroid(buffer_origen) %>% st_join(UPZ,largest = T) %>% st_set_geometry(NULL)
UPZ_destino <- st_centroid(buffer_destino) %>% st_join(UPZ,largest = T) %>% st_set_geometry(NULL)
TOI <- UPZ_origen %>% left_join(UPZ_destino,by=c("id_encuesta","numero_persona","numero_viaje"),suffix = c(".O",".D")) %>% 
  left_join(TOI,by=c("UPlCodigo.O"="orig","UPlCodigo.D"= "dest")) %>% select(id_encuesta,numero_persona,numero_viaje,toi)

save(TOI,file = paste0(path,"RESULTADOS/PROPENSION/GEO-DATA/TOI.RData"))
