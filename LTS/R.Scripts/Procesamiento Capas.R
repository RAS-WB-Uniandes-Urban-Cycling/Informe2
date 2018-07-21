#Preparación del entorno de trabajo----

  #Se cargan las siguiente librerias
    library(osmar)
    library(osmdata)
    library(sf)
    library(tidyverse)
    library(tmap)
    library(reshape)
    library(readxl)
    library(googleway)
    library(NISTunits)
    library(units)
    library(tmaptools)
    library(rcompanion)
    library(kernlab)
    library(stplanr)
    library(lwgeom)

  #Se define las ruta de las bases de datos
    
    ruta_base_datos<-"/Users/alejandropalacio/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/BASES DE DATOS/"
    ruta_resultados <-"/Users/alejandropalacio/Documents/GitHub/proBikePolicies/LTS/Resultados/"
    ruta_scripts<-"/Users/alejandropalacio/Documents/GitHub/proBikePolicies/LTS/R.Scripts/"
    
  #Se almacena los layer en cada variable definida
    
    malla_vial<-st_read(paste0(ruta_base_datos,"OpenStreetMaps/Malla Vial Bogotá/MVI.shp"), stringsAsFactors = FALSE)%>% filter(!(highway %in% c("cycleway","service","steps","footway"))) %>% st_transform(4326) 
    ciclo_rutas<-st_read(paste0(ruta_base_datos,"IDECA_0318.gdb"),layer = "RBic",stringsAsFactors = FALSE) %>% st_transform(4326) 
    zats<-st_read(paste0(ruta_base_datos,"IDECA_0318.gdb"),layer = "SCAT",stringsAsFactors = FALSE) %>% filter(SCaNombre %in% c("DOCE DE OCTUBRE","CIUDAD SALITRE NOR-ORIENTAL","CIUDAD SALITRE SUR-ORIENTAL")) %>%
      st_transform(4326)
    calzadas<-st_read(paste0(ruta_base_datos,"IDECA_0318.gdb"),layer = "Calz",stringsAsFactors = FALSE,promote_to_multi = FALSE) %>% st_transform(4326) %>% filter(st_is_valid(.))
    ruta_urbana <- st_read(paste0(ruta_base_datos,"RutasSitp.gdb"),layer = "Ruta_Urbana",stringsAsFactors = FALSE) %>% st_transform(4326) 
    
#Procesamiento Capa Malla Vial----
    
  #Se hace un Join espacial entre malla vial y zats
    
    malla_vial<-malla_vial %>% st_join(select(zats,SCaNombre),left = FALSE, largest=TRUE)  %>% mutate(ID=row_number())%>% rename(c(SCaNombre="ZAT"))
    
  #Se dividen cada segmento en uno o mas segmentos
    
    malla_vial <-tibble(gsection(malla_vial)) %>%st_as_sf() %>% st_join(malla_vial,largest=TRUE) %>% mutate(ID=row_number()) 
    
  #Se hace un Join espacial entre union_malla_vial y calzadas
    
    malla_vial<-malla_vial %>% st_join(select(calzadas,CalAncho, CalNCarril),left = TRUE, largest=TRUE)   %>% rename(c(CalAncho="Ancho", CalNCarril="Carriles"))
    
  #Se obtienen las coordenadas (Lat/Long) de punto inicial de cada segmento
    
    coordenadas_incio_seg<-st_coordinates(malla_vial) %>% as_tibble()%>% group_by(L1)%>% filter(row_number()==1) %>% 
      ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud_Inicio",Y="Latitud_Inicio"))
    
    coordenadas_fin_seg<-st_coordinates(malla_vial) %>% as_tibble() %>% group_by(L1) %>% filter(row_number()==n()) %>% 
      ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud_Fin",Y="Latitud_Fin"))
    
  #Se hace join con la capa union_malla_vial
    
    malla_vial<-malla_vial  %>% left_join(coordenadas_incio_seg, By=ID) %>% left_join(coordenadas_fin_seg, By=ID)
    
  #Se eliminan los datos que no se usaran
    
    rm(coordenadas_incio_seg,coordenadas_fin_seg)
    
  #Se guarda la Data de malla_vial (Input consultas Google API)
    
    save(malla_vial,file=paste0(ruta_resultados,"Input_Consultas_Google_API.Rdata"))
    
#Consultas Google API---- 
    
  #Se ejecuta el R.Script Consultas Google API
    
    source(paste0(ruta_scripts,"Consultas Google API"))
    