#Preparación del entorno de trabajo----

  #Se cargan las siguiente librerias

    library(sf)
    library(tidyverse)
    library(lwgeom)

  #Se define las ruta de las bases de datos

    ruta_base_datos<-"/Users/alejandropalacio/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/BASES DE DATOS/"
    ruta_resultados<-"/Users/alejandropalacio/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/RESULTADOS/PROPENSION/GEO-DATA/"
  
  #Se almacena los layer en cada variable definida
    
    layer_localidad<-st_read(paste0(ruta_base_datos,"Mapas de Referencia IDECA/MR0318.gdb"),layer = "Loca",stringsAsFactors = FALSE) %>% st_transform(4326)
    
  #Se carga la Data
    
    load(paste0(ruta_resultados,"Shiny.RData"))
    load(paste0(ruta_resultados,"Variables.RData"))
    
#Se procesan las capas para Shiny APP
    
    capa_Origen <- buffer_origen %>% left_join(Environmental_O) %>% left_join(select(Individual,id_encuesta, numero_persona, numero_viaje, BicycleCommuting)) %>% filter(BicycleCommuting==TRUE)
    capa_Destino <- buffer_destino %>% left_join(Environmental_D)
    capa_Ruta <- buffer_ruta %>% left_join(Environmental_R)
    capa_Camino <-CR 

    capa_Origen<- capa_Origen %>% st_join(select(layer_localidad,LocNombre),left = FALSE, largest=TRUE) 
    
    capa_Destino <- capa_Destino %>% left_join(select(st_set_geometry(capa_Origen,NULL), id_encuesta, numero_persona, numero_viaje, LocNombre, BicycleCommuting)) %>% filter(BicycleCommuting==TRUE)
    capa_Ruta <- capa_Ruta %>% left_join(select(st_set_geometry(capa_Origen,NULL), id_encuesta, numero_persona, numero_viaje, LocNombre, BicycleCommuting)) %>% filter(BicycleCommuting==TRUE)
    capa_Camino <- capa_Camino %>% left_join(select(st_set_geometry(capa_Origen,NULL), id_encuesta, numero_persona, numero_viaje, LocNombre, BicycleCommuting)) %>% filter(BicycleCommuting==TRUE)
    
    
    saveRDS(capa_Origen,"Capa_Origen_Propension.rds")
    saveRDS(capa_Destino,"Capa_Destino_Propension.rds")
    saveRDS(capa_Ruta,"Capa_Ruta_Propension.rds")
    saveRDS(capa_Camino,"Capa_Camino_Propension.rds")
    
    plot(head(capa_Camino$geometry,1))
    