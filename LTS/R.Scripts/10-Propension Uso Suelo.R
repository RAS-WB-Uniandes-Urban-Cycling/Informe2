#Preparación del entorno de trabajo----

  #Se cargan las siguiente librerias
    
    library(sf)
    library(tidyverse)
    library(lwgeom)
  
  #Se define las ruta de las bases de datos

    ruta_base_datos<-"/Users/alejandropalacio/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/BASES DE DATOS/"
    ruta_resultados<-"/Users/alejandropalacio/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/RESULTADOS/PROPENSION/GEO-DATA/"
    
  #Se cargan las siguientes layers
    
    cat_usos <- read_csv(paste0(ruta_base_datos,"Mapas de Referencia IDECA/UsosLote.csv"),locale = locale(encoding = stringi::stri_enc_get()))
    usos_lote <- read_sf(paste0(ruta_base_datos,"Mapas de Referencia IDECA/MR0318.gdb"),layer = "Uso",stringsAsFactors = FALSE)
    capa_lote <- read_sf(paste0(ruta_base_datos,"Mapas de Referencia IDECA/MR0318.gdb"),layer = "Lote",type = 3,stringsAsFactors = FALSE) %>% st_transform(4326) %>% 
      inner_join(usos_lote,by=c("LotCodigo"="UsoCLote")) %>% left_join(cat_usos) %>% select(UsoArea,Cat) %>% filter(!is.na(Cat))

    
  #Se carga la R.data de los poligonos origen y destino 
    
    load(paste0(ruta_resultados,"Shiny.RData"))
    
    t <- Sys.time()
    entropy_destino_7 <- capa_lote %>% st_join(buffer_destino[12001:14000,]) %>% group_by(id_encuesta,numero_persona,numero_viaje,Cat) %>% 
      st_set_geometry(NULL) %>% summarise_all(sum) %>% group_by(id_encuesta,numero_persona,numero_viaje) %>% 
      mutate(Prop=UsoArea/sum(UsoArea),entropyIndex=-Prop*log(Prop)/log(n())) %>% summarise_at("entropyIndex",sum)
    Sys.time()-t
    
    t <- Sys.time()
    entropy_destino_8 <- capa_lote %>% st_join(buffer_destino[14001:16000,]) %>% group_by(id_encuesta,numero_persona,numero_viaje,Cat) %>% 
      st_set_geometry(NULL) %>% summarise_all(sum) %>% group_by(id_encuesta,numero_persona,numero_viaje) %>% 
      mutate(Prop=UsoArea/sum(UsoArea),entropyIndex=-Prop*log(Prop)/log(n())) %>% summarise_at("entropyIndex",sum)
    Sys.time()-t
    
    t <- Sys.time()
    entropy_destino_9 <- capa_lote %>% st_join(buffer_destino[16001:18000,]) %>% group_by(id_encuesta,numero_persona,numero_viaje,Cat) %>% 
      st_set_geometry(NULL) %>% summarise_all(sum) %>% group_by(id_encuesta,numero_persona,numero_viaje) %>% 
      mutate(Prop=UsoArea/sum(UsoArea),entropyIndex=-Prop*log(Prop)/log(n())) %>% summarise_at("entropyIndex",sum)
    Sys.time()-t
    
    t <- Sys.time()
    entropy_destino_10 <- capa_lote %>% st_join(buffer_destino[18001:20000,]) %>% group_by(id_encuesta,numero_persona,numero_viaje,Cat) %>% 
      st_set_geometry(NULL) %>% summarise_all(sum) %>% group_by(id_encuesta,numero_persona,numero_viaje) %>% 
      mutate(Prop=UsoArea/sum(UsoArea),entropyIndex=-Prop*log(Prop)/log(n())) %>% summarise_at("entropyIndex",sum)
    Sys.time()-t
    
    t <- Sys.time()
    entropy_destino_11 <- capa_lote %>% st_join(buffer_destino[20001:20981,]) %>% group_by(id_encuesta,numero_persona,numero_viaje,Cat) %>% 
      st_set_geometry(NULL) %>% summarise_all(sum) %>% group_by(id_encuesta,numero_persona,numero_viaje) %>% 
      mutate(Prop=UsoArea/sum(UsoArea),entropyIndex=-Prop*log(Prop)/log(n())) %>% summarise_at("entropyIndex",sum)
    Sys.time()-t
    
    save(entropy_origen_1,entropy_origen_2,entropy_origen_3,entropy_origen_4,entropy_origen_5,entropy_origen_6,entropy_origen_7,entropy_origen_8,entropy_origen_9,entropy_origen_10,entropy_origen_11,file=paste0(ruta_resultados,"Entropy_origen.Rdata"))
    save(entropy_destino_1,entropy_destino_2,entropy_destino_3,entropy_destino_4,entropy_destino_5,entropy_destino_6,entropy_destino_7,entropy_destino_8,entropy_destino_9,entropy_destino_10,entropy_destino_11,file=paste0(ruta_resultados,"Entropy_destino.Rdata"))
    
    
    t <- Sys.time()
    entropy_destino_1 <- capa_lote %>% st_join(buffer_origen[1:2000,]) %>%
      filter(!is.na(UsoTUso)) %>% group_by(id_encuesta,numero_persona,numero_viaje,Cat) %>% 
      st_set_geometry(NULL) %>% summarise_all(sum) %>% group_by(id_encuesta,numero_persona,numero_viaje) %>% 
      mutate(Prop=UsoArea/sum(UsoArea),entropyIndex=-Prop*log(Prop)/log(n())) %>% summarise_at("entropyIndex",sum)
    Sys.time()-t