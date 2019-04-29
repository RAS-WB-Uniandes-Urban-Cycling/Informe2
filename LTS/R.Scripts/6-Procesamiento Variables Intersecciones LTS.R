#Preparación del entorno de trabajo----

  #Se cargan las siguiente librerias

    library(sf)
    library(tidyverse)
    library(reshape)
    library(NISTunits)
    library(tmap)
    library(tmaptools)
    library(lwgeom)

  #Se define las ruta de las bases de datos

    ruta_base_datos<-"/Users/alejandropalacio/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/BASES DE DATOS/"
    ruta_resultados <-"/Users/alejandropalacio/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/RESULTADOS/LTS/Bases de Datos/"

  #Se almacena los layer en cada variable definida  
    
    layer_localidad<-st_read(paste0(ruta_base_datos,"Mapas de Referencia IDECA/MR0318.gdb"),layer = "Loca",stringsAsFactors = FALSE) %>% st_transform(4326)
  
  #Se carga la Data

    load(paste0(ruta_resultados, "8-Capa_Predicción_LTS_Logit.Rdata"))
   
#Procesamiento Capa Malla Vial----
  
  #Se calcula a que cluster pertenece con base en la prediccion
    
    capa_variables_LTS <- Capa_Variables_Prediccion %>% mutate(Cluster=ifelse(X4>=0.5,1,ifelse(X3>=0.5,2,ifelse(X2>=0.5,3,4))))
    
  #Se obtienen las coordenadas (Lat/Long) de punto inicial de cada segmento la capa_malla_vial
    
    coordenadas_incio_seg<-st_coordinates(capa_variables_LTS) %>% as_tibble()%>% group_by(L1)%>% filter(row_number()==1) %>% 
      ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud",Y="Latitud"))
    
    coordenadas_fin_seg<-st_coordinates(capa_variables_LTS) %>% as_tibble() %>% group_by(L1) %>% filter(row_number()==n()) %>% 
      ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud",Y="Latitud"))
    
  #Se unen las dos trablas de coordenadas
    
    capa_Provisional_1<-capa_variables_LTS %>% transmute(ID,Cluster)  %>% left_join(coordenadas_incio_seg, By=ID)
    capa_Provisional_2<-capa_variables_LTS %>% transmute(ID,Cluster)  %>% left_join(coordenadas_fin_seg, By=ID)
    
    coordenadas_malla_vial <- capa_Provisional_1%>% rbind(capa_Provisional_2) %>% st_set_geometry(NULL)
    
  #Se cuentan y agrupan los repetidos
    
    intersecciones_malla_vial <- coordenadas_malla_vial %>% group_by(Latitud, Longitud) %>% summarise(Numero_Intersecciones=n(),Cluster=max(Cluster)) %>% filter(Numero_Intersecciones>=3) %>% ungroup()

  #Se genera la capa de intersecciones
    
    intersecciones_malla_vial<- intersecciones_malla_vial  %>% st_as_sf(coords=c(2,1)) %>% mutate(ID_Punto=row_number()) %>% st_set_crs(4326) 

  #Se hace un join espacial para determinar la localidad
    
    intersecciones_malla_vial <- intersecciones_malla_vial %>% st_join(select(layer_localidad,LocNombre))

  #Se guarda la capa
    
    save(intersecciones_malla_vial,file=paste0(ruta_resultados,"9-Intersecciones.Rdata"))
    
    saveRDS(intersecciones_malla_vial,"Intersecciones.rds")
   