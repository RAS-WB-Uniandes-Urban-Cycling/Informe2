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
    ruta_resultados <-"/Users/alejandropalacio/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/RESULTADOS/LTS/Bases de Datos/"
    ruta_scripts<-"/Users/alejandropalacio/Documents/GitHub/proBikePolicies/LTS/R.Scripts/"
    
  #Se almacena los layer en cada variable definida
    
    layer_malla_vial<-st_read(paste0(ruta_base_datos,"OpenStreetMaps/Malla Vial Bogotá/MVI.shp"), stringsAsFactors = FALSE)%>% filter(!(highway %in% c("cycleway","service","steps","footway","bus_stop","pedestrian","path","bridleway","proposed","raceway","construction","track"))) %>% st_transform(4326) 
    layer_ciclo_rutas<-st_read(paste0(ruta_base_datos,"Mapas de Referencia IDECA/MR0318.gdb"),layer = "RBic",stringsAsFactors = FALSE) %>% st_transform(4326) 
    layer_zats<-st_read(paste0(ruta_base_datos,"Mapas de Referencia IDECA/MR0318.gdb"),layer = "SCAT",stringsAsFactors = FALSE) %>% st_transform(4326)
    layer_calzadas<-st_read(paste0(ruta_base_datos,"Mapas de Referencia IDECA/MR0318.gdb"),layer = "Calz",stringsAsFactors = FALSE,promote_to_multi = FALSE) %>% st_transform(4326) %>% filter(st_is_valid(.))
    layer_ruta_urbana <- st_read(paste0(ruta_base_datos,"Rutas SITP.gdb"),layer = "Ruta_Urbana",stringsAsFactors = FALSE) %>% st_transform(4326) %>% st_cast( 'LINESTRING')  %>% transmute(Tipo="SITP")
    layer_ruta_alimentadora <- st_read(paste0(ruta_base_datos,"Rutas SITP.gdb"),layer = "Ruta_Alimentadora",stringsAsFactors = FALSE) %>% st_transform(4326) %>% st_cast('LINESTRING')  %>% transmute(Tipo="Alimentador")
    layer_ruta_complementaria<- st_read(paste0(ruta_base_datos,"Rutas SITP.gdb"),layer = "Ruta_Complementaria",stringsAsFactors = FALSE) %>% st_transform(4326) %>% st_cast('LINESTRING')  %>% transmute(Tipo="Complementaria")
    layer_ruta_especial<- st_read(paste0(ruta_base_datos,"Rutas SITP.gdb"),layer = "Ruta_Especial",stringsAsFactors = FALSE) %>% st_transform(4326) %>% st_cast('LINESTRING')   %>% transmute(Tipo="Especial")
    layer_localidad<-st_read(paste0(ruta_base_datos,"Mapas de Referencia IDECA/MR0318.gdb"),layer = "Loca",stringsAsFactors = FALSE) %>% st_transform(4326)
    
    
#Procesamiento Capa Malla Vial----
    
  #Se hace un Join espacial entre shape_malla_vial y shape_zats
    
    capa_malla_vial<-layer_malla_vial %>% st_join(select(layer_zats,SCaNombre),left = FALSE, largest=TRUE)  %>% mutate(ID=row_number())%>% rename(c(SCaNombre="SCatastral"))
    
  #Se completan los datos faltantes de numero de carriles.
    
    capa_malla_vial$lanes <- as.numeric(capa_malla_vial$lanes)
    capa_malla_vial$lanes <- ifelse(is.na(capa_malla_vial$lanes) & capa_malla_vial$highway=="residential", round(mean(as.numeric(capa_malla_vial$lanes)[capa_malla_vial$highway=="residential"],na.rm = TRUE),0) ,capa_malla_vial$lanes)
    capa_malla_vial$lanes <- ifelse(is.na(capa_malla_vial$lanes) & capa_malla_vial$highway=="primary", round(mean(as.numeric(capa_malla_vial$lanes)[capa_malla_vial$highway=="primary"],na.rm = TRUE),0) ,capa_malla_vial$lanes)
    capa_malla_vial$lanes <- ifelse(is.na(capa_malla_vial$lanes) & capa_malla_vial$highway=="primary_link", round(mean(as.numeric(capa_malla_vial$lanes)[capa_malla_vial$highway=="primary_link"],na.rm = TRUE),0) ,capa_malla_vial$lanes)
    capa_malla_vial$lanes <- ifelse(is.na(capa_malla_vial$lanes) & capa_malla_vial$highway=="secondary", round(mean(as.numeric(capa_malla_vial$lanes)[capa_malla_vial$highway=="secondary"],na.rm = TRUE),0) ,capa_malla_vial$lanes)
    capa_malla_vial$lanes <- ifelse(is.na(capa_malla_vial$lanes) & capa_malla_vial$highway=="secondary_link", round(mean(as.numeric(capa_malla_vial$lanes)[capa_malla_vial$highway=="secondary_link"],na.rm = TRUE),0) ,capa_malla_vial$lanes)
    capa_malla_vial$lanes <- ifelse(is.na(capa_malla_vial$lanes) & capa_malla_vial$highway=="tertiary", round(mean(as.numeric(capa_malla_vial$lanes)[capa_malla_vial$highway=="tertiary"],na.rm = TRUE),0) ,capa_malla_vial$lanes)
    capa_malla_vial$lanes <- ifelse(is.na(capa_malla_vial$lanes) & capa_malla_vial$highway=="tertiary_link", round(mean(as.numeric(capa_malla_vial$lanes)[capa_malla_vial$highway=="tertiary_link"],na.rm = TRUE),0) ,capa_malla_vial$lanes)
    capa_malla_vial$lanes <- ifelse(is.na(capa_malla_vial$lanes) & capa_malla_vial$highway=="road", round(mean(as.numeric(capa_malla_vial$lanes)[capa_malla_vial$highway=="road"],na.rm = TRUE),0) ,capa_malla_vial$lanes)
    capa_malla_vial$lanes <- ifelse(is.na(capa_malla_vial$lanes) & capa_malla_vial$highway=="trunk", round(mean(as.numeric(capa_malla_vial$lanes)[capa_malla_vial$highway=="trunk"],na.rm = TRUE),0) ,capa_malla_vial$lanes)
    capa_malla_vial$lanes <- ifelse(is.na(capa_malla_vial$lanes) & capa_malla_vial$highway=="trunk_link", round(mean(as.numeric(capa_malla_vial$lanes)[capa_malla_vial$highway=="trunk_link"],na.rm = TRUE),0) ,capa_malla_vial$lanes)
    capa_malla_vial$lanes <- ifelse(is.na(capa_malla_vial$lanes) & capa_malla_vial$highway=="unclassified", round(mean(as.numeric(capa_malla_vial$lanes)[capa_malla_vial$highway=="unclassified"],na.rm = TRUE),0) ,capa_malla_vial$lanes)
    capa_malla_vial$lanes <- ifelse(is.na(capa_malla_vial$lanes) & capa_malla_vial$highway=="living_street", round(mean(as.numeric(capa_malla_vial$lanes)[capa_malla_vial$highway=="living_street"],na.rm = TRUE),0) ,capa_malla_vial$lanes)
    
  #Se dividen cada segmento en uno o mas segmentos
    
    capa_malla_vial <-tibble(gsection(capa_malla_vial))%>%st_as_sf() %>% st_join(capa_malla_vial,largest=TRUE) %>% mutate(ID=row_number()) 
    
  #Se hace un Join espacial entre capa_malla_vial y shape_calzadas
    
    capa_malla_vial<-capa_malla_vial %>% st_join(select(layer_calzadas,CalAncho),left = TRUE, largest=TRUE)   %>% rename(c(CalAncho="Ancho"))
    
  #Se obtienen las coordenadas (Lat/Long) de punto inicial de cada segmento la capa_malla_vial
    
    coordenadas_incio_seg<-st_coordinates(capa_malla_vial) %>% as_tibble()%>% group_by(L1)%>% filter(row_number()==1) %>% 
      ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud_Inicio",Y="Latitud_Inicio"))
    
    coordenadas_fin_seg<-st_coordinates(capa_malla_vial) %>% as_tibble() %>% group_by(L1) %>% filter(row_number()==n()) %>% 
      ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud_Fin",Y="Latitud_Fin"))
    
  #Se hace join entre la capa_malla_vial y las coordenadas
    
    capa_malla_vial<-capa_malla_vial  %>% left_join(coordenadas_incio_seg, By=ID) %>% left_join(coordenadas_fin_seg, By=ID)
    
  #Se eliminan los datos que no se usaran
    
    rm(coordenadas_incio_seg,coordenadas_fin_seg)

  #Se guarda la Data de capa_malla_vial (Input consultas Google API)
    
    #save(capa_malla_vial,file=paste0(ruta_resultados,"Input_Consultas_Google_API.Rdata"))
    
#Consultas Google API---- 
    
  #Se carga la Data
    
    load(paste0(ruta_resultados, "3-Datos_Procesados_Consultas_Google_API.Rdata"))
  
  #Se unen los datos procesados de Google API con la capa malla vial
    
    capa_malla_vial<- capa_malla_vial %>% filter(highway!="corridor") %>%  transmute(ID,Ancho, lanes)  %>% left_join(datos_google_API ,By=ID)  %>% filter(Longitud1 !=0)  %>% filter(Longitud2 !=0) 

#Segmentación capa_malla_vial----      
    
  #Se divide cada segmento deacuerdo a su vertices
    
    capa_malla_vial<- capa_malla_vial %>% st_split(st_combine(st_cast(capa_malla_vial, 'POINT') )) %>% st_collection_extract( type = c("LINESTRING")) 
  
  #Se hace un Join espacial entre capa_malla_vial y shape_calzadas
    
    capa_malla_vial<-capa_malla_vial %>% st_join(select(layer_localidad,LocNombre),left = FALSE, largest=TRUE)  %>% st_join(select(layer_calzadas,CalAncho),left = TRUE, largest=TRUE)  %>% 
      transmute(ID=row_number(),Ancho=if_else(!is.na(CalAncho),as.numeric(CalAncho),as.numeric(Ancho)),
      Carriles=lanes,LocNombre,Longitud1,TProm1,TFF1,Vprom1, VpromFF1,Longitud2,TProm2,TFF2,Vprom2, VpromFF2)
    
  #Se corrigen los datos de ancho de la via
    
    capa_malla_vial$Ancho<-ifelse(capa_malla_vial$Ancho>=(capa_malla_vial$Carriles*1.8) & capa_malla_vial$Ancho<=(capa_malla_vial$Carriles*7),capa_malla_vial$Ancho,NA)
    capa_malla_vial$Ancho<-ifelse(is.na(capa_malla_vial$Ancho) & capa_malla_vial$Carriles==1 , mean(capa_malla_vial$Ancho[capa_malla_vial$Carriles==1], na.rm = TRUE), capa_malla_vial$Ancho)
    capa_malla_vial$Ancho<-ifelse(is.na(capa_malla_vial$Ancho) & capa_malla_vial$Carriles==2 , mean(capa_malla_vial$Ancho[capa_malla_vial$Carriles==2], na.rm = TRUE), capa_malla_vial$Ancho)
    capa_malla_vial$Ancho<-ifelse(is.na(capa_malla_vial$Ancho) & capa_malla_vial$Carriles==3 , mean(capa_malla_vial$Ancho[capa_malla_vial$Carriles==3], na.rm = TRUE), capa_malla_vial$Ancho)
    capa_malla_vial$Ancho<-ifelse(is.na(capa_malla_vial$Ancho) & capa_malla_vial$Carriles==4 , mean(capa_malla_vial$Ancho[capa_malla_vial$Carriles==4], na.rm = TRUE), capa_malla_vial$Ancho)
    capa_malla_vial$Ancho<-ifelse(is.na(capa_malla_vial$Ancho) & capa_malla_vial$Carriles==5 , mean(capa_malla_vial$Ancho[capa_malla_vial$Carriles==5], na.rm = TRUE), capa_malla_vial$Ancho)
    capa_malla_vial <- capa_malla_vial %>% filter(!is.na(Ancho))  %>%  mutate(ID=row_number())
    
  #Se obtienen las coordenadas (Lat/Long) de punto inicial de cada segmento de la capa_malla_vial
    
    coordenadas_incio_seg<-st_coordinates(capa_malla_vial) %>% as_tibble()%>% group_by(L1)%>% filter(row_number()==1) %>% 
      ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud_Inicio",Y="Latitud_Inicio"))
    
    coordenadas_fin_seg<-st_coordinates(capa_malla_vial) %>% as_tibble() %>% group_by(L1) %>% filter(row_number()==n()) %>% 
      ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud_Fin",Y="Latitud_Fin"))
    
  #Se hace join entre la capa_malla_vial y las coordenadas
    
    capa_malla_vial<-capa_malla_vial  %>% left_join(coordenadas_incio_seg, By=ID)  %>% left_join(coordenadas_fin_seg, By=ID)
    
  #Se eliminan los datos que no se usaran
    
    rm(coordenadas_incio_seg,coordenadas_fin_seg)
    

#Procesamiento Red Ciclorutas----
    
  #Se hace un Join espacial entre shape_ciclo_rutas y shape_zats
    
    capa_ciclo_rutas<-layer_ciclo_rutas %>% st_cast('LINESTRING', do_split=TRUE) %>% mutate(ID=row_number())
    
  #Se obtienen las coordenadas (Lat/Long) de punto inicial de cada segmentos
    
    coordenadas_incio_seg<-st_coordinates(capa_ciclo_rutas) %>% as_tibble() %>% group_by(L1) %>% filter(row_number()==1) %>% 
      ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud_Inicio_C",Y="Latitud_Inicio_C"))
    
    coordenadas_fin_seg<-st_coordinates(capa_ciclo_rutas) %>% as_tibble() %>% group_by(L1) %>% filter(row_number()==n()) %>% 
      ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud_Fin_C",Y="Latitud_Fin_C"))
    
  #Se hace join con la capa_ciclo_rutas y las coordenadas
    
    capa_ciclo_rutas<-capa_ciclo_rutas  %>% left_join(coordenadas_incio_seg , By=ID) %>% left_join(coordenadas_fin_seg, By=ID)
    
  #Se hace un buffer de x metros de anchos a la capa_ciclo_rutas
    
    buffer_ciclo_rutas<-st_buffer(capa_ciclo_rutas,dist = 0.000200)
    
  #Se hace un Join espacial entre capa_malla_vial y el buffer_ciclo_rutas
    
    union_malla_vial_ciclo_rutas<-capa_malla_vial %>% st_join(select(buffer_ciclo_rutas,RBiClase,"Latitud_Inicio_C","Longitud_Inicio_C","Latitud_Fin_C","Longitud_Fin_C")) 
    
  #Se calcula el la pendiente de cada segmento (capa_malla_vial y capa_ciclo_rutas), se calcula el angulo que forman dichos segmentos y se eliminan los segmentos cuyo angulo es mayor a 20 grados 
    
    union_malla_vial_ciclo_rutas <-union_malla_vial_ciclo_rutas %>% mutate(p1=((Longitud_Fin-Longitud_Inicio)/(Latitud_Fin-Latitud_Inicio)),p2=((Longitud_Fin_C-Longitud_Inicio_C)/(Latitud_Fin_C-Latitud_Inicio_C)))%>%
      mutate(angulo=NISTradianTOdeg(atan2(p2-p1,1+p2*p1))) %>% filter(abs(angulo)<=25)
    
  #Se ordenan los angulos de menor a mayor y se eligen el primer registro de cada segmento
    
    union_malla_vial_ciclo_rutas<-union_malla_vial_ciclo_rutas[order(union_malla_vial_ciclo_rutas$angulo),] %>% group_by(ID) %>% filter(row_number()==1) %>% 
      ungroup() %>% mutate(CicloRuta=1)
    
  #Se hace un left-join entre union_malla_vial y union_malla_vial_ciclo_rutas
    
    capa_malla_vial<-capa_malla_vial %>% left_join(select(st_set_geometry(union_malla_vial_ciclo_rutas,NULL), CicloRuta, RBiClase,ID)) %>% rename(c(RBiClase="Segregada"))
    
  #A los registros que en el campo ciclo ruta tenga valor NULL, se le asigna el valor de 0
    
    capa_malla_vial$CicloRuta<-ifelse(is.na(capa_malla_vial$CicloRuta),0,capa_malla_vial$CicloRuta)
    
  #Se define la variable segregación
    
    capa_malla_vial$Segregada<-ifelse(is.na(capa_malla_vial$Segregada),0,ifelse(capa_malla_vial$Segregada %in% c(1,2,3,6,9,10,12),1,0))  
    
    
  #Se eliminan los datos que no se usaran

    rm(coordenadas_incio_seg,coordenadas_fin_seg,buffer_ciclo_rutas,union_malla_vial_ciclo_rutas)
    
#Procesamiento Rutas SITP----
    
  #Se comvierte de MultiLinesting a Lingstring
    
    #rutas_transporte <- rbind(layer_ruta_urbana,layer_ruta_alimentadora, layer_ruta_complementaria, layer_ruta_especial)
  
  #Se divide cada ruta del SITP deacuerdo a su vertices
    
    #capa_SITP<-rutas_transporte %>% st_split(st_combine(st_cast(rutas_transporte, 'POINT')))%>% st_collection_extract( type = c("LINESTRING")) 
    
  #Se guarda la Data de capa_SITP (RutaSITP Segmentada)
    
    #save(capa_SITP,file=paste0(ruta_resultados,"4-RutaSITP_Segmentada.Rdata"))
  
  #Se carga la Data de capa_SITP (RutaSITP Segmentada)
    
    load(paste0(ruta_resultados,"4-RutaSITP_Segmentada.Rdata"))
    
  #Se hace un Join espacial entre malla vial y zats
    
    capa_SITP<-capa_SITP %>%  mutate(ID=row_number())
    
  #Se obtienen las coordenadas (Lat/Long) de punto inicial de cada segmentos
    
    coordenadas_incio_seg<-st_coordinates(capa_SITP) %>% as_tibble() %>% group_by(L1) %>% filter(row_number()==1) %>% 
      ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud_Inicio_S",Y="Latitud_Inicio_S"))
    
    coordenadas_fin_seg<-st_coordinates(capa_SITP) %>% as_tibble() %>% group_by(L1) %>% filter(row_number()==n()) %>% 
      ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud_Fin_S",Y="Latitud_Fin_S"))
    
  #Se hace join con la capa union_ciclo_rutas y las coordenadas
    
    capa_SITP<-capa_SITP  %>% left_join(coordenadas_incio_seg, By=ID)   %>% left_join(coordenadas_fin_seg, By=ID)  
    
    
  #Se hace un Join espacial entre union_malla_vial y segmentos_resultantes
    
    buffer_SITP<-st_buffer(capa_SITP,dist = 0.00010)  
    
  #Se hace un join espacial entre capa_malla_vial y el buffer_ciclo_rutas
    
    union_malla_vial_sitp<-capa_malla_vial %>% st_join(select(buffer_SITP,"Latitud_Inicio_S","Longitud_Inicio_S","Latitud_Fin_S","Longitud_Fin_S")) 
    
  #Se calcula el la pendiente de cada segmento (capa_malla_vial y capa_SITP), se calcula el angulo que forman dichos segmentos y se eliminan los segmentos cuyo angulo es mayor a 20 grados 
    
    union_malla_vial_sitp <-union_malla_vial_sitp %>% mutate(p1=((Longitud_Fin-Longitud_Inicio)/(Latitud_Fin-Latitud_Inicio)),p2=((Longitud_Fin_S-Longitud_Inicio_S)/(Latitud_Fin_S-Latitud_Inicio_S)))%>%
      mutate(angulo=NISTradianTOdeg(atan2(p2-p1,1+p2*p1))) %>% filter( abs(angulo)<=25 )
    
  #Se ordenan los angulos de menor a mayor y se eligen el primer registro de cada segmento
    
    union_malla_vial_sitp<-union_malla_vial_sitp[order(union_malla_vial_sitp$angulo),] %>% group_by(ID) %>% filter(row_number()==1) %>% 
      ungroup() %>% mutate(SITP=1)
    
  #Se hace un left-join entre capa_malla_vial y union_malla_vial_sitp
    
    capa_malla_vial<-capa_malla_vial %>% left_join(select( st_set_geometry(union_malla_vial_sitp,NULL), SITP, ID))
    
  #A los registros que en el campo ciclo ruta tenga valor NULL, se le asigna el valor de 0
    
    capa_malla_vial$SITP<-ifelse(is.na(capa_malla_vial$SITP),0,capa_malla_vial$SITP)
    
  #Se eliminan los datos que no se usaran
    
    rm(coordenadas_incio_seg,coordenadas_fin_seg,buffer_SITP,union_malla_vial_sitp)
    

#Calculo indices Trafico----
   
  #Se calcula la congestion  
    
    capa_malla_vial <- capa_malla_vial %>% mutate(Velocidad=ifelse(Longitud1==Longitud2,pmax(Vprom1,Vprom2),ifelse(Longitud1<Longitud2, Vprom1, Vprom2)))
    
  #Se calcula la congestion  
     
    capa_malla_vial <- capa_malla_vial %>% mutate(Congestion=ifelse(Longitud1==Longitud2,pmax((TProm1-TFF1)/TFF1,(TProm2-TFF2)/TFF2),ifelse(Longitud1<Longitud2, (TProm1-TFF1)/TFF1,(TProm2-TFF2)/TFF2)))
    capa_malla_vial$Congestion <- ifelse(capa_malla_vial$Congestion<0,0,capa_malla_vial$Congestion)
    capa_malla_vial$Congestion <- ifelse(capa_malla_vial$Congestion>1,1,capa_malla_vial$Congestion)
    
  #Se calcula la densidad  
    
    capa_malla_vial <- capa_malla_vial %>% mutate(Densidad=ifelse(Longitud1==Longitud2,pmax(199.6566*sqrt(2*log(VpromFF1/Vprom1)),199.6566*sqrt(2*log(VpromFF2/Vprom2))),ifelse(Longitud1<Longitud2, 199.6566*sqrt(2*log(VpromFF1/Vprom1)),199.6566*sqrt(2*log(VpromFF2/Vprom2)))))
    capa_malla_vial$Densidad <- ifelse(is.na(capa_malla_vial$Densidad),0,capa_malla_vial$Densidad)
    
  #Se calcula flujo vehicular  
    
    capa_malla_vial <- capa_malla_vial %>% mutate(Flujo=Velocidad*Densidad)

#Dirección de la via----
    
  #Se identifica la direccion de la via
    
    capa_malla_vial <- capa_malla_vial %>%  mutate(Direccion=ifelse(Longitud1==Longitud2,1,0))
        
#Capa LTS----
  
  #Se crea la capa LTS
    
    capa_variables_LTS<-capa_malla_vial %>% transmute (ID=row_number(),LocNombre,Ancho,Carriles,CicloRuta,SITP,Velocidad,Congestion,Densidad,Flujo,Segregada,Direccion)
    
  #Se guarda la Data de capa_variables_LTS (Variables LTS)

    save(capa_variables_LTS,file=paste0(ruta_resultados,"5-Variables_LTS.Rdata"))
    
    