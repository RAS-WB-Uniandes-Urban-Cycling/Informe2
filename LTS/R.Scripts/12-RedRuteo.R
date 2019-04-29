#Preparación del entorno de trabajo----

  #Se cargan las siguiente librerias

    library(sf)
    library(tidyverse)
    library(reshape)
    library(NISTunits)
    library(tmap)
    library(tmaptools)
    library(lwgeom)
    library(leaflet)
    library(nngeo)
    library(readxl)
    library(nnet)
    library(reshape2)
    library(ggplot2)  
    library(tidyverse)
    library(mlogit)
    library(Hmisc)

  #Ruta

  setwd("/Users/alejandropalacio/Documents/GitHub/proBikePolicies/LTS/Aplicacion/Data")


  #Se define las ruta de las bases de datos

    ruta_base_datos<-"/Users/alejandropalacio/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/BASES DE DATOS/"
    ruta_resultados <-"/Users/alejandropalacio/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/RESULTADOS/LTS/Bases de Datos/"

    
  #Se almacena los layer en cada variable definida  
    
    layer_localidad<-st_read(paste0(ruta_base_datos,"Mapas de Referencia IDECA/MR0318.gdb"),layer = "Loca",stringsAsFactors = FALSE) %>% st_transform(4326) 
    layer_ZATs<-st_read(paste0(ruta_base_datos,"ZATs/ZATs_2012_MAG.shp"), stringsAsFactors = FALSE) %>% st_transform(4326) 
    
  #Se carga la Data

    load(paste0(ruta_resultados, "8-Capa_Predicción_LTS_Logit.Rdata"))
    Matriz_OD <- read_excel("~/Desktop/Proyecto Opti-BajoIncertidumbre/Matriz OD.xlsx")
    capa_LTS <- readRDS("./LTS_Bogota.rds")
   
#Procesamiento Capa Malla Vial----
  
  #Se calcula a que cluster pertenece con base en la prediccion
    
    capa_variables_LTS <- capa_LTS %>% mutate(LTS=ifelse(X4>=0.5,1,ifelse(X3>=0.5,2,ifelse(X2>=0.5,3,4)))) %>% filter((LocNombre %in% c("USAQUEN","CHAPINERO","SUBA","ENGATIVA","BARRIOS UNIDOS","TEUSAQUILLO"))) %>% mutate(ID=row_number()) %>% mutate(S=0)
    
  #Se asigna la tasa S de acuerdo al nivel de LTS
    
    capa_variables_LTS$S <- ifelse(capa_variables_LTS$LTS==1,0.01984877,ifelse(capa_variables_LTS$LTS==2,0.2070296,ifelse(capa_variables_LTS$LTS==3,0.3863431,0.6764487)))
    
  #Se obtienen las coordenadas (Lat/Long) de punto inicial de cada segmento la capa_malla_vial
    
    coordenadas_incio_seg<-st_coordinates(capa_variables_LTS) %>% as_tibble()%>% group_by(L1)%>% filter(row_number()==1) %>% 
      ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud",Y="Latitud"))
    
    coordenadas_fin_seg<-st_coordinates(capa_variables_LTS) %>% as_tibble() %>% group_by(L1) %>% filter(row_number()==n()) %>% 
      ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud",Y="Latitud"))
    
  #Se unen las dos tablas de coordenadas
    
    capa_Provisional_1<-capa_variables_LTS %>% transmute(ID,LTS)  %>% left_join(coordenadas_incio_seg, By=ID)
    capa_Provisional_2<-capa_variables_LTS %>% transmute(ID,LTS)  %>% left_join(coordenadas_fin_seg, By=ID)
    
    coordenadas_malla_vial <- capa_Provisional_1%>% rbind(capa_Provisional_2) %>% st_set_geometry(NULL)
    
  #Se cuentan y agrupan los repetidos
    
    capa_Ruteo_Nodos <- coordenadas_malla_vial %>% group_by(Latitud, Longitud) %>% summarise(Numero_Intersecciones=n(),LTS=max(LTS))  %>% ungroup() %>% mutate(ID_Nodo=row_number())

  #Se le asigna la coordenadas de inicio y fin de cada segmento
    
    coordenadas_incio_seg <- coordenadas_incio_seg %>%  rename(c(ID="ID",Longitud="LongI",Latitud="LatI"))
    coordenadas_fin_seg <- coordenadas_fin_seg %>%  rename(c(ID="ID",Longitud="LongF",Latitud="LatF"))
    
    capa_Ruteo_Arcos<-capa_variables_LTS %>% left_join(coordenadas_incio_seg, By=ID)
    capa_Ruteo_Arcos<-capa_Ruteo_Arcos %>% left_join(coordenadas_fin_seg, By=ID) %>% transmute(ID,LTS,LongI,LatI,LongF,LatF,S,Ancho,Carriles,CicloRuta,SITP,Velocidad,Congestion,Densidad,Flujo)  %>% mutate(Distancia=as.numeric(st_length(capa_Ruteo_Arcos))/1000)
    
    
  #Se hace un join espacial para determinar la localidad del ZATs
    
    Zats <- layer_ZATs %>% st_join(select(layer_localidad,LocNombre), largest = TRUE) %>% filter((LocNombre %in% c("USAQUEN","CHAPINERO","SUBA","ENGATIVA","BARRIOS UNIDOS","TEUSAQUILLO")))
    Zats <- Zats %>% filter(!(id %in% c(19, 18,40, 51, 84, 763, 927, 930, 931, 932, 933)))
    Zats_Centroides <- st_set_geometry(Zats,NULL)
    Zats_Centroides<- Zats_Centroides  %>% st_as_sf(coords=c(3,4)) %>% st_set_crs(4326)
    
    Nodos_Provisional <- capa_Ruteo_Nodos %>% mutate(Lat=Latitud, Long=Longitud) %>% st_as_sf(coords=c(2,1))%>% st_set_crs(4326) %>% transmute(Lat,Long)
    capa_Ruteo_Zats_Nodos<- Zats_Centroides %>% st_join(Nodos_Provisional, join=st_nn, k=1)
    capa_Ruteo_Zats_Nodos <- st_set_geometry(capa_Ruteo_Zats_Nodos,NULL) %>% rename(c(Lat="Latitud",Long="Longitud"))
    
  #Datos regresion logit multinomial
    
    capa_Ruteo_Arcos_Add_Cicloruta <-st_set_geometry(capa_Ruteo_Arcos,NULL)  %>% filter(CicloRuta==0) %>% mutate(CicloRuta=1) 
    capa_Ruteo_Arcos_Delete_SITP <-st_set_geometry(capa_Ruteo_Arcos,NULL)  %>% filter(SITP==1) %>% mutate(SITP=0)
    capa_Ruteo_Arcos_Add_Cicloruta_Delete_SITP <-st_set_geometry(capa_Ruteo_Arcos,NULL)  %>% filter(CicloRuta==0) %>% filter(SITP==1) %>% mutate(CicloRuta=1,SITP=0)
    
  #Regresion logitmultinomial
    
    capa_variables_LTS_model <- capa_variables_LTS  %>% transmute(Ancho,Carriles,CicloRuta,SITP,Velocidad,Congestion,Densidad,Flujo,LTS) 
    
    logit_Multi<-multinom(capa_variables_LTS_model$LTS~Velocidad+Ancho+Carriles+CicloRuta+SITP+Congestion+Densidad+Flujo, data=capa_variables_LTS_model)
    
    #Modelo predictivo:
    
    Prediccion_1=predict(logit_Multi,capa_Ruteo_Arcos_Add_Cicloruta,type="probs")
    
    capa_Ruteo_Arcos_Add_Cicloruta <- cbind(capa_Ruteo_Arcos_Add_Cicloruta,Prediccion_1)%>% mutate(LTS_1=ifelse(`1`>=0.5,1,ifelse(`2`>=0.5,2,ifelse(`3`>=0.5,3,4)))) %>% transmute(ID,LTS_1)
    
    Prediccion_2=predict(logit_Multi,capa_Ruteo_Arcos_Delete_SITP,type="probs")
    
    capa_Ruteo_Arcos_Delete_SITP <- cbind(capa_Ruteo_Arcos_Delete_SITP,Prediccion_2)%>% mutate(LTS_2=ifelse(`1`>=0.5,1,ifelse(`2`>=0.5,2,ifelse(`3`>=0.5,3,4))))%>% transmute(ID,LTS_2)
    
    Prediccion_3=predict(logit_Multi,capa_Ruteo_Arcos_Add_Cicloruta_Delete_SITP,type="probs")
    
    capa_Ruteo_Arcos_Add_Cicloruta_Delete_SITP <- cbind(capa_Ruteo_Arcos_Add_Cicloruta_Delete_SITP,Prediccion_3)%>% mutate(LTS_3=ifelse(`1`>=0.5,1,ifelse(`2`>=0.5,2,ifelse(`3`>=0.5,3,4))))%>% transmute(ID,LTS_3)
    
    #Se unen los modelos predictivos con la base.
    
    capa_Ruteo_Arcos <- capa_Ruteo_Arcos %>% left_join(capa_Ruteo_Arcos_Add_Cicloruta, By=ID) %>% mutate(S_1=0)
    capa_Ruteo_Arcos$S_1 <- ifelse(capa_Ruteo_Arcos$LTS_1==1,0.01984877,ifelse(capa_Ruteo_Arcos$LTS_1==2,0.2070296,ifelse(capa_Ruteo_Arcos$LTS_1==3,0.3863431,ifelse(capa_Ruteo_Arcos$LTS_1==4,0.6764487,0))))
    
    
    capa_Ruteo_Arcos <- capa_Ruteo_Arcos %>% left_join(capa_Ruteo_Arcos_Delete_SITP,By=ID) %>% mutate(S_2=0)
    capa_Ruteo_Arcos$S_2 <- ifelse(capa_Ruteo_Arcos$LTS_2==1,0.01984877,ifelse(capa_Ruteo_Arcos$LTS_2==2,0.2070296,ifelse(capa_Ruteo_Arcos$LTS_2==3,0.3863431,ifelse(capa_Ruteo_Arcos$LTS_2==4,0.6764487,0))))
    
    
    capa_Ruteo_Arcos <- capa_Ruteo_Arcos %>% left_join(capa_Ruteo_Arcos_Add_Cicloruta_Delete_SITP,By=ID) %>% mutate(S_3=0)
    capa_Ruteo_Arcos$S_3 <- ifelse(capa_Ruteo_Arcos$LTS_3==1,0.01984877,ifelse(capa_Ruteo_Arcos$LTS_3==2,0.2070296,ifelse(capa_Ruteo_Arcos$LTS_3==3,0.3863431,ifelse(capa_Ruteo_Arcos$LTS_3==4,0.6764487,0))))
    
    capa_Ruteo_Arcos_txt <- st_set_geometry(capa_Ruteo_Arcos,NULL)
    
    #Se imprimen los resultados
    
    write.table(capa_Ruteo_Arcos_txt,"Arcos.txt",sep=";",row.names=FALSE)
    write.table(capa_Ruteo_Nodos,"Nodos.txt",sep=";",row.names=FALSE)
    write.table(capa_Ruteo_Zats_Nodos,"NodosZats.txt",sep=";",row.names=FALSE)
    write.table(Matriz_OD,"Matriz_OD.txt",sep=";",row.names=FALSE)
    
    #Mapas Rutas
    
    
    capa_Ruteo_Arcos_A <- capa_Ruteo_Arcos %>% filter((ID %in% Resultado_1$V1))
    capa_Ruteo_Arcos_1<- capa_Ruteo_Arcos_A%>% filter(!(ID %in% Resultado_2$V1))
    capa_Ruteo_Arcos_2 <- capa_Ruteo_Arcos_A %>% filter((ID %in% Resultado_2$V1))
    
    a <- capa_Ruteo_Arcos_A %>% filter(LTS==1)
    LTS1_A <- sum(a$Distancia)
    a <- capa_Ruteo_Arcos_A %>% filter(LTS==2)
    LTS2_A <- sum(a$Distancia)
    
    a <- capa_Ruteo_Arcos_A %>% filter(LTS==3)
    LTS3_A <- sum(a$Distancia)
    
    a <- capa_Ruteo_Arcos_A %>% filter(LTS==4)
    LTS4_A <- sum(a$Distancia)
    
    LTS1_A
    LTS2_A
    LTS3_A
    LTS4_A
    
    
    
    
    a <- capa_Ruteo_Arcos_1 %>% filter(LTS==1)
    b <- capa_Ruteo_Arcos_2 %>% filter(LTS_1==1)
    LTS1 <- sum(a$Distancia)+sum(b$Distancia)
    
    a <- capa_Ruteo_Arcos_1 %>% filter(LTS==2)
    b <- capa_Ruteo_Arcos_2 %>% filter(LTS_1==2)
    LTS2 <- sum(a$Distancia)+sum(b$Distancia)
    
    a <- capa_Ruteo_Arcos_1 %>% filter(LTS==3)
    b <- capa_Ruteo_Arcos_2 %>% filter(LTS_1==3)
    LTS3 <- sum(a$Distancia)+sum(b$Distancia)
    
    a <- capa_Ruteo_Arcos_1 %>% filter(LTS==4)
    b <- capa_Ruteo_Arcos_2 %>% filter(LTS_1==4)
    LTS4 <- sum(a$Distancia)+sum(b$Distancia)
    
    LTS1
    LTS2
    LTS3
    LTS4
    
    
    pal <- colorFactor(c("lime green","orange"), capa_Ruteo_Arcos_A$LTS_1)
    
    leaflet(capa_Ruteo_Arcos_A) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%setView(lng =-74.11886, lat =4.699099, zoom = 12.5) %>% 
    addPolylines(color =~pal(LTS_1),opacity = 1,smoothFactor = 1)  %>% addLegend("bottomleft", pal = pal, values = ~LTS_1, title = "LTS",opacity = 1)
    
    
    capa_Ruteo_Arcos_B <- capa_Ruteo_Arcos %>% filter((ID %in% c(7395, 57703, 57705, 57707, 57708, 57808, 29662, 58458, 58591, 51701, 18649, 54559, 53864, 53866, 55643, 56089, 56813, 24345, 57147, 57156)))
    
    
    leaflet() %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite") %>% addPolylines(data=capa_Ruteo_Arcos) %>% addCircles(data=Nodos_Provisional,color="Black")
    
    %>% addPolylines(data=capa_Ruteo_Arcos_B, color = "Yellow",opacity = 1)
                               
    leaflet() %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>% addPolygons(data=Zats) %>% addCircles(data=Zats_Centroides,color="Red")%>% addCircles(data=Zats_Nodos,color="Black")
    
    
    Zats_Nodos <- capa_Ruteo_Zats_Nodos %>%  st_as_sf(coords=c(6,5))  %>% st_set_crs(4326) %>% filter(id %in% c(902,900))
    
    x <- as.list(test)
    leaflet() %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite") %>% addCircles(data=Zats_Centroides,color="Black")%>% addCircles(data=Zats_Centroides_1,color="Blue") 
    
    st_write(capa_variables_LTS, "Ejercicio1.shp")
    st_write(Zats, "Ejercicio2.shp")
    
    