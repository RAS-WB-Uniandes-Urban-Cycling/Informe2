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

    ruta_resultados <-"/Users/alejandropalacio/Documents/GitHub/proBikePolicies/LTS/Resultados/"

  #Se carga la Data

    load(paste0(ruta_resultados, "Variables_LTS.Rdata"))
      
#Procesamiento Capa Malla Vial----
    
    capa_variables_LTS <- st_set_precision(capa_variables_LTS, precision = -1e-0001)
    centroides_capa_variables_LTS <- st_centroid(capa_variables_LTS$geometry)
    capa_variables_LTS <- capa_variables_LTS %>% st_split(st_combine(centroides_capa_variables_LTS)) %>% st_collection_extract( type = c("LINESTRING")) 
    capa_variables_LTS <- capa_variables_LTS %>% mutate(ID=row_number())
    
    saveRDS(capa_variables_LTS,"Prueba")
      
    capa_variables_LTS <- st_set_precision(capa_variables_LTS, precision = 0)
        
  #Se obtienen las coordenadas (Lat/Long) de punto inicial de cada segmento la capa_malla_vial
    
    coordenadas_incio_seg<-st_coordinates(capa_variables_LTS) %>% as_tibble()%>% group_by(L1)%>% filter(row_number()==1) %>% 
      ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud",Y="Latitud"))
    
    coordenadas_fin_seg<-st_coordinates(capa_variables_LTS) %>% as_tibble() %>% group_by(L1) %>% filter(row_number()==n()) %>% 
      ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud",Y="Latitud"))
    
  #Se unen las dos trablas de coordenadas
    
    coordenadas_malla_vial <- coordenadas_incio_seg %>% rbind(coordenadas_fin_seg) 
    
  #Se cuentan y agrupan los repetidos
    
    intersecciones_malla_vial <- coordenadas_malla_vial %>% group_by(Latitud, Longitud) %>% summarise(count=n()) %>% filter(count>=4) %>% ungroup()

  #Se genera la capa de intersecciones
    
    intersecciones_malla_vial<- intersecciones_malla_vial  %>% st_as_sf(coords=c(2,1)) %>% mutate(ID_Punto=row_number()) %>% st_set_crs(4326) 
    
  #Se obtienen las coordenadas (Lat/Long) de punto inicial de cada segmento la capa_malla_vial
    
    coordenadas_incio_seg<-st_coordinates(capa_variables_LTS) %>% as_tibble()%>% group_by(L1)%>% filter(row_number()==1) %>% 
      ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud_Inicio",Y="Latitud_Inicio"))
    
    coordenadas_fin_seg<-st_coordinates(capa_variables_LTS) %>% as_tibble() %>% group_by(L1) %>% filter(row_number()==n()) %>% 
      ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud_Fin",Y="Latitud_Fin"))
    
  #Se hace join entre la capa_malla_vial y las coordenadas  
    
    capa_variables_LTS<-capa_variables_LTS  %>% left_join(coordenadas_incio_seg,by="ID") %>% left_join(coordenadas_fin_seg, by="ID")
    
  #Se hace un Join espacial entre las intersecciones y las polilineas  
  
    intersecciones <-intersecciones_malla_vial %>% st_join(select(capa_variables_LTS,ID,Ancho,Velocidad,Latitud_Inicio, Longitud_Inicio, Latitud_Fin, Longitud_Fin,geometry)) %>% mutate(Igual=0, Angulo=0) 
    
  #Se identifican los pares de polilineas en las intersecciones
    
    intersecciones_segmentos <- data.frame(matrix(ncol=4,nrow=round(nrow(intersecciones)/2,0)))
    colnames(intersecciones_segmentos) <- c("ID_Interseccion","ID_Segmento_1","ID_Segmento_2","Numero_Interseccion")
    
    contador_1 <- 0
    
    contador_2 <- 0
    
    for(i in 1:1){
      
      angulo <- 180
      inicio <- i-8
      fin<- i+8
      
      if(inicio<1){inicio <- 1}
      
      if(fin>nrow(intersecciones)){fin <- nrow(intersecciones)}
      

      for(j in 4:4){
        if(intersecciones$ID_Punto[i]==intersecciones$ID_Punto[j] & i!=j ){
            if(intersecciones$Latitud_Fin[i]==intersecciones$Latitud_Inicio[i]){
              p1 <- (intersecciones$Longitud_Fin[i]-intersecciones$Longitud_Inicio[i])/(0.00000000001)
            }
            else{
              p1 <- (intersecciones$Longitud_Fin[i]-intersecciones$Longitud_Inicio[i])/(intersecciones$Latitud_Fin[i]-intersecciones$Latitud_Inicio[i])
            }
            
            if(intersecciones$Latitud_Fin[j]==intersecciones$Latitud_Inicio[j]){
              p2 <- (intersecciones$Longitud_Fin[j]-intersecciones$Longitud_Inicio[j])/(0.00000000001)
            }
            else{
              p2 <- (intersecciones$Longitud_Fin[j]-intersecciones$Longitud_Inicio[j])/(intersecciones$Latitud_Fin[j]-intersecciones$Latitud_Inicio[j])
            }  
            angulo_calculo <- abs(NISTradianTOdeg(atan2(p2-p1,1+p2*p1)))
          
          if(angulo>angulo_calculo){
            angulo <- angulo_calculo
            ID_2 <- intersecciones$ID[j]
            intersecciones$Igual[i] <- j
            intersecciones$Angulo[i] <-angulo
          }
        }
      }
      
      
      if(i==1){
        contador_1 <- contador_1+1
        contador_2 <- contador_2+1
        intersecciones_segmentos$ID_Interseccion[contador_1] <- intersecciones$ID_Punto[i]
        intersecciones_segmentos$ID_Segmento_1[contador_1] <- intersecciones$ID[i]
        intersecciones_segmentos$ID_Segmento_2[contador_1] <- ID_2
        intersecciones_segmentos$Numero_Interseccion[contador_1] <- contador_2
        ID_anterior=intersecciones$ID_Punto[i]
      }
      else{
        repetido <- FALSE
        for (z in 1:contador_1) {
          if( (intersecciones_segmentos$ID_Segmento_1[z] == intersecciones$ID[i] | intersecciones_segmentos$ID_Segmento_1[z] == ID_2) & (intersecciones_segmentos$ID_Segmento_2[z] == intersecciones$ID[i] | intersecciones_segmentos$ID_Segmento_2[z] == ID_2)){
            repetido <- TRUE
          }
        }
        
        if(repetido==FALSE){
          if(ID_anterior!=intersecciones$ID_Punto[i]){
            contador_2 <- 0 
          }
          contador_1 <- contador_1+1
          contador_2 <- contador_2+1
          intersecciones_segmentos$ID_Interseccion[contador_1] <- intersecciones$ID_Punto[i]
          intersecciones_segmentos$ID_Segmento_1[contador_1] <- intersecciones$ID[i]
          intersecciones_segmentos$ID_Segmento_2[contador_1] <- ID_2
          intersecciones_segmentos$Numero_Interseccion[contador_1] <- contador_2
          ID_anterior=intersecciones$ID_Punto[i]
            
        }
        
        
      }
      
  
    }
  
  #Se realiza un agrupamiento  
    
    prueba <- capa_variables_LTS %>% right_join(select(intersecciones_segmentos,Numero_Interseccion,ID_Segmento_1),by=c("ID"="ID_Segmento_1")) %>% filter(!is.na(Numero_Interseccion))  
    prueba1 <- capa_variables_LTS %>% right_join(select(intersecciones_segmentos,Numero_Interseccion,ID_Segmento_2),by=c("ID"="ID_Segmento_2")) %>% filter(!is.na(Numero_Interseccion)) 
    prueba <- rbind(prueba,prueba1) %>% filter(st_is_valid(.))%>% st_transform(4326)
    
    SpatialLinesMidPoints
    
    mapa<-tm_shape(prueba)+tm_lines(col="Numero_Interseccion",style ="cat" ,scale=5 ,palette ="-Set1",title.col ="Presencia de Cicloruta", popup.vars = TRUE)+tmap_mode("view")+tm_view(alpha = 1, basemaps = "OpenStreetMap.BlackAndWhite")
    
    mapa
    write_sf(prueba, "MarceloGAY7.shp")
    
    
    st_set_precision(a, precision = 1e-05)
    a
    a <- prueba %>% st_transform(4326)
    c <- st_centroid(a$geometry)
    
    a <- st_set_precision(a, precision = -1e-0001)
    
    c <- st_set_precision(st_centroid(a$geometry), precision = -1e-0001)
    d <-a %>% st_split(st_combine(c)) %>% st_collection_extract( type = c("LINESTRING")) 
    
    midpoints.psp(a)
    
    
    b <- st_transform(prueba, 29101)
    
    
    a <- st_centroid(b$geometry)
    a <- SpatialLinesMidPoints(prueba)
    hola<-prueba %>% st_split(st_combine(a))
    
    
    seg = st_segmentize(prueba$geometry, 0.0001)
    
    seg2 <- seg %>% 

    hola<-b %>% st_split(st_combine(a)) %>% st_collection_extract( type = c("LINESTRING")) 
        
    write_sf(hola, "MarceloGAY5.shp")
    write_sf(a, "MarceloGAY4.shp")
    