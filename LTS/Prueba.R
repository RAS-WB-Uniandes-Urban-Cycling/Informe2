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

#Se define la ruta de la base de datos
  ruta_archivo<-"/Users/alejandropalacio/Documents/BancoMundial-Targa/informacion_geografica/"
  ruta_archivo2<-"/Users/alejandropalacio/Desktop/"

#Consulta layers en la base de datos
  st_layers(paste0(ruta_archivo,"IDECA_0318.gdb"))
  st_layers(paste0(ruta_archivo2,"malla vial bogotá.shp"))
  st_layers(paste0(ruta_archivo2,"RutasSitp.gdb"))
  
#Se almacena los layer en cada variable definida

  malla_vial<-st_read(paste0(ruta_archivo2,"malla vial bogotá.shp"), stringsAsFactors = FALSE)%>% filter(!(highway %in% c("cycleway","service","steps","footway"))) %>% st_transform(4326) 
  ciclo_rutas<-st_read(paste0(ruta_archivo,"IDECA_0318.gdb"),layer = "RBic",stringsAsFactors = FALSE) %>% st_transform(4326) 
  zats<-st_read(paste0(ruta_archivo,"IDECA_0318.gdb"),layer = "SCAT",stringsAsFactors = FALSE) %>% filter(SCaNombre %in% c("DOCE DE OCTUBRE","CIUDAD SALITRE NOR-ORIENTAL","CIUDAD SALITRE SUR-ORIENTAL")) %>%
    st_transform(4326)
  calzadas<-st_read(paste0(ruta_archivo,"IDECA_0318.gdb"),layer = "Calz",stringsAsFactors = FALSE,promote_to_multi = FALSE) %>% st_transform(4326) %>% filter(st_is_valid(.))
  paraderosSITP <- st_read(paste0(ruta_archivo,"IDECA_0318.gdb"),layer = "NTra",stringsAsFactors = FALSE) %>% st_transform(4326) 
  ruta_urbana <- st_read(paste0(ruta_archivo2,"RutasSitp.gdb"),layer = "Ruta_Urbana",stringsAsFactors = FALSE) %>% st_transform(4326) 
  
#Muestra la informacion almacenada en la variable
  
  malla_vial
  ciclo_rutas
  zats

#Malla Vial
    
      #Se hace un Join espacial entre malla vial y zats
              
        union_malla_vial<-malla_vial %>% st_join(select(zats,SCaNombre),left = FALSE, largest=TRUE)  %>% mutate(ID=row_number())%>% rename(c(SCaNombre="ZAT_Nombre"))
            
      #Se dividen cada segmento en uno o mas segmentos
 
        segmentos<-tibble(gsection(union_malla_vial)) %>%st_as_sf()
                
        union_malla_vial <- segmentos %>% st_join(union_malla_vial,largest=TRUE) %>% mutate(ID=row_number()) 
            
      #Se hace un Join espacial entre union_malla_vial y calzadas
            
        union_malla_vial<-union_malla_vial %>% st_join(select(calzadas,CalAncho, CalNCarril),left = TRUE, largest=TRUE)   %>% rename(c(CalAncho="Ancho_Calzada", CalNCarril="Numero_Carriles"))
            
      #Se obtienen las coordenadas (Lat/Long) de punto inicial de cada segmento
          
        coordenadas_incio_seg<-st_coordinates(union_malla_vial) %>% as_tibble()%>% group_by(L1)%>% filter(row_number()==1) %>% 
          ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud Inicio",Y="Latitud Inicio"))
          
        coordenadas_fin_seg<-st_coordinates(union_malla_vial) %>% as_tibble() %>% group_by(L1) %>% filter(row_number()==n()) %>% 
          ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud Fin",Y="Latitud Fin"))
          
        coordenadas_malla_vial<-coordenadas_incio_seg  %>% left_join(coordenadas_fin_seg, By=ID)
          
      #Se hace join con la capa union_malla_vial
          
        union_malla_vial<-union_malla_vial  %>% left_join(coordenadas_malla_vial, By=ID)

#Google API consultas
            
    numero_registros <- nrow(union_malla_vial)
            
    datos_free_flow<-matrix(nrow=numero_registros,ncol=6)
    datos_hora_pico_manana<-matrix(nrow=numero_registros,ncol=3)
    datos_hora_pico_tarde<-matrix(nrow=numero_registros,ncol=3)
            
    colnames(datos_free_flow) <-c("Longitud_1","TProm_1","TFF_1","Longitud_2","TProm_2","TFF_2")
    colnames(datos_hora_pico_manana) <-c("Longitud","TProm","THPM")
    colnames(datos_hora_pico_tarde) <-c("Longitud","TProm","THPT")
            
      for(i in 1:numero_registros){
              
        origen=paste(toString(union_malla_vial$`Latitud Inicio`[i]) ,"+", toString(union_malla_vial$`Longitud Inicio`[i]))
        destino=paste(toString(union_malla_vial$`Latitud Fin`[i]) ,"+", toString(union_malla_vial$`Longitud Fin`[i]))
              
        #Free Flow
              
        distancia <- google_distance(origins = c(origen) ,destinations = c(destino) ,
                     mode="driving",  departure_time =as.POSIXct("09-04-2018 00:00:00", format = "%m-%d-%Y %H:%M:%S"),
                     traffic_model="best_guess",  key="AIzaSyCB484vIOsEMKJoKAK8Lp4OO4sd6EtM6bk",simplify = TRUE )
              
        datos_free_flow[i,1]<-distancia$rows$elements[[1]]$distance$value
        datos_free_flow[i,2]<-distancia$rows$elements[[1]]$duration$value
        datos_free_flow[i,3]<-distancia$rows$elements[[1]]$duration_in_traffic$value
              
        distancia <- google_distance(origins = c(destino) ,destinations = c(origen) ,
                     mode="driving",  departure_time =as.POSIXct("09-04-2018 00:00:00", format = "%m-%d-%Y %H:%M:%S"),
                     traffic_model="best_guess",  key="AIzaSyCB484vIOsEMKJoKAK8Lp4OO4sd6EtM6bk",simplify = TRUE )
              
              
        datos_free_flow[i,4]<-distancia$rows$elements[[1]]$distance$value
        datos_free_flow[i,5]<-distancia$rows$elements[[1]]$duration$value
        datos_free_flow[i,6]<-distancia$rows$elements[[1]]$duration_in_traffic$value
              
              #Hora Pico por la Mañana y tarde  
              
        if (datos_free_flow[i,1]<=datos_free_flow[i,4]) {
                
            hora_pico_manana <- google_distance(origins = c(origen) ,destinations = c(destino) ,
                                mode="driving",  departure_time =as.POSIXct("09-04-2018 7:30:00", format = "%m-%d-%Y %H:%M:%S"),
                                traffic_model="best_guess",  key="AIzaSyCB484vIOsEMKJoKAK8Lp4OO4sd6EtM6bk",simplify = TRUE )
                
            hora_pico_tarde <- google_distance(origins = c(origen) ,destinations = c(destino) ,
                               mode="driving",  departure_time =as.POSIXct("09-04-2018 18:00:00", format = "%m-%d-%Y %H:%M:%S"),
                               traffic_model="best_guess",  key="AIzaSyCB484vIOsEMKJoKAK8Lp4OO4sd6EtM6bk",simplify = TRUE )
                
        } else {
                
            hora_pico_manana <- google_distance(origins = c(destino) ,destinations = c(origen) ,
                                mode="driving",  departure_time =as.POSIXct("09-04-2018 7:30:00", format = "%m-%d-%Y %H:%M:%S"),
                                traffic_model="best_guess",  key="AIzaSyCB484vIOsEMKJoKAK8Lp4OO4sd6EtM6bk",simplify = TRUE )
                
            hora_pico_tarde <- google_distance(origins = c(destino) ,destinations = c(origen) ,
                               mode="driving",  departure_time =as.POSIXct("09-04-2018 18:00:00", format = "%m-%d-%Y %H:%M:%S"),
                               traffic_model="best_guess",  key="AIzaSyCB484vIOsEMKJoKAK8Lp4OO4sd6EtM6bk",simplify = TRUE )
        }        
              
              
        datos_hora_pico_manana[i,1]<-hora_pico_manana$rows$elements[[1]]$distance$value
        datos_hora_pico_manana[i,2]<-hora_pico_manana$rows$elements[[1]]$duration$value
        datos_hora_pico_manana[i,3]<-hora_pico_manana$rows$elements[[1]]$duration_in_traffic$value
              
        datos_hora_pico_tarde[i,1]<-hora_pico_tarde$rows$elements[[1]]$distance$value
        datos_hora_pico_tarde[i,2]<-hora_pico_tarde$rows$elements[[1]]$duration$value
        datos_hora_pico_tarde[i,3]<-hora_pico_tarde$rows$elements[[1]]$duration_in_traffic$value
              
      }
            
            
        write.csv(datos_free_flow,"Datos_Free_Flow.csv")
        write.csv(dist_vel2,"Datos_Hora_Pico_M.csv")
        
        save(datos_free_flow,datos_hora_pico_manana,datos_hora_pico_tarde,segmentos_resultantes_ruta_urbana,file="IMPORTANTE_RESULTADOS.Rdata")
        
            
#Procesamiento de datos obtenidos de Google API
            
      datos_google_API<-as.data.frame(matrix(nrow=numero_registros,ncol=11))
      colnames(datos_google_API) <-c("Longitud","TProm","TFF","THPM","THPT","VpromFF","Vprom","VpromPM","VpromPT","Trafico","ID")
            
      for (i in 1:numero_registros) {
              
        datos_google_API$ID[i]=i
              
        # Lectura longitud y tiempos promedio y de flujo libre
        if (datos_free_flow[i,1]<=datos_free_flow[i,4]) {
                
          datos_google_API$Longitud[i]=datos_free_flow[i,1]
          datos_google_API$TProm[i]=ifelse(datos_free_flow[i,2]==0,NA,datos_free_flow[i,2])
          datos_google_API$TFF[i]=ifelse(datos_free_flow[i,3]==0,NA,datos_free_flow[i,3])
                
        } else {
          
          datos_google_API$Longitud[i]=datos_free_flow[i,4]
          datos_google_API$TProm[i]=ifelse(datos_free_flow[i,5]==0,NA,datos_free_flow[i,5])
          datos_google_API$TFF[i]=ifelse(datos_free_flow[i,6]==0,NA,datos_free_flow[i,6])
      
        }
        
        #Lectura de tiempos máximos  
        datos_google_API$THPM[i]<-ifelse(datos_hora_pico_manana[i,3]==0,NA,datos_hora_pico_manana[i,3])
        datos_google_API$THPT[i]<-ifelse(datos_hora_pico_tarde[i,3]==0,NA,datos_hora_pico_tarde[i,3])
        
        if(!is.na(datos_google_API$TFF[i]) & !is.na(min(datos_google_API$THPM[i],datos_google_API$THPT[i],datos_google_API$TProm[i]))){
          if(datos_google_API$TFF[i]> min(datos_google_API$THPM[i],datos_google_API$THPT[i],datos_google_API$TProm[i])){
            datos_google_API$TFF[i]=NA
          }
        }
        
        #Computo de las velocidades
        datos_google_API$Vprom<-datos_google_API$Longitud*3.6/datos_google_API$TProm
        datos_google_API$VpromFF<-datos_google_API$Longitud*3.6/datos_google_API$TFF
        datos_google_API$VpromPM<-datos_google_API$Longitud*3.6/datos_google_API$THPM
        datos_google_API$VpromPT<-datos_google_API$Longitud*3.6/datos_google_API$THPT
      }
      
      #Filling of speed missing data
      datos_google_API$Vprom<-ifelse(is.na(datos_google_API$Vprom),mean(datos_google_API$Vprom, na.rm = TRUE), datos_google_API$Vprom)
      datos_google_API$VpromPM<-ifelse(is.na(datos_google_API$VpromPM),mean(datos_google_API$VpromPM, na.rm = TRUE), datos_google_API$VpromPM)
      datos_google_API$VpromPT<-ifelse(is.na(datos_google_API$VpromPT),mean(datos_google_API$VpromPT, na.rm = TRUE), datos_google_API$VpromPT)
      datos_google_API$VpromFF<-ifelse(is.na(datos_google_API$VpromFF),max(mean(datos_google_API$VpromFF, na.rm = TRUE),pmax(datos_google_API$Vprom,datos_google_API$VpromPM,datos_google_API$VpromPT)),datos_google_API$VpromFF)
      
      #Filling of time missing data
      datos_google_API$TProm<-ifelse(is.na(datos_google_API$TProm),datos_google_API$Longitud*3.6/datos_google_API$Vprom,datos_google_API$TProm)
      datos_google_API$TFF<-ifelse(is.na(datos_google_API$TFF),datos_google_API$Longitud*3.6/datos_google_API$VpromFF,datos_google_API$TFF)
      datos_google_API$THPM<-ifelse(is.na(datos_google_API$THPM),datos_google_API$Longitud*3.6/datos_google_API$VpromPM,datos_google_API$THPM)
      datos_google_API$THPT<-ifelse(is.na(datos_google_API$THPT),datos_google_API$Longitud*3.6/datos_google_API$VpromPT,datos_google_API$THPT)
      
      #Calculo del indicador del tráfico
      datos_google_API$Trafico=1-(datos_google_API$TFF/pmin(datos_google_API$THPM,datos_google_API$THPT))
      
      #Capa LTS
      
        union_malla_vial<- union_malla_vial%>% transmute(ID,Ancho_Calzada,Numero_Carriles, lanes) %>% left_join(select(datos_google_API, Vprom, Trafico,ID))
            
      
#Segmentación capa union_malla_vial      
      
      #Se encuentrar los vertices
      
        vertices_union_malla_vial<- st_cast(union_malla_vial, 'POINT') 
      
      #Se divide cada segmento deacuerdo a su vertices

        segmentos_resultantes<- union_malla_vial$geometry %>% st_split(st_combine(vertices_union_malla_vial)) %>% st_collection_extract( type = c("LINESTRING")) %>%
          as.data.frame() %>% st_as_sf()
      
      #Se hace un Join espacial entre union_malla_vial y segmentos_resultantes
        
        capa_malla_vial<-segmentos_resultantes %>% mutate(ID=row_number()) %>% st_join(select(union_malla_vial, Vprom, Trafico, Ancho_Calzada, Numero_Carriles, lanes), largest=TRUE)
      
      #Se hace un Join espacial entre union_malla_vial y calzadas

        capa_malla_vial<-capa_malla_vial %>% st_join(select(calzadas,CalAncho, CalNCarril),left = TRUE, largest=TRUE)  %>% 
          transmute(ID,Vprom,Trafico,Ancho_Calzada=if_else(!is.na(CalAncho),as.numeric(CalAncho),as.numeric(Ancho_Calzada)),Numero_Carriles=if_else(is.na(lanes),if_else(is.na(CalNCarril),as.numeric(Numero_Carriles),as.numeric(CalNCarril)),as.numeric(lanes)))

      #Se obtienen las coordenadas (Lat/Long) de punto inicial de cada segmento
        
        coordenadas_incio_seg<-st_coordinates(capa_malla_vial) %>% as_tibble()%>% group_by(L1)%>% filter(row_number()==1) %>% 
          ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud Inicio",Y="Latitud Inicio"))
        
        coordenadas_fin_seg<-st_coordinates(capa_malla_vial) %>% as_tibble() %>% group_by(L1) %>% filter(row_number()==n()) %>% 
          ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud Fin",Y="Latitud Fin"))
        
        coordenadas_malla_vial<-coordenadas_incio_seg  %>% left_join(coordenadas_fin_seg, By=ID)
        
      #Se hace join con la capa union_malla_vial
        
        capa_malla_vial<-capa_malla_vial  %>% left_join(coordenadas_malla_vial, By=ID)
        
#Red-ciclorutas
          
      #Se hace un Join espacial entre malla vial y zats
          
        capa_ciclo_rutas<-ciclo_rutas %>% st_join(select(zats,SCaNombre) %>% st_buffer(0.0001347),left = FALSE, largest=TRUE) 
        
        capa_ciclo_rutas <- st_cast(capa_ciclo_rutas, 'LINESTRING', do_split=TRUE) %>% mutate(ID=row_number())
        
      #Se obtienen las coordenadas (Lat/Long) de punto inicial de cada segmentos
          
        coordenadas_incio_seg<-st_coordinates(capa_ciclo_rutas) %>% as_tibble() %>% group_by(L1) %>% filter(row_number()==1) %>% 
            ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud Inicio C",Y="Latitud Inicio C"))
          
        coordenadas_fin_seg<-st_coordinates(capa_ciclo_rutas) %>% as_tibble() %>% group_by(L1) %>% filter(row_number()==n()) %>% 
            ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud Fin C",Y="Latitud Fin C"))
          
        coordenadas_ciclo_rutas<-coordenadas_incio_seg  %>% left_join(coordenadas_fin_seg, By=ID)
  
      #Se hace join con la capa union_ciclo_rutas
          
        capa_ciclo_rutas<-capa_ciclo_rutas  %>% left_join(coordenadas_ciclo_rutas, By=ID)
          
      #Se hace un buffer de x metros de anchos a la union_ciclo_rutas
          
        buffer_ciclo_rutas<-st_buffer(capa_ciclo_rutas,dist = 0.000200)

#Join Spacial union_malla_vial y buffer_ciclo_rutas
        
      #Se hace un join espacial entre union_malla_vial y el buffer_ciclo_rutas
            
        union_malla_vial_ciclo_rutas<-capa_malla_vial %>% st_join(select(buffer_ciclo_rutas,"Latitud Inicio C","Longitud Inicio C","Latitud Fin C","Longitud Fin C")) 
          
      #Se calcula el la pendiente de cada segmento (union_malla_vial y union_ciclo_rutas), se calcula el angulo que forman dichos segmentos y se eliminan los segmentos cuyo angulo es mayor a 20 grados 
            
        union_malla_vial_ciclo_rutas <-union_malla_vial_ciclo_rutas %>% mutate(p1=((`Longitud Fin`-`Longitud Inicio`)/(`Latitud Fin`-`Latitud Inicio`)),p2=((`Longitud Fin C`-`Longitud Inicio C`)/(`Latitud Fin C`-`Latitud Inicio C`)))%>%
          mutate(angulo=NISTradianTOdeg(atan2(p2-p1,1+p2*p1))) %>% filter( abs(angulo)<=25 )
          
      #Se ordenan los angulos de menor a mayor y se eligen el primer registro de cada segmento
          
        union_malla_vial_ciclo_rutas<-union_malla_vial_ciclo_rutas[order(union_malla_vial_ciclo_rutas$angulo),] %>% group_by(ID) %>% filter(row_number()==1) %>% 
          ungroup() %>% mutate(CicloRuta=1)
          
      #Se hace un left-join entre union_malla_vial y union_malla_vial_ciclo_rutas
 
        capa_malla_vial<-capa_malla_vial %>% left_join(select( st_set_geometry(union_malla_vial_ciclo_rutas,NULL), CicloRuta, ID))
          
      #A los registros que en el campo ciclo ruta tenga valor NULL, se le asigna el valor de 0
          
        capa_malla_vial$CicloRuta[is.na(capa_malla_vial$CicloRuta)] <- 0

#Rutas SITP
        
      #Se combierte de MultiLinesting a Lingstring
          
        ruta_urbana<- st_cast(ruta_urbana, 'LINESTRING')   
        
      #Se hace un buffer de x metros de anchos a la capa rutas
          
        #vertices_ruta_urbana<- st_cast(ruta_urbana, 'POINT') 
        
      #Se divide cada ruta del SITP deacuerdo a su vertices
      
        #segmentos_resultantes_ruta_urbana<- ruta_urbana$geometry %>% st_split(st_combine(vertices_ruta_urbana))%>% st_collection_extract( type = c("LINESTRING")) %>%
         # as.data.frame() %>% st_as_sf()
      
      #Se hace un Join espacial entre malla vial y zats
        
        capa_SITP<-segmentos_resultantes_ruta_urbana %>% st_join(select(zats,SCaNombre),left = FALSE, largest=TRUE)  %>% mutate(ID=row_number())%>% rename(c(SCaNombre="ZAT_Nombre"))
        
      #Se obtienen las coordenadas (Lat/Long) de punto inicial de cada segmentos
        
        coordenadas_incio_seg<-st_coordinates(capa_SITP) %>% as_tibble() %>% group_by(L1) %>% filter(row_number()==1) %>% 
          ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud Inicio S",Y="Latitud Inicio S"))
        
        coordenadas_fin_seg<-st_coordinates(capa_SITP) %>% as_tibble() %>% group_by(L1) %>% filter(row_number()==n()) %>% 
          ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud Fin S",Y="Latitud Fin S"))
        
        coordenadas_SITP<-coordenadas_incio_seg  %>% left_join(coordenadas_fin_seg, By=ID)  
        
      #Se hace join con la capa union_ciclo_rutas
        
        capa_SITP<-capa_SITP  %>% left_join(coordenadas_SITP, By=ID)
        
        
      #Se hace un Join espacial entre union_malla_vial y segmentos_resultantes
        
        buffer_SITP<-st_buffer(capa_SITP,dist = 0.00010)  
        
      #Se hace un join espacial entre capa_malla_vial y el buffer_ciclo_rutas
        
        union_malla_vial_sitp<-capa_malla_vial %>% st_join(select(buffer_SITP,"Latitud Inicio S","Longitud Inicio S","Latitud Fin S","Longitud Fin S")) 
        
        #Se calcula el la pendiente de cada segmento (union_malla_vial y union_ciclo_rutas), se calcula el angulo que forman dichos segmentos y se eliminan los segmentos cuyo angulo es mayor a 20 grados 
        
        union_malla_vial_sitp <-union_malla_vial_sitp %>% mutate(p1=((`Longitud Fin`-`Longitud Inicio`)/(`Latitud Fin`-`Latitud Inicio`)),p2=((`Longitud Fin S`-`Longitud Inicio S`)/(`Latitud Fin S`-`Latitud Inicio S`)))%>%
          mutate(angulo=NISTradianTOdeg(atan2(p2-p1,1+p2*p1))) %>% filter( abs(angulo)<=25 )
        
        #Se ordenan los angulos de menor a mayor y se eligen el primer registro de cada segmento
        
        union_malla_vial_sitp<-union_malla_vial_sitp[order(union_malla_vial_sitp$angulo),] %>% group_by(ID) %>% filter(row_number()==1) %>% 
          ungroup() %>% mutate(SITP=1)
        
        #Se hace un left-join entre union_malla_vial y union_malla_vial_ciclo_rutas
        
        capa_malla_vial<-capa_malla_vial %>% left_join(select( st_set_geometry(union_malla_vial_sitp,NULL), SITP, ID))
        
        #A los registros que en el campo ciclo ruta tenga valor NULL, se le asigna el valor de 0
        
        capa_malla_vial$SITP[is.na(capa_malla_vial$SITP)] <- 0
        
        
            
#Clustering 
      
        library(cluster)
        
        capa_clusters<-st_set_geometry(capa_malla_vial,NULL) %>% transmute(CicloRuta,SITP,Numero_Carriles=scale(as.matrix(capa_malla_vial$Numero_Carriles)),Ancho_Calzada=scale(as.matrix(capa_malla_vial$Ancho_Calzada)),Vprom=scale(as.matrix(capa_malla_vial$Vprom)),Trafico=scale(as.matrix(capa_malla_vial$Trafico)))
        capa_clusters$CicloRuta<-as.factor(capa_clusters$CicloRuta)
        capa_clusters$SITP<-as.factor(capa_clusters$SITP)
        capa_clusters$Numero_Carriles<-as.numeric(capa_clusters$Numero_Carriles)
        capa_clusters$Ancho_Calzada<-as.numeric(capa_clusters$Ancho_Calzada)
        capa_clusters$Vprom<-as.numeric(capa_clusters$Vprom)
        capa_clusters$Trafico<-as.numeric(capa_clusters$Trafico)
        
        dist<-daisy(capa_clusters, metric = "gower", stand=FALSE)
        
        clusters <- hclust(dist,method ="single")
        plot(clusters)
        clusterCut <- cutree(clusters, 4) 
        table(clusterCut)
        clusterCut
        hola<-data.frame(clusterCut) %>% mutate(ID=row_number())
        capa_LTS2<-capa_malla_vial  %>% left_join(hola)
        
        # Calculate silhouette width for many k using PAM
        
        sil_width <- c(NA)
        
        for(i in 2:10){
          
          pam_fit <- pam(dist,
                         diss = TRUE,
                         k = i)
          
          sil_width[i] <- pam_fit$silinfo$avg.width
          
        }
        
        # Plot sihouette width (higher is better)
        
        plot(1:10, sil_width,
             xlab = "Number of clusters",
             ylab = "Silhouette Width")
        lines(1:10, sil_width)
        
        pam_fit <- pam(dist,diss = TRUE, k = 4)
        pam_fit <- as.data.frame(pam_fit$clustering) %>% mutate(ID=row_number()) %>% rename(c("pam_fit$clustering"="cluster"))
        
        capa_LTS2<- st_set_geometry(capa_malla_vial,NULL) %>% left_join(pam_fit,By=ID)  %>% transmute(Vprom,Ancho_Calzada,Numero_Carriles,Trafico)
        
        library(MASS)
        
        LTS_lda <- lda(hola$clusterCut ~ ., data=capa_LTS2)
        LTS_lda
        LTS_lda_values <- predict(LTS_lda)
        ldahist(data = LTS_lda_values$x[,1], g=pam_fit$cluster)
        plot(LTS_lda_values$x[,1],LTS_lda_values$x[,2]) # make a scatterplot
        text(LTS_lda_values$x[,1],LTS_lda_values$x[,2],pam_fit$cluster,cex=0.7,pos=4,col="red") # add labels
        
        library(fpc)
        
        calinhara(capa_clusters, hola$clusterCut)
        
        mapa<-tm_shape(zats)+tm_polygons(col = "green",alpha = 0.3,title="ZATS")+
          tm_shape(capa_LTS2)+tm_lines(col="cluster",style ="cat" ,scale=5 ,palette = get_brewer_pal("Accent",4,plot = F) ,title.col ="Cluster", textNA = "No", colorNA = "Red")+
          tmap_mode("view")+tm_view(alpha = 1, basemaps = "OpenStreetMap.BlackAndWhite")
        mapa
        
        
        
        
        
        fi<- kmeans(capa_clusters, 4)
        
          
        capa_clusters<-st_set_geometry(capa_malla_vial,NULL) %>% transmute(CicloRuta,Vprom,Numero_Carriles,Ancho_Calzada,Trafico)
        capa_clusters<-st_set_geometry(capa_malla_vial,NULL) %>% transmute(CicloRuta,
                                                                            Vprom=((capa_malla_vial$Vprom-min(capa_malla_vial$Vprom))/(max(capa_malla_vial$Vprom)-min(capa_malla_vial$Vprom))),
                                                                            Numero_Carriles=((capa_malla_vial$Numero_Carriles-min(capa_malla_vial$Numero_Carriles))/(max(capa_malla_vial$Numero_Carriles)-min(capa_malla_vial$Numero_Carriles))),
                                                                            Ancho_Calzada=((capa_malla_vial$Ancho_Calzada-min(capa_malla_vial$Ancho_Calzada))/(max(capa_malla_vial$Ancho_Calzada)-min(capa_malla_vial$Ancho_Calzada))),
                                                                            Trafico)
        
        
        capa_malla_vial2 <-capa_malla_vial %>%  transmute(ID,Vprom,Trafico=if_else(Trafico>0.5,0.5,Trafico),Ancho_Calzada,Numero_Carriles,CicloRuta)
        
        
        
        capa_clusters<-st_set_geometry(capa_malla_vial2,NULL) %>% transmute(Numero_Carriles=normalize.decscale(as.matrix(capa_malla_vial2$Numero_Carriles)),Ancho_Calzada=normalize.decscale(as.matrix(capa_malla_vial2$Ancho_Calzada)),Trafico=normalize.decscale(as.matrix(capa_malla_vial2$Trafico)),Vprom=normalize.decscale(as.matrix(capa_malla_vial2$Vprom)))
        
     
        
        library(rknn)
        
        set.seed(2100)
          
        clusters <- tibble(cluster=as.vector(specc(as.matrix(capa_clusters),centers=4)))
        capa_LTS2 <- capa_malla_vial %>% bind_cols(clusters) %>% st_as_sf()
         
        
        
        
        fit <- kmeans(capa_clusters, 4)
        aggregate(capa_clusters,by=list(fit$cluster),FUN=mean)
        capa_LTS2 <- data.frame(capa_malla_vial2, fit$cluster) %>% st_as_sf()
        
        
        set.seed(2100)
        
        
        
        
        
        
        
        
       
          
          a=4
          estadisticas_vel<-st_set_geometry(capa_LTS2,NULL) %>% filter(cluster==a)%>% transmute(Vprom)%>% summary()
          estadisticas_vel
          estadisticas_ancho<-st_set_geometry(capa_LTS2,NULL) %>% filter(cluster==a)%>% transmute(CalAncho)%>% summary()
          estadisticas_ancho
          ciclo_resumen <- table(st_set_geometry(capa_LTS2,NULL) %>% filter(cluster==a)%>% transmute(CicloRuta))
          ciclo_resumen

          estadisticas_trafico<-st_set_geometry(capa_LTS2,NULL) %>% filter(cluster==a)%>%transmute(lanes)%>% summary()
          estadisticas_trafico
          
          capa_LTS2$cluster[capa_LTS2$cluster==1] <- "LTS 1"
          capa_LTS2$cluster[capa_LTS2$cluster==4] <- "LTS 2"
          capa_LTS2$cluster[capa_LTS2$cluster==2] <- "LTS 3"
          capa_LTS2$cluster[capa_LTS2$cluster==3] <- "LTS 4"
       
#Mapas 
          
          mapa<-tm_shape(zats)+tm_polygons(col = "yellow",alpha = 0.3,title="ZATS")+
            tm_shape(capa_malla_vial)+tm_lines(lwd =3,col = "red",style ="pretty",title.col ="Malla vial")+
            tm_shape(capa_ciclo_rutas)+tm_lines(lwd =3,col = "blue",title.col ="Ciclorutas")+
            tmap_mode("view")+tm_view(alpha = 1, basemaps = "OpenStreetMap.BlackAndWhite")
          mapa
          
          mapa<-tm_shape(zats)+tm_polygons(col = "blue",alpha = 0.3,title="ZATS")+
            tm_shape(capa_malla_vial)+tm_lines(col="Vprom",style ="pretty" ,scale=5 ,palette = get_brewer_pal("Reds",6,plot = F) ,title.col ="Velocidad Promedio", textNA = "No", colorNA = "Red")+
            tmap_mode("view")+tm_view(alpha = 1, basemaps = "OpenStreetMap.BlackAndWhite")
          mapa
          
          mapa<-tm_shape(zats)+tm_polygons(col = "blue",alpha = 0.3,title="ZATS")+
            tm_shape(capa_LTS2)+tm_lines(col="Trafico",style ="kmeans" ,scale=5,n=6 ,palette = get_brewer_pal("Reds",6,plot = F),title.col ="Tráfico", textNA = "No", colorNA = "Red")+
            tmap_mode("view")+tm_view(alpha = 1, basemaps = "OpenStreetMap.BlackAndWhite")
          mapa
          
          mapa<-tm_shape(buffer_ciclo_rutas)+tm_polygons(col = "pink",alpha = 0.3,title="ZATS")+
            tm_shape(capa_malla_vial)+tm_lines(col="CicloRuta",style ="cat" ,scale=5 ,palette = get_brewer_pal("Dark2",6,plot = F) ,title.col ="Presencia Ciclo Ruta", textNA = "No", colorNA = "Red")+
            tm_shape(capa_ciclo_rutas)+tm_lines(lwd =3,col = "blue",title.col ="Ciclorutas")+
            tmap_mode("view")+tm_view(alpha = 1, basemaps = "OpenStreetMap.BlackAndWhite")
          mapa
          
          mapa<-tm_shape(zats)+tm_polygons(col = "blue",alpha = 0.3,title="ZATS")+
            tm_shape(capa_malla_vial)+tm_lines(col="Ancho_Calzada",style ="pretty" ,scale=5 ,palette = get_brewer_pal("Reds",6,plot = F) ,title.col ="Ancho malla vial (Metros)")+
            tmap_mode("view")+tm_view(alpha = 1, basemaps = "OpenStreetMap.BlackAndWhite")
          mapa
          
          mapa<-tm_shape(zats)+tm_polygons(col = "blue",alpha = 0.3,title="ZATS")+
            tm_shape(capa_malla_vial)+tm_lines(col="Numero_Carriles",style ="cat" ,scale=5 ,palette = get_brewer_pal("Dark2",6,plot = F) ,title.col ="Numero Carriles")+
            tmap_mode("view")+tm_view(alpha = 1, basemaps = "OpenStreetMap.BlackAndWhite")
          mapa
          
          mapa<-tm_shape(zats)+tm_polygons(col = "blue",alpha = 0.3,title="ZATS")+
            tm_shape(union_malla_vial)+tm_lines(col="CalNCarril",style ="cat" ,scale=5 ,palette = get_brewer_pal("Dark2",6,plot = F) ,title.col ="Numero Carriles")+
            tmap_mode("view")+tm_view(alpha = 1, basemaps = "OpenStreetMap.BlackAndWhite")
          mapa
          
          mapa<-tm_shape(capa_malla_vial)+tm_lines(col="SITP",style ="cat" ,scale=5 ,palette = get_brewer_pal("Dark2",6,plot = F) ,title.col ="Presencia SITP", textNA = "No", colorNA = "Red")+
            tm_shape(ruta_urbana)+tm_lines(col = "yellow")+
            tmap_mode("view")+tm_view(alpha = 1, basemaps = "OpenStreetMap.BlackAndWhite")
          mapa   
          
     
          


#################################################################################################

vel_traf<-matrix(nrow=numero_registros,ncol=7)
colnames(vel_traf) <-c("Longitud","TProm","TFF","Vprom","VFF","Trafico","ID")

for (i in 1:numero_registros) {
  
  vel_traf[i,1]=min(dist_vel1[i,1],dist_vel2[i,1])
  vel_traf[i,2]=min(dist_vel1[i,2],dist_vel2[i,2])
  vel_traf[i,3]=min(dist_vel1[i,3],dist_vel2[i,3])

  
  vel_traf[i,4]=vel_traf[i,1]*(1/1000)*(3600/1)/vel_traf[i,2]
  vel_traf[i,5]=vel_traf[i,1]*(1/1000)*(3600/1)/vel_traf[i,3]
  
  if ( vel_traf[i,3]==0 & vel_traf[i,2]!=0 ) {
    vel_traf[i,5]=vel_traf[i,4]
  }
  
  vel_traf[i,6]=(vel_traf[i,5]-vel_traf[i,4])/vel_traf[i,5]
  
  if (vel_traf[i,1]==0||vel_traf[i,2]==0) {
    vel_traf[i,4]=0
    vel_traf[i,5]=0
    vel_traf[i,6]=0
  }
  vel_traf[i,7]=i
  
  if (vel_traf[i,6]<0) {
    vel_traf[i,6]=0
  }
}

vel_traf<-data.frame(vel_traf)

union_malla_vial <- union_malla_vial %>% left_join(select(vel_traf,ID,Vprom,VFF,Trafico))



#Estadísticas descriptivas

#Velocidad

estadisticas_vel<-st_set_geometry(capa_LTS,NULL)%>% transmute(Vprom)%>% summary()
estadisticas_vel

boxplot(capa_LTS$Vprom, col = "lightskyblue", main="Velocidad Promedio",ylab="Velocidad (km/h)")

mean_vprom<-groupwiseMean(data = capa_LTS, var = "Vprom", conf=0.95)
qplot(x="Velocidad Promedio", y=Mean, data=mean_vprom)+geom_errorbar(aes(ymin=Trad.lower, ymax=Trad.upper,width=0.05))

plotDensityHistogram(capa_LTS$Vprom, col="lightskyblue",xlab="Velocidad (km/h)",ylab="freq", main = "Velocidad Promedio")


#Tráfico

library(lof)
install.packages("lof")

capa_malla_vial$Trafico[o>=2]

estadisticas_trafico<-st_set_geometry(capa_malla_vial2,NULL) %>% transmute(Trafico)%>% summary()

estadisticas_trafico

boxplot(capa_malla_vial$Trafico, col = "lightskyblue", main="Tráfico", ylab="Índice de Tráfico")

plotDensityHistogram(capa_malla_vial$Trafico, col="lightskyblue",xlab="Índice de Tráfico",ylab="freq", main = "Tráfico")
hist(capa_malla_vial$Trafico, col = "lightskyblue", xlab="Carriles",ylab="Frecuencia", main = "Número de Carriles") 

#Cicloruta

ciclo_resumen <- table(capa_LTS$CicloRuta)

ciclo_resumen
porcentaje_cobertura = ciclo_resumen[2]/ciclo_resumen[1]
porcentaje_cobertura
pie(ciclo_resumen)
barplot(ciclo_resumen,col=c("white","lightskyblue"))


#CalAncho

estadisticas_ancho<-st_set_geometry(capa_LTS,NULL)  %>% transmute(CalAncho)%>% summary()
estadisticas_ancho

boxplot(capa_LTS$CalAncho, col = "lightskyblue", main="Ancho de la Vía", ylab="Ancho (m)")

plotDensityHistogram(capa_LTS$CalAncho, col="lightskyblue",xlab="Ancho (m)",ylab="freq", main = "Ancho de la Vía")


#Lanes

estadisticas_ancho<-st_set_geometry(capa_LTS,NULL)  %>% transmute(lanes)%>% summary()
estadisticas_ancho

boxplot(capa_LTS$lanes, col = "lightskyblue", main="Número de carriles", ylab="Número de carriles")

hist(capa_LTS$lanes, col = "lightskyblue", xlab="Carriles",ylab="Frecuencia", main = "Número de Carriles") 
lines(density(capa_LTS$Lanes), lwd=1)



c<-st_as_sf(coordenadas_incio_seg,coords = c("Longitud Inicio", "Latitud Inicio"),crs=4326)


st_write(capa_malla_vial, "CapaLTS.shp")


hola2 <- union_malla_vial$geometry%>% st_split(st_combine(hola))
st_write(union_malla_vial, "hola20.shp")

hola2

hola3 <- as.data.frame(st_collection_extract(hola2, type = c("LINESTRING")))

as_data_frame(hola3)

seg = st_segmentize(union_malla_vial, set_units(100, m))

hola <- st_cast(union_malla_vial, 'POINT') 

hola <- hola %>% group_by(ID) %>% st_cast("LINESTRING")

seg = st_geod_segmentize(union_malla_vial, set_units(0.1, m))




ruta_archivo2<-"/Users/alejandropalacio/Desktop/"
lineasDeseo<-st_read(paste0(ruta_archivo2,"LineasViajes.shp"), stringsAsFactors = FALSE)

library(stplanr)

for (i in 8:8){
  rutas <- line2route(lineasDeseo[(10*(i-1)+1):(10*i),],"route_osrm",l_id = "id")
  st_write(rutas,paste0("/Users/alejandropalacio/Desktop/",i,".shp"))
}

