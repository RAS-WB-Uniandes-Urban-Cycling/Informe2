#Preparación del entorno de trabajo----

  #Se cargan las siguiente librerias
    
    library(googleway)

  #Se define las ruta de las bases de datos
  
    ruta_resultados <-"/Users/alejandropalacio/Documents/GitHub/proBikePolicies/LTS/Resultados/"
  
  #Se carga la Data
  
    load(paste0(ruta_resultados, "Input_Consultas_Google_API.Rdata"))

#Google API consultas----

  #Se crean los Data Frame para almacenar los resultados de las consultas
    
    numero_registros <- nrow(malla_vial)
    
    datos_free_flow<-as.data.frame(matrix(nrow=numero_registros,ncol=6))
    datos_hora_pico_manana<-as.data.frame(matrix(nrow=numero_registros,ncol=3))
    datos_hora_pico_tarde<-as.data.frame(matrix(nrow=numero_registros,ncol=3))
  
    colnames(datos_free_flow) <-c("Longitud_1","TProm_1","TFF_1","Longitud_2","TProm_2","TFF_2")
    colnames(datos_hora_pico_manana) <-c("Longitud","TProm","THPM")
    colnames(datos_hora_pico_tarde) <-c("Longitud","TProm","THPT")
    
  #Se realizan las consultas de Google API

    for(i in 1:numero_registros){
      
      #Se define el origen y destino de la consulta
      
        origen=paste(toString(malla_vial$Latitud_Inicio[i]) ,"+", toString(malla_vial$Longitud_Inicio [i]))
        destino=paste(toString(malla_vial$Latitud_Fin[i]) ,"+", toString(malla_vial$Longitud_Fin[i]))
      
      #Consultas Free Flow
      
        #Dirección 1
        
          distancia <- google_distance(origins = c(origen) ,destinations = c(destino) ,
                              mode="driving",  departure_time =as.POSIXct("09-04-2018 00:00:00", format = "%m-%d-%Y %H:%M:%S"),
                              traffic_model="best_guess",  key="AIzaSyCB484vIOsEMKJoKAK8Lp4OO4sd6EtM6bk",simplify = TRUE )
        
          #Se almacenan los resultados de la consulta
          
          datos_free_flow$Longitud_1[i]<-distancia$rows$elements[[1]]$distance$value
          datos_free_flow$TProm_1[i]<-distancia$rows$elements[[1]]$duration$value
          datos_free_flow$TFF_1[i]<-distancia$rows$elements[[1]]$duration_in_traffic$value
      
          
        #Diracción 2
        
          distancia <- google_distance(origins = c(destino) ,destinations = c(origen) ,
                            mode="driving",  departure_time =as.POSIXct("09-04-2018 00:00:00", format = "%m-%d-%Y %H:%M:%S"),
                            traffic_model="best_guess",  key="AIzaSyCB484vIOsEMKJoKAK8Lp4OO4sd6EtM6bk",simplify = TRUE )
      
          #Se almacenan los resultados de la consulta
          
          datos_free_flow$Longitud_2[i]<-distancia$rows$elements[[1]]$distance$value
          datos_free_flow$TProm_2[i]<-distancia$rows$elements[[1]]$duration$value
          datos_free_flow$TFF_2[i]<-distancia$rows$elements[[1]]$duration_in_traffic$value
      
          
      #Consultas Hora Pico por la mañana y tarde
      
        if (datos_free_flow$Longitud_1[i]<=datos_free_flow$Longitud_2[i]) {
          
          
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
        
        
        datos_hora_pico_manana$Longitud[i]<-hora_pico_manana$rows$elements[[1]]$distance$value
        datos_hora_pico_manana$TProm[i]<-hora_pico_manana$rows$elements[[1]]$duration$value
        datos_hora_pico_manana$THPM[i]<-hora_pico_manana$rows$elements[[1]]$duration_in_traffic$value
        
        datos_hora_pico_tarde$Longitud[i]<-hora_pico_tarde$rows$elements[[1]]$distance$value
        datos_hora_pico_tarde$TProm[i]<-hora_pico_tarde$rows$elements[[1]]$duration$value
        datos_hora_pico_tarde$THPT[i]<-hora_pico_tarde$rows$elements[[1]]$duration_in_traffic$value
        
    }

  #Se guarda la Data (Resultados Consultas Google API)
    
    save(datos_free_flow,datos_hora_pico_manana,datos_hora_pico_tarde,segmentos_resultantes_ruta_urbana,file="Resultados_Consultas_Google_API.Rdata")


#Procesamiento resultados consultas Google API----

  #Se crean el Data Frame para almacenar los resultados
    
    datos_google_API<-as.data.frame(matrix(nrow=numero_registros,ncol=11))
    colnames(datos_google_API) <-c("ID","Longitud","TProm","TFF","THPM","THPT","VpromFF","Vprom","VpromPM","VpromPT","Trafico")

  #Se procesan los resultados de las consultas de Google API
    
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

    #Se llenan los datos faltantes de velocidad
    
      datos_google_API$Vprom<-ifelse(is.na(datos_google_API$Vprom),mean(datos_google_API$Vprom, na.rm = TRUE), datos_google_API$Vprom)
      datos_google_API$VpromPM<-ifelse(is.na(datos_google_API$VpromPM),mean(datos_google_API$VpromPM, na.rm = TRUE), datos_google_API$VpromPM)
      datos_google_API$VpromPT<-ifelse(is.na(datos_google_API$VpromPT),mean(datos_google_API$VpromPT, na.rm = TRUE), datos_google_API$VpromPT)
      datos_google_API$VpromFF<-ifelse(is.na(datos_google_API$VpromFF),max(mean(datos_google_API$VpromFF, na.rm = TRUE),pmax(datos_google_API$Vprom,datos_google_API$VpromPM,datos_google_API$VpromPT)),datos_google_API$VpromFF)

    #Se llenan los datos faltantes de tiempo
      
      datos_google_API$TProm<-ifelse(is.na(datos_google_API$TProm),datos_google_API$Longitud*3.6/datos_google_API$Vprom,datos_google_API$TProm)
      datos_google_API$TFF<-ifelse(is.na(datos_google_API$TFF),datos_google_API$Longitud*3.6/datos_google_API$VpromFF,datos_google_API$TFF)
      datos_google_API$THPM<-ifelse(is.na(datos_google_API$THPM),datos_google_API$Longitud*3.6/datos_google_API$VpromPM,datos_google_API$THPM)
      datos_google_API$THPT<-ifelse(is.na(datos_google_API$THPT),datos_google_API$Longitud*3.6/datos_google_API$VpromPT,datos_google_API$THPT)

    #Calculo del indicador del tráfico
      
      datos_google_API$Trafico=1-(datos_google_API$TFF/pmin(datos_google_API$THPM,datos_google_API$THPT))

