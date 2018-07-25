#Preparación del entorno de trabajo----

  #Se cargan las siguiente librerias
    
    library(googleway)

  #Se define las ruta de las bases de datos
  
    ruta_resultados <-"/Users/alejandropalacio/Documents/GitHub/proBikePolicies/LTS/Resultados/"
  
  #Se carga la Data
  
    load(paste0(ruta_resultados, "Input_Consultas_Google_API.Rdata"))

#Google API consultas----

  #Se crean los Data Frame para almacenar los resultados de las consultas
    
    numero_registros <- nrow(capa_malla_vial)
    
    datos_free_flow<-as.data.frame(matrix(nrow=numero_registros,ncol=6))
  
    colnames(datos_free_flow) <-c("Longitud_1","TProm_1","TFF_1","Longitud_2","TProm_2","TFF_2")
    
  #Se realizan las consultas de Google API

    for(i in 1:numero_registros){
      
      #Se define el origen y destino de la consulta
      
        origen=paste(toString(capa_malla_vial$Latitud_Inicio[i]) ,"+", toString(capa_malla_vial$Longitud_Inicio [i]))
        destino=paste(toString(capa_malla_vial$Latitud_Fin[i]) ,"+", toString(capa_malla_vial$Longitud_Fin[i]))
      
      #Consultas Free Flow
      
        #Dirección 1
        
          distancia <- google_distance(origins = c(origen) ,destinations = c(destino) ,
                              mode="driving",  departure_time =as.POSIXct("09-4-2018 00:00:00", format = "%m-%d-%Y %H:%M:%S"),
                              traffic_model="best_guess",  key="AIzaSyDZ_Q8oLaAfogbrq5__nzNqdabbPL0QCvo",simplify = TRUE )
        
          #Se almacenan los resultados de la consulta
          
          datos_free_flow$Longitud_1[i]<-distancia$rows$elements[[1]]$distance$value
          datos_free_flow$TProm_1[i]<-distancia$rows$elements[[1]]$duration$value
          datos_free_flow$TFF_1[i]<-distancia$rows$elements[[1]]$duration_in_traffic$value
      
          
        #Diracción 2
        
          distancia <- google_distance(origins = c(destino) ,destinations = c(origen) ,
                            mode="driving",  departure_time =as.POSIXct("09-4-2018 00:00:00", format = "%m-%d-%Y %H:%M:%S"),
                            traffic_model="best_guess",  key="AIzaSyDZ_Q8oLaAfogbrq5__nzNqdabbPL0QCvo",simplify = TRUE )
      
          #Se almacenan los resultados de la consulta
          
          datos_free_flow$Longitud_2[i]<-distancia$rows$elements[[1]]$distance$value
          datos_free_flow$TProm_2[i]<-distancia$rows$elements[[1]]$duration$value
          datos_free_flow$TFF_2[i]<-distancia$rows$elements[[1]]$duration_in_traffic$value
    }

  #Se guarda la Data (Resultados Consultas Google API)
    
    save(datos_free_flow,file=paste0(ruta_resultados,"Resultados_Consultas_Google_API.Rdata"))
    
#Procesamiento resultados consultas Google API----

  #Se crean el Data Frame para almacenar los resultados
    
    datos_google_API<-as.data.frame(matrix(nrow=numero_registros,ncol=7))
    colnames(datos_google_API) <-c("ID","Longitud","TProm","TFF","Vprom", "VpromFF","Trafico")

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
      
        if(!is.na(datos_google_API$TFF[i]) & !is.na(datos_google_API$TProm[i])){
          if(datos_google_API$TFF[i]> datos_google_API$TProm[i]){
            datos_google_API$TFF[i]=NA
          }
        }
      
      #Computo de las velocidades
        datos_google_API$Vprom<-datos_google_API$Longitud*3.6/datos_google_API$TProm
        datos_google_API$VpromFF<-datos_google_API$Longitud*3.6/datos_google_API$TFF

    }

    
    
    #Se llenan los datos faltantes de velocidad
    
      datos_google_API$Vprom<-ifelse(is.na(datos_google_API$Vprom),mean(datos_google_API$Vprom, na.rm = TRUE), datos_google_API$Vprom)
      datos_google_API$VpromFF<-ifelse(is.na(datos_google_API$VpromFF),ifelse(mean(datos_google_API$VpromFF, na.rm = TRUE)<datos_google_API$Vprom,datos_google_API$Vprom,mean(datos_google_API$VpromFF, na.rm = TRUE)),datos_google_API$VpromFF)
      
      
    #Se llenan los datos faltantes de tiempo
      
      datos_google_API$TProm<-ifelse(is.na(datos_google_API$TProm),datos_google_API$Longitud*3.6/datos_google_API$Vprom,datos_google_API$TProm)
      datos_google_API$TFF<-ifelse(is.na(datos_google_API$TFF),datos_google_API$Longitud*3.6/datos_google_API$VpromFF,datos_google_API$TFF)

    #Calculo del indicador del tráfico
      
      datos_google_API$Trafico=1-(datos_google_API$TFF/datos_google_API$TProm)
      
    #Se dejan los tiempos y velocidades en 0 cuando la longitud es igual a 0
      
      for (i in 1:numero_registros) {
        if(datos_google_API$Longitud[i]==0){
          datos_google_API$TProm[i]=0
          datos_google_API$TFF[i]=0
          datos_google_API$Vprom[i]=
          datos_google_API$VpromFF[i]=0
          datos_google_API$Trafico[i]=0
        }
      }
          
    #Se guarda la Data (Datos Procesados Consultas Google API)
      
      save(datos_google_API,file=paste0(ruta_resultados,"Datos_Procesados_Consultas_Google_API.Rdata"))
      
    #Se eliminan los datos que no se usaran
      
      rm(distancia,datos_free_flow)
