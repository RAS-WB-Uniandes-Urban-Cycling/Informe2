#Preparación del entorno de trabajo----

  #Se cargan las siguiente librerias
    
    library(googleway)

  #Se define las ruta de las bases de datos
  
    ruta_resultados <-"/Users/alejandropalacio/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/RESULTADOS/LTS/Bases de Datos/"
  
  #Se carga la Data
  
    load(paste0(ruta_resultados, "1-Input_Consultas_Google_API.Rdata"))

#Google API consultas----

  #Se crean los Data Frame para almacenar los resultados de las consultas
    
    numero_registros <- nrow(capa_malla_vial)
    
    datos_free_flow<-as.data.frame(matrix(nrow=numero_registros,ncol=6))
  
    colnames(datos_free_flow) <-c("Longitud_1","TProm_1","TFF_1","Longitud_2","TProm_2","TFF_2")
    
  #Se realizan las consultas de Google API

    for(i in 1:10000){
      
      #Se define el origen y destino de la consulta
      
        origen=paste(toString(capa_malla_vial$Latitud_Inicio[i]) ,"+", toString(capa_malla_vial$Longitud_Inicio [i]))
        destino=paste(toString(capa_malla_vial$Latitud_Fin[i]) ,"+", toString(capa_malla_vial$Longitud_Fin[i]))
      
      #Consultas Free Flow
      
        #Dirección 1
        
          distancia <- google_distance(origins = c(origen) ,destinations = c(destino) ,
                              mode="driving",  departure_time =as.POSIXct("09-4-2018 00:00:00", format = "%m-%d-%Y %H:%M:%S"),
                              traffic_model="best_guess",  key="AIzaSyDwg38QJIog7fJF2qXxnlj9ae54t8xTWTU",simplify = TRUE )
        
          #Se almacenan los resultados de la consulta
          
          datos_free_flow$Longitud_1[i]<-distancia$rows$elements[[1]]$distance$value
          datos_free_flow$TProm_1[i]<-distancia$rows$elements[[1]]$duration$value
          datos_free_flow$TFF_1[i]<-distancia$rows$elements[[1]]$duration_in_traffic$value
      
          
        #Diracción 2
        
          distancia <- google_distance(origins = c(destino) ,destinations = c(origen) ,
                            mode="driving",  departure_time =as.POSIXct("09-4-2018 00:00:00", format = "%m-%d-%Y %H:%M:%S"),
                            traffic_model="best_guess",  key="AIzaSyDwg38QJIog7fJF2qXxnlj9ae54t8xTWTU",simplify = TRUE )
      
          #Se almacenan los resultados de la consulta
          
          datos_free_flow$Longitud_2[i]<-distancia$rows$elements[[1]]$distance$value
          datos_free_flow$TProm_2[i]<-distancia$rows$elements[[1]]$duration$value
          datos_free_flow$TFF_2[i]<-distancia$rows$elements[[1]]$duration_in_traffic$value
    }

  #Se guarda la Data (Resultados Consultas Google API)
    
    save(datos_free_flow,file=paste0(ruta_resultados,"2-Resultados_Consultas_Google_API.Rdata"))
    
