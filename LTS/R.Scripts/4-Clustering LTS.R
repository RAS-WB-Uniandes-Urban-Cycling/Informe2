#Preparación del entorno de trabajo----

  #Se cargan las siguiente librerias

    library(cluster)
    library(tidyverse)
    library(sf)
    library(tmap)

  #Se define las ruta de las bases de datos

    ruta_resultados <-"/Users/alejandropalacio/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/RESULTADOS/LTS/Bases de Datos/"

  #Se carga la Data

    load(paste0(ruta_resultados, "5-Variables_LTS.Rdata"))
    
    
    capa_variables_LTS <-capa_variables_LTS %>%  filter(LocNombre %in% c("SUBA"))
    
#Matriz de distancia metodo Gower----
    
  #Se normalizan las variables de la capa_variables_LTS 
    
    capa_clusters<-st_set_geometry(capa_variables_LTS,NULL) %>% transmute(CicloRuta,SITP,Ancho=scale(as.matrix(capa_variables_LTS$Ancho)),Carriles=scale(as.matrix(capa_variables_LTS$Carriles)),
                   Velocidad=scale(as.matrix(capa_variables_LTS$Velocidad)),Congestion=scale(as.matrix(capa_variables_LTS$Congestion)),Densidad=scale(as.matrix(capa_variables_LTS$Densidad)),
                   Flujo=scale(as.matrix(capa_variables_LTS$Flujo))) 
    
  #Se define el tipo de cada variables as.factor o as.numeric
    
    capa_clusters$CicloRuta<-as.factor(capa_clusters$CicloRuta)
    capa_clusters$SITP<-as.factor(capa_clusters$SITP)
    capa_clusters$Ancho<-as.numeric(capa_clusters$Ancho)
    capa_clusters$Carriles<-as.numeric(capa_clusters$Carriles)
    capa_clusters$Velocidad<-as.numeric(capa_clusters$Velocidad)
    capa_clusters$Congestion<-as.numeric(capa_clusters$Congestion)
    capa_clusters$Densidad<-as.numeric(capa_clusters$Densidad)
    capa_clusters$Flujo<-as.numeric(capa_clusters$Flujo)
    
  #Se crea la matriz de distancias por el metodo de Gower
    
    dist<-daisy(capa_clusters, metric = "gower", stand=FALSE)

#Clustering PAM Algorithm----
    
  #Numero de clusters por el metodo Silhoutte   
    
    silhouette_with <- c(NA)
    
    for(i in 3:6){
      
      clusters_PAM <- pam(dist,diss = TRUE,k = i)
      
      silhouette_with[i] <-clusters_PAM$silinfo$avg.width
      
    }
    
  #Se plotea silhouette width
    
    plot(1:6, silhouette_with,xlab = "Number of clusters",ylab = "Silhouette Width")
    lines(1:6, silhouette_with)
    
  #Se hace hace clustering
    
    clusters_PAM <- pam(dist,diss = TRUE, k = 4) 
    
    clusters_PAM <- as.data.frame(clusters_PAM$clustering) %>% transmute(clusters_PAM=clusters_PAM$clustering)
    
    capa_LTS_PAM<- cbind.data.frame(capa_variables_LTS,clusters_PAM) %>% st_as_sf 
    
     mapa<-tm_shape(capa_LTS_PAM)+tm_lines(col="clusters_PAM",style ="cat" ,scale=5 ,palette = "Accent" ,title.col ="Cluster", popup.vars = TRUE)+tmap_mode("view")+tm_view(alpha = 1, basemaps = "OpenStreetMap.BlackAndWhite")
    mapa
    
    mapa<-tm_shape(capa_variables_LTS)+tm_lines(col="CicloRuta",style ="cat" ,scale=5 ,palette = "Accent" ,title.col ="Cluster", popup.vars = TRUE)+tmap_mode("view")+tm_view(alpha = 1, basemaps = "OpenStreetMap.BlackAndWhite")
    mapa
    
#Se alamcenan los resultados----
    
  #Se guarda la Data (Resutados Clustering)
    
    save(capa_LTS_PAM,file=paste0(ruta_resultados,"Resultados_Clustering_Suba.Rdata"))
    
  #Se eliminan los datos que no se usaran  
    
    rm(capa_clusters,capa_variables_LTS,clusters_PAM,mapa, ruta_resultados,silhouette_with)
  