#Preparación del entorno de trabajo----

  #Se cargan las siguiente librerias

    library(cluster)
    library(tidyverse)
    library(sf)

  #Se define las ruta de las bases de datos

    ruta_resultados <-"/Users/alejandropalacio/Documents/GitHub/proBikePolicies/LTS/Resultados/"

  #Se carga la Data

    load(paste0(ruta_resultados, "Variables_LTS.Rdata"))
    
#Matriz de distancia metodo Gower----
    
  #Se normalizan las variables de la capa_variables_LTS  
  
    capa_clusters<-st_set_geometry(capa_variables_LTS,NULL) %>% transmute(CicloRuta,SITP,Vprom=scale(as.matrix(capa_variables_LTS$Vprom)),
      Trafico=scale(as.matrix(capa_variables_LTS$Trafico)),Ancho=scale(as.matrix(capa_variables_LTS$Ancho)),Carriles=scale(as.matrix(capa_variables_LTS$Carriles)))
    
  #Se define el tipo de cada variables as.factor o as.numeric
    
    capa_clusters$CicloRuta<-as.factor(capa_clusters$CicloRuta)
    capa_clusters$SITP<-as.factor(capa_clusters$SITP)
    capa_clusters$Vprom<-as.numeric(capa_clusters$Vprom)
    capa_clusters$Trafico<-as.numeric(capa_clusters$Trafico)
    capa_clusters$Carriles<-as.numeric(capa_clusters$Carriles)
    capa_clusters$Ancho<-as.numeric(capa_clusters$Ancho)
    
  #Se crea la matriz de distancias por el metodo de Gower
    
    dist<-daisy(capa_clusters, metric = "gower", stand=FALSE)
    
#Clustering Kmeans   
    
    resultados_clusters_Kmeans <- hclust(dist,method ="single")
    plot(resultados_clusters_Kmeans)
    resultados_clusters_Kmeans <- cutree(resultados_clusters_Kmeans, 4) 
    table(resultados_clusters_Kmeans)
    
    capa_LTS<- cbind.data.frame(capa_variables_LTS,data.frame(resultados_clusters_Kmeans)) %>% st_as_sf

 
#Clustering PAM Algorithm
    
  #Numero de clusters por el metodo Silhoutte   
    
    silhouette_with <- c(NA)
    
    for(i in 2:10){
      
      resultados_clusters_PAM <- pam(dist,diss = TRUE,k = i)
      
      silhouette_with[i] <- resultados_clusters_PAM$silinfo$avg.width
      
    }
    
  # Se plotea silhouette width
    
    plot(1:10, silhouette_with,xlab = "Number of clusters",ylab = "Silhouette Width")
    lines(1:10, silhouette_with)
    
    pam_fit <- pam(dist,diss = TRUE, k = 4)
    pam_fit <- as.data.frame(pam_fit$clustering) %>% mutate(ID=row_number()) %>% rename(c("pam_fit$clustering"="cluster"))
    
    capa_LTS2<- st_set_geometry(capa_malla_vial,NULL) %>% left_join(pam_fit,By=ID)  %>% transmute(Vprom,Ancho_Calzada,Numero_Carriles,Trafico)
    
    
    
    
    mapa_Velocidad<-tm_shape(capa_Clusters)+tm_lines(col="resultados_clusters",style ="cat" ,scale=5 ,palette = "Accent" ,title.col ="Velocidad Promedio", popup.vars = TRUE)+tmap_mode("view")+tm_view(alpha = 1, basemaps = "OpenStreetMap.BlackAndWhite")
    mapa_Velocidad
    
    
