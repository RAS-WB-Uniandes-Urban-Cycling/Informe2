#Preparación del entorno de trabajo----

  #Se cargan las siguiente librerias

    library(cluster)
    library(tidyverse)
    library(sf)
    library(tmap)

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
    
#Clustering Hierarchical Clustering----   
    
    clusters_Hclust <- hclust(dist,method ="single")
    grafico_HClust <- clusters_Hclust
    clusters_Hclust <- cutree(clusters_Hclust, 4) 
    table(clusters_Hclust)
    
    capa_LTS_Hclust<- cbind.data.frame(capa_variables_LTS,data.frame(clusters_Hclust)) %>% st_as_sf %>% mutate(LTS="")
 
    mapa<-tm_shape(capa_LTS_Hclust)+tm_lines(col="clusters_Hclust",style ="cat" ,scale=5 ,palette = "Accent" ,title.col ="Cluster", popup.vars = TRUE)+tmap_mode("view")+tm_view(alpha = 1, basemaps = "OpenStreetMap.BlackAndWhite")
    mapa
    
    capa_LTS_Hclust$clusters_Hclust <- ifelse(capa_LTS_Hclust$clusters_Hclust==3, 1, ifelse(capa_LTS_Hclust$clusters_Hclust==2, 2,ifelse(capa_LTS_Hclust$clusters_Hclust==4, 3,4)))
    
#Clustering PAM Algorithm----
    
  #Numero de clusters por el metodo Silhoutte   
    
    silhouette_with <- c(NA)
    
    for(i in 2:10){
      
      clusters_PAM <- pam(dist,diss = TRUE,k = i)
      
      silhouette_with[i] <-clusters_PAM$silinfo$avg.width
      
    }
    
  #Se plotea silhouette width
    
    plot(1:10, silhouette_with,xlab = "Number of clusters",ylab = "Silhouette Width")
    lines(1:10, silhouette_with)
    
  #Se hace hace clustering
    
    clusters_PAM <- pam(dist,diss = TRUE, k = 4) 
    
    clusters_PAM <- as.data.frame(clusters_PAM$clustering) %>% transmute(clusters_PAM=clusters_PAM$clustering)
    
    capa_LTS_PAM<- cbind.data.frame(capa_variables_LTS,clusters_PAM) %>% st_as_sf %>% mutate(LTS="")
    
    mapa<-tm_shape(capa_LTS_PAM)+tm_lines(col="clusters_PAM",style ="cat" ,scale=5 ,palette = "Accent" ,title.col ="Cluster", popup.vars = TRUE)+tmap_mode("view")+tm_view(alpha = 1, basemaps = "OpenStreetMap.BlackAndWhite")
    mapa
    
    capa_LTS_PAM$clusters_PAM <- ifelse(capa_LTS_PAM$clusters_PAM==3, 1, ifelse(capa_LTS_PAM$clusters_PAM, 2,ifelse(capa_LTS_PAM$clusters_PAM==4, 3,4)))
    
#Se alamcenan los resultados----
    
  #Se guarda la Data (Resutados Clustering)
    
    save(capa_LTS_Hclust,capa_LTS_PAM,silhouette_with,grafico_HClust,file=paste0(ruta_resultados,"Resultados_Clustering.Rdata"))
    
  #Se eliminan los datos que no se usaran  
    
    rm(capa_clusters,capa_variables_LTS,clusters_PAM,mapa, ruta_resultados,silhouette_with,grafico_HClust)
  