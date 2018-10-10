#Preparación del entorno de trabajo----

  #Se cargan las siguiente librerias
  
    library(cluster)
    library(tidyverse)
    library(sf)
    library(tmap)
    
  #Se define las ruta de las bases de datos
  
    ruta_resultados <-"/Users/alejandropalacio/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/RESULTADOS/LTS/Bases de Datos/"
  
  #Se carga la Data
  
    load(paste0(ruta_resultados,"Capas Clustering LTS/Resultados_Clustering_Usaquen.Rdata"))
    load(paste0(ruta_resultados,"Capas Clustering LTS/Resultados_Clustering_Suba.Rdata"))
    load(paste0(ruta_resultados,"Capas Clustering LTS/Resultados_Clustering_Kennedy.Rdata"))
    
    
    load(paste0(ruta_resultados,"Capas Prediccion LTS Logit/Capa_Predicción_LTS_Logit_Usaquen.Rdata"))
    load(paste0(ruta_resultados,"Capas Prediccion LTS Logit/Capa_Predicción_LTS_Logit_Suba.Rdata"))
    load(paste0(ruta_resultados,"Capas Prediccion LTS Logit/Capa_Predicción_LTS_Logit_Kennedy.Rdata"))

    
#Se asignan los valores de LTS a las capas   
    
    capa_LTS_PAM_Usaquen <- capa_LTS_PAM_Usaquen %>% mutate(Cluster=ifelse(clusters_PAM==4,1,ifelse(clusters_PAM==3,2,ifelse(clusters_PAM==2,3,4))))
    capa_LTS_PAM_Usaquen[,c("ID","LocNombre","Segregada","Direccion","clusters_PAM")]<-NULL
    capa_LTS_PAM_Usaquen <- capa_LTS_PAM_Usaquen %>% st_set_geometry(NULL)
    
    
    capa_LTS_PAM_Suba <- capa_LTS_PAM_Suba %>% mutate(Cluster=ifelse(clusters_PAM==4,3,ifelse(clusters_PAM==3,1,ifelse(clusters_PAM==2,4,2))))
    capa_LTS_PAM_Suba[,c("ID","LocNombre","Segregada","Direccion","clusters_PAM")]<-NULL
    capa_LTS_PAM_Suba <- capa_LTS_PAM_Suba %>% st_set_geometry(NULL)
    
    capa_LTS_PAM_Kennedy <- capa_LTS_PAM_Kennedy %>% mutate(Cluster=ifelse(clusters_PAM==4,1,ifelse(clusters_PAM==3,3,ifelse(clusters_PAM==2,4,2))))
    capa_LTS_PAM_Kennedy [,c("ID","LocNombre","Segregada","Direccion","clusters_PAM")]<-NULL
    capa_LTS_PAM_Kennedy <- capa_LTS_PAM_Kennedy %>% st_set_geometry(NULL)
    
    Capa_Variables_Prediccion_Usaquen <- Capa_Variables_Prediccion_Usaquen %>% st_as_sf() %>% mutate(Cluster=ifelse(X4>=0.5,1,ifelse(X3>=0.5,2,ifelse(X2>=0.5,3,4))))
    Capa_Variables_Prediccion_Usaquen <- Capa_Variables_Prediccion_Usaquen %>% filter(LocNombre=="KENNEDY")
    Capa_Variables_Prediccion_Usaquen[,c("ID","LocNombre","X1","X2","X3","X4","Segregada","Direccion")]<-NULL
    Capa_Variables_Prediccion_Usaquen <- Capa_Variables_Prediccion_Usaquen%>% st_set_geometry(NULL)
    
    Capa_Variables_Prediccion_Suba <- Capa_Variables_Prediccion_Suba %>% st_as_sf() %>% mutate(Cluster=ifelse(X4>=0.5,3,ifelse(X3>=0.5,1,ifelse(X2>=0.5,4,2))))
    Capa_Variables_Prediccion_Suba <- Capa_Variables_Prediccion_Suba %>% filter(LocNombre=="KENNEDY")
    Capa_Variables_Prediccion_Suba[,c("ID","LocNombre","X1","X2","X3","X4","Segregada","Direccion")]<-NULL
    Capa_Variables_Prediccion_Suba <- Capa_Variables_Prediccion_Suba %>% st_set_geometry(NULL)
    
    Capa_Variables_Prediccion_Kennedy<- Capa_Variables_Prediccion_Kennedy %>% st_as_sf() %>% mutate(Cluster=ifelse(X4>=0.5,1,ifelse(X3>=0.5,3,ifelse(X2>=0.5,4,2))))
    Capa_Variables_Prediccion_Kennedy <- Capa_Variables_Prediccion_Kennedy %>% filter(LocNombre=="SUBA")
    Capa_Variables_Prediccion_Kennedy[,c("ID","LocNombre","X1","X2","X3","X4","Segregada","Direccion")]<-NULL
    Capa_Variables_Prediccion_Kennedy <- Capa_Variables_Prediccion_Kennedy %>% st_set_geometry(NULL)

    
    pacman::p_load(crossmatch)
    
    for (i in 1:4){
    x<-capa_LTS_PAM_Kennedy %>%  filter(Cluster==i)
    ind_x<-1:dim(x)[1]
    x$Cluster<-NULL
    y<-Capa_Variables_Prediccion_Suba %>% filter(Cluster==i)
    ind_y<-1:dim(y)[1]
    y$Cluster<-NULL
    n_use<-min(dim(x)[1],dim(y)[1],100)
    x<-x[sample(ind_x,n_use),]
    y<-y[sample(ind_y,n_use),]
    class<-c(rep(0,n_use),rep(1,n_use))
    obs<-rbind(x,y)
    obs$CicloRuta<-as.factor(obs$CicloRuta)
    obs$SITP<-as.factor(obs$SITP)
    obs$Ancho<-as.numeric(obs$Ancho)
    obs$Carriles<-as.numeric(obs$Carriles)
    obs$Velocidad<-as.numeric(obs$Velocidad)
    obs$Congestion<-as.numeric(obs$Congestion)
    obs$Densidad<-as.numeric(obs$Densidad)
    obs$Flujo<-as.numeric(obs$Flujo)
    dist<-daisy(obs, metric = "gower", stand=FALSE)
    dist<-as.matrix(dist)
    print(i)
    print(crossmatchtest(class,dist))
    }
    