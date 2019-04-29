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
    
    
    capa_variables_LTS <-capa_variables_LTS %>%  filter(LocNombre %in% c("USAQUEN"))
    
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
    
    capa_LTS_PAM_Usaquen<- cbind.data.frame(capa_variables_LTS,clusters_PAM) %>% st_as_sf 
    
       
     
#Grafica radar----

    estadisticas1 <- capa_LTS_PAM_Usaquen %>% filter(clusters_PAM==1)
    estadisticas2 <- capa_LTS_PAM_Usaquen %>% filter(clusters_PAM==2)
    estadisticas3 <- capa_LTS_PAM_Usaquen %>% filter(clusters_PAM==3)
    estadisticas4 <- capa_LTS_PAM_Usaquen %>% filter(clusters_PAM==4)
    
    
    datos<-as.data.frame(matrix(nrow=4,ncol=8))
    colnames(datos)=c("Roadway width (m)", "Number of lanes" , "Vehicles speed (km/h)" , "Traffic density","Traffic flow","Congestion" ,"Cycling infrastructure","Heavy vehicles" )
    rownames(datos)=c("Cluster-1", "Cluster-2", "Cluster-3", "Cluster-4")
    
    
    datos$`Roadway width (m)`[1]=mean(estadisticas1$Ancho)
    datos$`Roadway width (m)`[2]=mean(estadisticas2$Ancho)
    datos$`Roadway width (m)`[3]=mean(estadisticas3$Ancho)
    datos$`Roadway width (m)`[4]=mean(estadisticas4$Ancho)
    
    datos$`Number of lanes`[1]=mean(estadisticas1$Carriles)
    datos$`Number of lanes`[2]=mean(estadisticas2$Carriles)
    datos$`Number of lanes`[3]=mean(estadisticas3$Carriles)
    datos$`Number of lanes`[4]=mean(estadisticas4$Carriles)
    
    datos$`Vehicles speed (km/h)`[1]=mean(estadisticas1$Velocidad)
    datos$`Vehicles speed (km/h)`[2]=mean(estadisticas2$Velocidad)
    datos$`Vehicles speed (km/h)`[3]=mean(estadisticas3$Velocidad)
    datos$`Vehicles speed (km/h)`[4]=mean(estadisticas4$Velocidad)
    
    datos$`Traffic density`[1]=mean(estadisticas1$Densidad)
    datos$`Traffic density`[2]=mean(estadisticas2$Densidad)
    datos$`Traffic density`[3]=mean(estadisticas3$Densidad)
    datos$`Traffic density`[4]=mean(estadisticas4$Densidad)
    
    datos$`Traffic flow`[1]=mean(estadisticas1$Flujo)
    datos$`Traffic flow`[2]=mean(estadisticas2$Flujo)
    datos$`Traffic flow`[3]=mean(estadisticas3$Flujo)
    datos$`Traffic flow`[4]=mean(estadisticas4$Flujo)
    
    datos$Congestion[1]=mean(estadisticas1$Congestion)
    datos$Congestion[2]=mean(estadisticas2$Congestion)
    datos$Congestion[3]=mean(estadisticas3$Congestion)
    datos$Congestion[4]=mean(estadisticas4$Congestion)
    
    datos$`Cycling infrastructure`[1]=nrow(estadisticas1 %>% filter(CicloRuta==1))/(nrow(estadisticas1 %>% filter(CicloRuta==1))+nrow(estadisticas1 %>% filter(CicloRuta==0)))
    datos$`Cycling infrastructure`[2]=nrow(estadisticas2 %>% filter(CicloRuta==1))/(nrow(estadisticas2 %>% filter(CicloRuta==1))+nrow(estadisticas2 %>% filter(CicloRuta==0)))
    datos$`Cycling infrastructure`[3]=nrow(estadisticas3 %>% filter(CicloRuta==1))/(nrow(estadisticas3 %>% filter(CicloRuta==1))+nrow(estadisticas3 %>% filter(CicloRuta==0)))
    datos$`Cycling infrastructure`[4]=nrow(estadisticas4 %>% filter(CicloRuta==1))/(nrow(estadisticas4 %>% filter(CicloRuta==1))+nrow(estadisticas4 %>% filter(CicloRuta==0)))
    
    datos$`Heavy vehicles`[1]=nrow(estadisticas1 %>% filter(SITP==1))/(nrow(estadisticas1 %>% filter(SITP==1))+nrow(estadisticas1 %>% filter(SITP==0)))
    datos$`Heavy vehicles`[2]=nrow(estadisticas2 %>% filter(SITP==1))/(nrow(estadisticas2 %>% filter(SITP==1))+nrow(estadisticas2 %>% filter(SITP==0)))
    datos$`Heavy vehicles`[3]=nrow(estadisticas3 %>% filter(SITP==1))/(nrow(estadisticas3 %>% filter(SITP==1))+nrow(estadisticas3 %>% filter(SITP==0)))
    datos$`Heavy vehicles`[4]=nrow(estadisticas4 %>% filter(SITP==1))/(nrow(estadisticas4 %>% filter(SITP==1))+nrow(estadisticas4 %>% filter(SITP==0)))
      
    # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
    
    datos=rbind(c(max(datos$`Roadway width (m)`),max(datos$`Number of lanes`),max(datos$`Vehicles speed (km/h)`),max(datos$`Traffic density`),max(datos$`Traffic flow`),max(datos$Congestion),1,1),
                c(0,0,0,0,0,0,0,0,0) , datos)
    
    colors_border=c("lime green","dark green","orange","Red")
    
    library(fmsb)
    
    radarchart( datos  , axistype=1.5 , 
                #custom polygon
                pcol=colors_border  , plwd=4 , plty=1,
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.9, caxislabels=seq(0,1,0.25),
                #custom labels
                vlcex=0.6 ,
                vlabels = c(paste0("Roadway\nwidth (m)\n[0-",round(max(datos$`Roadway width (m)`),2),"]"), paste0("Number of\nlanes\n[0-",round(max(datos$`Number of lanes`),2),"]") , paste0("Vehicles\nspeed (km/h)\n[0-",round(max(datos$`Vehicles speed (km/h)`),2),"]") , paste0("Traffic\ndensity (cars/h)\n[0-",round(max(datos$`Traffic density`),2),"]"),paste0("Traffic\nflow (cars/km)\n[0-",round(max(datos$`Traffic flow`),2),"]"),paste0("Congestion\n[0-",round(max(datos$Congestion),2),"]") ,"% Cycling\ninfrastructure\n[0-1]","% Heavy\nvehicles\n[0-1]" )
    )
    
    legend(x=1.1, y=-0.6, legend = rownames(datos[-c(1,2),]), bty = "n", pch=20 , col=colors_border , text.col = "grey", cex=1.3, pt.cex=3)
    
    
    
    
    
      
    
    
    
    
#Se alamcenan los resultados----
    
  #Se guarda la Data (Resutados Clustering)
    
    save(capa_LTS_PAM_Kennedy,file=paste0(ruta_resultados,"Resultados_Clustering_Kennedy.Rdata"))
    
  #Se eliminan los datos que no se usaran  
    
    rm(capa_clusters,capa_variables_LTS,clusters_PAM,mapa, ruta_resultados,silhouette_with)
  