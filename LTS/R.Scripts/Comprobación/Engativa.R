#Preparación del entorno de trabajo----

  #Se cargan las siguiente librerias

    library(cluster)
    library(tidyverse)
    library(sf)
    library(tmap)
    library(fmsb)
    library(reshape)
    library(units)
    library(nnet)
    library(reshape2)
    library(ggplot2)  
    library(tidyverse)
    library(mlogit)
    library(Hmisc)

  #Se define las ruta de las bases de datos

    ruta_base_datos<-"/Users/alejandropalacio/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/BASES DE DATOS/"
    ruta_resultados <-"/Users/alejandropalacio/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/RESULTADOS/LTS/Bases de Datos/"

  #Se almacena los layer en cada variable definida  
    
    layer_localidad<-st_read(paste0(ruta_base_datos,"Mapas de Referencia IDECA/MR0318.gdb"),layer = "Loca",stringsAsFactors = FALSE) %>% st_transform(4326)
    
  #Se carga la Data

    load(paste0(ruta_resultados, "5-Variables_LTS.Rdata"))
    load(paste0(ruta_resultados, "Accidentes.Rdata"))
    
#Se filtra por la localidad a analizar----
    
    capa_variables_LTS_Final <- capa_variables_LTS  
    capa_variables_LTS <-capa_variables_LTS %>%  filter(LocNombre %in% c("ENGATIVA"))
    
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

#Clustering PAM Algorithm-Silhouette----
    
  #Numero de clusters por el metodo Silhoutte   
    
    silhouette_with <- c(NA)
    
    for(i in 3:6){
      
      clusters_PAM <- pam(dist,diss = TRUE,k = i)
      
      silhouette_with[i] <-clusters_PAM$silinfo$avg.width
      
    }
    
   #Se plotea silhouette width
    
    plot(1:6, silhouette_with,xlab = "Number of clusters",ylab = "Silhouette Width")
    lines(1:6, silhouette_with)
    
#Clustering PAM Algorithm----    
    
  #Se hace hace clustering
    
    clusters_PAM <- pam(dist,diss = TRUE, k = 4) 
    
    clusters_PAM <- as.data.frame(clusters_PAM$clustering) %>% transmute(Cluster=clusters_PAM$clustering)
    
    capa_Cluster_Localidad <- cbind.data.frame(capa_variables_LTS,clusters_PAM) %>% st_as_sf

    
#Grafica radar----

    #Se alamcena los datos por cluster
    
    estadisticas1 <- capa_Cluster_Localidad %>% filter(Cluster==1)
    estadisticas2 <- capa_Cluster_Localidad %>% filter(Cluster==2)
    estadisticas3 <- capa_Cluster_Localidad %>% filter(Cluster==3)
    estadisticas4 <- capa_Cluster_Localidad %>% filter(Cluster==4)
    
    #Se crear un data frame para almacenar los resultados de cada cluster por variable
    
    datos<-as.data.frame(matrix(nrow=4,ncol=8))
    colnames(datos)=c("Roadway width (m)", "Number of lanes" , "Vehicles speed (km/h)" , "Traffic density","Traffic flow","Congestion" ,"Cycling infrastructure","Heavy vehicles" )
    rownames(datos)=c("Cluster-1", "Cluster-2", "Cluster-3", "Cluster-4")
    
    #Se calculan las estadisticas por cluster y variable
    
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
      
    #Se halla y el minimo de cada categoria
    
    datos=rbind(c(max(datos$`Roadway width (m)`),max(datos$`Number of lanes`),max(datos$`Vehicles speed (km/h)`),max(datos$`Traffic density`),max(datos$`Traffic flow`),max(datos$Congestion),1,1),
                c(0,0,0,0,0,0,0,0,0) , datos)
    
    colors_border=c("lime green","Red","dark green","orange")
    
    #Se genera el radar
    
    radarchart( datos  , axistype=1.5 , 
                pcol=colors_border  , plwd=4 , plty=1,
                cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.9, caxislabels=seq(0,1,0.25),
                vlcex=0.9 ,
                vlabels = c(paste0("Roadway\nwidth (m)\n[0-",round(max(datos$`Roadway width (m)`),2),"]"), paste0("Number of\nlanes\n[0-",round(max(datos$`Number of lanes`),2),"]") , paste0("Vehicles\nspeed (km/h)\n[0-",round(max(datos$`Vehicles speed (km/h)`),2),"]") , paste0("Traffic\ndensity (cars/h)\n[0-",round(max(datos$`Traffic density`),2),"]"),paste0("Traffic\nflow (cars/km)\n[0-",round(max(datos$`Traffic flow`),2),"]"),paste0("Congestion\n[0-",round(max(datos$Congestion),2),"]") ,"% Cycling\ninfrastructure\n[0-1]","% Heavy\nvehicles\n[0-1]" )
    )
    
    legend(x=1, y=-0.90, legend = rownames(datos[-c(1,2),]), bty = "n", pch=20 , col=colors_border , text.col = "grey", cex=1, pt.cex=2)
    
#Asignacion LTS a intersecciones----
    
    #Se asigna el valor del LTS a cada cluster
    
    capa_LTS_Localidad <- capa_Cluster_Localidad %>% mutate(ID=row_number(),LTS=0)
    capa_LTS_Localidad$LTS <- ifelse(capa_LTS_Localidad$Cluster==1,1, ifelse(capa_LTS_Localidad$Cluster==2,4, ifelse(capa_LTS_Localidad$Cluster==3,2,3))) 
    
    #Se guarda el data frame
    capa_cluster_Engativa <- capa_LTS_Localidad[ , !(names(capa_LTS_Localidad) %in% c("ID","LocNombre","Segregada","Direccion","Cluster"))] 
    capa_cluster_Engativa<- capa_cluster_Engativa %>% st_set_geometry(NULL)
    
    save(capa_cluster_Engativa,file=paste0(ruta_resultados,"Capas Clustering LTS/Resultados_Clustering_Engativa.Rdata"))
    
    #Se obtienen las coordenadas (Lat/Long) de punto inicial de cada segmento la capa_malla_vial
    
    coordenadas_incio_seg<-st_coordinates(capa_LTS_Localidad) %>% as_tibble()%>% group_by(L1)%>% filter(row_number()==1) %>% 
      ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud",Y="Latitud"))
    
    coordenadas_fin_seg<-st_coordinates(capa_LTS_Localidad) %>% as_tibble() %>% group_by(L1) %>% filter(row_number()==n()) %>% 
      ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud",Y="Latitud"))
    
    #Se unen las dos trablas de coordenadas
    
    capa_Provisional_1<-capa_LTS_Localidad %>% transmute(ID,LTS)  %>% left_join(coordenadas_incio_seg, By=ID)
    capa_Provisional_2<-capa_LTS_Localidad %>% transmute(ID,LTS)  %>% left_join(coordenadas_fin_seg, By=ID)
    
    coordenadas_malla_vial <- capa_Provisional_1%>% rbind(capa_Provisional_2) %>% st_set_geometry(NULL)
    
    #Se cuentan y agrupan los repetidos
    
    capa_Intersecciones <- coordenadas_malla_vial %>% group_by(Latitud, Longitud) %>% summarise(Numero_Intersecciones=n(),LTS=max(LTS)) %>% filter(Numero_Intersecciones>=3) %>% ungroup()
    
    #Se genera la capa de intersecciones
    
    capa_Intersecciones <- capa_Intersecciones   %>% st_as_sf(coords=c(2,1)) %>% mutate(ID_Punto=row_number()) %>% st_set_crs(4326) 
    
    #Se hace un join espacial para determinar la localidad
    
    capa_Intersecciones  <-capa_Intersecciones  %>% st_join(select(layer_localidad,LocNombre)) %>% rename(c(LocNombre="Localidad"))
    
    
#Cruce accidentes----
    
    #Se determina en que LTS sucedio el accidente
    
    capa_Provisional1 <- st_buffer(capa_Accidentes, 0.0002) %>% mutate(ID=row_number()) %>% st_join(capa_Intersecciones[,c("LTS")],largest = TRUE) %>% mutate(LTSIn=LTS)  %>%  st_set_geometry(NULL)
    capa_Provisional2 <- st_buffer(capa_Accidentes, 0.0002) %>% mutate(ID=row_number()) %>% st_join(capa_LTS_Localidad[,c("LTS")],largest = TRUE) %>% mutate(LTSSeg=LTS) %>% st_set_geometry(NULL)
    capa_Accidentes <- capa_Accidentes %>% mutate(ID=row_number()) %>% left_join(select(capa_Provisional1, LTSIn, ID)) 
    capa_Accidentes <- capa_Accidentes %>% mutate(ID=row_number()) %>% left_join(select(capa_Provisional2, LTSSeg, ID)) %>% mutate(LTS=ifelse(!is.na(LTSIn),LTSIn,LTSSeg))
    
    #Se halla la distancia de cada segmento
    
    capa_LTS_Localidad <- capa_LTS_Localidad  %>% mutate(Distancia=st_length(capa_LTS_Localidad ))
    
    #Se crea la varible Año en capa accidentes y se filtra para el año 2017
    
    capa_Accidentes <- capa_Accidentes %>% mutate(Ano=format(as.Date(Accidentes.Fecha, format="%d/%m/%Y"),"%Y"))
    capa_Accidentes <- capa_Accidentes%>% filter(Ano=="2017")
    
    #Se generan las estadisticas
    
    estadisticas_accidentes<-as.data.frame(matrix(nrow=4,ncol=6))
    colnames(estadisticas_accidentes)=c("Km LTS","# Segmentos","# Accidentes Fatales","# Accidentes No Fatales","# Accidentes Fatales/Km LTS","# Accidentes No Fatales/Km LTS" ) 
    rownames(estadisticas_accidentes)=c("LTS 1", "LTS 2", "LTS 3", "LTS 4")
    
    for(i in 1:4){
      
      estadisticas_accidentes$`# Accidentes Fatales`[i] <-  nrow(capa_Accidentes %>% filter(Gravedad2=="Dead") %>% filter(LTS==i))
      estadisticas_accidentes$`# Accidentes No Fatales`[i] <-  nrow(capa_Accidentes %>% filter(Gravedad2=="Not Dead") %>% filter(LTS==i))
      
      capa_provisional <- capa_LTS_Localidad %>% filter(LTS==i)
      estadisticas_accidentes$`# Segmentos`[i] <- nrow(capa_provisional)
      estadisticas_accidentes$`Km LTS`[i] <- drop_units(sum(capa_provisional$Distancia))/1000
      
      estadisticas_accidentes$`# Accidentes Fatales/Km LTS`[i] <- estadisticas_accidentes$`# Accidentes Fatales`[i]/estadisticas_accidentes$`Km LTS`[i]
      estadisticas_accidentes$`# Accidentes No Fatales/Km LTS` [i] <- estadisticas_accidentes$`# Accidentes No Fatales`[i]/estadisticas_accidentes$`Km LTS`[i]
    }
    
    
#Regresion logistica multinomial----
    
    #Se almacena la capa_LTS_PAM en capa_variables_LTS_model    
    
    capa_LTS_Localidad$LTS <-   as.numeric(capa_LTS_Localidad$LTS)
    
    capa_variables_LTS_model <-capa_LTS_Localidad
    
    #Se declara la regresion Logit Multinomial con un método de clustering elegido
  
    logit_Multi<-multinom(capa_variables_LTS_model$LTS~Velocidad+Ancho+Carriles+CicloRuta+SITP+Congestion+Densidad+Flujo, data=capa_variables_LTS_model)
    
   
    #Resultados Logit Multinomial con base en LTS 1
    
    summary(logit_Multi)
    
    #Modelo predictivo:
    
    predicted=predict(logit_Multi,capa_variables_LTS_Final,type="probs")
    
    capa_variables_LTS_Final <- cbind(capa_variables_LTS_Final,predicted)
    
    capa_variables_LTS_Final <- capa_variables_LTS_Final %>% mutate(LTS=0)
    
    capa_variables_LTS_Final$LTS <- ifelse(capa_variables_LTS_Final$X1>0.5,1,ifelse(capa_variables_LTS_Final$X2>0.5,2,ifelse(capa_variables_LTS_Final$X3>0.5,3,4)))
    
    #Se guarda el data frame
    capa_regresion_Engativa <- capa_variables_LTS_Final[ , !(names(capa_LTS_Localidad) %in% c("ID","Segregada","Direccion","Cluster","X1","X2","X3","X4","Distancia"))] 
    capa_regresion_Engativa<- capa_regresion_Engativa %>% st_set_geometry(NULL)
    
    save(capa_regresion_Engativa,file=paste0(ruta_resultados,"Capas Prediccion LTS Logit/Resultados_Regresion_Engativa.Rdata"))
  
#Grafica radar capa completa----
    
    #Se alamcena los datos por LTS
    
    estadisticas1 <- capa_variables_LTS_Final %>% filter(LTS==1)
    estadisticas2 <- capa_variables_LTS_Final %>% filter(LTS==2)
    estadisticas3 <- capa_variables_LTS_Final %>% filter(LTS==3)
    estadisticas4 <- capa_variables_LTS_Final %>% filter(LTS==4)
    
    #Se crear un data frame para almacenar los resultados de cada cluster por variable
    
    datos<-as.data.frame(matrix(nrow=4,ncol=8))
    colnames(datos)=c("Roadway width (m)", "Number of lanes" , "Vehicles speed (km/h)" , "Traffic density","Traffic flow","Congestion" ,"Cycling infrastructure","Heavy vehicles" )
    rownames(datos)=c("LTS 1", "LTS 2", "LTS 3", "LTS 4")
    
    #Se calculan las estadisticas por cluster y variable
    
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
    
    #Se halla y el minimo de cada categoria
    
    datos=rbind(c(max(datos$`Roadway width (m)`),max(datos$`Number of lanes`),max(datos$`Vehicles speed (km/h)`),max(datos$`Traffic density`),max(datos$`Traffic flow`),max(datos$Congestion),1,1),
                c(0,0,0,0,0,0,0,0,0) , datos)
    
    colors_border=c("lime green","dark green","orange","Red")
    
    #Se genera el radar
    
    radarchart( datos  , axistype=1.5 , 
                pcol=colors_border  , plwd=4 , plty=1,
                cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.9, caxislabels=seq(0,1,0.25),
                vlcex=0.9 ,
                vlabels = c(paste0("Roadway\nwidth (m)\n[0-",round(max(datos$`Roadway width (m)`),2),"]"), paste0("Number of\nlanes\n[0-",round(max(datos$`Number of lanes`),2),"]") , paste0("Vehicles\nspeed (km/h)\n[0-",round(max(datos$`Vehicles speed (km/h)`),2),"]") , paste0("Traffic\ndensity (cars/h)\n[0-",round(max(datos$`Traffic density`),2),"]"),paste0("Traffic\nflow (cars/km)\n[0-",round(max(datos$`Traffic flow`),2),"]"),paste0("Congestion\n[0-",round(max(datos$Congestion),2),"]") ,"% Cycling\ninfrastructure\n[0-1]","% Heavy\nvehicles\n[0-1]" )
    )
    
    legend(x=1, y=-0.90, legend = rownames(datos[-c(1,2),]), bty = "n", pch=20 , col=colors_border , text.col = "grey", cex=1, pt.cex=2)
    
    
    
    
    

#Asignacion LTS a intersecciones capa completa----
    
    #Se asigna el valor del LTS a cada cluster
    
    capa_variables_LTS_Final <- capa_variables_LTS_Final%>% mutate(ID=row_number())

    #Se obtienen las coordenadas (Lat/Long) de punto inicial de cada segmento la capa_malla_vial
    
    coordenadas_incio_seg<-st_coordinates(capa_variables_LTS_Final) %>% as_tibble()%>% group_by(L1)%>% filter(row_number()==1) %>% 
      ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud",Y="Latitud"))
    
    coordenadas_fin_seg<-st_coordinates(capa_variables_LTS_Final) %>% as_tibble() %>% group_by(L1) %>% filter(row_number()==n()) %>% 
      ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud",Y="Latitud"))
    
    #Se unen las dos trablas de coordenadas
    
    capa_Provisional_1<-capa_variables_LTS_Final %>% transmute(ID,LTS)  %>% left_join(coordenadas_incio_seg, By=ID)
    capa_Provisional_2<-capa_variables_LTS_Final %>% transmute(ID,LTS)  %>% left_join(coordenadas_fin_seg, By=ID)
    
    coordenadas_malla_vial <- capa_Provisional_1%>% rbind(capa_Provisional_2) %>% st_set_geometry(NULL)
    
    #Se cuentan y agrupan los repetidos
    
    capa_Intersecciones <- coordenadas_malla_vial %>% group_by(Latitud, Longitud) %>% summarise(Numero_Intersecciones=n(),LTS=max(LTS)) %>% filter(Numero_Intersecciones>=3) %>% ungroup()
    
    #Se genera la capa de intersecciones
    
    capa_Intersecciones <- capa_Intersecciones   %>% st_as_sf(coords=c(2,1)) %>% mutate(ID_Punto=row_number()) %>% st_set_crs(4326) 
    
    #Se hace un join espacial para determinar la localidad
    
    capa_Intersecciones  <-capa_Intersecciones  %>% st_join(select(layer_localidad,LocNombre)) %>% rename(c(LocNombre="Localidad"))
    
    
    
#Cruce accidentes capa completa----
    
    #Se determina en que LTS sucedio el accidente
    
    capa_Provisional1 <- st_buffer(capa_Accidentes, 0.0002) %>% mutate(ID=row_number()) %>% st_join(capa_Intersecciones[,c("LTS")],largest = TRUE) %>% mutate(LTSIn=LTS)  %>%  st_set_geometry(NULL)
    capa_Provisional2 <- st_buffer(capa_Accidentes, 0.0002) %>% mutate(ID=row_number()) %>% st_join(capa_variables_LTS_Final[,c("LTS")],largest = TRUE) %>% mutate(LTSSeg=LTS) %>% st_set_geometry(NULL)
    capa_Accidentes <- capa_Accidentes %>% mutate(ID=row_number()) %>% left_join(select(capa_Provisional1, LTSIn, ID)) 
    capa_Accidentes <- capa_Accidentes %>% mutate(ID=row_number()) %>% left_join(select(capa_Provisional2, LTSSeg, ID)) %>% mutate(LTS=ifelse(!is.na(LTSIn),LTSIn,LTSSeg))
    
    #Se halla la distancia de cada segmento
    
    capa_variables_LTS_Final <- capa_variables_LTS_Final %>% mutate(Distancia=st_length(capa_variables_LTS_Final))
    
    #Se crea la varible Año en capa accidentes y se filtra para el año 2017
    
    capa_Accidentes <- capa_Accidentes %>% mutate(Ano=format(as.Date(Accidentes.Fecha, format="%d/%m/%Y"),"%Y"))
    capa_Accidentes <- capa_Accidentes%>% filter(Ano=="2017")
    
    #Se generan las estadisticas
    
    estadisticas_accidentes<-as.data.frame(matrix(nrow=4,ncol=6))
    colnames(estadisticas_accidentes)=c("Km LTS","# Segmentos","# Accidentes Fatales","# Accidentes No Fatales","# Accidentes Fatales/Km LTS","# Accidentes No Fatales/Km LTS" ) 
    rownames(estadisticas_accidentes)=c("LTS 1", "LTS 2", "LTS 3", "LTS 4")
    
    for(i in 1:4){
      
      estadisticas_accidentes$`# Accidentes Fatales`[i] <-  nrow(capa_Accidentes %>% filter(Gravedad2=="Dead") %>% filter(LTS==i))
      estadisticas_accidentes$`# Accidentes No Fatales`[i] <-  nrow(capa_Accidentes %>% filter(Gravedad2=="Not Dead") %>% filter(LTS==i))
      
      capa_provisional <- capa_variables_LTS_Final %>% filter(LTS==i)
      estadisticas_accidentes$`# Segmentos`[i] <- nrow(capa_provisional)
      estadisticas_accidentes$`Km LTS`[i] <- drop_units(sum(capa_provisional$Distancia))/1000
      
      estadisticas_accidentes$`# Accidentes Fatales/Km LTS`[i] <- estadisticas_accidentes$`# Accidentes Fatales`[i]/estadisticas_accidentes$`Km LTS`[i]
      estadisticas_accidentes$`# Accidentes No Fatales/Km LTS` [i] <- estadisticas_accidentes$`# Accidentes No Fatales`[i]/estadisticas_accidentes$`Km LTS`[i]
    }
    
    
    
    
