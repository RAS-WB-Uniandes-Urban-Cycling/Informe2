#Preparación del entorno de trabajo----

#Se cargan las siguiente librerias

library(sf)
library(tidyverse)
library(reshape)
library(NISTunits)
library(tmap)
library(tmaptools)
library(lwgeom)
library(leaflet)
library(fmsb)

#Se define las ruta de las bases de datos

ruta_base_datos<-"/Users/alejandropalacio/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/BASES DE DATOS/"
ruta_resultados <-"/Users/alejandropalacio/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/RESULTADOS/LTS/Bases de Datos/"

#Se almacena los layer en cada variable definida  

layer_zats<-st_read(paste0(ruta_base_datos,"Mapas de Referencia IDECA/MR0318.gdb"),layer = "SCAT",stringsAsFactors = FALSE) %>% st_transform(4326)

#Se carga la Data

Accidentes <- readRDS(paste0(ruta_resultados,"Accidentes_LTS_Bogota.rds"))
load(paste0(ruta_resultados, "8-Capa_Predicción_LTS_Logit.Rdata"))
load(paste0(ruta_resultados,"9-Intersecciones.Rdata"))

#Procesamiento Capa Malla Vial----

  #Se calcula a que cluster pertenece con base en la prediccion

    capa_variables_LTS <- Capa_Variables_Prediccion %>% mutate(Cluster=ifelse(X4>=0.5,1,ifelse(X3>=0.5,2,ifelse(X2>=0.5,3,4)))) %>% mutate(Proba=ifelse(X4>=0.5,X4,ifelse(X3>=0.5,X3,ifelse(X2>=0.5,X2,X1))))

  #Se hace un Join espacial entre shape_malla_vial y shape_zats

    capa_variables_LTS<-capa_variables_LTS %>% st_join(select(layer_zats,SCaNombre),left = FALSE, largest=TRUE) 
    intersecciones_LTS<-intersecciones_malla_vial %>% st_join(select(layer_zats,SCaNombre),left = FALSE, largest=TRUE) 
    
  #Se grafica el mapa
    
    
    a <- capa_variables_LTS %>% filter(SCaNombre %in% c("CIUDAD SALITRE SUR-ORIENTAL","CIUDAD SALITRE NOR-ORIENTAL"))
    b <-intersecciones_LTS %>% filter(SCaNombre %in% c("CIUDAD SALITRE SUR-ORIENTAL","CIUDAD SALITRE NOR-ORIENTAL"))
    
    pal <- colorNumeric("PuRd", a$Ancho)
    
    leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
      addPolylines(color =~pal(Ancho),opacity = 1,smoothFactor = 1) %>% addLegend("bottomleft", pal = pal, values = ~Ancho, title = "Roadway width (m)",opacity = 1)
    
    pal <- colorFactor("Purples", a$Carriles)
    
    leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
      addPolylines(color =~pal(Carriles),opacity = 1,smoothFactor = 1) %>% addLegend("bottomleft", pal = pal, values = ~Carriles, title = "Number of lanes",opacity = 1)
    
    pal <- colorFactor("Spectral", a$CicloRuta)
    
    leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
      addPolylines(color =~pal(CicloRuta),opacity = 1,smoothFactor = 1) %>% addLegend("bottomleft", pal = pal, values = ~CicloRuta, title = "Cycling infrastructure",opacity = 1)
    
    pal <- colorFactor("Spectral", a$SITP)
    
    leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
      addPolylines(color =~pal(SITP),opacity = 1,smoothFactor = 1) %>% addLegend("bottomleft", pal = pal, values = ~SITP, title = "Presence of SITP",opacity = 1)
    
    pal <- colorNumeric(palette = "YlOrRd", a$Velocidad)
    
    leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
      addPolylines(color =~pal(Velocidad),opacity = 1,smoothFactor = 1) %>% addLegend("bottomleft", pal = pal, values = ~Velocidad, title = "Vehicles speed (km/h)",opacity = 1)
    
    pal <- colorNumeric(palette = "YlOrRd", a$Congestion)
    
    leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
      addPolylines(color =~pal(Congestion),opacity = 1,smoothFactor = 1) %>% addLegend("bottomleft", pal = pal, values = ~Congestion, title = "Congestion",opacity = 1)
    
    pal <- colorNumeric(palette = "YlOrRd", a$Densidad)
    
    leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
      addPolylines(color =~pal(Densidad),opacity = 1,smoothFactor = 1) %>% addLegend("bottomleft", pal = pal, values = ~Densidad, title = "Traffic density",opacity = 1)
    
    pal <- colorNumeric(palette = "YlOrRd", a$Flujo)
    
    leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
      addPolylines(color =~pal(Flujo),opacity = 1,smoothFactor = 1) %>% addLegend("bottomleft", pal = pal, values = ~Flujo, title = "Traffic flow",opacity = 1)
    
    pal <- colorFactor(c("lime green","dark green","orange","Red"), a$Cluster)
    
    leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
      addPolylines(color =~pal(Cluster),opacity = 1,smoothFactor = 1) %>% addLegend("bottomleft", pal = pal, values = ~Cluster, title = "LTS",opacity = 1)
    
    
    pal <- colorFactor(c("lime green","dark green","orange","Red"), b$Cluster)
    
    leaflet(b) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>% addPolylines(data=a) %>% 
      addCircles(color =~pal(Cluster),opacity = 1)%>% addLegend("bottomleft", pal = pal, values = ~Cluster, title = "LTS",opacity = 1)
    
    
  #Se grafican los accidentes
    
    Accidentes<- Accidentes %>%  mutate(Cluster2=ifelse(Cluster==1,4, ifelse(Cluster==2,2,ifelse(Cluster==3,3,1))))
    
    b <- Accidentes
    
    barplot(table(b$Cluster), main="Accidents vs LTS", xlab="LTS", ylab = "Frequency")
    barplot(table(b$Cluster2), main="Accidents vs Clusters", xlab="Cluster", ylab = "Frequency",col = "lightskyblue")
    
  #Estadisticas descriptivas por cluster
    
    estadisticas1 <- capa_variables_LTS %>% filter(Cluster==1)
    estadisticas2 <- capa_variables_LTS %>% filter(Cluster==2)
    estadisticas3 <- capa_variables_LTS %>% filter(Cluster==3)
    estadisticas4 <- capa_variables_LTS %>% filter(Cluster==4)
    
    sd(estadisticas1$Ancho)
    sd(estadisticas2$Ancho)
    sd(estadisticas3$Ancho)
    sd(estadisticas4$Ancho)
    
    sd(estadisticas1$Carriles)
    sd(estadisticas2$Carriles)
    sd(estadisticas3$Carriles)
    sd(estadisticas4$Carriles)
    
    sd(estadisticas1$Velocidad)
    sd(estadisticas2$Velocidad)
    sd(estadisticas3$Velocidad)
    sd(estadisticas4$Velocidad)
    
    sd(estadisticas1$Congestion)
    sd(estadisticas2$Congestion)
    sd(estadisticas3$Congestion)
    sd(estadisticas4$Congestion)
    
    sd(estadisticas1$Densidad)
    sd(estadisticas2$Densidad)
    sd(estadisticas3$Densidad)
    sd(estadisticas4$Densidad)
    
    sd(estadisticas1$Flujo)
    sd(estadisticas2$Flujo)
    sd(estadisticas3$Flujo)
    sd(estadisticas4$Flujo)
    
    Accidentes2 <-Accidentes %>% filter(Gravedad2=="Not Dead") 
  
    
    
    
#Grafica de radar variables LTS----
    
    capa_variables_LTS <- Capa_Variables_Prediccion %>% mutate(Cluster=ifelse(X4>=0.5,1,ifelse(X3>=0.5,2,ifelse(X2>=0.5,3,4)))) %>% mutate(Proba=ifelse(X4>=0.5,X4,ifelse(X3>=0.5,X3,ifelse(X2>=0.5,X2,X1))))
    capa_variables_LTS<-capa_variables_LTS %>% mutate(AnchoE=0,CarrilesE=0,VelocidadE=0,DensidadE=0,FlujoE=0,CongestionE=0)
    
    capa_variables_LTS$AnchoE <- (capa_variables_LTS$Ancho-min(capa_variables_LTS$Ancho))/(max(capa_variables_LTS$Ancho)-min(capa_variables_LTS$Ancho))
    capa_variables_LTS$CarrilesE <- (capa_variables_LTS$Carriles-min(capa_variables_LTS$Carriles))/(max(capa_variables_LTS$Carriles)-min(capa_variables_LTS$Carriles))
    capa_variables_LTS$VelocidadE <- (capa_variables_LTS$Velocidad-min(capa_variables_LTS$Velocidad))/(max(capa_variables_LTS$Velocidad)-min(capa_variables_LTS$Velocidad))
    capa_variables_LTS$DensidadE <- (capa_variables_LTS$Densidad-min(capa_variables_LTS$Densidad))/(max(capa_variables_LTS$Densidad)-min(capa_variables_LTS$Densidad))
    capa_variables_LTS$FlujoE <- (capa_variables_LTS$Flujo-min(capa_variables_LTS$Flujo))/(max(capa_variables_LTS$Flujo)-min(capa_variables_LTS$Flujo))
    capa_variables_LTS$CongestionE <- (capa_variables_LTS$Congestion-min(capa_variables_LTS$Congestion))/(max(capa_variables_LTS$Congestion)-min(capa_variables_LTS$Congestion))
    
    
    estadisticas1 <- capa_variables_LTS %>% filter(Cluster==1)
    estadisticas2 <- capa_variables_LTS %>% filter(Cluster==2)
    estadisticas3 <- capa_variables_LTS %>% filter(Cluster==3)
    estadisticas4 <- capa_variables_LTS %>% filter(Cluster==4)
    
    
    datos<-as.data.frame(matrix(nrow=4,ncol=8))
    colnames(datos)=c("Ancho de la vía (m)", "Número de carriles" , "Velocidad (km/h)" , "Densidad de Trafico","Traffic flow","Congestion" ,"Cycling infrastructure","Heavy vehicles " )
    rownames(datos)=c("Cluster-3", "Cluster-1", "Cluster-4", "Cluster-2")
    
    
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
    
    datos$`Cycling infrastructure`[1]=as.data.frame(table(estadisticas1$CicloRuta))$Freq[2]/(as.data.frame(table(estadisticas1$CicloRuta))$Freq[1]+as.data.frame(table(estadisticas1$CicloRuta))$Freq[2])
    datos$`Cycling infrastructure`[2]=as.data.frame(table(estadisticas2$CicloRuta))$Freq[2]/(as.data.frame(table(estadisticas2$CicloRuta))$Freq[1]+as.data.frame(table(estadisticas2$CicloRuta))$Freq[2])
    datos$`Cycling infrastructure`[3]=as.data.frame(table(estadisticas3$CicloRuta))$Freq[2]/(as.data.frame(table(estadisticas3$CicloRuta))$Freq[1]+as.data.frame(table(estadisticas3$CicloRuta))$Freq[2])
    datos$`Cycling infrastructure`[4]=0
    
    datos$`Heavy vehicles `[1]=0
    datos$`Heavy vehicles `[2]=0
    datos$`Heavy vehicles `[3]=as.data.frame(table(estadisticas3$SITP))$Freq[2]/(as.data.frame(table(estadisticas3$SITP))$Freq[1]+as.data.frame(table(estadisticas3$SITP))$Freq[2])
    datos$`Heavy vehicles `[4]=as.data.frame(table(estadisticas4$SITP))$Freq[2]/(as.data.frame(table(estadisticas4$SITP))$Freq[1]+as.data.frame(table(estadisticas4$SITP))$Freq[2])
    
    
    # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
    
    datos=rbind(c(max(datos$`Roadway width (m)`),max(datos$`Number of lanes`),max(datos$`Vehicles speed (km/h)`),max(datos$`Traffic density`),max(datos$`Traffic flow`),max(datos$Congestion),1,1),
                 c(0,0,0,0,0,0,0,0,0) , datos)
    
    colors_border=c("lime green","dark green","orange","Red")

    radarchart( datos  , axistype=1.5 , 
                #custom polygon
                pcol=colors_border  , plwd=4 , plty=1,
                #custom the grid
                cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.9, caxislabels=seq(0,1,0.25),
                #custom labels
                vlcex=1 ,
                vlabels = c(paste0("Roadway\nwidth (m)\n[0-",round(max(datos$`Roadway width (m)`),2),"]"), paste0("Number of\nlanes\n[0-",round(max(datos$`Number of lanes`),2),"]") , paste0("Vehicles\nspeed (km/h)\n[0-",round(max(datos$`Vehicles speed (km/h)`),2),"]") , paste0("Traffic\ndensity (cars/h)\n[0-",round(max(datos$`Traffic density`),2),"]"),paste0("Traffic\nflow (cars/km)\n[0-",round(max(datos$`Traffic flow`),2),"]"),paste0("Congestion\n[0-",round(max(datos$Congestion),2),"]") ,"Cycling\ninfrastructure %\n[0-1]","Heavy\nvehicles %\n[0-1]" )
    )
    
    legend(x=1.1, y=-0.6, legend = rownames(datos[-c(1,2),]), bty = "n", pch=20 , col=colors_border , text.col = "grey", cex=1.3, pt.cex=4)
    
    
    