#Preparación del entorno de trabajo----

#Se cargan las siguiente librerias

library(sf)
library(tidyverse)
library(reshape)
library(NISTunits)
library(tmap)
library(tmaptools)
library(lwgeom)

#Se define las ruta de las bases de datos

ruta_base_datos<-"/Users/alejandropalacio/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/BASES DE DATOS/"
ruta_resultados <-"/Users/alejandropalacio/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/RESULTADOS/LTS/Bases de Datos/"

#Se almacena los layer en cada variable definida  

layer_zats<-st_read(paste0(ruta_base_datos,"Mapas de Referencia IDECA/MR0318.gdb"),layer = "SCAT",stringsAsFactors = FALSE) %>% st_transform(4326)

#Se carga la Data

load(paste0(ruta_resultados, "8-Capa_Predicción_LTS_Logit.Rdata"))

#Procesamiento Capa Malla Vial----

  #Se calcula a que cluster pertenece con base en la prediccion

    capa_variables_LTS <- Capa_Variables_Prediccion %>% mutate(Cluster=ifelse(X4>=0.5,1,ifelse(X3>=0.5,2,ifelse(X2>=0.5,3,4))))

  #Se hace un Join espacial entre shape_malla_vial y shape_zats

    capa_variables_LTS<-capa_variables_LTS %>% st_join(select(layer_zats,SCaNombre),left = FALSE, largest=TRUE)  

  #Se grafica el mapa
    
    
    a <- capa_variables_LTS %>% filter(LocNombre==input$Localidad)
    
    pal <- colorNumeric("PuRd", a$Ancho)
    
    leaflet(a) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BlackAndWhite")  %>%
      addPolylines(color =~pal(Ancho),opacity = 1,smoothFactor = 1) %>% addLegend("bottomleft", pal = pal, values = ~Ancho, title = "Ancho de la vía (m)",opacity = 1)
    