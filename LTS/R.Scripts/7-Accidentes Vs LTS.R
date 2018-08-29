#Preparación del entorno de trabajo----

library(tidyverse)
library(sf)

setwd("/Users/alejandropalacio/Desktop/Aplicacion/Data")

capa_LTS <- readRDS("./LTS_Bogota.rds")
capa_Accidentes <- readRDS("./Accidentes.rds") 

capa_LTS <- capa_LTS %>% st_as_sf() %>% mutate(Cluster=ifelse(X4>=0.5,1,ifelse(X3>=0.5,2,ifelse(X2>=0.5,3,4))))
capa_Provisional <- st_buffer(capa_Accidentes, 0.0002) %>% mutate(ID=row_number()) %>% st_join(capa_LTS[,c("Cluster")],largest = TRUE) %>% st_set_geometry(NULL)
capa_Accidentes <- capa_Accidentes %>% mutate(ID=row_number()) %>% left_join(select(capa_Provisional, Cluster, ID)) %>% filter(!is.na(Cluster))

saveRDS(capa_Accidentes,"Accidentes_LTS_Bogota.rds")