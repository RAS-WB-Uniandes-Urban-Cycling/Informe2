#Preparación del entorno de trabajo----

library(tidyverse)
library(sf)

setwd("/Users/alejandropalacio/Documents/GitHub/proBikePolicies/LTS/Aplicacion/Data")

capa_LTS <- readRDS("./LTS_Bogota.rds")
capa_Accidentes <- readRDS("./Accidentes_LTS_Bogota.rds") 
capa_Intersecciones <-readRDS("./Intersecciones.rds")  

borrar <- c("Cluster")
capa_Accidentes<- capa_Accidentes[ , !(names(capa_Accidentes) %in% borrar)] 

ruta_base_datos<-"/Users/alejandropalacio/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/BASES DE DATOS/"
layer_localidad<-st_read(paste0(ruta_base_datos,"Mapas de Referencia IDECA/MR0318.gdb"),layer = "Loca",stringsAsFactors = FALSE) %>% st_transform(4326)


capa_LTS <- capa_LTS %>% st_as_sf() %>% mutate(Cluster=ifelse(X4>=0.5,1,ifelse(X3>=0.5,2,ifelse(X2>=0.5,3,4)))) 
capa_Provisional1 <- st_buffer(capa_Accidentes, 0.0002) %>% mutate(ID=row_number()) %>% st_join(capa_Intersecciones[,c("Cluster")],largest = TRUE) %>% mutate(ClusterIn=Cluster)  %>%  st_set_geometry(NULL)
capa_Provisional2 <- st_buffer(capa_Accidentes, 0.0002) %>% mutate(ID=row_number()) %>% st_join(capa_LTS[,c("Cluster")],largest = TRUE) %>% mutate(ClusterSeg=Cluster) %>% st_set_geometry(NULL)
capa_Accidentes <- capa_Accidentes %>% mutate(ID=row_number()) %>% left_join(select(capa_Provisional1, ClusterIn, ID)) 
capa_Accidentes <- capa_Accidentes %>% mutate(ID=row_number()) %>% left_join(select(capa_Provisional2, ClusterSeg, ID)) %>% mutate(Cluster=ifelse(!is.na(ClusterIn),ClusterIn,ClusterSeg))

capa_LTS <- capa_LTS %>% mutate(Distancia=st_length(capa_LTS))

hola <- capa_Accidentes
capa_Accidentes <- hola

capa_Accidentes <- capa_Accidentes %>% mutate(Ano=format(as.Date(Accidentes.Fecha, format="%d/%m/%Y"),"%Y"))
capa_Accidentes <- capa_Accidentes%>% filter(Ano=="2017")
#capa_Accidentes <- capa_Accidentes%>% filter(Gravedad2=="Dead")


capa_LTS_1 <- capa_LTS %>% filter(Cluster==1)
metros_LTS_1 <- sum(capa_LTS_1$Distancia)
accidentes_LTS_1 <- capa_Accidentes %>% filter(Cluster==1)
resultados_LTS_1 <- nrow(accidentes_LTS_1)/(metros_LTS_1/1000)

capa_LTS_2 <- capa_LTS %>% filter(Cluster==2)
metros_LTS_2 <- sum(capa_LTS_2$Distancia)
accidentes_LTS_2 <- capa_Accidentes %>% filter(Cluster==2)
resultados_LTS_2 <- nrow(accidentes_LTS_2)/(metros_LTS_2/1000)

capa_LTS_3 <- capa_LTS %>% filter(Cluster==3)
metros_LTS_3 <- sum(capa_LTS_3$Distancia)
accidentes_LTS_3 <- capa_Accidentes %>% filter(Cluster==3)
resultados_LTS_3 <- nrow(accidentes_LTS_3)/(metros_LTS_3/1000)

capa_LTS_4 <- capa_LTS %>% filter(Cluster==4)
metros_LTS_4 <- sum(capa_LTS_4$Distancia)
accidentes_LTS_4 <- capa_Accidentes %>% filter(Cluster==4)
resultados_LTS_4 <- nrow(accidentes_LTS_4)/(metros_LTS_4/1000)

resultados <- c(resultados_LTS_1,resultados_LTS_2,resultados_LTS_3,resultados_LTS_4)
resultados1 <- c(nrow(accidentes_LTS_1),nrow(accidentes_LTS_2),nrow(accidentes_LTS_3),nrow(accidentes_LTS_4))

resultados
resultados1

barplot(table(resultados), main="Accidents vs LTS (Bogotá)", xlab="LTS", ylab = "Frequency",col = "lightskyblue")

saveRDS(capa_Accidentes,"Accidentes_LTS_Bogota_2.rds")
prueba <- capa_Accidentes %>% filter(format(capa_Accidentes$Accidentes.Fecha,"%Y")=="2017")
prueba2 <- prueba %>% filter(Gravedad2=="Dead")
table(prueba2$Cluster)



numero_registros <- nrow(capa_malla_vial)

resultadosJorge<-as.data.frame(matrix(nrow=20,ncol=13))

colnames(resultadosJorge) <-c("Localidad","LTS1Kitlometros","LTS1NumeroAccidentesM","LTS1NumeroAccidentesNM","LTS2Kitlometros","LTS2NumeroAccidentesM","LTS2NumeroAccidentesNM","LTS3Kitlometros","LTS3NumeroAccidentesM","LTS3NumeroAccidentesNM","LTS4Kitlometros","LTS4NumeroAccidentesM","LTS4NumeroAccidentesNM")



capa_Accidentes <- capa_Accidentes %>% mutate(Ano=format(as.Date(Accidentes.Fecha, format="%d/%m/%Y"),"%Y"))
capa_Accidentes <- capa_Accidentes%>% filter(Ano=="2017")
capa_AccidentesNM <- capa_Accidentes%>% filter(Gravedad2=="Not Dead")
capa_AccidentesM <- capa_Accidentes%>% filter(Gravedad2=="Dead")

for(i in 1:20){
  resultadosJorge$Localidad[i] <- layer_localidad$LocNombre[i]
  capa_LTS_1 <- capa_LTS %>% filter(Cluster==1) %>% filter(LocNombre==layer_localidad$LocNombre[i])
  resultadosJorge$LTS1Kitlometros[i] <- sum(capa_LTS_1$Distancia)/1000
  capa_LTS_1_AccidentesM <- capa_AccidentesM %>% filter(Cluster==1) %>% filter(LocNombre==layer_localidad$LocNombre[i])
  resultadosJorge$LTS1NumeroAccidentesM[i] <- nrow(capa_LTS_1_AccidentesM )
  capa_LTS_1_AccidentesNM <- capa_AccidentesNM %>% filter(Cluster==1) %>% filter(LocNombre==layer_localidad$LocNombre[i])
  resultadosJorge$LTS1NumeroAccidentesNM[i] <- nrow(capa_LTS_1_AccidentesNM )
  
  capa_LTS_2 <- capa_LTS %>% filter(Cluster==2) %>% filter(LocNombre==layer_localidad$LocNombre[i])
  resultadosJorge$LTS2Kitlometros[i] <- sum(capa_LTS_2$Distancia)/1000
  capa_LTS_2_AccidentesM <- capa_AccidentesM %>% filter(Cluster==2) %>% filter(LocNombre==layer_localidad$LocNombre[i])
  resultadosJorge$LTS2NumeroAccidentesM[i] <- nrow(capa_LTS_2_AccidentesM )
  capa_LTS_2_AccidentesNM <- capa_AccidentesNM %>% filter(Cluster==2) %>% filter(LocNombre==layer_localidad$LocNombre[i])
  resultadosJorge$LTS2NumeroAccidentesNM[i] <- nrow(capa_LTS_2_AccidentesNM )
  
  capa_LTS_3 <- capa_LTS %>% filter(Cluster==3) %>% filter(LocNombre==layer_localidad$LocNombre[i])
  resultadosJorge$LTS3Kitlometros[i] <- sum(capa_LTS_3$Distancia)/1000
  capa_LTS_3_AccidentesM <- capa_AccidentesM %>% filter(Cluster==3) %>% filter(LocNombre==layer_localidad$LocNombre[i])
  resultadosJorge$LTS3NumeroAccidentesM[i] <- nrow(capa_LTS_3_AccidentesM )
  capa_LTS_3_AccidentesNM <- capa_AccidentesNM %>% filter(Cluster==3) %>% filter(LocNombre==layer_localidad$LocNombre[i])
  resultadosJorge$LTS3NumeroAccidentesNM[i] <- nrow(capa_LTS_3_AccidentesNM )
  
  capa_LTS_4 <- capa_LTS %>% filter(Cluster==4) %>% filter(LocNombre==layer_localidad$LocNombre[i])
  resultadosJorge$LTS4Kitlometros[i] <- sum(capa_LTS_4$Distancia)/1000
  capa_LTS_4_AccidentesM <- capa_AccidentesM %>% filter(Cluster==4) %>% filter(LocNombre==layer_localidad$LocNombre[i])
  resultadosJorge$LTS4NumeroAccidentesM[i] <- nrow(capa_LTS_4_AccidentesM )
  capa_LTS_4_AccidentesNM <- capa_AccidentesNM %>% filter(Cluster==4) %>% filter(LocNombre==layer_localidad$LocNombre[i])
  resultadosJorge$LTS4NumeroAccidentesNM[i] <- nrow(capa_LTS_4_AccidentesNM )
  
  
}



