#Preparación del entorno de trabajo----
rm(list=ls())
carpetaRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/Uniandes - RAS - SDM"
gitRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/proBikePolicies"
require(pacman)
pacman::p_load(readxl)
pacman::p_load(gdata)
pacman::p_load(tidyverse)
pacman::p_load(ggplot2)
pacman::p_load(ggpmisc)
pacman::p_load(RColorBrewer)
pacman::p_load(tidyr)
pacman::p_load(padr)
pacman::p_load(lubridate)
pacman::p_load(showtext)
pacman::p_load(sf)
pacman::p_load(tmap)
pacman::p_load(viridis)
pacman::p_load(lwgeom)
font_add("Helvetica Light",paste(gitRAS,"/Seguridad/Helvetica Light.ttf",sep=""))
setwd(paste(carpetaRAS,"/RESULTADOS/SEGURIDAD/Resultados-Inglés",sep=""))
cmHeight<-7
cmWidth<-10

#Leer bases de datos preprocesada----
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/3. AccidentesBiciTotalGeoData.Rdata"))
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/3. AccidentesTotal2011_2017.Rdata"))
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/4. Edad-Sexo-Defunciones-Bogota.Rdata"))
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/5. biciusuarios.Rdata"))
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/6. Poblacion.Rdata"))
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/7. MortalityRates.Rdata"))
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/10. Viajes_KilometrosRecorridos.Rdata"))
ciclTime<-read.csv(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Resultados/TABLAS/8. lengthCiclTime.csv"),stringsAsFactors = FALSE)
ciclTime$Fecha<-as.Date(ciclTime$FECHA,format="%F")
ciclTime$X<-NULL

#Lectura de bases de datos originales auxiliares----
Localidades<-st_read(paste(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR0318.gdb",sep=""),layer="Loca",crs=4326)
zat_loca<-st_read(paste0(carpetaRAS,"/RESULTADOS/GENERAL/GEO-DATA/ZATs_Localidad.shp"),crs=4326)
red_cicloruta<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR0318.gdb"),layer = "RBic",type = 5,crs=4326) %>% st_transform(4326) %>% st_cast("LINESTRING",do_split = TRUE)
red_cicloruta$LENGTH_GEO<-as.numeric(st_length(red_cicloruta))

#Eliminación del grupo de edad de 0-4 años por limitaciones de información----
AccidentesBiciTotal<-AccidentesBiciTotal %>% filter(EdadCategorica!="0-4")
AccidentesTotal<-AccidentesTotal %>% filter(EdadCategorica!="0-4")
Agregado<-Agregado %>% filter(GR_EDAD!="0-4")
biciusuarios<-biciusuarios %>% filter(GR_EDAD!="0-4")
Poblacion<-Poblacion %>% filter(GR_EDAD!="0-4")

############### ESTADISTICAS DESCRIPTIVAS ##########################################################
#Conteos por sexo----
table(AccidentesBiciTotal$Sexo)
table(AccidentesBiciTotal$Sexo,AccidentesBiciTotal$Gravedad2)

#Distribución de la edad----
sum(5<=AccidentesBiciTotal$Edad & AccidentesBiciTotal$Edad<=17,na.rm=TRUE)
sum(18<=AccidentesBiciTotal$Edad & AccidentesBiciTotal$Edad<=29,na.rm=TRUE)
sum(30<=AccidentesBiciTotal$Edad & AccidentesBiciTotal$Edad<=49,na.rm=TRUE)
sum(50<=AccidentesBiciTotal$Edad,na.rm=TRUE)

sum(5<=AccidentesBiciTotal$Edad & AccidentesBiciTotal$Edad<=17 & AccidentesBiciTotal$Gravedad2=="Dead",na.rm=TRUE)
sum(18<=AccidentesBiciTotal$Edad & AccidentesBiciTotal$Edad<=29 & AccidentesBiciTotal$Gravedad2=="Dead",na.rm=TRUE)
sum(30<=AccidentesBiciTotal$Edad & AccidentesBiciTotal$Edad<=49 & AccidentesBiciTotal$Gravedad2=="Dead",na.rm=TRUE)
sum(50<=AccidentesBiciTotal$Edad & AccidentesBiciTotal$Gravedad2=="Dead",na.rm=TRUE)

sum(5<=AccidentesBiciTotal$Edad & AccidentesBiciTotal$Edad<=17 & AccidentesBiciTotal$Gravedad2=="Not Dead",na.rm=TRUE)
sum(18<=AccidentesBiciTotal$Edad & AccidentesBiciTotal$Edad<=29 & AccidentesBiciTotal$Gravedad2=="Not Dead",na.rm=TRUE)
sum(30<=AccidentesBiciTotal$Edad & AccidentesBiciTotal$Edad<=49 & AccidentesBiciTotal$Gravedad2=="Not Dead",na.rm=TRUE)
sum(50<=AccidentesBiciTotal$Edad & AccidentesBiciTotal$Gravedad2=="Not Dead",na.rm=TRUE)

#Licencia de conducción----
table(AccidentesBiciTotal$PortaLicencia)
table(AccidentesBiciTotal$PortaLicencia,AccidentesBiciTotal$Gravedad2)

#Uso del casco----
table(AccidentesBiciTotal$LLevaCasco)
table(AccidentesBiciTotal$LLevaCasco,AccidentesBiciTotal$Gravedad2)

#Antiguedad del vehiculo----
sum(AccidentesBiciTotal$AntiguedadBici<=2,na.rm=TRUE)
sum(3<=AccidentesBiciTotal$AntiguedadBici&AccidentesBiciTotal$AntiguedadBici<=5,na.rm=TRUE)
sum(6<=AccidentesBiciTotal$AntiguedadBici&AccidentesBiciTotal$AntiguedadBici<=10,na.rm=TRUE)
sum(11<=AccidentesBiciTotal$AntiguedadBici,na.rm=TRUE)

sum(AccidentesBiciTotal$AntiguedadBici<=2& AccidentesBiciTotal$Gravedad2=="Dead",na.rm=TRUE)
sum(3<=AccidentesBiciTotal$AntiguedadBici&AccidentesBiciTotal$AntiguedadBici<=5& AccidentesBiciTotal$Gravedad2=="Dead",na.rm=TRUE)
sum(6<=AccidentesBiciTotal$AntiguedadBici&AccidentesBiciTotal$AntiguedadBici<=10& AccidentesBiciTotal$Gravedad2=="Dead",na.rm=TRUE)
sum(11<=AccidentesBiciTotal$AntiguedadBici& AccidentesBiciTotal$Gravedad2=="Dead",na.rm=TRUE)

sum(AccidentesBiciTotal$AntiguedadBici<=2& AccidentesBiciTotal$Gravedad2=="Not Dead",na.rm=TRUE)
sum(3<=AccidentesBiciTotal$AntiguedadBici&AccidentesBiciTotal$AntiguedadBici<=5& AccidentesBiciTotal$Gravedad2=="Not Dead",na.rm=TRUE)
sum(6<=AccidentesBiciTotal$AntiguedadBici&AccidentesBiciTotal$AntiguedadBici<=10& AccidentesBiciTotal$Gravedad2=="Not Dead",na.rm=TRUE)
sum(11<=AccidentesBiciTotal$AntiguedadBici& AccidentesBiciTotal$Gravedad2=="Not Dead",na.rm=TRUE)

#Weekday----
table(weekdays(AccidentesBiciTotal$Accidentes.Fecha))
table(weekdays(AccidentesBiciTotal$Accidentes.Fecha),AccidentesBiciTotal$Gravedad2)

#Hora de ocurrencia----
sum((0+0/60)<=as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60 &
      as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60 <=(6+0/60),na.rm=TRUE)
sum((6+1/60)<=as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60 &
      as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60<=(8+30/60),na.rm=TRUE)
sum((8+31/60)<=as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60 &
      as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60<=(15+0/60),na.rm=TRUE)
sum((15+1/60)<=as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60 &
      as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60<=(19+30/60),na.rm=TRUE)
sum((19+31/60)<=as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60 &
      as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60<=(24+0/60),na.rm=TRUE)

sum((0+0/60)<=as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60 &
      as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60 <=(6+0/60) & AccidentesBiciTotal$Gravedad2=="Dead",na.rm=TRUE)
sum((6+1/60)<=as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60 &
      as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60<=(8+30/60) & AccidentesBiciTotal$Gravedad2=="Dead",na.rm=TRUE)
sum((8+31/60)<=as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60 &
      as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60<=(15+0/60) & AccidentesBiciTotal$Gravedad2=="Dead",na.rm=TRUE)
sum((15+1/60)<=as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60 &
      as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60<=(19+30/60) & AccidentesBiciTotal$Gravedad2=="Dead",na.rm=TRUE)
sum((19+31/60)<=as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60 &
      as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60<=(24+0/60) & AccidentesBiciTotal$Gravedad2=="Dead",na.rm=TRUE)

sum((0+0/60)<=as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60 &
      as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60 <=(6+0/60) & AccidentesBiciTotal$Gravedad2=="Not Dead",na.rm=TRUE)
sum((6+1/60)<=as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60 &
      as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60<=(8+30/60) & AccidentesBiciTotal$Gravedad2=="Not Dead",na.rm=TRUE)
sum((8+31/60)<=as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60 &
      as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60<=(15+0/60) & AccidentesBiciTotal$Gravedad2=="Not Dead",na.rm=TRUE)
sum((15+1/60)<=as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60 &
      as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60<=(19+30/60) & AccidentesBiciTotal$Gravedad2=="Not Dead",na.rm=TRUE)
sum((19+31/60)<=as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60 &
      as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60<=(24+0/60) & AccidentesBiciTotal$Gravedad2=="Not Dead",na.rm=TRUE)

#Diseño de la infraestructura----
table(AccidentesBiciTotal$Accidentes.TipoDiseno2)
table(AccidentesBiciTotal$Accidentes.TipoDiseno2,AccidentesBiciTotal$Gravedad2)

#Tipo de vehiculo involucrado en el accidente----
aux1<-AccidentesBiciTotal %>% st_set_geometry(NULL)
relacionados<-as.data.frame(rbind(AccidentesTotal[AccidentesTotal$KEY %in% unique(aux1$KEY),c("KEY","ClaseVehiculo2")] %>% filter(ClaseVehiculo2!="Bicycle")))
table(relacionados$ClaseVehiculo2)

aux1<-AccidentesBiciTotal %>% filter(Gravedad2=="Dead") %>% st_set_geometry(NULL)
relacionados<-as.data.frame(rbind(AccidentesTotal[AccidentesTotal$KEY %in% unique(aux1$KEY),c("KEY","ClaseVehiculo2")] %>% filter(ClaseVehiculo2!="Bicycle")))
table(relacionados$ClaseVehiculo2)

aux1<-AccidentesBiciTotal %>% filter(Gravedad2=="Not Dead") %>% st_set_geometry(NULL)
relacionados<-as.data.frame(rbind(AccidentesTotal[AccidentesTotal$KEY %in% unique(aux1$KEY),c("KEY","ClaseVehiculo2")] %>% filter(ClaseVehiculo2!="Bicycle")))
table(relacionados$ClaseVehiculo2)

#Condiciones climáticas
table(AccidentesBiciTotal$Clima)
table(AccidentesBiciTotal$Clima, AccidentesBiciTotal$Gravedad2)

#Totales----
sum(AccidentesBiciTotal$Gravedad2=="Dead",na.rm=TRUE)
sum(AccidentesBiciTotal$Gravedad2=="Not Dead",na.rm=TRUE)

# El número de Zat es ZAT_num_n -> Por verificación directa de un par de encuestas contra el mapa
# El factor de expansión es el Ponderador Calibrado de acuerdo a David Gonzalez de la Secretaria de Movilidad
# Para ejecutar OSRM: en una consola de terminal dirigirse al folder osrm-backend: ejecutar: osrm-routed --algorithm=MLD map.xml.osrm
# Este código hace buffers cuadrados
# for(i in 1:dim(intersecciones)[1]){
#   st_geometry(intersecciones[i,])<-st_geometry(stplanr::geo_bb(intersecciones[i,],scale_factor=1,distance=x)) 
# }


#-----

#Estacionalidad por análisis de frecuencias----
pacman::p_load(TSA)
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(as.Date(AccidentesBiciTotal$Accidentes.Fecha)),FUN=length)
aux<-pad(aux, interval="day") %>% mutate(x = replace(x, is.na(x), 0))
aux$AÑO<-getYear(aux$Group.1)
aux<-left_join(aux,aggregate(biciusuarios$BICIUSRSINT,by=list(getYear(biciusuarios$FECHA)),FUN=sum) %>% rename(AÑO=Group.1,BICIUSRS=x),by="AÑO")
aux$x2<-aux$x*1000/aux$BICIUSRS
p<-periodogram(aux$x)
dd = data.frame(freq=p$freq, spec=p$spec)
order = dd[order(-dd$spec),]
top2 = head(order, 5)
top2
time = 1/top2$f
time

#Tabla con los datos de resumen de accidentalidad para el año 2017----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(getYear(AccidentesBiciTotal$Accidentes.Fecha),AccidentesBiciTotal$Accidentes.Localidad),FUN=length)
names(aux)<-c("Group.1","LocNombre","Accidentes")
aux$Group.1<-as.numeric(aux$Group.1)
aux$LocNombre<-factor(aux$LocNombre)
aux<-merge(aux,aggregate(Poblacion$POBLACION,by=list(Poblacion$AÑO,Poblacion$LOCALIDAD),FUN=sum) %>% rename(POBLACION=x,LocNombre=Group.2),by=c("Group.1","LocNombre"))
aux$EstanPop<-aux$Accidentes*100000/aux$POBLACION
aux<-merge(aux,aggregate(biciusuarios$BICIUSRSINT,by=list(getYear(biciusuarios$FECHA),biciusuarios$LOCALIDAD),FUN=sum) %>% rename(BICIUSRS=x,LocNombre=Group.2),by=c("Group.1","LocNombre"))
aux$EstanBici<-aux$Accidentes*1000/aux$BICIUSRS
aux<-aux[,c(2,3,4,5,6,7)]
names(aux)<-c("Locality","Casualties","Population","Injuries per 100.000 people","Bicycle users","Injuries per 1.000 cyclists")

#Otros----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(AccidentesBiciTotal$Gravedad),FUN="length")
aux$y<-aux$x/sum(aux$x)

aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(AccidentesBiciTotal$Accidentes.TipoTiempo),FUN="length")
aux$y<-aux$x/sum(aux$x)
aux
