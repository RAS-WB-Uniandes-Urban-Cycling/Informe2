# Preparación del entorno de trabajo----
rm(list=ls())
carpetaRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/UNIANDES - RAS - SDM"
gitRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/proBikePolicies"
require(pacman)
pacman::p_load(sf)
pacman::p_load(tidyverse)
pacman::p_load(tmap)
pacman::p_load(readxl)
pacman::p_load(zoo)
pacman::p_load(gdata)
pacman::p_load(padr)

# Lectura de la función de codificación de edades en grupos----
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/4. GruposEdadCodificacion.Rdata"))

# Viajes por ZAT origen y ZAT destino de encuesta de movilidad 2005----
viajes2005<-read_excel(paste0(carpetaRAS,"/BASES DE DATOs/Encuesta de Movilidad/2005/MODULOD.xlsx"))

# Filtrar medio de transporte bicicleta y ubicación del hogar en bogotá
viajes2005<-viajes2005[(viajes2005$D35_MEDIO==2)&(!is.na(viajes2005$ID_UPZ)),c("ID","D7_NPER","DISTANCIA","D27_NVIA","ID_UPZ","D29_UPZ","D32_UPZ","D34_MOTI","FACTRED_FI")]
viajes2005$D29_UPZ[is.na(viajes2005$D29_UPZ)]<-viajes2005$ID_UPZ[is.na(viajes2005$D29_UPZ)]
viajes2005$DISTANCIA<-viajes2005$DISTANCIA*viajes2005$FACTRED_FI
viajes2005$KEY<-paste(viajes2005$ID,viajes2005$D7_NPER,sep="-")

# Extracción del sexo de los viajeros-commuters
personas2005<-read_excel(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta de Movilidad/2005/MODULOC.xlsx"))
personas2005$KEY<-paste(personas2005$ID,personas2005$C7_PERS,sep="-")
viajes2005usrs<-viajes2005[viajes2005$D34_MOTI%in%c(2,3,4),]$KEY
personas2005<-personas2005[personas2005$KEY%in%viajes2005usrs,]
personas2005<-personas2005[,c("KEY","C9_SEXO","C8_EDAD")]
personas2005$C8_EDAD<-as.numeric(personas2005$C8_EDAD)
personas2005$GR_EDAD<-factor(apply(as.data.frame(personas2005$C8_EDAD),MARGIN=1,FUN=encodeEdad2,g=gruposEdad),levels = paste0(gruposEdad$ED_MIN,"-",gruposEdad$ED_MAX))
personas2005$C9_SEXO<-as.factor(ifelse(personas2005$C9_SEXO==1,"Male","Female"))
viajes2005<-viajes2005[viajes2005$KEY%in%personas2005$KEY,]
viajes2005<-merge(viajes2005,personas2005,by="KEY",all.x = TRUE)
rm(personas2005,viajes2005usrs)

# Intersección de UPZ y ZAT
UPZ<-st_read(paste0(carpetaRAS,"/RESULTADOS/GENERAL/GEO-DATA/UPZ_bogota.shp"),crs=4326)
ZAT_loca<-st_read(paste0(carpetaRAS,"/RESULTADOS/GENERAL/GEO-DATA/ZATs_Localidad.shp"),crs=4326)
UPZ_ZAT<-st_join(st_centroid(UPZ),ZAT_loca,left=TRUE,join=st_within) %>% st_set_geometry(NULL) %>% select(UPZ=UPlCodigo,ZAT=Zn_Nm_N)
UPZ_ZAT<-UPZ_ZAT[-grep(glob2rx("UPR*"),UPZ_ZAT$UPZ),]
UPZ_ZAT$UPZ<-as.numeric(substr(UPZ_ZAT$UPZ,4,6))
rm(UPZ)

# Traducción de UPZ a ZAT
viajes2005<-merge(viajes2005,UPZ_ZAT,by.x="D29_UPZ",by.y="UPZ",all.x=TRUE)
viajes2005<-merge(viajes2005,UPZ_ZAT,by.x="D32_UPZ",by.y="UPZ",suffixes = c("","_DST"),all.x=TRUE)
rm(UPZ_ZAT)

# Resumen
viajes2005<-aggregate(viajes2005[,c("FACTRED_FI","DISTANCIA")],by=list(viajes2005$ZAT,viajes2005$ZAT_DST,viajes2005$C9_SEXO,viajes2005$GR_EDAD),FUN=sum,na.rm=TRUE) %>% select(ZAT_SRC=Group.1,ZAT_DST=Group.2,SEXO=Group.3,GR_EDAD=Group.4,VIAJES=FACTRED_FI,DIST_TOTAL_KM=DISTANCIA)
viajes2005$Año<-2005

# Viajes por ZAT origen y ZAT destino de encuesta de movilidad 2011----
viajes2011<-read_excel(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta de Movilidad/2011/120927_Base de Datos EODH 2011/Mod_D_VIAJES2_BaseImputacion_Definitiva.xlsx"),na="N/A")
viajes2011<-viajes2011[,c("ORDEN","ID_PERSO","E1_P1_D","E2_P1_D","E3_P1_D","E4_P1_D","E5_P1_D","F_EXP","ZAT_ORIG","ZAT_DEST","P13_D")]

# Filtrado de los viajes realizados en bicicleta
viajes2011$Bici<-ifelse(viajes2011$E1_P1_D==18|viajes2011$E1_P1_D==19|viajes2011$E2_P1_D==18|viajes2011$E2_P1_D==19|viajes2011$E3_P1_D==18|viajes2011$E3_P1_D==19|viajes2011$E4_P1_D==18|viajes2011$E4_P1_D==19|viajes2011$E5_P1_D==18|viajes2011$E5_P1_D==19,TRUE,FALSE)
viajes2011$KEY<-paste(viajes2011$ORDEN,viajes2011$ID_PERSO,sep="-")
viajes2011<-viajes2011[!is.na(viajes2011$Bici),]

# Eliminación de viajes de hogares fuera de bogotá, reducción a viajes de commuters y extracción del sexo de los viajeros
personas2011<-read_excel(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta de Movilidad/2011/120927_Base de Datos EODH 2011/MOD_B_PERSONAS_Tipico.xlsx"),na="N/A")
personas2011$BOG<-ifelse(personas2011$MUN_D=="11001",TRUE,FALSE)
personas2011<-personas2011[!is.na(personas2011$BOG),]
personas2011$KEY<-paste(personas2011$ORDEN,personas2011$ID_PERSO,sep="-")
viajes2011usrs<-viajes2011[viajes2011$P13_D%in%c("01","02","03","13"),]$KEY
personas2011<-personas2011[personas2011$KEY%in%viajes2011usrs,]
personas2011<-personas2011[,c("KEY","P4_B","P5_B")]
personas2011$P5_B<-as.numeric(personas2011$P5_B)
personas2011$GR_EDAD<-factor(apply(as.data.frame(personas2011$P5_B),MARGIN=1,FUN=encodeEdad2,g=gruposEdad),levels = paste0(gruposEdad$ED_MIN,"-",gruposEdad$ED_MAX))
personas2011$P4_B<-as.factor(ifelse(personas2011$P4_B==1,"Male","Female"))
viajes2011<-viajes2011[viajes2011$KEY%in%personas2011$KEY,]
viajes2011<-merge(viajes2011,personas2011,by="KEY",all.x=TRUE)
rm(personas2011,viajes2011usrs)

# Resumen
viajes2011<-aggregate(viajes2011$F_EXP,by=list(viajes2011$ZAT_ORIG,viajes2011$ZAT_DEST,viajes2011$P4_B,viajes2011$GR_EDAD),FUN=sum) %>% select(ZAT_SRC=Group.1,ZAT_DST=Group.2,SEXO=Group.3,GR_EDAD=Group.4,VIAJES=x)
viajes2011$Año<-2011
viajes2011$DIST_TOTAL_KM<-NA

# Viajes por ZAT origen y ZAT destino de encuesta de movilidad 2015----
viajes2015<-read.csv(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta de Movilidad/2015/Encuesta/encuesta 2015 - viajes.csv"),sep=";",fileEncoding = "Latin1",encoding = "Latin1",stringsAsFactors = FALSE)
viajes2015$PONDERADOR_CALIBRADO_VIAJES<-as.numeric(gsub(",",".",viajes2015$PONDERADOR_CALIBRADO_VIAJES))
viajes2015$ZAT_ORIGEN<-as.numeric(viajes2015$ZAT_ORIGEN)
viajes2015$ZAT_DESTINO<-as.numeric(viajes2015$ZAT_DESTINO)

# Eliminación de viajes de hogares fuera de bogotá
encuesta2015<-read.csv(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta de Movilidad/2015/Encuesta/encuesta 2015 - encuestas.csv"),sep=";",fileEncoding = "Latin1",encoding = "Latin1",stringsAsFactors = FALSE)
encuesta2015<-encuesta2015[encuesta2015$DEPARTAMENTO=="Bogota D.C.",c("ID_ENCUESTA")]
viajes2015<-viajes2015[viajes2015$ID_ENCUESTA%in%encuesta2015,]
rm(encuesta2015)

#Filtrado de los viajes en dias hábiles
viajes2015<-viajes2015[viajes2015$DIA_HABIL=="S",]

# Extracción de los viajes en bicicleta
etapas2015<-read.csv(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta de Movilidad/2015/Encuesta/encuesta 2015 - etapas.csv"),sep=";",fileEncoding = "Latin1",encoding = "Latin1")
etapas2015<-etapas2015[etapas2015$MEDIOTRASPORTE=="Bicicleta"|etapas2015$MEDIOTRASPORTE=="Bicicleta con motor"|etapas2015$MEDIOTRASPORTE=="Bicicletas publicas",]
etapas2015<-paste(etapas2015$ID_ENCUESTA,etapas2015$NUMERO_PERSONA,etapas2015$NUMERO_VIAJE,sep="-")
viajes2015$KEY<-paste(viajes2015$ID_ENCUESTA,viajes2015$NUMERO_PERSONA,viajes2015$NUMERO_VIAJE,sep="-")
viajes2015<-viajes2015[viajes2015$KEY%in%etapas2015,]
rm(etapas2015)
viajes2015$KEY<-paste(viajes2015$ID_ENCUESTA,viajes2015$NUMERO_PERSONA,sep="-")

# Extracción del sexo de los viajeros y filtrado de commuters
personas2015<-read.csv(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta de Movilidad/2015/Encuesta/encuesta 2015 - personas.csv"),sep=";",fileEncoding = "Latin1",encoding = "Latin1")
personas2015$KEY<-paste(personas2015$ID_ENCUESTA,personas2015$NUMERO_PERSONA,sep="-")
viajes2015usrs<-viajes2015[viajes2015$MOTIVOVIAJE%in%c("Estudiar","Trabajar","Asuntos de Trabajo","Buscar trabajo"),]$KEY
personas2015<-personas2015[personas2015$KEY%in%viajes2015usrs,]
personas2015<-personas2015[,c("KEY","SEXO","EDAD")]
personas2015$SEXO<-as.factor(ifelse(personas2015$SEXO=="Hombre","Male","Female"))
personas2015$EDAD<-as.numeric(personas2015$EDAD)
personas2015$GR_EDAD<-factor(apply(as.data.frame(personas2015$EDAD),MARGIN=1,FUN=encodeEdad2,g=gruposEdad),levels = paste0(gruposEdad$ED_MIN,"-",gruposEdad$ED_MAX))
viajes2015<-viajes2015[viajes2015$KEY%in%personas2015$KEY,]
viajes2015<-merge(viajes2015,personas2015,by="KEY",all.x=TRUE)
rm(personas2015,viajes2015usrs)

# Resumen
viajes2015<-aggregate(viajes2015$PONDERADOR_CALIBRADO_VIAJES,by=list(viajes2015$ZAT_ORIGEN,viajes2015$ZAT_DESTINO,viajes2015$SEXO,viajes2015$GR_EDAD),FUN=sum) %>% select(ZAT_SRC=Group.1,ZAT_DST=Group.2,SEXO=Group.3,GR_EDAD=Group.4,VIAJES=x)
viajes2015$Año<-2015
viajes2015$DIST_TOTAL_KM<-NA

# Integración de la base de datos de viajes----
viajes<-rbind(viajes2005,viajes2011,viajes2015)
rm(viajes2005,viajes2011,viajes2015)

# Lectura de la matriz de rutas más cortas entre centroides de ZAT----
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/9. DistZat2Zat.Rdata"))

# Extracción de la distancia recorrida por cada viaje entre pares ZAT Origen-Destino----
viajes$id<-paste(viajes$ZAT_SRC,viajes$ZAT_DST,sep="_")
bike_dist_zat2zat<-as.data.frame(bike_dist_zat2zat)
bike_dist_zat2zat<-bike_dist_zat2zat[,c("id","distance")]
viajes<-merge(viajes,bike_dist_zat2zat,by="id",all.x=TRUE)
viajes$distance[viajes$ZAT_SRC==viajes$ZAT_DST]<-0
rm(bike_dist_zat2zat)

# Calculo de la distancia total recorrorrida por los biciusuarios en todos los viajes realizados----
viajes$DIST_TOTAL_KM_EST<-viajes$VIAJES*viajes$distance/1000

# Identificación de las localidades de origen y destino de los viajes y kilómetros recorridos----
st_geometry(ZAT_loca)<-NULL 
ZAT_loca<-ZAT_loca[,c("Zn_Nm_N","LocNmbr")]
viajes<-merge(viajes,ZAT_loca,by.x="ZAT_SRC",by.y="Zn_Nm_N",all.x=TRUE)
names(viajes)[grep("LocNmbr",names(viajes))]<-"LOC_SRC"
viajes<-merge(viajes,ZAT_loca,by.x="ZAT_DST",by.y="Zn_Nm_N",all.x=TRUE)
names(viajes)[grep("LocNmbr",names(viajes))]<-"LOC_DST"
rm(ZAT_loca)

# Resumen de los viajes y kilómetros recorridos por localidad de origen y localidad destino----
viajes<-aggregate(viajes[,c("VIAJES","DIST_TOTAL_KM","DIST_TOTAL_KM_EST")],by=list(viajes$LOC_SRC, viajes$LOC_DST,viajes$Año,viajes$SEXO,viajes$GR_EDAD),FUN=sum) %>% rename(LOC_SRC=Group.1, LOC_DST=Group.2,AÑO=Group.3,SEXO=Group.4,GR_EDAD=Group.5)
viajes$FECHA<-as.POSIXct(paste0(viajes$AÑO,"-01-01"),format="%F")
viajes<-pad(viajes,interval="year", group=c("GR_EDAD","SEXO","LOC_SRC","LOC_DST"),start_val=as.Date("2005-01-01"),end_val=as.Date("2017-01-01"))
viajes$FECHA<-as.Date(viajes$FECHA)
viajes$AÑO<-getYear(viajes$FECHA)
viajes$VIAJES[viajes$AÑO==2005&is.na(viajes$VIAJES)]<-0
viajes$DIST_TOTAL_KM_EST[viajes$AÑO==2005&is.na(viajes$DIST_TOTAL_KM_EST)]<-0
viajes$VIAJES[viajes$AÑO==2011&is.na(viajes$VIAJES)]<-0
viajes$DIST_TOTAL_KM_EST[viajes$AÑO==2011&is.na(viajes$DIST_TOTAL_KM_EST)]<-0
viajes$VIAJES[viajes$AÑO==2015&is.na(viajes$VIAJES)]<-0
viajes$DIST_TOTAL_KM_EST[viajes$AÑO==2015&is.na(viajes$DIST_TOTAL_KM_EST)]<-0

# Interpolación lineal de valores faltantes----
#Interpolacion de años intermedios
viajes %<>%
  group_by(GR_EDAD,SEXO,LOC_SRC,LOC_DST) %>%
  mutate(VIAJESINT = na.approx(VIAJES, na.rm=FALSE),DIST_TOTAL_KMINT = na.approx(DIST_TOTAL_KM_EST,na.rm=FALSE))
viajes<-as.data.frame(viajes)

#Interpolación de años fuera del rango
viajes %<>%
  group_by(GR_EDAD,SEXO,LOC_SRC,LOC_DST) %>%
  mutate(VIAJESINT2 = pmax(0,cbind(rep(1,2),c(2005:2017))%*%as.matrix(lm(VIAJESINT[7:11]~c(2011:2015))$coefficients)),
         DIST_TOTAL_KMINT2 = pmax(0,cbind(rep(1,2),c(2005:2017))%*%as.matrix(lm(DIST_TOTAL_KMINT[7:11]~c(2011:2015))$coefficients)))
viajes<-as.data.frame(viajes)
viajes$VIAJESINT[is.na(viajes$VIAJESINT)]<-viajes$VIAJESINT2[is.na(viajes$VIAJESINT)]
viajes$VIAJESINT2<-NULL
viajes$DIST_TOTAL_KMINT[is.na(viajes$DIST_TOTAL_KMINT)]<-viajes$DIST_TOTAL_KMINT2[is.na(viajes$DIST_TOTAL_KMINT)]
viajes$DIST_TOTAL_KMINT2<-NULL

# Guardado de los resultados de viajes y distancia recorrida entre localidades----
save(viajes,file=paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/10. Viajes_KilometrosRecorridos.Rdata"))
