#Preparacion del entorno de trabajo----
rm(list=ls())
carpetaRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/UNIANDES - RAS - SDM"
gitRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/proBikePolicies"
require(pacman)
pacman::p_load(plyr)
pacman::p_load(tidyverse)
pacman::p_load(gdata)
pacman::p_load(readr)
pacman::p_load(readxl)
pacman::p_load(reshape2)

# Carga de la función de codificación de la edad por grupos----
load(paste(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/4. GruposEdadCodificacion.Rdata",sep=""))

# Lectura de la base de datos de poblacion por grupos de edad, por género y por localidades para los años 2005-2015----
Poblacion<- lapply(excel_sheets(paste0(carpetaRAS,"/BASES DE DATOS/DANE/dice015-proyeccionesbogota-loca_edad_sex.xlsx")), read_excel, path=paste0(carpetaRAS,"/BASES DE DATOS/DANE/dice015-proyeccionesbogota-loca_edad_sex.xlsx"),skip=2)
names(Poblacion)<-substring(excel_sheets(paste0(carpetaRAS,"/BASES DE DATOS/DANE/dice015-proyeccionesbogota-loca_edad_sex.xlsx")),4)
Poblacion[[1]]<-NULL
Poblacion[[1]]<-NULL
for(i in 1:20){
  Poblacion[[i]]$SEXO<-NA
  Poblacion[[i]]$SEXO[92:108]<-"Male"
  Poblacion[[i]]$SEXO[113:129]<-"Female"
  Poblacion[[i]]<-na.omit(Poblacion[[i]])
  Poblacion[[i]]$LocNombre<-toupper(names(Poblacion)[i])
}
Poblacion <- ldply(Poblacion, data.frame,.id = NULL)
Poblacion$Age<-as.numeric(ldply(map(strsplit(gsub(" ","-",Poblacion$Grupos.de.edad),split = "-",fixed=FALSE),1),data.frame.id=NULL)[,1])
Poblacion$Grupos.de.edad<-NULL
Poblacion$GR_EDAD<-factor(apply(as.data.frame(Poblacion$Age),MARGIN=1,FUN=encodeEdad2,g=gruposEdad),levels = paste0(gruposEdad$ED_MIN,"-",gruposEdad$ED_MAX))
Poblacion$Age<-NULL
Poblacion<-melt(Poblacion,id.vars = c("SEXO","LocNombre","GR_EDAD"),variable.name = "AÑO",value.name = "POBLACION") %>% transform(AÑO=gsub("X","",AÑO))
Poblacion$LocNombre[Poblacion$LocNombre=="LA_CANDELARIA"]<-"CANDELARIA"
Poblacion$LocNombre[Poblacion$LocNombre==" SANTA FE"]<-"SANTA FE"
Poblacion$LocNombre<-gsub("Á","A",Poblacion$LocNombre)
Poblacion$LocNombre<-gsub("É","E",Poblacion$LocNombre)
Poblacion$LocNombre<-gsub("Í","I",Poblacion$LocNombre)
Poblacion$LocNombre<-gsub("Ó","O",Poblacion$LocNombre)
Poblacion<-aggregate(Poblacion$POBLACION,by=list(Poblacion$SEXO,Poblacion$LocNombre,Poblacion$GR_EDAD,Poblacion$AÑO),FUN=sum) %>% select(SEXO=Group.1,LOCALIDAD=Group.2,GR_EDAD=Group.3,AÑO=Group.4,POBLACION=x)  

# Poblacion total por localidad----
PoblacionLoc<-read_excel(paste(carpetaRAS,"/BASES DE DATOS/DANE/DICE015A-ProyeccionesLocalidades-2016.xls.xlsx",sep=""), sheet = "Serie_regularizada_SDP", range = "A6:AK26")
PoblacionLoc<-melt(PoblacionLoc,id.vars=c("Localidad"),variable.name = "AÑO",value.name = "POBLACION")
PoblacionLoc$AÑO<-as.character(PoblacionLoc$AÑO)

#Estimación de la población por grupo de edad y género para cada localidad en los años 2016 y 2017----
referencia <- Poblacion[Poblacion$AÑO==2015,] 
referencia2<-aggregate(referencia$POBLACION,by=list(referencia$LOCALIDAD),FUN=sum)
referencia<-merge(referencia,referencia2,by.x="LOCALIDAD",by.y="Group.1")
rm(referencia2)
referencia$w<-referencia$POBLACION/referencia$x
referencia$POBLACION<-NULL
referencia$x<-NULL
referencia$AÑO<-NULL
est2016<-merge(referencia,PoblacionLoc[PoblacionLoc$AÑO==2016,],by.x="LOCALIDAD",by.y="Localidad",all.x=TRUE)
est2016$POBLACION<-est2016$w*est2016$POBLACION
est2016$w<-NULL
est2017<-merge(referencia,PoblacionLoc[PoblacionLoc$AÑO==2017,],by.x="LOCALIDAD",by.y="Localidad",all.x=TRUE)
est2017$POBLACION<-est2017$w*est2017$POBLACION
est2017$w<-NULL
est16_17<-rbind(est2016,est2017)
rm(est2016,est2017,referencia,PoblacionLoc)

#Integración de la estimacion para 2016 y 2017 con la base de datos de poblacion por género, grupo de edad, localidad y año
Poblacion<-rbind(Poblacion,est16_17)

#Camparación de las estimaciones propuestas y las estimaciones oficiales para los años 2016 y 2017----
# Lectura de las estimaciones de poblacion oficiales para 2016 y 2017
PoblacionTot<-read.csv(paste0(carpetaRAS,"/BASES DE DATOS/DANE/VisorCertificaPPO_Oct11_SinProtección.csv"),skip=2) #Base de datos nacional
PoblacionTot<-PoblacionTot[PoblacionTot$DPMP==11,1:251] #Filtrado para la ciudad de bogotá
PoblacionTot<-PoblacionTot[,5:251] #PoblacionTot$Año>=2011
PoblacionTot<-reshape(data=PoblacionTot,varying = names(PoblacionTot)[-1],timevar="Edad",idvar = "Año",direction = "long",sep=".")
PoblacionTot<-PoblacionTot[PoblacionTot$Edad!="Total",]
PoblacionTot<-PoblacionTot[,-3]
names(PoblacionTot)<-c("ANO","EDAD","Male","Female")
PoblacionTot<-melt(PoblacionTot,id = c("ANO","EDAD"),value.name="POBLACION")
names(PoblacionTot)[3]<-"SEXO"
PoblacionTot$EDAD<-as.numeric(PoblacionTot$EDAD)
PoblacionTot$POBLACION<-as.numeric(gsub(",","",PoblacionTot$POBLACION))
PoblacionTot$GR_EDAD<-factor(apply(as.data.frame(PoblacionTot$EDAD),MARGIN=1,FUN=encodeEdad2,g=gruposEdad),levels = paste0(gruposEdad$ED_MIN,"-",gruposEdad$ED_MAX))
PoblacionTot<-aggregate(PoblacionTot$POBLACION,by=list(PoblacionTot$GR_EDAD,PoblacionTot$SEXO,PoblacionTot$ANO),FUN=sum)
names(PoblacionTot)<-c("GR_EDAD","SEXO","AÑO","POBLACION")
PoblacionTot<-PoblacionTot[PoblacionTot$AÑO%in%2016:2017,]

#Agregación de las estimaciones de poblacion propuestas
prop<-aggregate(est16_17$POBLACION,by=list(est16_17$SEXO,est16_17$GR_EDAD,est16_17$AÑO),FUN=sum) %>% select(SEXO=Group.1,GR_EDAD=Group.2,AÑO=Group.3,POBLACION_prop=x)

#Integración de las estimaciones
estimations<-merge(PoblacionTot,prop,by=c("AÑO","SEXO","GR_EDAD"))
rm(PoblacionTot,prop,est16_17)  
estimations$DEV_FROM_OF<-paste((estimations$POBLACION_prop-estimations$POBLACION)*100/estimations$POBLACION,"%")

#Almacenamiento de resultados----
write.csv(estimations,paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/TABLAS/6. EstimacionesPoblacion2016_2017.csv"))
save(Poblacion,file=paste(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/6. Poblacion.Rdata",sep=""))
