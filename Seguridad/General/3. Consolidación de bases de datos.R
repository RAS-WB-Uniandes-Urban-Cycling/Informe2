#Preparación del entorno de trabajo----
rm(list=ls())
carpetaRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/Uniandes - RAS - SDM"
gitRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/proBikePolicies"
require(pacman)
pacman::p_load(sf)
pacman::p_load(dplyr)
pacman::p_load(gdata)

#Carga de las diferentes bases de datos en memoria----
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/2. AccidentesCoords.Rdata"))
despacio<-AccidentesBici
rm(AccidentesBici)
despacio<-st_as_sf(despacio, coords = c("lon", "lat"), crs = 4326)
rm(AccidentesBici)
wri<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/WRI"))
SDM1<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Shapefiles"),layer="Accidente")
names(SDM1)<-paste0("Accidentes.",names(SDM1))
names(SDM1)[2]<-"FORMULARIO"
SDM2<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Shapefiles"),layer="Causa")
SDM2<-SDM2[!duplicated(SDM2$FORMULARIO),]
names(SDM2)<-paste0("Causa.",names(SDM2))
names(SDM2)[2]<-"FORMULARIO"
SDM3<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Shapefiles"),layer="Actor_vial")
SDM3<-SDM3[!duplicated(SDM3$FORMULARIO),]
SDM<-merge(as.data.frame(SDM1),SDM2,by="FORMULARIO",all.x=TRUE)
SDM<-merge(SDM,SDM3,by="FORMULARIO",all.x=TRUE)
st_geometry(SDM)<-SDM$Accidentes.geometry
rm(SDM1,SDM2,SDM3)

#Filtrado de los accidentes de ciclistas----
wri<-wri[(wri$CONDICION=="CICLISTA"),]

# Identificación de accidentes adicionales para adición de las bases de datos----
#Construcción de llaves de identificación únicas de accidentes entre bases de datos
SDM$KEY<-paste(as.POSIXct(paste(SDM$Accidentes.FECHA_OCUR,SDM$Accidentes.HORA_OCURR)),"-",SDM$Accidentes.DIRECCION)
wri$KEY<-paste(as.POSIXct(paste(wri$FECHA_OCUR,wri$HORA_OCURR)),"-",wri$DIRECCION)
despacio$KEY<-paste(despacio$Accidentes.HoraOcurrencia,"-",despacio$Accidentes.Direccion)

# Se define un orden de prioridad para las bases de datos dada su connotación oficial: 1. SDM, 2. WRI, 3. Despacio
wri_add<-wri[!wri$KEY%in%SDM$KEY,]
despacio_add<-despacio[!despacio$KEY%in%SDM$KEY,]
despacio_add<-despacio_add[!despacio_add$KEY%in%wri$KEY,]
rm(wri,despacio)

#Integración de variables y nombres para unificación----
namesDespacio<-names(despacio_add)
length(namesDespacio)<-59
nameswri<-names(wri_add)
length(nameswri)<-59
namesSDM<-names(SDM)
length(namesSDM)<-59
write.csv(cbind(namesDespacio,nameswri,namesSDM),paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Resultados/TABLAS/3. NombresUnificarBases.csv"))
rm(namesDespacio,namesSDM,nameswri)

#Nombres despacio
despacio_add$Condicion<-NA
despacio_add$Muerte_POS<-NA
despacio_add$Fecha_POS<-NA
despacio_add$Fallece<-NA
despacio_add$Source<-"Despacio"

#Nombres wri
wri_add$Accidentes.Fecha<-as.POSIXct(as.character(wri_add$FECHA_OCUR), format="%Y-%m-%d")
wri_add$Accidentes.HoraOcurrencia<-as.POSIXct(paste(wri_add$FECHA_OCUR,wri_add$HORA_OCURR))
wri_add$HORA_OCURR<-NULL
wri_add$FECHA_OCUR<-NULL
wri_add$OBJECTID_1<-NULL
wri_add$OBJECTID<-NULL
wri_add$FORMULARIO<-NULL
wri_add$ANO_OCURRE<-NULL
wri_add$MES_OCURRE<-NULL
wri_add$DIA_OCURRE<-NULL
wri_add$AREAS_TRAN<-NULL
wri_add$CUADRANTE_<-NULL
wri_add$FECHA_HORA<-NULL
names(wri_add)[grep("CODIGO_ACC",names(wri_add))]<-c("Accidente")
names(wri_add)[grep("DIRECCION",names(wri_add))]<-c("Accidentes.Direccion")
names(wri_add)[grep("CLASE",names(wri_add))]<-c("Accidentes.ClaseNombre")
names(wri_add)[grep("LOCALIDAD",names(wri_add))]<-c("Accidentes.Localidad")
names(wri_add)[grep("CODIGO_VEH",names(wri_add))]<-c("Vehiculo")
names(wri_add)[grep("CONDICION",names(wri_add))]<-c("Condicion")
names(wri_add)[grep("HOSP_VALOR",names(wri_add))]<-c("Gravedad")
names(wri_add)[grep("GENERO",names(wri_add))]<-c("Sexo")
names(wri_add)[grep("EDAD",names(wri_add))]<-c("Edad")
names(wri_add)[grep("MUERTE_POS",names(wri_add))]<-c("Muerte_POS")
names(wri_add)[grep("FECHA_POS",names(wri_add))]<-c("Fecha_POS")
names(wri_add)[grep("f",names(wri_add))]<-c("Fallece")
wri_add$Source<-"WRI"
wri_add$Accidentes.CardinalVia1<-NA
wri_add$Accidentes.CardinalVia2<-NA
wri_add$Accidentes.CausaIDescripcion<-NA
wri_add$Accidentes.CausaIIDescripcion<-NA
wri_add$Accidentes.ChoqueNombre<-NA
wri_add$Accidentes.CorredorVial<-NA
wri_add$Accidentes.GravedadNombre<-NA
wri_add$Accidentes.Latitud<-NA
wri_add$Accidentes.LetraVia1<-NA
wri_add$Accidentes.LetraVia2<-NA
wri_add$Accidentes.Longitud<-NA
wri_add$Accidentes.NumeroVia1<-NA
wri_add$Accidentes.NumeroVia2<-NA
wri_add$Accidentes.ObjetoFijoCodigo<-NA
wri_add$Accidentes.ObjetoFijoNombre<-NA
wri_add$Accidentes.TipoDiseno<-NA
wri_add$Accidentes.TipoTiempo<-NA
wri_add$Accidentes.TipoVia1<-NA
wri_add$Accidentes.TipoVia2<-NA
wri_add$Accidentes.TotalHeridos<-NA
wri_add$Accidentes.TotalMuertos<-NA
wri_add$CantidadPasajeros<-NA
wri_add$CapacidadCarga<-NA
wri_add$ClaseVehiculo<-NA
wri_add$Clinica<-NA
wri_add$CodigoMarcaVehiculo<-NA
wri_add$EsPropietarioVehiculo<-NA
wri_add$Fecha<-NA
wri_add$LLevaCasco<-NA
wri_add$LLevaCinturon<-NA
wri_add$ModalidadVehiculo<-NA
wri_add$ModeloVehiculo<-NA
wri_add$NacionalidadVehiculo<-NA
wri_add$PortaLicencia<-NA
wri_add$ServicioVehiculo<-NA
wri_add$VehiculoEnFuga<-NA
wri_add$Victimas.Clinica<-NA
wri_add$Victimas.CodigoVehiculo<-NA
wri_add$Victimas.Edad<-NA
wri_add$Victimas.Fecha<-NA
wri_add$Victimas.Gravedad<-NA
wri_add$`Victimas.LLevaCasco?`<-NA
wri_add$`Victimas.LLevaCinturon?`<-NA
wri_add$`Victimas.Peaton-Pasajero?`<-NA
wri_add$Victimas.Sexo<-NA
wri_add$Adress<-NA
wri_add$Accidente.1<-NA

#Nombres SDM
SDM$Source<-"SDM"
SDM$Fallece<-NA
SDM$Accidentes.CardinalVia1<-NA
SDM$Accidentes.CardinalVia2<-NA
SDM$Accidentes.CausaIIDescripcion<-NA
SDM$Accidentes.ChoqueNombre<-NA
SDM$Accidentes.CorredorVial<-NA
SDM$Accidentes.Latitud<-NA
SDM$Accidentes.LetraVia1<-NA
SDM$Accidentes.LetraVia2<-NA
SDM$Accidentes.Longitud<-NA
SDM$Accidentes.NumeroVia1<-NA
SDM$Accidentes.NumeroVia2<-NA
SDM$Accidentes.ObjetoFijoCodigo<-NA
SDM$Accidentes.ObjetoFijoNombre<-NA
SDM$Accidentes.TipoDiseno<-NA
SDM$Accidentes.TipoTiempo<-NA
SDM$Accidentes.TipoVia1<-NA
SDM$Accidentes.TipoVia2<-NA
SDM$Accidentes.TotalHeridos<-NA
SDM$Accidentes.TotalMuertos<-NA
SDM$CantidadPasajeros<-NA
SDM$CapacidadCarga<-NA
SDM$ClaseVehiculo<-NA
SDM$Clinica<-NA
SDM$CodigoMarcaVehiculo<-NA
SDM$EsPropietarioVehiculo<-NA
SDM$Fecha<-NA
SDM$LLevaCasco<-NA
SDM$LLevaCinturon<-NA
SDM$ModalidadVehiculo<-NA
SDM$ModeloVehiculo<-NA
SDM$NacionalidadVehiculo<-NA
SDM$PortaLicencia<-NA
SDM$ServicioVehiculo<-NA
SDM$VehiculoEnFuga<-NA
SDM$Victimas.Clinica<-NA
SDM$Victimas.CodigoVehiculo<-NA
SDM$Victimas.Edad<-NA
SDM$Victimas.Fecha<-NA
SDM$Victimas.Gravedad<-NA
SDM$`Victimas.LLevaCasco?`<-NA
SDM$`Victimas.LLevaCinturon?`<-NA
SDM$`Victimas.Peaton-Pasajero?`<-NA
SDM$Victimas.Sexo<-NA
SDM$Adress<-NA
SDM$Accidente.1<-NA
SDM$Accidentes.Fecha<-as.POSIXct(as.character(SDM$Accidentes.FECHA_OCUR), format="%Y-%m-%d")
SDM$Accidentes.HoraOcurrencia<-as.POSIXct(paste(SDM$Accidentes.FECHA_OCUR,SDM$Accidentes.HORA_OCURR))
SDM$Accidentes.HORA_OCURR<-NULL
SDM$Accidentes.FECHA_OCUR<-NULL
SDM$Accidentes.ANO_OCURRE<-NULL
SDM$Accidentes.AREAS_TRAN<-NULL
SDM$Accidentes.CUADRANTE_<-NULL
SDM$Accidentes.DIA_OCURRE<-NULL
SDM$Accidentes.ESTADO<-NULL
SDM$Accidentes.FECHA_HORA<-NULL
SDM$Accidentes.MES_OCURRE<-NULL
SDM$Accidentes.MUNICIPIO<-NULL
SDM$Accidentes.OBJECTID<-NULL
SDM$Accidentes.TEXTO_OBSE<-NULL
SDM$Causa.CODIGO<-NULL
SDM$Causa.CODIGO_ACC<-NULL
SDM$Causa.CODIGO_CAU<-NULL
SDM$Causa.CODIGO_VEH<-NULL
SDM$Causa.OBJECTID<-NULL
SDM$Causa.TIPO<-NULL
SDM$Causa.TIPO_CAUSA<-NULL
SDM$CODIGO<-NULL
SDM$CODIGO_ACC<-NULL
SDM$CODIGO_VIC<-NULL
SDM$FECHA_NACI<-NULL
SDM$FORMULARIO<-NULL
SDM$HOSP_VALOR<-NULL
SDM$OBJECTID<-NULL
names(SDM)[grep("Accidentes.CODIGO_ACC",names(SDM))]<-c("Accidente")
names(SDM)[grep("Accidentes.DIRECCION",names(SDM))]<-c("Accidentes.Direccion")
names(SDM)[grep("Accidentes.GRAVEDAD",names(SDM))]<-c("Accidentes.GravedadNombre")
names(SDM)[grep("Accidentes.CLASE",names(SDM))]<-c("Accidentes.ClaseNombre")
names(SDM)[grep("Accidentes.LOCALIDAD",names(SDM))]<-c("Accidentes.Localidad")
names(SDM)[grep("Accidentes.geometry",names(SDM))]<-c("geometry")
names(SDM)[grep("Causa.NOMBRE",names(SDM))]<-c("Accidentes.CausaIDescripcion")
names(SDM)[grep("CODIGO_VEH",names(SDM))]<-c("Vehiculo")
names(SDM)[grep("CONDICION",names(SDM))]<-c("Condicion")
names(SDM)[grep("ESTADO",names(SDM))]<-c("Gravedad")
names(SDM)[grep("MUERTE_POS",names(SDM))]<-c("Muerte_POS")
names(SDM)[grep("FECHA_POST",names(SDM))]<-c("Fecha_POS")
names(SDM)[grep("GENERO",names(SDM))]<-c("Sexo")
names(SDM)[grep("EDAD",names(SDM))]<-c("Edad")
st_geometry(SDM)<-SDM$geometry
SDM$Accidentes.geometry<-NULL

# Integración de bases de datos----
AccidentesBiciTotal<-as.data.frame(rbind(as.data.frame(SDM),as.data.frame(despacio_add),as.data.frame(wri_add)))
st_geometry(AccidentesBiciTotal)<-AccidentesBiciTotal$geometry
AccidentesBiciTotal$Accidentes.Localidad[AccidentesBiciTotal$Accidentes.Localidad=="ANTONIO NARINO"]<-"ANTONIO NARIÑO"
rm(SDM,despacio_add,wri_add)

#Datos faltantes en cada base de datos----
DataInformation<-as.data.frame(matrix(NA,length(unique(getYear(AccidentesBiciTotal$Accidentes.Fecha)))+1,3))
colnames(DataInformation)<-c("Año","Records","Percent Missing")
DataInformation$Año[1:length(unique(getYear(AccidentesBiciTotal$Accidentes.Fecha)))]<-sort(as.numeric(unique(getYear(AccidentesBiciTotal$Accidentes.Fecha))))
DataInformation$Año[length(unique(getYear(AccidentesBiciTotal$Accidentes.Fecha)))+1]<-"Total"
for(i in sort(as.numeric(unique(getYear(AccidentesBiciTotal$Accidentes.Fecha))))){
  DataInformation$Records[grep(i,DataInformation$Año)]<-dim(AccidentesBiciTotal[getYear(AccidentesBiciTotal$Accidentes.Fecha)==i,])[1]
  DataInformation$`Percent Missing`[grep(i,DataInformation$Año)]<-sum(is.na(AccidentesBiciTotal[getYear(AccidentesBiciTotal$Accidentes.Fecha)==i,]))/(dim(AccidentesBiciTotal[getYear(AccidentesBiciTotal$Accidentes.Fecha)==i,])[1]*dim(AccidentesBiciTotal[getYear(AccidentesBiciTotal$Accidentes.Fecha)==i,])[2])
}
rm(i)
DataInformation$Records[length(unique(getYear(AccidentesBiciTotal$Accidentes.Fecha)))+1]<-dim(AccidentesBiciTotal)[1]
DataInformation$`Percent Missing`[length(unique(getYear(AccidentesBiciTotal$Accidentes.Fecha)))+1]<-sum(is.na(AccidentesBiciTotal))/(dim(AccidentesBiciTotal)[1]*dim(AccidentesBiciTotal)[2])
write.csv(DataInformation,paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Resultados/TABLAS/3. PercentMissingConsolidado.csv"))
rm(DataInformation)

#Filtrado de la base para remover los años previos al 2011 por cambio de orden de magnitud (x5)----
AccidentesBiciTotal<-AccidentesBiciTotal[getYear(AccidentesBiciTotal$Accidentes.Fecha)>=2011,]

# Guardado de la base de datos definitiva
save(AccidentesBiciTotal,file=paste(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/3. AccidentesBiciTotalGeoData.Rdata",sep=""))
