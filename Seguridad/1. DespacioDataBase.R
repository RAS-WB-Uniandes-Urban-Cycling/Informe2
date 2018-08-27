#Preparación del entorno de trabajo----
rm(list=ls())
carpetaRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/Uniandes - RAS - SDM"
gitRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/proBikePolicies"
require(pacman)
pacman::p_load(readxl)
pacman::p_load(gdata)
setwd(paste(carpetaRAS,"/RESULTADOS/SEGURIDAD/Resultados",sep=""))
readPath<-paste(carpetaRAS,"/BASES DE DATOS/Despacio",sep="")

#Leer bases de datos de accidentalidad----
Accidentes<-lapply(file.path(readPath,list.files(readPath)),read_excel, sheet="Accidentes")
names(Accidentes)<-list.files(readPath)
Victimas<-lapply(file.path(readPath,list.files(readPath)),read_excel, sheet="Victimas")
names(Victimas)<-list.files(readPath)
Conductores<-lapply(file.path(readPath,list.files(readPath)),read_excel, sheet="Conductores")
names(Conductores)<-list.files(readPath)

#Consolidación de nombres de variables para Accidentes y Victimas----
names(Accidentes$`2011-BASE_SINIESTROS.xlsx`)<-paste("Accidentes.",names(Accidentes$`2011-BASE_SINIESTROS.xlsx`),sep="")
names(Accidentes$`2012_BASE _SINIESTROS.xlsx`)<-paste("Accidentes.",names(Accidentes$`2012_BASE _SINIESTROS.xlsx`),sep="")
names(Accidentes$`2013_BASE_SINIESTROS.xlsx`)<-paste("Accidentes.",names(Accidentes$`2013_BASE_SINIESTROS.xlsx`),sep="")
names(Accidentes$`2014_BASE_SINIESTROS.xlsx`)<-paste("Accidentes.",names(Accidentes$`2014_BASE_SINIESTROS.xlsx`),sep="")
names(Accidentes$`2015_BASE_SINIESTROS.xlsx`)<-paste("Accidentes.",names(Accidentes$`2015_BASE_SINIESTROS.xlsx`),sep="")

names(Accidentes$`2011-BASE_SINIESTROS.xlsx`)[names(Accidentes$`2011-BASE_SINIESTROS.xlsx`)=="Accidentes.Accidente"]<-"Accidente"
names(Accidentes$`2012_BASE _SINIESTROS.xlsx`)[names(Accidentes$`2012_BASE _SINIESTROS.xlsx`)=="Accidentes.Accidente"]<-"Accidente"
names(Accidentes$`2013_BASE_SINIESTROS.xlsx`)[names(Accidentes$`2013_BASE_SINIESTROS.xlsx`)=="Accidentes.Accidente"]<-"Accidente"
names(Accidentes$`2014_BASE_SINIESTROS.xlsx`)[names(Accidentes$`2014_BASE_SINIESTROS.xlsx`)=="Accidentes.Accidente"]<-"Accidente"
names(Accidentes$`2015_BASE_SINIESTROS.xlsx`)[names(Accidentes$`2015_BASE_SINIESTROS.xlsx`)=="Accidentes.Accidente"]<-"Accidente"

names(Victimas$`2011-BASE_SINIESTROS.xlsx`)<-paste("Victimas.",names(Victimas$`2011-BASE_SINIESTROS.xlsx`),sep="")
names(Victimas$`2012_BASE _SINIESTROS.xlsx`)<-paste("Victimas.",names(Victimas$`2012_BASE _SINIESTROS.xlsx`),sep="")
names(Victimas$`2013_BASE_SINIESTROS.xlsx`)<-paste("Victimas.",names(Victimas$`2013_BASE_SINIESTROS.xlsx`),sep="")
names(Victimas$`2014_BASE_SINIESTROS.xlsx`)<-paste("Victimas.",names(Victimas$`2014_BASE_SINIESTROS.xlsx`),sep="")
names(Victimas$`2015_BASE_SINIESTROS.xlsx`)<-paste("Victimas.",names(Victimas$`2015_BASE_SINIESTROS.xlsx`),sep="")

names(Victimas$`2011-BASE_SINIESTROS.xlsx`)[names(Victimas$`2011-BASE_SINIESTROS.xlsx`)=="Victimas.Accidente"]<-"Accidente"
names(Victimas$`2012_BASE _SINIESTROS.xlsx`)[names(Victimas$`2012_BASE _SINIESTROS.xlsx`)=="Victimas.Accidente"]<-"Accidente"
names(Victimas$`2013_BASE_SINIESTROS.xlsx`)[names(Victimas$`2013_BASE_SINIESTROS.xlsx`)=="Victimas.Accidente"]<-"Accidente"
names(Victimas$`2014_BASE_SINIESTROS.xlsx`)[names(Victimas$`2014_BASE_SINIESTROS.xlsx`)=="Victimas.Accidente"]<-"Accidente"
names(Victimas$`2015_BASE_SINIESTROS.xlsx`)[names(Victimas$`2015_BASE_SINIESTROS.xlsx`)=="Victimas.Accidente"]<-"Accidente"

#Integración de las bases de accidentes y victimas a la base de conductores----
Conductores$`2011-BASE_SINIESTROS.xlsx`<-merge(x=Conductores$`2011-BASE_SINIESTROS.xlsx`,y=Accidentes$`2011-BASE_SINIESTROS.xlsx`, by="Accidente",all.x=TRUE)
Conductores$`2011-BASE_SINIESTROS.xlsx`<-merge(x=Conductores$`2011-BASE_SINIESTROS.xlsx`,y=Victimas$`2011-BASE_SINIESTROS.xlsx`,by="Accidente",all.x=TRUE)
Conductores$`2012_BASE _SINIESTROS.xlsx`<-merge(x=Conductores$`2012_BASE _SINIESTROS.xlsx`,y=Accidentes$`2012_BASE _SINIESTROS.xlsx`, by="Accidente",all.x=TRUE)
Conductores$`2012_BASE _SINIESTROS.xlsx`<-merge(x=Conductores$`2012_BASE _SINIESTROS.xlsx`,y=Victimas$`2012_BASE _SINIESTROS.xlsx`,by="Accidente",all.x=TRUE)
Conductores$`2013_BASE_SINIESTROS.xlsx`<-merge(x=Conductores$`2013_BASE_SINIESTROS.xlsx`,y=Accidentes$`2013_BASE_SINIESTROS.xlsx`, by="Accidente",all.x=TRUE)
Conductores$`2013_BASE_SINIESTROS.xlsx`<-merge(x=Conductores$`2013_BASE_SINIESTROS.xlsx`,y=Victimas$`2013_BASE_SINIESTROS.xlsx`,by="Accidente",all.x=TRUE)
names(Accidentes$`2014_BASE_SINIESTROS.xlsx`)[1]<-"Accidente"
Conductores$`2014_BASE_SINIESTROS.xlsx`<-merge(x=Conductores$`2014_BASE_SINIESTROS.xlsx`,y=Accidentes$`2014_BASE_SINIESTROS.xlsx`, by="Accidente",all.x=TRUE)
Conductores$`2014_BASE_SINIESTROS.xlsx`<-merge(x=Conductores$`2014_BASE_SINIESTROS.xlsx`,y=Victimas$`2014_BASE_SINIESTROS.xlsx`,by="Accidente",all.x=TRUE)
Conductores$`2015_BASE_SINIESTROS.xlsx`<-merge(x=Conductores$`2015_BASE_SINIESTROS.xlsx`,y=Accidentes$`2015_BASE_SINIESTROS.xlsx`, by="Accidente",all.x=TRUE)
Conductores$`2015_BASE_SINIESTROS.xlsx`<-merge(x=Conductores$`2015_BASE_SINIESTROS.xlsx`,y=Victimas$`2015_BASE_SINIESTROS.xlsx`,by="Accidente",all.x=TRUE)
rm(Accidentes,Victimas)

#Unificación de la base de datos----
c1<-sort(names(Conductores$`2011-BASE_SINIESTROS.xlsx`))
length(c1)<-117
c2<-sort(names(Conductores$`2012_BASE _SINIESTROS.xlsx`))
length(c2)<-117
c3<-sort(names(Conductores$`2013_BASE_SINIESTROS.xlsx`))
length(c3)<-117
c4<-sort(names(Conductores$`2014_BASE_SINIESTROS.xlsx`))
length(c4)<-117
c5<-sort(names(Conductores$`2015_BASE_SINIESTROS.xlsx`))
length(c5)<-117
variables<-cbind(c1,c2,c3,c4,c5)
write.csv(variables,"./1. Variables1.csv")
lista<-c(c1,c2,c3,c4,c5)
write.csv(sort(unique(lista)),"./1. Lista_variables1.csv")

names(Conductores$`2011-BASE_SINIESTROS.xlsx`)[names(Conductores$`2011-BASE_SINIESTROS.xlsx`)=="Accidentes.Tpoa1"]<-"Accidentes.TipoVia1"
names(Conductores$`2011-BASE_SINIESTROS.xlsx`)[names(Conductores$`2011-BASE_SINIESTROS.xlsx`)=="Accidentes.Tpoa2"]<-"Accidentes.TipoVia2"
names(Conductores$`2013_BASE_SINIESTROS.xlsx`)[names(Conductores$`2013_BASE_SINIESTROS.xlsx`)=="EDAD"]<-"Edad"
Conductores$`2013_BASE_SINIESTROS.xlsx`$PortaLicencia<-NA
Conductores$`2014_BASE_SINIESTROS.xlsx`$Victimas.Clinica<-NA
names(Conductores$`2015_BASE_SINIESTROS.xlsx`)[names(Conductores$`2015_BASE_SINIESTROS.xlsx`)=="Victimas.EDAD_PROCESADA"]<-"Victimas.Edad"
names(Conductores$`2013_BASE_SINIESTROS.xlsx`)[names(Conductores$`2013_BASE_SINIESTROS.xlsx`)=="Victimas.EDAD"]<-"Victimas.Edad"
Conductores$`2013_BASE_SINIESTROS.xlsx`$Victimas.Fecha<-NA
names(Conductores$`2014_BASE_SINIESTROS.xlsx`)[names(Conductores$`2014_BASE_SINIESTROS.xlsx`)=="Accidentes.CausaConductorIDesc"]<-"Accidentes.CausaIDescripcion"
names(Conductores$`2014_BASE_SINIESTROS.xlsx`)[names(Conductores$`2014_BASE_SINIESTROS.xlsx`)=="Accidentes.CausaConductorIIDesc"]<"Accidentes.CausaIIDescripcion"
names(Conductores$`2015_BASE_SINIESTROS.xlsx`)[names(Conductores$`2015_BASE_SINIESTROS.xlsx`)=="Accidentes.CausaConductorIDesc"]<-"Accidentes.CausaIDescripcion"
names(Conductores$`2015_BASE_SINIESTROS.xlsx`)[names(Conductores$`2015_BASE_SINIESTROS.xlsx`)=="Accidentes.CausaConductorIIDesc"]<-"Accidentes.CausaIIDescripcion"
names(Conductores$`2011-BASE_SINIESTROS.xlsx`)[names(Conductores$`2011-BASE_SINIESTROS.xlsx`)=="Accidentes.TipoDiseño"]<-c("Accidentes.TipoDiseno")
names(Conductores$`2012_BASE _SINIESTROS.xlsx`)[names(Conductores$`2012_BASE _SINIESTROS.xlsx`)=="Accidentes.TipoDise?o"]<-c("Accidentes.TipoDiseno")
names(Conductores$`2013_BASE_SINIESTROS.xlsx`)[names(Conductores$`2013_BASE_SINIESTROS.xlsx`)=="Accidentes.TipoDise?o"]<-c("Accidentes.TipoDiseno")
names(Conductores$`2014_BASE_SINIESTROS.xlsx`)[names(Conductores$`2014_BASE_SINIESTROS.xlsx`)=="Accidentes.TipoDiseño"]<-c("Accidentes.TipoDiseno")
names(Conductores$`2015_BASE_SINIESTROS.xlsx`)[names(Conductores$`2015_BASE_SINIESTROS.xlsx`)=="Accidentes.TipoDiseño"]<-c("Accidentes.TipoDiseno")
names(Conductores$`2012_BASE _SINIESTROS.xlsx`)[names(Conductores$`2012_BASE _SINIESTROS.xlsx`)=="Victimas.GRAVEDAD"]<-c("Victimas.Gravedad")
names(Conductores$`2013_BASE_SINIESTROS.xlsx`)[names(Conductores$`2013_BASE_SINIESTROS.xlsx`)=="Victimas.GRAVEDAD_CALCULADA"]<-c("Victimas.Gravedad")
names(Conductores$`2014_BASE_SINIESTROS.xlsx`)[names(Conductores$`2014_BASE_SINIESTROS.xlsx`)=="Victimas.GRAVEDAD"]<-c("Victimas.Gravedad")
names(Conductores$`2015_BASE_SINIESTROS.xlsx`)[names(Conductores$`2015_BASE_SINIESTROS.xlsx`)=="Victimas.GRAVEDAD_PROCESADA"]<-c("Victimas.Gravedad")
names(Conductores$`2014_BASE_SINIESTROS.xlsx`)[names(Conductores$`2014_BASE_SINIESTROS.xlsx`)=="Accidentes.CausaConductorIIDesc"]<-"Accidentes.CausaIIDescripcion"
names(Conductores$`2015_BASE_SINIESTROS.xlsx`)[names(Conductores$`2015_BASE_SINIESTROS.xlsx`)=="EDAD_PROCESADA"]<-"Edad"
names(Conductores$`2012_BASE _SINIESTROS.xlsx`)[names(Conductores$`2012_BASE _SINIESTROS.xlsx`)=="GRAVEDAD"]<-"Gravedad"
names(Conductores$`2013_BASE_SINIESTROS.xlsx`)[names(Conductores$`2013_BASE_SINIESTROS.xlsx`)=="GRAVEDAD"]<-"Gravedad"
names(Conductores$`2014_BASE_SINIESTROS.xlsx`)[names(Conductores$`2014_BASE_SINIESTROS.xlsx`)=="GRAVEDAD_CALCULADA"]<-"Gravedad"
names(Conductores$`2015_BASE_SINIESTROS.xlsx`)[names(Conductores$`2015_BASE_SINIESTROS.xlsx`)=="GRAVEDAD_PROCESADA"]<-"Gravedad"
c1<-sort(names(Conductores$`2011-BASE_SINIESTROS.xlsx`))
length(c1)<-117
c2<-sort(names(Conductores$`2012_BASE _SINIESTROS.xlsx`))
length(c2)<-117
c3<-sort(names(Conductores$`2013_BASE_SINIESTROS.xlsx`))
length(c3)<-117
c4<-sort(names(Conductores$`2014_BASE_SINIESTROS.xlsx`))
length(c4)<-117
c5<-sort(names(Conductores$`2015_BASE_SINIESTROS.xlsx`))
length(c5)<-117
variables<-cbind(c1,c2,c3,c4,c5)
write.csv(variables,"./1. Variables2.csv")
lista<-c(c1,c2,c3,c4,c5)
write.csv(sort(unique(lista)),"./1. Lista_variables2.csv")

VariablesFinales<-c("Accidente",
                    "Accidentes.CardinalVia1",
                    "Accidentes.CardinalVia2",
                    "Accidentes.CausaIDescripcion",
                    "Accidentes.CausaIIDescripcion",
                    "Accidentes.ChoqueNombre",
                    "Accidentes.ClaseNombre",
                    "Accidentes.CorredorVial",
                    "Accidentes.Direccion",
                    "Accidentes.Fecha",
                    "Accidentes.GravedadNombre",
                    "Accidentes.HoraOcurrencia",
                    "Accidentes.Latitud",
                    "Accidentes.LetraVia1",
                    "Accidentes.LetraVia2",
                    "Accidentes.Localidad",
                    "Accidentes.Longitud",
                    "Accidentes.NumeroVia1",
                    "Accidentes.NumeroVia2",
                    "Accidentes.ObjetoFijoCodigo",
                    "Accidentes.ObjetoFijoNombre",
                    "Accidentes.TipoDiseno",
                    "Accidentes.TipoTiempo",
                    "Accidentes.TipoVia1",
                    "Accidentes.TipoVia2",
                    "Accidentes.TotalHeridos",
                    "Accidentes.TotalMuertos",
                    "CantidadPasajeros",
                    "CapacidadCarga",
                    "ClaseVehiculo",
                    "Clinica",
                    "CodigoMarcaVehiculo",
                    "Edad",
                    "EsPropietarioVehiculo",
                    "Fecha",
                    "Gravedad",
                    "LLevaCasco",
                    "LLevaCinturon",
                    "ModalidadVehiculo",
                    "ModeloVehiculo",
                    "NacionalidadVehiculo",
                    "PortaLicencia",
                    "ServicioVehiculo",
                    "Sexo",
                    "Vehiculo",
                    "VehiculoEnFuga",
                    "Victimas.Clinica",
                    "Victimas.CodigoVehiculo",
                    "Victimas.Edad",
                    "Victimas.Fecha",
                    "Victimas.Gravedad",
                    "Victimas.LLevaCasco?",
                    "Victimas.LLevaCinturon?",
                    "Victimas.Peaton-Pasajero?",
                    "Victimas.Sexo")

AccidentesTotales<-rbind(Conductores$`2011-BASE_SINIESTROS.xlsx`[,VariablesFinales],
                         Conductores$`2012_BASE _SINIESTROS.xlsx`[,VariablesFinales],
                         Conductores$`2013_BASE_SINIESTROS.xlsx`[,VariablesFinales],
                         Conductores$`2014_BASE_SINIESTROS.xlsx`[,VariablesFinales],
                         Conductores$`2015_BASE_SINIESTROS.xlsx`[,VariablesFinales])

AccidentesTotales$Accidentes.HoraOcurrencia<-as.POSIXct(paste(AccidentesTotales$Accidentes.Fecha," ",as.POSIXlt(AccidentesTotales$Accidentes.HoraOcurrencia)$hour,":",as.POSIXlt(AccidentesTotales$Accidentes.HoraOcurrencia)$min,sep=""),format="%Y-%m-%d %H:%M",zt="UTC")

#Extracción de los accidentes con bicicletas----
AccidentesBici<-AccidentesTotales[AccidentesTotales$ClaseVehiculo=="Bicicleta",]
bici2011<-AccidentesBici[getYear(AccidentesBici$Accidentes.Fecha)==2011,]
bici2012<-AccidentesBici[getYear(AccidentesBici$Accidentes.Fecha)==2012,]
bici2013<-AccidentesBici[getYear(AccidentesBici$Accidentes.Fecha)==2013,]
bici2014<-AccidentesBici[getYear(AccidentesBici$Accidentes.Fecha)==2014,]
bici2015<-AccidentesBici[getYear(AccidentesBici$Accidentes.Fecha)==2015,]

#Datos faltantes en cada base de datos----
PercentMissing<-as.data.frame(matrix(NA,6,2))
colnames(PercentMissing)<-c("Año","Percent Missing")
PercentMissing$Año[1:5]<-2011:2015
PercentMissing$Año[6]<-"Total"
PercentMissing[1,2]<-sum(is.na(bici2011))/(dim(bici2011)[1]*dim(bici2011)[2])
PercentMissing[2,2]<-sum(is.na(bici2012))/(dim(bici2012)[1]*dim(bici2012)[2])
PercentMissing[3,2]<-sum(is.na(bici2013))/(dim(bici2013)[1]*dim(bici2013)[2])
PercentMissing[4,2]<-sum(is.na(bici2014))/(dim(bici2014)[1]*dim(bici2014)[2])
PercentMissing[5,2]<-sum(is.na(bici2015))/(dim(bici2015)[1]*dim(bici2015)[2])
PercentMissing[6,2]<-sum(is.na(AccidentesBici))/(dim(AccidentesBici)[1]*dim(AccidentesBici)[2])
write.csv(PercentMissing,"./1. PercentMissingDespacio.csv")
rm(PercentMissing)

#Limpieza del espacio de trabajo----
rm(bici2011,bici2012,bici2013,bici2014,bici2015,variables,c1,c2,c3,c4,c5,lista,VariablesFinales,readPath,Conductores)
save(AccidentesBici,AccidentesTotales,file=paste(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/1. AccidentesDespacio2011_2015.Rdata",sep=""))
