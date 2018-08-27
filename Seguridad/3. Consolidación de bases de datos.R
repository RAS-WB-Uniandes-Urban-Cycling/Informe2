#Preparación del entorno de trabajo----
rm(list=ls())
carpetaRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/Uniandes - RAS - SDM"
gitRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/proBikePolicies"
require(pacman)
pacman::p_load(lwgeom)
pacman::p_load(sf)
pacman::p_load(dplyr)
pacman::p_load(gdata)
pacman::p_load(tmap)

#Carga de las diferentes bases de datos en memoria----
intersecciones<-readRDS(paste0(carpetaRAS,"/RESULTADOS/LTS/Bases de datos/Intersecciones.rds"))
Puentes<-st_read(paste(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR0318.gdb",sep=""),layer="Puen") %>% st_transform(crs=4326) %>%  st_make_valid
intersecciones<-st_join(intersecciones,Puentes[,c("PueCodigo")],left=TRUE) %>% filter(is.na(PueCodigo)) %>% mutate(PueCodigo=NULL)
Localidades<-st_read(paste(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR0318.gdb",sep=""),layer="Loca",crs=4326)
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/1. AccidentesDespacio2011_2015.Rdata"))
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/4. GruposEdadCodificacion.Rdata"))
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/2. AccidentesCoords.Rdata"))
despacio<-AccidentesBici
rm(AccidentesBici)
despacio<-st_as_sf(despacio, coords = c("lon", "lat"), crs = 4326)
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
wriTotales<-wri %>% st_set_geometry(NULL)
wri<-wri[(wri$CONDICION=="CICLISTA"),]

#Integración de variables y nombres  para las bases de datos de accidentes de bicicletas----
namesDespacio<-names(despacio)
length(namesDespacio)<-59
nameswri<-names(wri)
length(nameswri)<-59
namesSDM<-names(SDM)
length(namesSDM)<-59
write.csv(cbind(namesDespacio,nameswri,namesSDM),paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/TABLAS/3. NombresUnificarBases.csv"))
rm(namesDespacio,namesSDM,nameswri)

#Nombres despacio
despacio$Condicion<-NA
despacio$Muerte_POS<-NA
despacio$Fecha_POS<-NA
despacio$Fallece<-NA
despacio$Source<-"Despacio"

#Nombres wri
wri$Accidentes.Fecha<-as.POSIXct(as.character(wri$FECHA_OCUR), format="%Y-%m-%d")
wri$Accidentes.HoraOcurrencia<-as.POSIXct(paste(wri$FECHA_OCUR,wri$HORA_OCURR))
wri$HORA_OCURR<-NULL
wri$FECHA_OCUR<-NULL
wri$OBJECTID_1<-NULL
wri$OBJECTID<-NULL
wri$FORMULARIO<-NULL
wri$ANO_OCURRE<-NULL
wri$MES_OCURRE<-NULL
wri$DIA_OCURRE<-NULL
wri$AREAS_TRAN<-NULL
wri$CUADRANTE_<-NULL
wri$FECHA_HORA<-NULL
names(wri)[grep("CODIGO_ACC",names(wri))]<-c("Accidente")
names(wri)[grep("DIRECCION",names(wri))]<-c("Accidentes.Direccion")
names(wri)[grep("CLASE",names(wri))]<-c("Accidentes.ClaseNombre")
names(wri)[grep("LOCALIDAD",names(wri))]<-c("Accidentes.Localidad")
names(wri)[grep("CODIGO_VEH",names(wri))]<-c("Vehiculo")
names(wri)[grep("CONDICION",names(wri))]<-c("Condicion")
names(wri)[grep("HOSP_VALOR",names(wri))]<-c("Gravedad")
names(wri)[grep("GENERO",names(wri))]<-c("Sexo")
names(wri)[grep("EDAD",names(wri))]<-c("Edad")
names(wri)[grep("MUERTE_POS",names(wri))]<-c("Muerte_POS")
names(wri)[grep("FECHA_POS",names(wri))]<-c("Fecha_POS")
names(wri)[grep("f",names(wri))]<-c("Fallece")
wri$Source<-"WRI"
wri$Accidentes.CardinalVia1<-NA
wri$Accidentes.CardinalVia2<-NA
wri$Accidentes.CausaIDescripcion<-NA
wri$Accidentes.CausaIIDescripcion<-NA
wri$Accidentes.ChoqueNombre<-NA
wri$Accidentes.CorredorVial<-NA
wri$Accidentes.GravedadNombre<-NA
wri$Accidentes.Latitud<-NA
wri$Accidentes.LetraVia1<-NA
wri$Accidentes.LetraVia2<-NA
wri$Accidentes.Longitud<-NA
wri$Accidentes.NumeroVia1<-NA
wri$Accidentes.NumeroVia2<-NA
wri$Accidentes.ObjetoFijoCodigo<-NA
wri$Accidentes.ObjetoFijoNombre<-NA
wri$Accidentes.TipoDiseno<-NA
wri$Accidentes.TipoTiempo<-NA
wri$Accidentes.TipoVia1<-NA
wri$Accidentes.TipoVia2<-NA
wri$Accidentes.TotalHeridos<-NA
wri$Accidentes.TotalMuertos<-NA
wri$CantidadPasajeros<-NA
wri$CapacidadCarga<-NA
wri$ClaseVehiculo<-NA
wri$Clinica<-NA
wri$CodigoMarcaVehiculo<-NA
wri$EsPropietarioVehiculo<-NA
wri$Fecha<-NA
wri$LLevaCasco<-NA
wri$LLevaCinturon<-NA
wri$ModalidadVehiculo<-NA
wri$ModeloVehiculo<-NA
wri$NacionalidadVehiculo<-NA
wri$PortaLicencia<-NA
wri$ServicioVehiculo<-NA
wri$VehiculoEnFuga<-NA
wri$Victimas.Clinica<-NA
wri$Victimas.CodigoVehiculo<-NA
wri$Victimas.Edad<-NA
wri$Victimas.Fecha<-NA
wri$Victimas.Gravedad<-NA
wri$`Victimas.LLevaCasco?`<-NA
wri$`Victimas.LLevaCinturon?`<-NA
wri$`Victimas.Peaton-Pasajero?`<-NA
wri$Victimas.Sexo<-NA
wri$Adress<-NA
wri$Accidente.1<-NA

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

#Integración de bases de datos de accidentes de bicicletas----
AccidentesBiciTotal<-as.data.frame(rbind(as.data.frame(SDM),as.data.frame(despacio),as.data.frame(wri)))
st_geometry(AccidentesBiciTotal)<-AccidentesBiciTotal$geometry
rm(SDM,despacio,wri)

#Estandarización de variables de la base de datos de accidentes de bicicletas----
AccidentesBiciTotal<-st_transform(AccidentesBiciTotal,crs=4326)
AccidentesBiciTotal$Accidentes.Localidad<-as.character(AccidentesBiciTotal$Accidentes.Localidad)
AccidentesBiciTotal$Accidentes.Localidad[AccidentesBiciTotal$Accidentes.Localidad=="ANTONIO NARINO"]<-"ANTONIO NARIÑO"
AccidentesBiciTotal$Gravedad<-as.character(AccidentesBiciTotal$Gravedad)
AccidentesBiciTotal$Gravedad[AccidentesBiciTotal$Gravedad=="HERIDO"]<-"Injured"
AccidentesBiciTotal$Gravedad[AccidentesBiciTotal$Gravedad=="ILESO"]<-"Not injured"
AccidentesBiciTotal$Gravedad[AccidentesBiciTotal$Gravedad=="MUERTO"]<-"Dead"
AccidentesBiciTotal$Gravedad[AccidentesBiciTotal$Gravedad=="Herida"]<-"Injured"
AccidentesBiciTotal$Gravedad[AccidentesBiciTotal$Gravedad=="Ilesa"]<-"Not injured"
AccidentesBiciTotal$Gravedad[AccidentesBiciTotal$Gravedad=="Muerta"]<-"Dead"
AccidentesBiciTotal$Gravedad[AccidentesBiciTotal$Gravedad=="Herido Valorado"]<-"Injured\nNo Hospital\nCare"
AccidentesBiciTotal$Gravedad[AccidentesBiciTotal$Gravedad=="Herido Hospitalizado"]<-"Injured\nHospital\nCare"
AccidentesBiciTotal$Gravedad[AccidentesBiciTotal$Gravedad=="HERIDO VALORADO"]<-"Injured\nNo Hospital\nCare"
AccidentesBiciTotal$Gravedad[AccidentesBiciTotal$Gravedad=="HERIDO HOSPITALIZADO"]<-"Injured\nHospital\nCare"
AccidentesBiciTotal$Gravedad[AccidentesBiciTotal$Gravedad=="MUERTA"]<-"Dead"
AccidentesBiciTotal$Gravedad[AccidentesBiciTotal$Gravedad=="VALORADO"]<-"Injured\nNo Hospital\nCare"
AccidentesBiciTotal$Gravedad[AccidentesBiciTotal$Gravedad=="HOSPITALIZADO"]<-"Injured\nHospital\nCare"
AccidentesBiciTotal$Gravedad<-ifelse(!is.na(AccidentesBiciTotal$Fallece),ifelse(AccidentesBiciTotal$Fallece==1,"Dead",AccidentesBiciTotal$Gravedad),AccidentesBiciTotal$Gravedad)
AccidentesBiciTotal$Gravedad<-factor(AccidentesBiciTotal$Gravedad,c("Not injured","Injured","Injured\nNo Hospital\nCare","Injured\nHospital\nCare","Dead"),ordered = TRUE)
AccidentesBiciTotal$Gravedad2<-factor(ifelse(AccidentesBiciTotal$Gravedad=="Dead","Dead","Not Dead"),c("Not Dead","Dead"),ordered = TRUE)
AccidentesBiciTotal$Accidentes.ClaseNombre<-as.character(AccidentesBiciTotal$Accidentes.ClaseNombre)
AccidentesBiciTotal$Accidentes.ClaseNombre[AccidentesBiciTotal$Accidentes.ClaseNombre=="CHOQUE"]<-"Choque"
AccidentesBiciTotal$Accidentes.ClaseNombre[AccidentesBiciTotal$Accidentes.ClaseNombre=="ATROPELLO"]<-"Atropello"
AccidentesBiciTotal$Accidentes.ClaseNombre[AccidentesBiciTotal$Accidentes.ClaseNombre=="VOLCAMIENTO"]<-"Volcamiento"
AccidentesBiciTotal$Accidentes.ClaseNombre[AccidentesBiciTotal$Accidentes.ClaseNombre=="CAIDA DE OCUPANTE"]<-"Caida Ocupante"
AccidentesBiciTotal$Accidentes.ClaseNombre[AccidentesBiciTotal$Accidentes.ClaseNombre=="OTRO"]<-"Otro"
AccidentesBiciTotal$Accidentes.ClaseNombre[AccidentesBiciTotal$Accidentes.ClaseNombre=="AUTOLESION"]<-"Autolesion"
AccidentesBiciTotal$Accidentes.ClaseNombre[AccidentesBiciTotal$Accidentes.ClaseNombre=="Caida de ocupante"]<-"Caida Ocupante"
AccidentesBiciTotal$Edad[AccidentesBiciTotal$Edad=="SIN INFORMACION"]<-NA
AccidentesBiciTotal$Edad<-as.numeric(AccidentesBiciTotal$Edad)
AccidentesBiciTotal$EdadCategorica<-factor(apply(as.data.frame(AccidentesBiciTotal$Edad),MARGIN=1,FUN=encodeEdad2,g=gruposEdad),levels = paste0(gruposEdad$ED_MIN,"-",gruposEdad$ED_MAX))
AccidentesBiciTotal$LLevaCasco[AccidentesBiciTotal$LLevaCasco=="S"]<-"Yes"
AccidentesBiciTotal$LLevaCasco[AccidentesBiciTotal$LLevaCasco=="N"]<-"No"
AccidentesBiciTotal$Accidentes.TipoDiseno[AccidentesBiciTotal$Accidentes.TipoDiseno=="Interseccion"]<-"Intersección"
AccidentesBiciTotal$Accidentes.TipoDiseno[AccidentesBiciTotal$Accidentes.TipoDiseno=="Tramo de Via"]<-"Tramo de Vía"
AccidentesBiciTotal$Accidentes.TipoDiseno[AccidentesBiciTotal$Accidentes.TipoDiseno=="Via peatonal"]<-"Vía peatonal"
AccidentesBiciTotal$Sexo<-as.character(AccidentesBiciTotal$Sexo)
AccidentesBiciTotal$Sexo[AccidentesBiciTotal$Sexo=="FEMENINO"]<-"Female"
AccidentesBiciTotal$Sexo[AccidentesBiciTotal$Sexo=="MASCULINO"]<-"Male"
AccidentesBiciTotal$Sexo[AccidentesBiciTotal$Sexo!="Female" & AccidentesBiciTotal$Sexo!="Male"]<-NA

AccidentesBiciTotal<-st_join(AccidentesBiciTotal,Localidades[,c("LocNombre")],left=TRUE)
AccidentesBiciTotal$Accidentes.Localidad<-AccidentesBiciTotal$LocNombre
AccidentesBiciTotal$LocNombre<-NULL

AccidentesBiciTotal$Accidentes.TipoDiseno2<-
  ifelse(AccidentesBiciTotal$Accidentes.TipoDiseno=="Cicloruta","Bikeway",
  ifelse(AccidentesBiciTotal$Accidentes.TipoDiseno=="Intersección","Intersection",
  ifelse(AccidentesBiciTotal$Accidentes.TipoDiseno=="Tramo de Vía","Segment",
  "Other")))
intersecciones<-st_buffer(intersecciones,dist = (0.0001/11.132)*3.6153225) #El ancho promedio de una via es 7.230645
AccidentesBiciTotal<-st_join(AccidentesBiciTotal,intersecciones[,c("Cluster")],left=TRUE,largest=TRUE)
AccidentesBiciTotal$Accidentes.TipoDiseno2[!is.na(AccidentesBiciTotal$Cluster)]<-"Intersection"

AccidentesBiciTotal$ClaseVehiculo2<-
  ifelse(AccidentesBiciTotal$ClaseVehiculo=="Automóvil"|AccidentesBiciTotal$ClaseVehiculo=="Camioneta"|AccidentesBiciTotal$ClaseVehiculo=="Campero","Car",
  ifelse(AccidentesBiciTotal$ClaseVehiculo=="Bicicleta","Bicycle",
  ifelse(AccidentesBiciTotal$ClaseVehiculo=="Bus","Bus",
  ifelse(AccidentesBiciTotal$ClaseVehiculo=="Bus Articulado","BRT",
  ifelse(AccidentesBiciTotal$ClaseVehiculo=="Camión, Furgón"|AccidentesBiciTotal$ClaseVehiculo=="Tractocamión"|AccidentesBiciTotal$ClaseVehiculo=="Volqueta","Cargo",
  ifelse(AccidentesBiciTotal$ClaseVehiculo=="Motocarro"|AccidentesBiciTotal$ClaseVehiculo=="Motocicleta","Motorcycle",
  "Other"))))))

AccidentesBiciTotal$Clima<-
  ifelse(AccidentesBiciTotal$Accidentes.TipoTiempo=="Normal"|AccidentesBiciTotal$Accidentes.TipoTiempo=="Normal/Normal","Normal",
  ifelse(AccidentesBiciTotal$Accidentes.TipoTiempo=="Lluvia","Rain",
  ifelse(AccidentesBiciTotal$Accidentes.TipoTiempo=="Niebla","Fog",
  ifelse(AccidentesBiciTotal$Accidentes.TipoTiempo=="Viento","Wind",
  "Other"))))

AccidentesBiciTotal$ModeloVehiculo<-as.numeric(AccidentesBiciTotal$ModeloVehiculo)
AccidentesBiciTotal$AntiguedadBici<-pmax(0,as.numeric(getYear(AccidentesBiciTotal$Accidentes.Fecha))-AccidentesBiciTotal$ModeloVehiculo)

AccidentesBiciTotal$PortaLicencia<-
  ifelse(AccidentesBiciTotal$PortaLicencia=="N","No",
  ifelse(AccidentesBiciTotal$PortaLicencia=="S","Yes",
  NA))

#Identificación y eliminación de accidentes de bicicletas duplicados----
AccidentesBiciTotal$KEY<-paste(AccidentesBiciTotal$Accidentes.HoraOcurrencia,"-",AccidentesBiciTotal$Accidentes.Direccion)
AccidentesBiciTotal$ID<-paste(AccidentesBiciTotal$Accidentes.HoraOcurrencia,"-",AccidentesBiciTotal$Accidentes.Direccion,"-",AccidentesBiciTotal$Edad,"-",AccidentesBiciTotal$Sexo)
AccidentesBiciTotal$Missing<-rowSums(is.na(AccidentesBiciTotal))
AccidentesBiciTotal<-AccidentesBiciTotal[order(AccidentesBiciTotal$ID,AccidentesBiciTotal$Missing,decreasing = FALSE),]
AccidentesBiciTotal<-AccidentesBiciTotal[!duplicated(AccidentesBiciTotal$ID),]
AccidentesBiciTotal$ID<-NULL

#Integración de variables y nombres  para las bases de datos de accidentes totales----
#Nombres AccidentesTotales
AccidentesTotales$Condicion<-NA
AccidentesTotales$Muerte_POS<-NA
AccidentesTotales$Fecha_POS<-NA
AccidentesTotales$Fallece<-NA
AccidentesTotales$Source<-"Despacio"

#Nombres wriTotales
wriTotales$Accidentes.Fecha<-as.POSIXct(as.character(wriTotales$FECHA_OCUR), format="%Y-%m-%d")
wriTotales$Accidentes.HoraOcurrencia<-as.POSIXct(paste(wriTotales$FECHA_OCUR,wriTotales$HORA_OCURR))
wriTotales$HORA_OCURR<-NULL
wriTotales$FECHA_OCUR<-NULL
wriTotales$OBJECTID_1<-NULL
wriTotales$OBJECTID<-NULL
wriTotales$FORMULARIO<-NULL
wriTotales$ANO_OCURRE<-NULL
wriTotales$MES_OCURRE<-NULL
wriTotales$DIA_OCURRE<-NULL
wriTotales$AREAS_TRAN<-NULL
wriTotales$CUADRANTE_<-NULL
wriTotales$FECHA_HORA<-NULL
names(wriTotales)[grep("CODIGO_ACC",names(wriTotales))]<-c("Accidente")
names(wriTotales)[grep("DIRECCION",names(wriTotales))]<-c("Accidentes.Direccion")
names(wriTotales)[grep("CLASE",names(wriTotales))]<-c("Accidentes.ClaseNombre")
names(wriTotales)[grep("LOCALIDAD",names(wriTotales))]<-c("Accidentes.Localidad")
names(wriTotales)[grep("CODIGO_VEH",names(wriTotales))]<-c("Vehiculo")
names(wriTotales)[grep("CONDICION",names(wriTotales))]<-c("Condicion")
names(wriTotales)[grep("HOSP_VALOR",names(wriTotales))]<-c("Gravedad")
names(wriTotales)[grep("GENERO",names(wriTotales))]<-c("Sexo")
names(wriTotales)[grep("EDAD",names(wriTotales))]<-c("Edad")
names(wriTotales)[grep("MUERTE_POS",names(wriTotales))]<-c("Muerte_POS")
names(wriTotales)[grep("FECHA_POS",names(wriTotales))]<-c("Fecha_POS")
names(wriTotales)[grep("f",names(wriTotales))]<-c("Fallece")
wriTotales$Source<-"WRI"
wriTotales$Accidentes.CardinalVia1<-NA
wriTotales$Accidentes.CardinalVia2<-NA
wriTotales$Accidentes.CausaIDescripcion<-NA
wriTotales$Accidentes.CausaIIDescripcion<-NA
wriTotales$Accidentes.ChoqueNombre<-NA
wriTotales$Accidentes.CorredorVial<-NA
wriTotales$Accidentes.GravedadNombre<-NA
wriTotales$Accidentes.Latitud<-NA
wriTotales$Accidentes.LetraVia1<-NA
wriTotales$Accidentes.LetraVia2<-NA
wriTotales$Accidentes.Longitud<-NA
wriTotales$Accidentes.NumeroVia1<-NA
wriTotales$Accidentes.NumeroVia2<-NA
wriTotales$Accidentes.ObjetoFijoCodigo<-NA
wriTotales$Accidentes.ObjetoFijoNombre<-NA
wriTotales$Accidentes.TipoDiseno<-NA
wriTotales$Accidentes.TipoTiempo<-NA
wriTotales$Accidentes.TipoVia1<-NA
wriTotales$Accidentes.TipoVia2<-NA
wriTotales$Accidentes.TotalHeridos<-NA
wriTotales$Accidentes.TotalMuertos<-NA
wriTotales$CantidadPasajeros<-NA
wriTotales$CapacidadCarga<-NA
wriTotales$ClaseVehiculo<-NA
wriTotales$Clinica<-NA
wriTotales$CodigoMarcaVehiculo<-NA
wriTotales$EsPropietarioVehiculo<-NA
wriTotales$Fecha<-NA
wriTotales$LLevaCasco<-NA
wriTotales$LLevaCinturon<-NA
wriTotales$ModalidadVehiculo<-NA
wriTotales$ModeloVehiculo<-NA
wriTotales$NacionalidadVehiculo<-NA
wriTotales$PortaLicencia<-NA
wriTotales$ServicioVehiculo<-NA
wriTotales$VehiculoEnFuga<-NA
wriTotales$Victimas.Clinica<-NA
wriTotales$Victimas.CodigoVehiculo<-NA
wriTotales$Victimas.Edad<-NA
wriTotales$Victimas.Fecha<-NA
wriTotales$Victimas.Gravedad<-NA
wriTotales$`Victimas.LLevaCasco?`<-NA
wriTotales$`Victimas.LLevaCinturon?`<-NA
wriTotales$`Victimas.Peaton-Pasajero?`<-NA
wriTotales$Victimas.Sexo<-NA
wriTotales$Adress<-NULL

#Integración de bases de datos de accidentes totales----
AccidentesTotal<-as.data.frame(rbind(as.data.frame(AccidentesTotales),as.data.frame(wriTotales)))
rm(AccidentesTotales,wriTotales)

#Estandarización de variables de la base de datos de accidentes totales----
AccidentesTotal$Accidentes.Localidad[AccidentesTotal$Accidentes.Localidad=="ANTONIO NARINO"]<-"ANTONIO NARIÑO"
AccidentesTotal$Gravedad<-as.character(AccidentesTotal$Gravedad)
AccidentesTotal$Gravedad[AccidentesTotal$Gravedad=="HERIDO"]<-"Injured"
AccidentesTotal$Gravedad[AccidentesTotal$Gravedad=="ILESO"]<-"Not injured"
AccidentesTotal$Gravedad[AccidentesTotal$Gravedad=="MUERTO"]<-"Dead"
AccidentesTotal$Gravedad[AccidentesTotal$Gravedad=="Herida"]<-"Injured"
AccidentesTotal$Gravedad[AccidentesTotal$Gravedad=="Ilesa"]<-"Not injured"
AccidentesTotal$Gravedad[AccidentesTotal$Gravedad=="Muerta"]<-"Dead"
AccidentesTotal$Gravedad[AccidentesTotal$Gravedad=="Herido Valorado"]<-"Injured\nNo Hospital\nCare"
AccidentesTotal$Gravedad[AccidentesTotal$Gravedad=="Herido Hospitalizado"]<-"Injured\nHospital\nCare"
AccidentesTotal$Gravedad[AccidentesTotal$Gravedad=="HERIDO VALORADO"]<-"Injured\nNo Hospital\nCare"
AccidentesTotal$Gravedad[AccidentesTotal$Gravedad=="HERIDO HOSPITALIZADO"]<-"Injured\nHospital\nCare"
AccidentesTotal$Gravedad[AccidentesTotal$Gravedad=="MUERTA"]<-"Dead"
AccidentesTotal$Gravedad[AccidentesTotal$Gravedad=="VALORADO"]<-"Injured\nNo Hospital\nCare"
AccidentesTotal$Gravedad[AccidentesTotal$Gravedad=="HOSPITALIZADO"]<-"Injured\nHospital\nCare"
AccidentesTotal$Gravedad<-ifelse(!is.na(AccidentesTotal$Fallece),ifelse(AccidentesTotal$Fallece==1,"Dead",AccidentesTotal$Gravedad),AccidentesTotal$Gravedad)
AccidentesTotal$Gravedad<-factor(AccidentesTotal$Gravedad,c("Not injured","Injured","Injured\nNo Hospital\nCare","Injured\nHospital\nCare","Dead"),ordered = TRUE)
AccidentesTotal$Gravedad2<-factor(ifelse(AccidentesTotal$Gravedad=="Dead","Dead","Not Dead"),c("Not Dead","Dead"),ordered = TRUE)
AccidentesTotal$Accidentes.ClaseNombre<-as.character(AccidentesTotal$Accidentes.ClaseNombre)
AccidentesTotal$Accidentes.ClaseNombre[AccidentesTotal$Accidentes.ClaseNombre=="CHOQUE"]<-"Choque"
AccidentesTotal$Accidentes.ClaseNombre[AccidentesTotal$Accidentes.ClaseNombre=="ATROPELLO"]<-"Atropello"
AccidentesTotal$Accidentes.ClaseNombre[AccidentesTotal$Accidentes.ClaseNombre=="VOLCAMIENTO"]<-"Volcamiento"
AccidentesTotal$Accidentes.ClaseNombre[AccidentesTotal$Accidentes.ClaseNombre=="CAIDA DE OCUPANTE"]<-"Caida Ocupante"
AccidentesTotal$Accidentes.ClaseNombre[AccidentesTotal$Accidentes.ClaseNombre=="OTRO"]<-"Otro"
AccidentesTotal$Accidentes.ClaseNombre[AccidentesTotal$Accidentes.ClaseNombre=="AUTOLESION"]<-"Autolesion"
AccidentesTotal$Accidentes.ClaseNombre[AccidentesTotal$Accidentes.ClaseNombre=="Caida de ocupante"]<-"Caida Ocupante"
AccidentesTotal$Edad[AccidentesTotal$Edad=="SIN INFORMACION"]<-NA
AccidentesTotal$Edad<-as.numeric(AccidentesTotal$Edad)
AccidentesTotal$EdadCategorica<-factor(apply(as.data.frame(AccidentesTotal$Edad),MARGIN=1,FUN=encodeEdad2,g=gruposEdad),levels = paste0(gruposEdad$ED_MIN,"-",gruposEdad$ED_MAX))
AccidentesTotal$LLevaCasco[AccidentesTotal$LLevaCasco=="S"]<-"Yes"
AccidentesTotal$LLevaCasco[AccidentesTotal$LLevaCasco=="N"]<-"No"
AccidentesTotal$Accidentes.TipoDiseno[AccidentesTotal$Accidentes.TipoDiseno=="Interseccion"]<-"Intersección"
AccidentesTotal$Accidentes.TipoDiseno[AccidentesTotal$Accidentes.TipoDiseno=="Tramo de Via"]<-"Tramo de Vía"
AccidentesTotal$Accidentes.TipoDiseno[AccidentesTotal$Accidentes.TipoDiseno=="Via peatonal"]<-"Vía peatonal"
AccidentesTotal$ClaseVehiculo[AccidentesTotal$ClaseVehiculo=="Automovil"]<-"Automóvil"
AccidentesTotal$ClaseVehiculo[AccidentesTotal$ClaseVehiculo=="Bus Alimentador"]<-"Bus"
AccidentesTotal$ClaseVehiculo[AccidentesTotal$ClaseVehiculo=="Buseta"]<-"Bus"
AccidentesTotal$ClaseVehiculo[AccidentesTotal$ClaseVehiculo=="Camion, Furgon"]<-"Camion, Furgón"
AccidentesTotal$ClaseVehiculo[AccidentesTotal$ClaseVehiculo=="Camion, Furgón"]<-"Camión, Furgón"
AccidentesTotal$ClaseVehiculo[AccidentesTotal$ClaseVehiculo=="Microbus"]<-"Bus"
AccidentesTotal$ClaseVehiculo[AccidentesTotal$ClaseVehiculo=="Motociclo"]<-"Motocicleta"
AccidentesTotal$ClaseVehiculo[AccidentesTotal$ClaseVehiculo=="No identificado"]<-"Otro"
AccidentesTotal$ClaseVehiculo[AccidentesTotal$ClaseVehiculo=="null"]<-"Otro"
AccidentesTotal$ClaseVehiculo[AccidentesTotal$ClaseVehiculo=="Tractocamion"]<-"Tractocamión"
AccidentesTotal$ClaseVehiculo[AccidentesTotal$ClaseVehiculo=="Traccion animal"]<-"Tracción animal"
AccidentesTotal$Sexo<-as.character(AccidentesTotal$Sexo)
AccidentesTotal$Sexo[AccidentesTotal$Sexo=="FEMENINO"]<-"Female"
AccidentesTotal$Sexo[AccidentesTotal$Sexo=="MASCULINO"]<-"Male"
AccidentesTotal$Sexo[AccidentesTotal$Sexo!="Female" & AccidentesTotal$Sexo!="Male"]<-NA

AccidentesTotal$ClaseVehiculo2<-
  ifelse(AccidentesTotal$ClaseVehiculo=="Automóvil"|AccidentesTotal$ClaseVehiculo=="Camioneta"|AccidentesTotal$ClaseVehiculo=="Campero","Car",
  ifelse(AccidentesTotal$ClaseVehiculo=="Bicicleta","Bicycle",
  ifelse(AccidentesTotal$ClaseVehiculo=="Bus","Bus",
  ifelse(AccidentesTotal$ClaseVehiculo=="Bus Articulado","BRT",
  ifelse(AccidentesTotal$ClaseVehiculo=="Camión, Furgón"|AccidentesTotal$ClaseVehiculo=="Tractocamión"|AccidentesTotal$ClaseVehiculo=="Volqueta","Cargo",
  ifelse(AccidentesTotal$ClaseVehiculo=="Motocarro"|AccidentesTotal$ClaseVehiculo=="Motocicleta","Motorcycle",
  "Other"))))))

#Identificación y eliminación de accidentes totales duplicados----
AccidentesTotal$KEY<-paste(AccidentesTotal$Accidentes.HoraOcurrencia,"-",AccidentesTotal$Accidentes.Direccion)
AccidentesTotal$ID<-paste(AccidentesTotal$Accidentes.HoraOcurrencia,"-",AccidentesTotal$Accidentes.Direccion,"-",AccidentesTotal$Edad,"-",AccidentesTotal$Sexo)
AccidentesTotal$Missing<-rowSums(is.na(AccidentesTotal))
AccidentesTotal<-AccidentesTotal[order(AccidentesTotal$ID,AccidentesTotal$Missing,decreasing = FALSE),]
AccidentesTotal<-AccidentesTotal[!duplicated(AccidentesTotal$ID),]
AccidentesTotal$ID<-NULL

#Datos faltantes en cada base de datos de accidentes de bicicletas----
DataInformation<-as.data.frame(matrix(NA,length(unique(getYear(AccidentesBiciTotal$Accidentes.Fecha)))+1,8))
colnames(DataInformation)<-c("Año","Records","Percent Missing","SDM","WRI","Despacio","Min Missing","Max Missing")
DataInformation$Año[1:length(unique(getYear(AccidentesBiciTotal$Accidentes.Fecha)))]<-sort(as.numeric(unique(getYear(AccidentesBiciTotal$Accidentes.Fecha))))
DataInformation$Año[length(unique(getYear(AccidentesBiciTotal$Accidentes.Fecha)))+1]<-"Total"
for(i in sort(as.numeric(unique(getYear(AccidentesBiciTotal$Accidentes.Fecha))))){
  DataInformation$Records[grep(i,DataInformation$Año)]<-dim(AccidentesBiciTotal[getYear(AccidentesBiciTotal$Accidentes.Fecha)==i,])[1]
  DataInformation$`Percent Missing`[grep(i,DataInformation$Año)]<-sum(is.na(AccidentesBiciTotal[getYear(AccidentesBiciTotal$Accidentes.Fecha)==i,]))/(dim(AccidentesBiciTotal[getYear(AccidentesBiciTotal$Accidentes.Fecha)==i,])[1]*dim(AccidentesBiciTotal[getYear(AccidentesBiciTotal$Accidentes.Fecha)==i,])[2])
  DataInformation$SDM[grep(i,DataInformation$Año)]<-dim(AccidentesBiciTotal[getYear(AccidentesBiciTotal$Accidentes.Fecha)==i & AccidentesBiciTotal$Source=="SDM",])[1]
  DataInformation$WRI[grep(i,DataInformation$Año)]<-dim(AccidentesBiciTotal[getYear(AccidentesBiciTotal$Accidentes.Fecha)==i & AccidentesBiciTotal$Source=="WRI",])[1]
  DataInformation$Despacio[grep(i,DataInformation$Año)]<-dim(AccidentesBiciTotal[getYear(AccidentesBiciTotal$Accidentes.Fecha)==i & AccidentesBiciTotal$Source=="Despacio",])[1]
  DataInformation$`Min Missing`[grep(i,DataInformation$Año)]<-min(AccidentesBiciTotal$Missing[getYear(AccidentesBiciTotal$Accidentes.Fecha)==i])
  DataInformation$`Max Missing`[grep(i,DataInformation$Año)]<-max(AccidentesBiciTotal$Missing[getYear(AccidentesBiciTotal$Accidentes.Fecha)==i])
}
rm(i)
DataInformation$Records[length(unique(getYear(AccidentesBiciTotal$Accidentes.Fecha)))+1]<-dim(AccidentesBiciTotal)[1]
DataInformation$`Percent Missing`[length(unique(getYear(AccidentesBiciTotal$Accidentes.Fecha)))+1]<-sum(is.na(AccidentesBiciTotal))/(dim(AccidentesBiciTotal)[1]*dim(AccidentesBiciTotal)[2])
DataInformation$SDM[length(unique(getYear(AccidentesBiciTotal$Accidentes.Fecha)))+1]<-dim(AccidentesBiciTotal[AccidentesBiciTotal$Source=="SDM",])[1]
DataInformation$WRI[length(unique(getYear(AccidentesBiciTotal$Accidentes.Fecha)))+1]<-dim(AccidentesBiciTotal[AccidentesBiciTotal$Source=="WRI",])[1]
DataInformation$Despacio[length(unique(getYear(AccidentesBiciTotal$Accidentes.Fecha)))+1]<-dim(AccidentesBiciTotal[AccidentesBiciTotal$Source=="Despacio",])[1]
DataInformation$`Min Missing`[length(unique(getYear(AccidentesBiciTotal$Accidentes.Fecha)))+1]<-min(AccidentesBiciTotal$Missing)
DataInformation$`Max Missing`[length(unique(getYear(AccidentesBiciTotal$Accidentes.Fecha)))+1]<-max(AccidentesBiciTotal$Missing)
write.csv(DataInformation,paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/TABLAS/3. PercentMissingConsolidadoBicicletas.csv"))
rm(DataInformation)

#Datos faltantes en cada base de datos de accidentes totales----
DataInformationT<-as.data.frame(matrix(NA,length(unique(getYear(AccidentesTotal$Accidentes.Fecha)))+1,8))
colnames(DataInformationT)<-c("Año","Records","Percent Missing","SDM","WRI","Despacio","Min Missing","Max Missing")
DataInformationT$Año[1:length(unique(getYear(AccidentesTotal$Accidentes.Fecha)))]<-sort(as.numeric(unique(getYear(AccidentesTotal$Accidentes.Fecha))))
DataInformationT$Año[length(unique(getYear(AccidentesTotal$Accidentes.Fecha)))+1]<-"Total"
for(i in sort(as.numeric(unique(getYear(AccidentesTotal$Accidentes.Fecha))))){
  DataInformationT$Records[grep(i,DataInformationT$Año)]<-dim(AccidentesTotal[getYear(AccidentesTotal$Accidentes.Fecha)==i,])[1]
  DataInformationT$`Percent Missing`[grep(i,DataInformationT$Año)]<-sum(is.na(AccidentesTotal[getYear(AccidentesTotal$Accidentes.Fecha)==i,]))/(dim(AccidentesTotal[getYear(AccidentesTotal$Accidentes.Fecha)==i,])[1]*dim(AccidentesTotal[getYear(AccidentesTotal$Accidentes.Fecha)==i,])[2])
  DataInformationT$SDM[grep(i,DataInformationT$Año)]<-dim(AccidentesTotal[getYear(AccidentesTotal$Accidentes.Fecha)==i & AccidentesTotal$Source=="SDM",])[1]
  DataInformationT$WRI[grep(i,DataInformationT$Año)]<-dim(AccidentesTotal[getYear(AccidentesTotal$Accidentes.Fecha)==i & AccidentesTotal$Source=="WRI",])[1]
  DataInformationT$Despacio[grep(i,DataInformationT$Año)]<-dim(AccidentesTotal[getYear(AccidentesTotal$Accidentes.Fecha)==i & AccidentesTotal$Source=="Despacio",])[1]
  DataInformationT$`Min Missing`[grep(i,DataInformationT$Año)]<-min(AccidentesTotal$Missing[getYear(AccidentesTotal$Accidentes.Fecha)==i])
  DataInformationT$`Max Missing`[grep(i,DataInformationT$Año)]<-max(AccidentesTotal$Missing[getYear(AccidentesTotal$Accidentes.Fecha)==i])
}
rm(i)
DataInformationT$Records[length(unique(getYear(AccidentesTotal$Accidentes.Fecha)))+1]<-dim(AccidentesTotal)[1]
DataInformationT$`Percent Missing`[length(unique(getYear(AccidentesTotal$Accidentes.Fecha)))+1]<-sum(is.na(AccidentesTotal))/(dim(AccidentesTotal)[1]*dim(AccidentesTotal)[2])
DataInformationT$SDM[length(unique(getYear(AccidentesTotal$Accidentes.Fecha)))+1]<-dim(AccidentesTotal[AccidentesTotal$Source=="SDM",])[1]
DataInformationT$WRI[length(unique(getYear(AccidentesTotal$Accidentes.Fecha)))+1]<-dim(AccidentesTotal[AccidentesTotal$Source=="WRI",])[1]
DataInformationT$Despacio[length(unique(getYear(AccidentesTotal$Accidentes.Fecha)))+1]<-dim(AccidentesTotal[AccidentesTotal$Source=="Despacio",])[1]
DataInformationT$`Min Missing`[length(unique(getYear(AccidentesTotal$Accidentes.Fecha)))+1]<-min(AccidentesTotal$Missing)
DataInformationT$`Max Missing`[length(unique(getYear(AccidentesTotal$Accidentes.Fecha)))+1]<-max(AccidentesTotal$Missing)
write.csv(DataInformationT,paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/TABLAS/3. PercentMissingConsolidadoTotal.csv"))
rm(DataInformationT)

#Filtrado de la base para remover los años previos al 2011 por cambio de orden de magnitud (x5)----
AccidentesBiciTotal<-AccidentesBiciTotal[getYear(AccidentesBiciTotal$Accidentes.Fecha)>=2011,]
AccidentesTotal<-AccidentesTotal[getYear(AccidentesTotal$Accidentes.Fecha)>=2011,]

# Guardado de la base de datos definitiva----
save(AccidentesBiciTotal,file=paste(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/3. AccidentesBiciTotalGeoData.Rdata",sep=""))
save(AccidentesTotal,file=paste(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/3. AccidentesTotal2011_2017.Rdata",sep=""))
