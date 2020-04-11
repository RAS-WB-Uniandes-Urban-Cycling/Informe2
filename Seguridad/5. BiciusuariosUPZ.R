#Preparacion del entorno de trabajo----
rm(list=ls())
carpetaRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/UNIANDES - RAS - SDM"
gitRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/proBikePolicies"
require(pacman)
pacman::p_load(gdata)
pacman::p_load(padr)
pacman::p_load(dplyr)
pacman::p_load(readxl)
pacman::p_load(zoo)
pacman::p_load(readr)
pacman::p_load(sf)

#Lectura de función de edad----
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/4. GruposEdadCodificacion.Rdata"))

#Lectura de UPZ----
UPZ<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR1217.gdb"),layer = "UPla") %>% st_transform(crs=4326) %>% filter(UPlTipo==1)
UPZ$Codigo<-substr(UPZ$UPlCodigo,start = 4,stop = 10)
UPZ_list<-as.numeric(unique(substr(UPZ$UPlCodigo,start = 4,stop = 10)))

#Lectura de manzanas censales----
Manzana<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Marco Geoestadistico Nacional - DANE/2012/11_BOGOTA/MGN/MGN_Manzana.shp")) %>% st_transform(crs=4326)
Manzana<-Manzana %>% 
  transmute(ID_MANZANA=as.numeric(paste0(SETR_CLSE_,SECR_SETR_,CPOB_SECR_,SECU_SET_1,SECU_SECU_,MANZ_CCDGO)))
Manzana<-st_join(st_centroid(Manzana), UPZ[,c("Codigo")])

#Tabla de combinaciones posibles de género y grupo de edad----
GENDER_AGE<-expand.grid(c("Male","Female"),paste0(gruposEdad$ED_MIN,"-",gruposEdad$ED_MAX),UPZ_list)
names(GENDER_AGE)<-c("SEXO","GR_EDAD","UPZ")

#Estimación del número de biciusuarios 2017----
encuesta2017<-read.csv(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta multiproposito/2017/Identificacion ( Capitulo A)/Identificacion ( Capitulo A).csv"),sep=";",encoding = "Latin1",stringsAsFactors = FALSE)
encuesta2017<-encuesta2017[encuesta2017$DPTOMPIO=="11001",c("DIRECTORIO","COD_UPZ")] 

viajesEstu<-read.csv(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta multiproposito/2017/Educacion (capitulo H)/Educacion (capitulo H).csv"),sep=";",encoding = "Latin1")
viajesEstu<-viajesEstu[viajesEstu$DIRECTORIO%in%encuesta2017$DIRECTORIO,c("DIRECTORIO","DIRECTORIO_PER","DIRECTORIO_HOG","NPCHP18G","FEX_C")]
viajesEstu$NPCHP18G<-na.fill(viajesEstu$NPCHP18G,0)
viajesEstu<-viajesEstu[viajesEstu$NPCHP18G==1,-4]
viajesEstu$FEX_C<-as.numeric(gsub(",",".",viajesEstu$FEX_C))
viajesEstu<-merge(viajesEstu,encuesta2017,by=c("DIRECTORIO"),all.x=TRUE)

viajesTr<-read.csv(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta multiproposito/2017/Fuerza de trabajo (capitulo K)/Fuerza de trabajo  (capitulo K).csv"),sep=";",encoding = "Latin1")
viajesTr<-viajesTr[viajesTr$DIRECTORIO%in%encuesta2017$DIRECTORIO,c("DIRECTORIO","DIRECTORIO_PER","DIRECTORIO_HOG","NPCKP45G","FEX_C")]
viajesTr$NPCKP45G<-na.fill(viajesTr$NPCKP45G,0)
viajesTr<-viajesTr[viajesTr$NPCKP45G==1,-4]
viajesTr$FEX_C<-as.numeric(gsub(",",".",viajesTr$FEX_C))
viajesTr<-merge(viajesTr,encuesta2017,by=c("DIRECTORIO"),all.x=TRUE)

viajes2017<-rbind(viajesEstu,viajesTr)

viajesPr<-read.csv(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta multiproposito/2017/Composicion del hogar y demografia ( Capitulo E)/Composicion del hogar y demografia ( Capitulo E).csv"),sep=";",encoding = "Latin1")
viajesPr<-viajesPr[viajesPr$DIRECTORIO%in%encuesta2017$DIRECTORIO,c("DIRECTORIO","DIRECTORIO_PER","DIRECTORIO_HOG","NPCEP4","NPCEP5")]
rm(viajesEstu,viajesTr,encuesta2017)
viajesPr<-merge(viajesPr,viajes2017,by = c("DIRECTORIO","DIRECTORIO_HOG","DIRECTORIO_PER"),all.x=TRUE)
rm(viajes2017)
viajesPr$NPCEP5<-as.factor(ifelse(viajesPr$NPCEP5==1,"Male",ifelse(viajesPr$NPCEP5==2,"Female",NA)))
viajesPr$GR_EDAD<-factor(apply(as.data.frame(viajesPr$NPCEP4),MARGIN=1,FUN=encodeEdad2,g=gruposEdad),levels = paste0(gruposEdad$ED_MIN,"-",gruposEdad$ED_MAX))
viajesPr$FECHA<-as.POSIXct("2017-01-01")
viajesPr<-na.omit(viajesPr)
biciusuarios2017<-aggregate(viajesPr$FEX_C,by=list(viajesPr$GR_EDAD,viajesPr$NPCEP5,viajesPr$FECHA,viajesPr$COD_UPZ),FUN=sum)
rm(viajesPr)
names(biciusuarios2017)<-c("GR_EDAD","SEXO","FECHA","UPZ","BICIUSRS")

biciusuarios2017<-merge(GENDER_AGE,biciusuarios2017,by=c("GR_EDAD","SEXO","UPZ"),all.x=TRUE) %>% mutate(FECHA = replace(FECHA, is.na(FECHA),"2017-01-01"),BICIUSRS = replace(BICIUSRS, is.na(BICIUSRS),0))

#Estimación del número de biciusuarios 2015----
biciusuarios2015<-read.csv(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta de Movilidad/2015/Encuesta/encuesta 2015 - personas.csv"),sep=";",fileEncoding = "Latin1",encoding = "Latin1")

etapas2015<-read.csv(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta de Movilidad/2015/Encuesta/encuesta 2015 - etapas.csv"),sep=";",fileEncoding = "Latin1",encoding = "Latin1")
etapas2015$BICI<-ifelse(etapas2015$MEDIOTRASPORTE=="Bicicleta"|etapas2015$MEDIOTRASPORTE=="Bicicleta con motor"|etapas2015$MEDIOTRASPORTE=="Bicicletas publicas",TRUE,FALSE)
etapas2015<-etapas2015[,c("ID_ENCUESTA","NUMERO_PERSONA","NUMERO_VIAJE","BICI")]

viajes2015<-read.csv(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta de Movilidad/2015/Encuesta/encuesta 2015 - viajes.csv"),sep=";",fileEncoding = "Latin1",encoding = "Latin1",stringsAsFactors = FALSE)
viajes2015<-viajes2015[,c("ID_ENCUESTA","NUMERO_PERSONA","NUMERO_VIAJE","MOTIVOVIAJE","DIA_HABIL")]
viajes2015<-left_join(viajes2015,etapas2015,by=c("ID_ENCUESTA","NUMERO_PERSONA","NUMERO_VIAJE"))
viajes2015<-viajes2015[viajes2015$BICI,]
viajes2015<-viajes2015[viajes2015$MOTIVOVIAJE%in%c("Estudiar","Trabajar","Asuntos de Trabajo","Buscar trabajo") & viajes2015$DIA_HABIL=="S",]
viajes2015<-paste0(viajes2015$ID_ENCUESTA,"-",viajes2015$NUMERO_PERSONA)
rm(etapas2015)

encuesta2015<-read.csv(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta de Movilidad/2015/Encuesta/encuesta 2015 - encuestas.csv"),sep=";",fileEncoding = "Latin1",encoding = "Latin1",stringsAsFactors = FALSE)
encuesta2015<-encuesta2015[encuesta2015$DEPARTAMENTO=="Bogota D.C.",c("ID_ENCUESTA","ID_MANZANA")]
encuesta2015<-left_join(encuesta2015,Manzana)

biciusuarios2015$Key<-paste0(biciusuarios2015$ID_ENCUESTA,"-",biciusuarios2015$NUMERO_PERSONA)
biciusuarios2015<-biciusuarios2015[biciusuarios2015$Key%in%viajes2015,]
biciusuarios2015<-biciusuarios2015[biciusuarios2015$ID_ENCUESTA%in%encuesta2015$ID_ENCUESTA, c("ID_ENCUESTA","SEXO","EDAD","PONDERADOR_CALIBRADO")]
biciusuarios2015<-merge(biciusuarios2015,encuesta2015,by.x = "ID_ENCUESTA",by.y="ID_ENCUESTA",all.x=TRUE)
rm(encuesta2015,viajes2015)

names(biciusuarios2015)<-toupper(names(biciusuarios2015))

biciusuarios2015$PONDERADOR_CALIBRADO<-as.numeric(gsub(",",".",biciusuarios2015$PONDERADOR_CALIBRADO))
biciusuarios2015$GR_EDAD<-factor(apply(as.data.frame(biciusuarios2015$EDAD),MARGIN=1,FUN=encodeEdad2,g=gruposEdad),levels = paste0(gruposEdad$ED_MIN,"-",gruposEdad$ED_MAX))
biciusuarios2015$FECHA<-as.POSIXct("2015-01-01")
biciusuarios2015<-aggregate(biciusuarios2015$PONDERADOR_CALIBRADO,by=list(biciusuarios2015$GR_EDAD,biciusuarios2015$SEXO,biciusuarios2015$FECHA,biciusuarios2015$CODIGO),FUN=sum)
names(biciusuarios2015)<-c("GR_EDAD","SEXO","FECHA","UPZ","BICIUSRS")
biciusuarios2015$SEXO<-as.factor(ifelse(biciusuarios2015$SEXO=="Hombre","Male","Female"))

biciusuarios2015<-merge(GENDER_AGE,biciusuarios2015,by=c("GR_EDAD","SEXO","UPZ"),all.x=TRUE) %>% mutate(FECHA = replace(FECHA, is.na(FECHA),"2015-01-01"),BICIUSRS = replace(BICIUSRS, is.na(BICIUSRS),0))

#Estimación del número de biciusuarios 2011----
biciusuarios2011<-read_excel(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta de Movilidad/2011/120927_Base de Datos EODH 2011/MOD_B_PERSONAS_Tipico.xlsx"),na="N/A")
biciusuarios2011<-biciusuarios2011[biciusuarios2011$MUN_D=="11001",c("ORDEN","ID_PERSO","P4_B","P5_B","F_EXP")]

encuesta2011<-read_excel(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta de Movilidad/2011/120927_Base de Datos EODH 2011/MOD_A_ID_HOGAR_Tipico.xlsx"),na="N/A")
encuesta2011<-encuesta2011[,c("ORDEN","UPZ")]

biciusuarios2011<-left_join(biciusuarios2011,encuesta2011)
rm(encuesta2011)

viajes2011<-read_excel(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta de Movilidad/2011/120927_Base de Datos EODH 2011/Mod_D_VIAJES2_BaseImputacion_Definitiva.xlsx"),na="N/A")
viajes2011<-viajes2011[,c("ORDEN","ID_PERSO","P13_D","E1_P1_D","E2_P1_D","E3_P1_D","E4_P1_D","E5_P1_D")]
viajes2011<-viajes2011[!is.na(viajes2011$E1_P1_D),]
viajes2011$Bici<-ifelse(viajes2011$E1_P1_D==18|viajes2011$E1_P1_D==19|viajes2011$E2_P1_D==18|viajes2011$E2_P1_D==19|viajes2011$E3_P1_D==18|viajes2011$E3_P1_D==19|viajes2011$E4_P1_D==18|viajes2011$E4_P1_D==19|viajes2011$E5_P1_D==18|viajes2011$E5_P1_D==19,TRUE,FALSE)
viajes2011<-viajes2011[viajes2011$P13_D%in%c("01","02","03","13"),]
viajes2011<-unique(na.omit(viajes2011[viajes2011$Bici==TRUE,c("ORDEN","ID_PERSO")]))

biciusuarios2011$Key<-paste(biciusuarios2011$ORDEN,biciusuarios2011$ID_PERSO,sep="-")
viajes2011$Key<-paste(viajes2011$ORDEN,viajes2011$ID_PERSO,sep="-")
biciusuarios2011<-biciusuarios2011[biciusuarios2011$Key %in% viajes2011$Key,]
rm(viajes2011)
biciusuarios2011$Key<-NULL
biciusuarios2011$P4_B<-as.factor(ifelse(biciusuarios2011$P4_B=="1","Male","Female"))
biciusuarios2011$P5_B<-as.numeric(biciusuarios2011$P5_B)
biciusuarios2011$GR_EDAD<-factor(apply(as.data.frame(biciusuarios2011$P5_B),MARGIN=1,FUN=encodeEdad2,g=gruposEdad),levels = paste0(gruposEdad$ED_MIN,"-",gruposEdad$ED_MAX))
biciusuarios2011$FECHA<-as.POSIXct("2011-01-01")
biciusuarios2011<-aggregate(biciusuarios2011$F_EXP,by=list(biciusuarios2011$GR_EDAD,biciusuarios2011$P4_B,biciusuarios2011$FECHA,biciusuarios2011$UPZ),FUN=sum)
names(biciusuarios2011)<-c("GR_EDAD","SEXO","FECHA","UPZ","BICIUSRS")

biciusuarios2011<-merge(GENDER_AGE,biciusuarios2011,by=c("GR_EDAD","SEXO","UPZ"),all.x=TRUE) %>% mutate(FECHA = replace(FECHA, is.na(FECHA),"2011-01-01"),BICIUSRS = replace(BICIUSRS, is.na(BICIUSRS),0))

#Estimacion del número de biciusuarios 2005----
viajes2005<-read_excel(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta de Movilidad/2005/MODULOD.xlsx"))
viajes2005<-viajes2005[,c("ID","NR_FOR","AI10_NHOG","D7_NPER","D34_MOTI","D35_MEDIO")]
viajes2005<-viajes2005[viajes2005$D35_MEDIO==2,]
viajes2005<-viajes2005[viajes2005$D34_MOTI%in%c(2,3,4),]

biciusuarios2005<-read_excel(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta de Movilidad/2005/MODULOC.xlsx"))
biciusuarios2005<-biciusuarios2005[!is.na(biciusuarios2005$ID_UPZ),c("ID","NR_FOR","AI10_NHOG","C7_PERS","C8_EDAD","C9_SEXO","FEXPROY","ID_UPZ")]
biciusuarios2005$Key<-paste(biciusuarios2005$ID,biciusuarios2005$NR_FOR,biciusuarios2005$AI10_NHOG,biciusuarios2005$C7_PERS,sep="-")
viajes2005$Key<-paste(viajes2005$ID,viajes2005$NR_FOR,viajes2005$AI10_NHOG,viajes2005$D7_NPER,sep="-")
biciusuarios2005<-na.omit(biciusuarios2005[biciusuarios2005$Key%in%viajes2005$Key,])
rm(viajes2005)

biciusuarios2005$Key<-NULL
biciusuarios2005$C9_SEXO<-as.factor(ifelse(biciusuarios2005$C9_SEXO==1,"Male","Female"))
biciusuarios2005$GR_EDAD<-factor(apply(as.data.frame(biciusuarios2005$C8_EDAD),MARGIN=1,FUN=encodeEdad2,g=gruposEdad),levels = paste0(gruposEdad$ED_MIN,"-",gruposEdad$ED_MAX))
biciusuarios2005$FECHA<-as.POSIXct("2005-01-01")
biciusuarios2005<-aggregate(biciusuarios2005$FEXPROY,by=list(biciusuarios2005$GR_EDAD,biciusuarios2005$C9_SEXO,biciusuarios2005$FECHA,biciusuarios2005$ID_UPZ),FUN=sum)
names(biciusuarios2005)<-c("GR_EDAD","SEXO","FECHA","UPZ","BICIUSRS")

biciusuarios2005<-merge(GENDER_AGE,biciusuarios2005,by=c("GR_EDAD","SEXO","UPZ"),all.x=TRUE) %>% mutate(FECHA = replace(FECHA, is.na(FECHA),"2005-01-01"),BICIUSRS = replace(BICIUSRS, is.na(BICIUSRS),0))

#Integración del número de biciusuarios----
biciusuarios<-rbind(biciusuarios2005,biciusuarios2011,biciusuarios2015,biciusuarios2017)
biciusuarios<-pad(biciusuarios,interval="year", group=c("GR_EDAD","SEXO","UPZ"),start_val=as.Date("2005-01-01"),end_val=as.Date("2017-01-01"))
biciusuarios$FECHA<-as.Date(biciusuarios$FECHA)

#Interpolación lineal de valores para años intermedios----
biciusuarios %<>%
  group_by(GR_EDAD,SEXO,UPZ) %>%
  mutate(BICIUSRSINT = na.approx(BICIUSRS, na.rm=FALSE))
rm(biciusuarios2005,biciusuarios2011,biciusuarios2015,biciusuarios2017,GENDER_AGE)

#Almacenamiento de los biciusuarios por grupo de edad y género----
save(biciusuarios,file=paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/5. biciusuariosUPZ.Rdata"))
