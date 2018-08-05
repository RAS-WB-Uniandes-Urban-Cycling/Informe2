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
loca_fact<-c("ANTONIO NARIÑO","BARRIOS UNIDOS","BOSA","CANDELARIA","CHAPINERO","CIUDAD BOLIVAR","ENGATIVA","FONTIBON","KENNEDY","LOS MARTIRES","PUENTE ARANDA","RAFAEL URIBE URIBE","SAN CRISTOBAL","SANTA FE","SUBA","SUMAPAZ","TEUSAQUILLO","TUNJUELITO","USAQUEN","USME","OTRA LOCALIDAD RURAL",NA)

#Lectura de las bases de datos de defunciones----
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/4. GruposEdadCodificacion.Rdata"))
ZAT_loca<-as.data.frame(st_read(paste0(carpetaRAS,"/RESULTADOS/GENERAL/GEO-DATA/ZATS_Localidad.shp"))) %>% select(ZAT_HOGAR=Zn_Nm_N,LOCALIDAD=LocNmbr)

#Tabla de combinaciones posibles de género y grupo de edad----
GENDER_AGE<-expand.grid(c("Male","Female"),paste0(gruposEdad$ED_MIN,"-",gruposEdad$ED_MAX),loca_fact)
names(GENDER_AGE)<-c("SEXO","GR_EDAD","LOCALIDAD")

#Estimación del número de biciusuarios 2017----
encuesta2017<-read.csv(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta multiproposito/2017/Identificacion ( Capitulo A)/Identificacion ( Capitulo A).csv"),sep=";",encoding = "Latin1",stringsAsFactors = FALSE)
encuesta2017<-encuesta2017[encuesta2017$DPTOMPIO=="11001",c("DIRECTORIO","LOCALIDAD_TEX")] 

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
biciusuarios2017<-aggregate(viajesPr$FEX_C,by=list(viajesPr$GR_EDAD,viajesPr$NPCEP5,viajesPr$FECHA,viajesPr$LOCALIDAD_TEX),FUN=sum)
rm(viajesPr)
names(biciusuarios2017)<-c("GR_EDAD","SEXO","FECHA","LOCALIDAD","BICIUSRS")

biciusuarios2017<-merge(GENDER_AGE,biciusuarios2017,by=c("GR_EDAD","SEXO","LOCALIDAD"),all.x=TRUE) %>% mutate(FECHA = replace(FECHA, is.na(FECHA),"2017-01-01"),BICIUSRS = replace(BICIUSRS, is.na(BICIUSRS),0))

#Estimación del número de biciusuarios 2015----
biciusuarios2015<-read.csv(paste0(carpetaRAS,"/RESULTADOS/GENERAL/TABLAS/encuesta 2015 - personasTipoDia.csv"))
biciusuarios2015<-biciusuarios2015[biciusuarios2015$tipo_dia=="dia_habil",]

etapas2015<-read.csv(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta de Movilidad/2015/Encuesta/encuesta 2015 - etapas.csv"),sep=";",fileEncoding = "Latin1",encoding = "Latin1")
etapas2015<-etapas2015[etapas2015$MEDIOTRASPORTE=="Bicicleta"|etapas2015$MEDIOTRASPORTE=="Bicicleta con motor"|etapas2015$MEDIOTRASPORTE=="Bicicletas publicas",]
etapas2015<-paste0(etapas2015$ID_ENCUESTA,"-",etapas2015$NUMERO_PERSONA)

encuesta2015<-read.csv(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta de Movilidad/2015/Encuesta/encuesta 2015 - encuestas.csv"),sep=";",fileEncoding = "Latin1",encoding = "Latin1",stringsAsFactors = FALSE)
encuesta2015<-encuesta2015[encuesta2015$DEPARTAMENTO=="Bogota D.C.",c("ID_ENCUESTA","ZAT_HOGAR")]
encuesta2015<-merge(encuesta2015,ZAT_loca,by=c("ZAT_HOGAR"),all.x=TRUE)

biciusuarios2015$Key<-paste0(biciusuarios2015$id_encuesta,"-",biciusuarios2015$numero_persona)
biciusuarios2015<-biciusuarios2015[biciusuarios2015$Key%in%etapas2015,]
biciusuarios2015<-biciusuarios2015[biciusuarios2015$id_encuesta%in%encuesta2015$ID_ENCUESTA,c("id_encuesta","sexo","edad","moviliza_bicicleta","ponderador_calibrado")]
biciusuarios2015<-merge(biciusuarios2015,encuesta2015,by.x = "id_encuesta",by.y="ID_ENCUESTA",all.x=TRUE)
rm(encuesta2015,etapas2015)

biciusuarios2015<-na.omit(biciusuarios2015)
names(biciusuarios2015)<-toupper(names(biciusuarios2015))

biciusuarios2015$PONDERADOR_CALIBRADO<-as.numeric(gsub(",",".",biciusuarios2015$PONDERADOR_CALIBRADO))
biciusuarios2015$GR_EDAD<-factor(apply(as.data.frame(biciusuarios2015$EDAD),MARGIN=1,FUN=encodeEdad2,g=gruposEdad),levels = paste0(gruposEdad$ED_MIN,"-",gruposEdad$ED_MAX))
biciusuarios2015$FECHA<-as.POSIXct("2015-01-01")
biciusuarios2015<-aggregate(biciusuarios2015$PONDERADOR_CALIBRADO,by=list(biciusuarios2015$GR_EDAD,biciusuarios2015$SEXO,biciusuarios2015$FECHA,biciusuarios2015$LOCALIDAD),FUN=sum)
names(biciusuarios2015)<-c("GR_EDAD","SEXO","FECHA","LOCALIDAD","BICIUSRS")
biciusuarios2015$SEXO<-as.factor(ifelse(biciusuarios2015$SEXO=="Hombre","Male","Female"))

biciusuarios2015<-merge(GENDER_AGE,biciusuarios2015,by=c("GR_EDAD","SEXO","LOCALIDAD"),all.x=TRUE) %>% mutate(FECHA = replace(FECHA, is.na(FECHA),"2015-01-01"),BICIUSRS = replace(BICIUSRS, is.na(BICIUSRS),0))

#Estimación del número de biciusuarios 2014----
encuesta2014<-read.csv(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta multiproposito/2014/capa/DBF_MTP_256_1.txt"),sep = "\t",encoding="Latin1",fileEncoding = "Latin1",stringsAsFactors = FALSE)
encuesta2014<-encuesta2014[encuesta2014$COD_MPIO=="11001",c("DIRECTORIO","NOMBRE_LOCALIDAD")]
encuesta2014$NOMBRE_LOCALIDAD[encuesta2014$NOMBRE_LOCALIDAD=="ANTONIO NARIO"]<-"ANTONIO NARIÑO"
encuesta2014$NOMBRE_LOCALIDAD[encuesta2014$NOMBRE_LOCALIDAD=="LA CANDELARIA"]<-"CANDELARIA"

viajesEstu<-read_delim(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta multiproposito/2014/caph/DBF_MTP_258_4.txt"),"\t", escape_double = FALSE, trim_ws = TRUE)
viajesEstu<-viajesEstu[viajesEstu$DIRECTORIO%in%encuesta2014$DIRECTORIO,c("DIRECTORIO","DIRECTORIO_PER","DIRECTORIO_HOG","NPCHP18G","FEX_C")]
viajesEstu$NPCHP18G<-na.fill(viajesEstu$NPCHP18G,0)
viajesEstu<-viajesEstu[viajesEstu$NPCHP18G==1,-4]
viajesEstu<-merge(viajesEstu,encuesta2014,by=c("DIRECTORIO"),all.x=TRUE)

viajesTr<-read_delim(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta multiproposito/2014/capk/DBF_MTP_258_7.txt"),"\t", escape_double = FALSE, trim_ws = TRUE)
viajesTr<-viajesTr[viajesTr$DIRECTORIO%in%encuesta2014$DIRECTORIO,c("DIRECTORIO","DIRECTORIO_PER","DIRECTORIO_HOG","NPCKP45G","FEX_C")]
viajesTr$NPCKP45G<-na.fill(viajesTr$NPCKP45G,0)
viajesTr<-viajesTr[viajesTr$NPCKP45G==1,-4]
viajesTr<-merge(viajesTr,encuesta2014,by=c("DIRECTORIO"),all.x=TRUE)

viajes2014<-rbind(viajesEstu,viajesTr)
viajesPr<-read_delim(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta multiproposito/2014/cape/DBF_MTP_258_1.txt"),"\t", escape_double = FALSE, trim_ws = TRUE)
viajesPr<-viajesPr[viajesPr$DIRECTORIO%in%encuesta2014$DIRECTORIO,c("DIRECTORIO","DIRECTORIO_PER","DIRECTORIO_HOG","NPCEP4","NPCEP5")]
rm(viajesEstu,viajesTr,encuesta2014)
viajesPr<-merge(viajesPr,viajes2014,by = c("DIRECTORIO","DIRECTORIO_HOG","DIRECTORIO_PER"),all.x=TRUE)
rm(viajes2014)
viajesPr$NPCEP5<-as.factor(ifelse(viajesPr$NPCEP5==1,"Male",ifelse(viajesPr$NPCEP5==2,"Female",NA)))
viajesPr$GR_EDAD<-factor(apply(as.data.frame(viajesPr$NPCEP4),MARGIN=1,FUN=encodeEdad2,g=gruposEdad),levels = paste0(gruposEdad$ED_MIN,"-",gruposEdad$ED_MAX))
viajesPr$FECHA<-as.POSIXct("2014-01-01")
viajesPr<-na.omit(viajesPr)
biciusuarios2014<-aggregate(viajesPr$FEX_C,by=list(viajesPr$GR_EDAD,viajesPr$NPCEP5,viajesPr$FECHA,viajesPr$NOMBRE_LOCALIDAD),FUN=sum)
rm(viajesPr)
names(biciusuarios2014)<-c("GR_EDAD","SEXO","FECHA","LOCALIDAD","BICIUSRS")

biciusuarios2014<-merge(GENDER_AGE,biciusuarios2014,by=c("GR_EDAD","SEXO","LOCALIDAD"),all.x=TRUE) %>% mutate(FECHA = replace(FECHA, is.na(FECHA),"2014-01-01"),BICIUSRS = replace(BICIUSRS, is.na(BICIUSRS),0))

#Estimación del número de biciusuarios 2011----
biciusuarios2011<-read_excel(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta de Movilidad/2011/120927_Base de Datos EODH 2011/MOD_B_PERSONAS_Tipico.xlsx"),na="N/A")
biciusuarios2011<-biciusuarios2011[biciusuarios2011$MUN_D=="11001",c("ORDEN","ID_PERSO","P4_B","P5_B","F_EXP","ZAT")]

viajes2011<-read_excel(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta de Movilidad/2011/120927_Base de Datos EODH 2011/Mod_D_VIAJES2_BaseImputacion_Definitiva.xlsx"),na="N/A")
viajes2011<-viajes2011[,c("ORDEN","ID_PERSO","E1_P1_D","E2_P1_D","E3_P1_D","E4_P1_D","E5_P1_D")]
viajes2011<-viajes2011[!is.na(viajes2011$E1_P1_D),]
viajes2011$Bici<-ifelse(viajes2011$E1_P1_D==18|viajes2011$E1_P1_D==19|viajes2011$E2_P1_D==18|viajes2011$E2_P1_D==19|viajes2011$E3_P1_D==18|viajes2011$E3_P1_D==19|viajes2011$E4_P1_D==18|viajes2011$E4_P1_D==19|viajes2011$E5_P1_D==18|viajes2011$E5_P1_D==19,TRUE,FALSE)
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
biciusuarios2011<-merge(biciusuarios2011,ZAT_loca,by.x="ZAT",by.y="ZAT_HOGAR",all.x=TRUE)
biciusuarios2011<-aggregate(biciusuarios2011$F_EXP,by=list(biciusuarios2011$GR_EDAD,biciusuarios2011$P4_B,biciusuarios2011$FECHA,biciusuarios2011$LOCALIDAD),FUN=sum)
names(biciusuarios2011)<-c("GR_EDAD","SEXO","FECHA","LOCALIDAD","BICIUSRS")

biciusuarios2011<-merge(GENDER_AGE,biciusuarios2011,by=c("GR_EDAD","SEXO","LOCALIDAD"),all.x=TRUE) %>% mutate(FECHA = replace(FECHA, is.na(FECHA),"2011-01-01"),BICIUSRS = replace(BICIUSRS, is.na(BICIUSRS),0))

#Estimacion del número de biciusuarios 2005----

        #Intersección de localidades con UPZ
        UPZ<-st_read(paste0(carpetaRAS,"/RESULTADOS/GENERAL/GEO-DATA/UPZ_bogota.shp"),crs=4326)
        Localidad<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR0318.gdb"),layer = "loca",crs=4326)
        UPZ_loca<-st_join(st_centroid(UPZ),Localidad,left=TRUE,join=st_within)
        st_geometry(UPZ_loca)<-st_geometry(UPZ)
        rm(Localidad,UPZ)
        UPZ_loca<-UPZ_loca[-grep(glob2rx("UPR*"),UPZ_loca$UPlCodigo),]
        UPZ_loca$UPlCodigo<-as.numeric(substr(UPZ_loca$UPlCodigo,4,6))
        st_geometry(UPZ_loca)<-NULL
        
viajes2005<-read_excel(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta de Movilidad/2005/MODULOD.xlsx"))
viajes2005<-viajes2005[,c("ID","NR_FOR","AI10_NHOG","D7_NPER","D35_MEDIO")]
viajes2005<-viajes2005[viajes2005$D35_MEDIO==2,]

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
biciusuarios2005<-merge(biciusuarios2005,UPZ_loca,by.x="ID_UPZ",by.y="UPlCodigo",all.x=TRUE)
biciusuarios2005<-aggregate(biciusuarios2005$FEXPROY,by=list(biciusuarios2005$GR_EDAD,biciusuarios2005$C9_SEXO,biciusuarios2005$FECHA,biciusuarios2005$LocNombre),FUN=sum)
names(biciusuarios2005)<-c("GR_EDAD","SEXO","FECHA","LOCALIDAD","BICIUSRS")

biciusuarios2005<-merge(GENDER_AGE,biciusuarios2005,by=c("GR_EDAD","SEXO","LOCALIDAD"),all.x=TRUE) %>% mutate(FECHA = replace(FECHA, is.na(FECHA),"2005-01-01"),BICIUSRS = replace(BICIUSRS, is.na(BICIUSRS),0))

#Integración del número de biciusuarios----
biciusuarios<-rbind(biciusuarios2005,biciusuarios2011,biciusuarios2014,biciusuarios2015,biciusuarios2017)
biciusuarios<-pad(biciusuarios,interval="year", group=c("GR_EDAD","SEXO","LOCALIDAD"),start_val=as.Date("2005-01-01"),end_val=as.Date("2017-01-01"))
biciusuarios$FECHA<-as.Date(biciusuarios$FECHA)

#Interpolación lineal de valores para años intermedios----
biciusuarios %<>%
  group_by(GR_EDAD,SEXO,LOCALIDAD) %>%
  mutate(BICIUSRSINT = na.approx(BICIUSRS, na.rm=FALSE))
rm(biciusuarios2005,biciusuarios2011,biciusuarios2014,biciusuarios2015,biciusuarios2017,GENDER_AGE)

#Almacenamiento de los biciusuarios por grupo de edad y género----
save(biciusuarios,file=paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/5. biciusuarios.Rdata"))
