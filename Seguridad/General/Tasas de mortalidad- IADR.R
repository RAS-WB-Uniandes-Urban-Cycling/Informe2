#Preparacion del entorno de trabajo----
rm(list=ls())
carpetaRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/Uniandes - RAS - SDM"
gitRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/proBikePolicies"
pacman::p_load(reshape2)
pacman::p_load(gdata)
pacman::p_load(padr)
pacman::p_load(dplyr)
pacman::p_load(readxl)
pacman::p_load(zoo)

#Lectura de las bases de datos de defunciones----
Defu2011<-read.delim(paste0(carpetaRAS,"/BASES DE DATOS/DANE/Defunciones/Defun_2008_2011/Defun_2011.txt"),quote="",fileEncoding = "Latin1",encoding = "Latin1",stringsAsFactors = FALSE)
Defu2012<-read.delim(paste0(carpetaRAS,"/BASES DE DATOS/DANE/Defunciones/Defun_2012_2013/Defun_2012.txt"),quote="",fileEncoding = "Latin1",encoding = "Latin1",stringsAsFactors = FALSE)
Defu2013<-read.delim(paste0(carpetaRAS,"/BASES DE DATOS/DANE/Defunciones/Defun_2012_2013/Defun_2013.txt"),quote="",fileEncoding = "Latin1",encoding = "Latin1",stringsAsFactors = FALSE)
Defu2014<-read.delim(paste0(carpetaRAS,"/BASES DE DATOS/DANE/Defunciones/Defun_2014/Defun_2014.txt"),quote="",fileEncoding = "Latin1",encoding = "Latin1",stringsAsFactors = FALSE)
Defu2015<-read.delim(paste0(carpetaRAS,"/BASES DE DATOS/DANE/Defunciones/Defun_2015/Defun_2015.txt"),quote="",fileEncoding = "Latin1",encoding = "Latin1",stringsAsFactors = FALSE)

#Integración de las bases de datos de defunciones----
names(Defu2011)<-toupper(names(Defu2011))
names(Defu2012)<-toupper(names(Defu2012))
names(Defu2013)<-toupper(names(Defu2013))
names(Defu2014)<-toupper(names(Defu2014))
names(Defu2014)<-toupper(names(Defu2015))
Defun<-rbind(Defu2011,Defu2012,Defu2013,Defu2014,Defu2015)
rm(Defu2011,Defu2012,Defu2013,Defu2014,Defu2015)
Defun<-Defun[Defun$COD_DPTO==11,c("ANO","MES","SEXO","GRU_ED1","CAUSA_666","CAU_HOMOL")]
Defun$ID<-c(1:dim(Defun)[1])

#Función para decodificar la edad de las defunciones----
encodeEdad<-function(x){
  edad=NULL
  if(x==0|x==1|x==2|x==3|x==4|x==5|x==6){edad=c(0,4)} #0
  else if(x==7){edad=c(0,4)} #1
  else if(x==8){edad=c(0,4)} #2-4
  else if(x==9){edad=c(5,9)}
  else if(x==10){edad=c(10,14)}
  else if(x==11){edad=c(15,19)}
  else if(x==12){edad=c(20,24)}
  else if(x==13){edad=c(25,29)}
  else if(x==14){edad=c(30,34)}
  else if(x==15){edad=c(35,39)}
  else if(x==16){edad=c(40,44)}
  else if(x==17){edad=c(45,49)}
  else if(x==18){edad=c(50,54)}
  else if(x==19){edad=c(55,59)}
  else if(x==20){edad=c(60,64)}
  else if(x==21){edad=c(65,69)}
  else if(x==22){edad=c(70,74)}
  else if(x==23){edad=c(75,79)}
  #Se tienen grupos quinquenales del mismo modo hasta 99 años y luego 100+. Sin embargo se hace una agrupación de 80+ puesto que es como esta disponible la información de población
  else if(x==24|x==25|x==26|x==27|x==28){edad=c(80,Inf)}
  else if(x==29){edad=c(NA,NA)}
  return(edad)
}

#Decodificación de la edad de las defunciones----
Defun$ED_MIN<-NA
Defun$ED_MAX<-NA
Defun[,c("ED_MIN","ED_MAX")]<-t(apply(as.data.frame(Defun$GRU_ED1),MARGIN=1,FUN=encodeEdad))
Defun<-na.omit(Defun)
gruposEdad<-unique(Defun[,8:9]) #Arreglo que almacena los rangos de los grupos de edad
Defun$GRU_ED1<-NULL
Defun$GR_EDAD<-as.factor(paste0(Defun$ED_MIN,"-",Defun$ED_MAX))
Defun$ED_MIN<-NULL
Defun$ED_MAX<-NULL

#Consolidacion de las defunciones pode grupos de edad, sexo, año y mes
Defun<-aggregate(Defun$ID,by=list(Defun$GR_EDAD,Defun$SEXO,Defun$ANO,Defun$MES),FUN=length)
names(Defun)<-c("GR_EDAD","SEXO","ANO","MES","DEFUNCIONES")
Defun<-Defun[Defun$SEXO %in% c(1:2),]
Defun$SEXO<-ifelse(Defun$SEXO==1,"Male","Female")
Defun$SEXO<-as.factor(Defun$SEXO)
Defun$FECHA<-as.POSIXct(paste(Defun$ANO,Defun$MES,1,sep="-"),format="%Y-%m-%e")
Defun$MES<-NULL


#Filling de las combinaciones de grupos de edad y sexo sin defunciones durante algun mes sobre los 60 meses esperados de observacion
for(i in levels(Defun$GR_EDAD)){
  for(j in levels(Defun$SEXO)){
    if(dim(Defun[((Defun$GR_EDAD==i)&(Defun$SEXO==j)),])[1]<60){
      print(paste(i,"-",j,"-",dim(Defun[((Defun$GR_EDAD==i)&(Defun$SEXO==j)),])[1])) 
    }
  }
}
Defun<-pad(Defun,interval="month", group=c("GR_EDAD","SEXO"),start_val=as.Date("2011-01-01"),end_val=as.Date("2015-12-01")) %>% mutate(DEFUNCIONES = replace(DEFUNCIONES, is.na(DEFUNCIONES), 0))

#Lectura de la base de datos de poblacion por grupos de edad y por género nacional
Poblacion<-read.csv(paste0(carpetaRAS,"/BASES DE DATOS/DANE/VisorCertificaPPO_Oct11_SinProtección.csv"),skip=2)
Poblacion<-Poblacion[Poblacion$DPMP==11,1:251]
Poblacion<-Poblacion[Poblacion$Año%in%c(2011:2015),5:251]
Poblacion<-reshape(data=Poblacion,varying = names(Poblacion)[-1],timevar="Edad",idvar = "Año",direction = "long",sep=".")
Poblacion<-Poblacion[Poblacion$Edad!="Total",]
Poblacion<-Poblacion[,-3]
names(Poblacion)<-c("ANO","EDAD","Male","Female")
Poblacion<-melt(Poblacion,id = c("ANO","EDAD"),value.name="POBLACION")
names(Poblacion)[3]<-"SEXO"
Poblacion$EDAD<-as.numeric(Poblacion$EDAD)
Poblacion$POBLACION<-as.numeric(gsub(",","",Poblacion$POBLACION))

#Funcion para codificar la edad en los grupos de edad de las defunciones
encodeEdad2<-function(x,g){
  for(i in 1:dim(g)[1]){
    if((g[i,1]<=x)&& (x<=g[i,2])){
      return(paste0(g[i,1],"-",g[i,2]))
      break
    }
  }
}
rm(i,j)
Poblacion$GR_EDAD<-as.factor(apply(as.data.frame(Poblacion$EDAD),MARGIN=1,FUN=encodeEdad2,g=gruposEdad))

#Consolidacion de las defunciones y poblacion sobre los grupos de edad por género por més
Poblacion<-aggregate(Poblacion$POBLACION,by=list(Poblacion$GR_EDAD,Poblacion$SEXO,Poblacion$ANO),FUN=sum)
names(Poblacion)<-c("GR_EDAD","SEXO","ANO","POBLACION")
Agregado<-merge(Defun, Poblacion, by=c("GR_EDAD","SEXO","ANO"),all.x=TRUE)
rm(Defun,Poblacion,encodeEdad)
Agregado$ANO<-NULL

#Lectura y pre-procesamiento de la base de datos de accidentes de bici-usuarios de despacio después de los gráficos
load(paste(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/AccidentesBiciDesp2011_2015_AfterGraphs.Rdata",sep=""))
AccidentesBici<-AccidentesBici[,c("Accidentes.Fecha","Edad","Sexo","Gravedad2")]
AccidentesBici<-AccidentesBici[AccidentesBici$Gravedad2=="Dead",]
AccidentesBici$Gravedad2<-NULL
AccidentesBici$Sexo<-factor(AccidentesBici$Sexo,levels=c("Female","Male"))
AccidentesBici$Fecha<-as.POSIXct(paste(getYear(AccidentesBici$Accidentes.Fecha),getMonth(AccidentesBici$Accidentes.Fecha),1,sep="-"),format="%Y-%m-%e")
AccidentesBici$Accidentes.Fecha<-NULL
AccidentesBici<-na.omit(AccidentesBici)
names(AccidentesBici)<-c("EDAD","SEXO","FECHA")
AccidentesBici$GR_EDAD<-as.factor(apply(as.data.frame(AccidentesBici$EDAD),MARGIN=1,FUN=encodeEdad2,g=gruposEdad))
AccidentesBici<-aggregate(AccidentesBici$EDAD,by=list(AccidentesBici$GR_EDAD,AccidentesBici$SEXO,AccidentesBici$FECHA),FUN=length)
names(AccidentesBici)<-c("GR_EDAD","SEXO","FECHA","MUERTES")

#Consolidación de la la tabla de defunciones totales, muertes por accidentes de bici-usuarios y población total por grupo de edad, sexo y més
Agregado<-merge(Agregado, AccidentesBici, by=c("GR_EDAD","SEXO","FECHA"),all.x=TRUE)
rm(AccidentesBici)
Agregado$MUERTES[is.na(Agregado$MUERTES)]<-0

#Estimación del número de biciusuarios 2015----
biciusuariosAgo2015<-read_excel(paste0(carpetaRAS,"/BASES DE DATOS/EncuestaMovilidad2015/ENCUESTA/encuesta 2015 - personas.xlsx"))
encuesta2015<-read_excel(paste0(carpetaRAS,"/BASES DE DATOS/EncuestaMovilidad2015/ENCUESTA/encuesta 2015 - encuestas.xlsx"))
encuesta2015<-encuesta2015[encuesta2015$DEPARTAMENTO=="Bogota D.C.",c("ID_ENCUESTA")]
biciusuariosAgo2015<-biciusuariosAgo2015[biciusuariosAgo2015$ID_ENCUESTA%in%encuesta2015$ID_ENCUESTA,c("SEXO","EDAD","MOVILIZA_BICICLETA","PONDERADOR_CALIBRADO")]
rm(encuesta2015)
biciusuariosAgo2015<-na.omit(biciusuariosAgo2015)
biciusuariosAgo2015<-biciusuariosAgo2015[biciusuariosAgo2015$MOVILIZA_BICICLETA=="S",]
biciusuariosAgo2015$PONDERADOR_CALIBRADO<-as.numeric(gsub(",",".",biciusuariosAgo2015$PONDERADOR_CALIBRADO))
biciusuariosAgo2015$GR_EDAD<-as.factor(apply(as.data.frame(biciusuariosAgo2015$EDAD),MARGIN=1,FUN=encodeEdad2,g=gruposEdad))
biciusuariosAgo2015$FECHA<-as.POSIXct("2015-08-01")
biciusuariosAgo2015<-aggregate(biciusuariosAgo2015$PONDERADOR_CALIBRADO,by=list(biciusuariosAgo2015$GR_EDAD,biciusuariosAgo2015$SEXO,biciusuariosAgo2015$FECHA),FUN=sum)
names(biciusuariosAgo2015)<-c("GR_EDAD","SEXO","FECHA","BICIUSRS")
biciusuariosAgo2015$SEXO<-as.factor(ifelse(biciusuariosAgo2015$SEXO=="Hombre","Male","Female"))

#Estimación de número de biciusurios 2011----
biciusuariosJul2011<-read_excel(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta de Movilidad 2011/120927_Base de Datos EODH 2011/MOD_B_PERSONAS_Tipico.xlsx"),na="N/A")
biciusuariosJul2011<-biciusuariosJul2011[biciusuariosJul2011$MUN_D=="11001",c("ORDEN","ID_PERSO","P4_B","P5_B","F_EXP")]

viajes2011<-read_excel(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta de Movilidad 2011/120927_Base de Datos EODH 2011/Mod_D_VIAJES2_BaseImputacion_Definitiva.xlsx"),na="N/A")
viajes2011<-viajes2011[,c("ORDEN","ID_PERSO","E1_P1_D","E2_P1_D","E3_P1_D","E4_P1_D","E5_P1_D")]
viajes2011<-viajes2011[!is.na(viajes2011$E1_P1_D),]
viajes2011$Bici<-ifelse(viajes2011$E1_P1_D==18|viajes2011$E1_P1_D==19|viajes2011$E2_P1_D==18|viajes2011$E2_P1_D==19|viajes2011$E3_P1_D==18|viajes2011$E3_P1_D==19|viajes2011$E4_P1_D==18|viajes2011$E4_P1_D==19|viajes2011$E5_P1_D==18|viajes2011$E5_P1_D==19,TRUE,FALSE)
viajes2011<-unique(na.omit(viajes2011[viajes2011$Bici==TRUE,c("ORDEN","ID_PERSO")]))

biciusuariosJul2011$Key<-paste(biciusuariosJul2011$ORDEN,biciusuariosJul2011$ID_PERSO,sep="-")
viajes2011$Key<-paste(viajes2011$ORDEN,viajes2011$ID_PERSO,sep="-")
biciusuariosJul2011<-biciusuariosJul2011[biciusuariosJul2011$Key %in% viajes2011$Key,]
rm(viajes2011)
biciusuariosJul2011$Key<-NULL
biciusuariosJul2011$P4_B<-as.factor(ifelse(biciusuariosJul2011$P4_B=="1","Male","Female"))
biciusuariosJul2011$P5_B<-as.numeric(biciusuariosJul2011$P5_B)
biciusuariosJul2011$GR_EDAD<-as.factor(apply(as.data.frame(biciusuariosJul2011$P5_B),MARGIN=1,FUN=encodeEdad2,g=gruposEdad))
biciusuariosJul2011$FECHA<-as.POSIXct("2011-07-01")
biciusuariosJul2011<-aggregate(biciusuariosJul2011$F_EXP,by=list(biciusuariosJul2011$GR_EDAD,biciusuariosJul2011$P4_B,biciusuariosJul2011$FECHA),FUN=sum)
names(biciusuariosJul2011)<-c("GR_EDAD","SEXO","FECHA","BICIUSRS")

#Estimacion de número de biciusuarios 2005----
viajes2005<-read_excel(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta de Movilidad 2005/MODULOD.xlsx"))
viajes2005<-viajes2005[,c("ID","NR_FOR","AI10_NHOG","D7_NPER","D35_MEDIO")]
viajes2005<-viajes2005[viajes2005$D35_MEDIO==2,]

biciusuariosOct2005<-read_excel(paste0(carpetaRAS,"/BASES DE DATOS/Encuesta de Movilidad 2005/MODULOC.xlsx"))
biciusuariosOct2005<-biciusuariosOct2005[!is.na(biciusuariosOct2005$ID_UPZ),c("ID","NR_FOR","AI10_NHOG","C7_PERS","C8_EDAD","C9_SEXO","FEXPROY")]

biciusuariosOct2005$Key<-paste(biciusuariosOct2005$ID,biciusuariosOct2005$NR_FOR,biciusuariosOct2005$AI10_NHOG,biciusuariosOct2005$C7_PERS,sep="-")
viajes2005$Key<-paste(viajes2005$ID,viajes2005$NR_FOR,viajes2005$AI10_NHOG,viajes2005$D7_NPER,sep="-")

biciusuariosOct2005<-na.omit(biciusuariosOct2005[biciusuariosOct2005$Key%in%viajes2005$Key,])
rm(viajes2005)
biciusuariosOct2005$Key<-NULL
biciusuariosOct2005$C9_SEXO<-as.factor(ifelse(biciusuariosOct2005$C9_SEXO==1,"Male","Female"))
biciusuariosOct2005$GR_EDAD<-as.factor(apply(as.data.frame(biciusuariosOct2005$C8_EDAD),MARGIN=1,FUN=encodeEdad2,g=gruposEdad))
biciusuariosOct2005$FECHA<-as.POSIXct("2005-11-01")
biciusuariosOct2005<-aggregate(biciusuariosOct2005$FEXPROY,by=list(biciusuariosOct2005$GR_EDAD,biciusuariosOct2005$C9_SEXO,biciusuariosOct2005$FECHA),FUN=sum)
names(biciusuariosOct2005)<-c("GR_EDAD","SEXO","FECHA","BICIUSRS")

#Integración del número de biciusuarios e interpolación----
biciusuarios<-rbind(biciusuariosOct2005,biciusuariosJul2011,biciusuariosAgo2015)
biciusuarios<-pad(biciusuarios,interval="month", group=c("GR_EDAD","SEXO"),start_val=as.Date("2005-11-01"),end_val=as.Date("2015-12-01"))

library(zoo)
prueba<-biciusuarios[biciusuarios$GR_EDAD=="20-24" & biciusuarios$SEXO=="Male",]
plot(approx(prueba$BICIUSRS,method="linear",xout=1:122)$y)

ggplot(data = prueba, aes(x=FECHA,y = BICIUSRS)) + geom_point(group=1, color="red") + theme_minimal()

 Agregado<-merge(Agregado, biciusuarios, by=c("GR_EDAD","SEXO","FECHA"),all.x=TRUE)
Agregado[Agregado$FECHA=="2005-11-01" & is.na(Agregado$BICIUSRS),]$BICIUSRS<-0
Agregado[Agregado$FECHA=="2011-07-01" & is.na(Agregado$BICIUSRS),]$BICIUSRS<-0
Agregado[Agregado$FECHA=="2015-08-01" & is.na(Agregado$BICIUSRS),]$BICIUSRS<-0

h2<-Agregado[Agregado$GR_EDAD=="30-34" & Agregado$SEXO=="Male",]$POBLACION

plot(h2)

