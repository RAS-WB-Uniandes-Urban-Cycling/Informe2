#Preparacion del entorno de trabajo----
rm(list=ls())
carpetaRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/UNIANDES - RAS - SDM"
gitRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/proBikePolicies"
require(pacman)
pacman::p_load(reshape2)
pacman::p_load(gdata)
pacman::p_load(padr)
pacman::p_load(dplyr)
pacman::p_load(readxl)
pacman::p_load(zoo)
pacman::p_load(readr)

#Lectura de las bases de datos de defunciones----
Defu2011<-read.delim(paste0(carpetaRAS,"/BASES DE DATOS/DANE/Defunciones/Defun_2008_2011/Defun_2011.txt"),quote="",fileEncoding = "Latin1",encoding = "Latin1",stringsAsFactors = FALSE)
Defu2012<-read.delim(paste0(carpetaRAS,"/BASES DE DATOS/DANE/Defunciones/Defun_2012_2013/Defun_2012.txt"),quote="",fileEncoding = "Latin1",encoding = "Latin1",stringsAsFactors = FALSE)
Defu2013<-read.delim(paste0(carpetaRAS,"/BASES DE DATOS/DANE/Defunciones/Defun_2012_2013/Defun_2013.txt"),quote="",fileEncoding = "Latin1",encoding = "Latin1",stringsAsFactors = FALSE)
Defu2014<-read.delim(paste0(carpetaRAS,"/BASES DE DATOS/DANE/Defunciones/Defun_2014/Defun_2014.txt"),quote="",fileEncoding = "Latin1",encoding = "Latin1",stringsAsFactors = FALSE)
Defu2015<-read.delim(paste0(carpetaRAS,"/BASES DE DATOS/DANE/Defunciones/Defun_2015/Defun_2015.txt"),quote="",fileEncoding = "Latin1",encoding = "Latin1",stringsAsFactors = FALSE)
Defu2016<-read.delim(paste0(carpetaRAS,"/BASES DE DATOS/DANE/Defunciones/Nofetal_2016/Nofetal_2016.txt"),quote="",fileEncoding = "Latin1",encoding = "Latin1",stringsAsFactors = FALSE)

#Integración de las bases de datos de defunciones----
names(Defu2011)<-toupper(names(Defu2011))
names(Defu2012)<-toupper(names(Defu2012))
names(Defu2013)<-toupper(names(Defu2013))
names(Defu2014)<-toupper(names(Defu2014))
names(Defu2015)<-toupper(names(Defu2015))
names(Defu2016)<-toupper(names(Defu2016))
Agregado<-rbind(Defu2011,Defu2012,Defu2013,Defu2014,Defu2015,Defu2016)
rm(Defu2011,Defu2012,Defu2013,Defu2014,Defu2015,Defu2016)
Agregado<-Agregado[Agregado$COD_DPTO==11,c("ANO","MES","SEXO","GRU_ED1","CAUSA_666","CAU_HOMOL")]
Agregado$ID<-c(1:dim(Agregado)[1])

#Filtrado de las defunciones por causas----
#Lista filtrada con base en la lista 6/67 de la OPS (CIE-10): ACCIDENTES DE TRANSPORTE  TERRESTRE, INCLUSIVE SECUELAS [501], OTROS ACCIDENTES DE TRANSPORTE Y LOS NO ESPECIFICADOS, INCLUSIVE SECUELAS [502]
Agregado<-Agregado[Agregado$CAUSA_666==501|Agregado$CAUSA_666==502,]

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
  else if(x==22){edad=c(70,Inf)} #70-74
  else if(x==23){edad=c(70,Inf)} #75-79
  #Se tienen grupos quinquenales del mismo modo hasta 99 años y luego 100+. Sin embargo se hace una agrupación de 80+ puesto que es como esta disponible la información de población. Finalmente tambien se reagrupa desde 70+ debido a los datos existentes sobre biciusuarios
  else if(x==24|x==25|x==26|x==27|x==28){edad=c(70,Inf)}
  else if(x==29){edad=c(NA,NA)}
  return(edad)
}

#Decodificación de la edad de las defunciones----
Agregado$ED_MIN<-NA
Agregado$ED_MAX<-NA
Agregado[,c("ED_MIN","ED_MAX")]<-t(apply(as.data.frame(Agregado$GRU_ED1),MARGIN=1,FUN=encodeEdad))
Agregado<-na.omit(Agregado)
gruposEdad<-unique(Agregado[,8:9]) #Arreglo que almacena los rangos de los grupos de edad
gruposEdad<-gruposEdad[order(gruposEdad$ED_MIN),]
Agregado$GRU_ED1<-NULL
Agregado$GR_EDAD<-factor(paste0(Agregado$ED_MIN,"-",Agregado$ED_MAX),levels = paste0(gruposEdad$ED_MIN,"-",gruposEdad$ED_MAX))
Agregado$ED_MIN<-NULL
Agregado$ED_MAX<-NULL

#Funcion para codificar la edad en los grupos de edad de las defunciones----
encodeEdad2<-function(x,g=gruposEdad){
  if(!is.na(x)){
    for(i in 1:dim(g)[1]){
      if((g[i,1]<=x)&& (x<=g[i,2])){
        return(paste0(g[i,1],"-",g[i,2]))
        break
      }
    }
  }
  else{
    return(NA)
  }
}

#Consolidacion de las defunciones por grupos de edad, sexo, año y mes----
Agregado<-aggregate(Agregado$ID,by=list(Agregado$GR_EDAD,Agregado$SEXO,Agregado$ANO,Agregado$MES),FUN=length)
names(Agregado)<-c("GR_EDAD","SEXO","ANO","MES","DEFUNCIONES")
Agregado<-Agregado[Agregado$SEXO %in% c(1:2),]
Agregado$SEXO<-ifelse(Agregado$SEXO==1,"Male","Female")
Agregado$SEXO<-as.factor(Agregado$SEXO)
Agregado$FECHA<-as.POSIXct(paste(Agregado$ANO,Agregado$MES,1,sep="-"),format="%Y-%m-%e")
Agregado$MES<-NULL

#Filling de las combinaciones de grupos de edad y sexo sin defunciones durante algun mes sobre los 60 meses esperados de observacion----
for(i in levels(Agregado$GR_EDAD)){
  for(j in levels(Agregado$SEXO)){
    if(dim(Agregado[((Agregado$GR_EDAD==i)&(Agregado$SEXO==j)),])[1]<6*12){
      print(paste(i,"-",j,"-","Obs:",dim(Agregado[((Agregado$GR_EDAD==i)&(Agregado$SEXO==j)),])[1])) 
    }
  }
}
rm(i,j)
Agregado<-pad(Agregado,interval="month", group=c("GR_EDAD","SEXO"),start_val=as.Date("2011-01-01"),end_val=as.Date("2016-12-01")) %>% mutate(DEFUNCIONES = replace(DEFUNCIONES, is.na(DEFUNCIONES), 0))
Agregado$ANO<-getYear(Agregado$FECHA)

# Guardado de la población y defunciones para la ciudad de bogotá por género y grupos de edad; almacenamiento de la funcion de codificación de eadd----
save(Agregado,file=paste(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/4. Edad-Sexo-Defunciones-Bogota.Rdata",sep=""))
save(gruposEdad,encodeEdad2,file=paste(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/4. GruposEdadCodificacion.Rdata",sep=""))
