#Preparacion del entorno de trabajo----
rm(list=ls())
carpetaRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/UNIANDES - RAS - SDM"
gitRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/proBikePolicies"
require(pacman)
pacman::p_load(gdata)
pacman::p_load(sf)
pacman::p_load(readr)
pacman::p_load(tidyverse)
pacman::p_load(reshape2)

#Lectura de bases de datos para construir las tasas de mortalidad----
#Base de datos de defunciones en bogota por grupo de edad y género
load(paste(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/4. Edad-Sexo-Defunciones-Bogota.Rdata",sep=""))
#Base de datos de poblacion en bogota por grupo de edad, género y localidad
load(paste(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/6. Poblacion.Rdata",sep=""))
#Base de datos de bicisuarios por grupo de edad, género y localidad
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/5. biciusuarios.Rdata"))
#Base de datos de accidentes consolidados
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/3. AccidentesBiciTotalGeoData.Rdata"))

#Lectura de la función de codificación de la edad en grupos----
load(paste(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/4. GruposEdadCodificacion.Rdata",sep=""))

#Lectura de la base de datos de población estandar de referencia----
stndpop<-as.data.frame(read_lines(paste0(carpetaRAS,"/BASES DE DATOS/National Cancer Institute - StandPop/stdpop.singleagesthru99.txt")))
names(stndpop)<-c("init")
stndpop$Standard<-substr(stndpop$init,1,3)
stndpop$Age<-as.numeric(substr(stndpop$init,4,6))
stndpop$Pop<-as.numeric(substr(stndpop$init,7,14))
#Selección de una población de referencia: US2000 stndpop$Standard=="205" - WHO 1M stndpop$Standard=="012"
stndpop<-stndpop[stndpop$Standard=="012",]
stndpop$GR_EDAD<-factor(apply(as.data.frame(stndpop$Age),MARGIN=1,FUN=encodeEdad2,g=gruposEdad),levels = paste0(gruposEdad$ED_MIN,"-",gruposEdad$ED_MAX))
stndpop<-aggregate(stndpop$Pop,by=list(stndpop$GR_EDAD),FUN=sum)
names(stndpop)<-c("GR_EDAD","STNDPOP")

#Pre-procesamiento de la base de datos de accidentes de bici-usuarios para obtener las muertes----
st_geometry(AccidentesBiciTotal)<-NULL
AccidentesBiciTotal<-AccidentesBiciTotal[,c("Sexo","Edad","Accidentes.Localidad","Accidentes.Fecha","Gravedad2")]
AccidentesBiciTotal<-AccidentesBiciTotal[AccidentesBiciTotal$Gravedad2=="Dead",]
AccidentesBiciTotal$Gravedad2<-NULL
AccidentesBiciTotal$Edad<-as.numeric(AccidentesBiciTotal$Edad)
AccidentesBiciTotal$Fecha<-as.POSIXct(paste(getYear(AccidentesBiciTotal$Accidentes.Fecha),getMonth(AccidentesBiciTotal$Accidentes.Fecha),1,sep="-"),format="%Y-%m-%e")
AccidentesBiciTotal$Accidentes.Fecha<-NULL
AccidentesBiciTotal$Accidentes.Localidad<-as.character(AccidentesBiciTotal$Accidentes.Localidad)
AccidentesBiciTotal$Accidentes.Localidad[AccidentesBiciTotal$Accidentes.Localidad=="ANTONIO NARINO"]<-"ANTONIO NARIÑO"
AccidentesBiciTotal$Accidentes.Localidad<-as.factor(AccidentesBiciTotal$Accidentes.Localidad)
AccidentesBiciTotal<-na.omit(AccidentesBiciTotal)
AccidentesBiciTotal$GR_EDAD<-as.factor(apply(as.data.frame(AccidentesBiciTotal$Edad),MARGIN=1,FUN=encodeEdad2,g=gruposEdad))
names(AccidentesBiciTotal)<-c("SEXO","EDAD","LOCALIDAD","FECHA","GR_EDAD")

#Pre-procesamiento de la base de datos de biciusuarios----
biciusuarios$AÑO<-as.numeric(getYear(biciusuarios$FECHA))
biciusuarios$FECHA<-NULL
biciusuarios$BICIUSRS<-NULL

#Eliminación del grupo de edad de 0-4 años por limitaciones de información----
AccidentesBiciTotal<-AccidentesBiciTotal %>% filter(GR_EDAD!="0-4")
Agregado<-Agregado %>% filter(GR_EDAD!="0-4")
biciusuarios<-biciusuarios %>% filter(GR_EDAD!="0-4")
Poblacion<-Poblacion %>% filter(GR_EDAD!="0-4")
stndpop<-stndpop %>% filter(GR_EDAD!="0-4")

####### Calculo de las tosas de mortalidad para toda bogota - estimado de poblacion oficial (Agregado)----
# Lectura de las estimaciones de poblacion oficiales para bogota (Sexo y grupos de edad)
Poblacion_NLOC<-read.csv(paste0(carpetaRAS,"/BASES DE DATOS/DANE/VisorCertificaPPO_Oct11_SinProtección.csv"),skip=2) #Base de datos nacional
Poblacion_NLOC<-Poblacion_NLOC[Poblacion_NLOC$DPMP==11,1:251] #Filtrado para la ciudad de bogotá
Poblacion_NLOC<-Poblacion_NLOC[,5:251] #Poblacion_NLOC$Año>=2011
Poblacion_NLOC<-reshape(data=Poblacion_NLOC,varying = names(Poblacion_NLOC)[-1],timevar="Edad",idvar = "Año",direction = "long",sep=".")
Poblacion_NLOC<-Poblacion_NLOC[Poblacion_NLOC$Edad!="Total",]
Poblacion_NLOC<-Poblacion_NLOC[,-3]
names(Poblacion_NLOC)<-c("ANO","EDAD","Male","Female")
Poblacion_NLOC<-melt(Poblacion_NLOC,id = c("ANO","EDAD"),value.name="POBLACION")
names(Poblacion_NLOC)[3]<-"SEXO"
Poblacion_NLOC$EDAD<-as.numeric(Poblacion_NLOC$EDAD)
Poblacion_NLOC$POBLACION<-as.numeric(gsub(",","",Poblacion_NLOC$POBLACION))
Poblacion_NLOC$GR_EDAD<-factor(apply(as.data.frame(Poblacion_NLOC$EDAD),MARGIN=1,FUN=encodeEdad2,g=gruposEdad),levels = paste0(gruposEdad$ED_MIN,"-",gruposEdad$ED_MAX))
Poblacion_NLOC<-aggregate(Poblacion_NLOC$POBLACION,by=list(Poblacion_NLOC$GR_EDAD,Poblacion_NLOC$ANO),FUN=sum)
names(Poblacion_NLOC)<-c("GR_EDAD","AÑO","POBLACION")

# Eliminación del grupo de edad 0-4 años por limitación de la información disponible
Poblacion_NLOC<-Poblacion_NLOC[Poblacion_NLOC$GR_EDAD!="0-4",]

# Consolidación de las muertes de biciusuarios por edad y por sexo
AccidentesBiciTotal_NLOC<-aggregate(AccidentesBiciTotal$EDAD,by=list(AccidentesBiciTotal$GR_EDAD,getYear(AccidentesBiciTotal$FECHA)),FUN=length)
names(AccidentesBiciTotal_NLOC)<-c("GR_EDAD","FECHA","MUERTES")

#Consolidación de los biciusuarios por grupos de edad y sexo
biciusuarios_bogota<-aggregate(biciusuarios$BICIUSRSINT,by=list(biciusuarios$GR_EDAD,biciusuarios$AÑO),FUN=sum)
names(biciusuarios_bogota)<-c("GR_EDAD","AÑO","BICIUSRSINT")

#Consolidación de la tabla de defunciones totales, población total, muertes de biciusuarios, bicisusuarios y poblacion estandar por grupo de edad, sexo y més
Agregado_aux<-aggregate(Agregado$DEFUNCIONES,by=list(Agregado$GR_EDAD,Agregado$ANO,getYear(Agregado$FECHA)),FUN=sum) %>% select(GR_EDAD=Group.1,ANO=Group.2,FECHA=Group.3,DEFUNCIONES=x)
Agregado_bogota<-merge(Agregado_aux,AccidentesBiciTotal_NLOC, by=c("GR_EDAD","FECHA"),all.x =TRUE)
Agregado_bogota$AÑO<-as.numeric(Agregado_bogota$FECHA)
Agregado_bogota<-merge(Agregado_bogota,Poblacion_NLOC,by=c("GR_EDAD","AÑO"),all.x=TRUE)
Agregado_bogota$MUERTES[is.na(Agregado_bogota$MUERTES)]<-0
Agregado_bogota<-merge(Agregado_bogota, biciusuarios_bogota, by=c("GR_EDAD","AÑO"),all.x=TRUE)
Agregado_bogota<-merge(Agregado_bogota, stndpop, by=c("GR_EDAD"),all.x=TRUE)
rm(AccidentesBiciTotal_NLOC,biciusuarios_bogota,Poblacion_NLOC,Agregado_aux)

#Calculo de las tasas de mortalidad para cada mes por género
# Age-specific death rates de la población total de bogotá
Agregado_bogota$ASDRbog<-(Agregado_bogota$DEFUNCIONES/Agregado_bogota$POBLACION)*100000
# Age-specific death rates de los biciusuarios
Agregado_bogota$ASDRbic<-(Agregado_bogota$MUERTES/Agregado_bogota$BICIUSRSINT)*100000
Agregado_bogota$ASDRbic[is.nan(Agregado_bogota$ASDRbic)]<-0
Agregado_bogota$ASDRbic[is.infinite(Agregado_bogota$ASDRbic)]<-0
# Varianza de las ASDR's
Agregado_bogota$VarASDRbog<-Agregado_bogota$ASDRbog*(100000-Agregado_bogota$ASDRbog)/Agregado_bogota$POBLACION
Agregado_bogota$VarASDRbic<-Agregado_bogota$ASDRbic*(100000-Agregado_bogota$ASDRbic)/Agregado_bogota$BICIUSRSINT
Agregado_bogota$VarASDRbic[is.nan(Agregado_bogota$VarASDRbic)]<-0
# Pesos de los grupos de edad en la población estándar
Agregado_bogota$w<-Agregado_bogota$STNDPOP/sum(stndpop$STNDPOP)
# Contribución de varianzas a los ADR's de cada grupo de edad en la población estandar
Agregado_bogota$VarBog<-(Agregado_bogota$w**2)*Agregado_bogota$VarASDRbog
Agregado_bogota$VarBic<-(Agregado_bogota$w**2)*Agregado_bogota$VarASDRbic

#Calculo de la mortalidad para cada grupo en ADR e IADR
# Muertes esperadas en los biciusuarios para IADR suponiendo la población total de bogotá como población estándar
Agregado_bogota$Exp<-Agregado_bogota$ASDRbog*(Agregado_bogota$BICIUSRSINT/100000)
# Muertes esperadas de los biciusuarios en la población estandar
Agregado_bogota$ADRbic<-Agregado_bogota$ASDRbic*Agregado_bogota$w
# Muertes esperadas de la población total de bogotá en la población estandar
Agregado_bogota$ADRbog<-Agregado_bogota$ASDRbog*Agregado_bogota$w

# Agregación de los valores de muertes esperadas en las tasas de mortalidad
WorkTable_bogotaAg_of<-aggregate(Agregado_bogota[,c("MUERTES","Exp","BICIUSRSINT","DEFUNCIONES","POBLACION","ADRbic","VarBic","ADRbog","VarBog")],by=list(Agregado_bogota$FECHA),FUN=sum)
names(WorkTable_bogotaAg_of)[1]<-c("FECHA")
rm(Agregado_bogota)
WorkTable_bogotaAg_of$CDRbog<-(WorkTable_bogotaAg_of$DEFUNCIONES/WorkTable_bogotaAg_of$POBLACION)*100000
WorkTable_bogotaAg_of$CDRbic<-(WorkTable_bogotaAg_of$MUERTES/WorkTable_bogotaAg_of$BICIUSRSINT)*100000
WorkTable_bogotaAg_of$SMRbic_bog<-WorkTable_bogotaAg_of$MUERTES/WorkTable_bogotaAg_of$Exp
WorkTable_bogotaAg_of$IADRbic_bog<-WorkTable_bogotaAg_of$CDRbog*WorkTable_bogotaAg_of$SMR

####### Calculo de las tosas de mortalidad para toda bogota - estimado de poblacion propuesto (Agregado)----

# Lectura de las estimaciones de poblacion oficiales para bogota (Sexo y grupos de edad)
Poblacion_NLOC<-aggregate(Poblacion$POBLACION,by=list(Poblacion$GR_EDAD,Poblacion$AÑO),FUN=sum)
names(Poblacion_NLOC)<-c("GR_EDAD","AÑO","POBLACION")

# Consolidación de las muertes de biciusuarios por edad y por sexo
AccidentesBiciTotal_NLOC<-aggregate(AccidentesBiciTotal$EDAD,by=list(AccidentesBiciTotal$GR_EDAD,getYear(AccidentesBiciTotal$FECHA)),FUN=length)
names(AccidentesBiciTotal_NLOC)<-c("GR_EDAD","FECHA","MUERTES")

#Consolidación de los biciusuarios por grupos de edad y sexo
biciusuarios_bogota<-aggregate(biciusuarios$BICIUSRSINT,by=list(biciusuarios$GR_EDAD,biciusuarios$AÑO),FUN=sum)
names(biciusuarios_bogota)<-c("GR_EDAD","AÑO","BICIUSRSINT")

#Consolidación de la tabla de defunciones totales, población total, muertes de biciusuarios, bicisusuarios y poblacion estandar por grupo de edad, sexo y més
Agregado_aux<-aggregate(Agregado$DEFUNCIONES,by=list(Agregado$GR_EDAD,Agregado$ANO,getYear(Agregado$FECHA)),FUN=sum) %>% select(GR_EDAD=Group.1,ANO=Group.2,FECHA=Group.3,DEFUNCIONES=x)
Agregado_bogota<-merge(Agregado_aux,AccidentesBiciTotal_NLOC, by=c("GR_EDAD","FECHA"),all.x =TRUE)
Agregado_bogota$AÑO<-as.numeric(Agregado_bogota$FECHA)
Agregado_bogota<-merge(Agregado_bogota,Poblacion_NLOC,by=c("GR_EDAD","AÑO"),all.x=TRUE)
Agregado_bogota$MUERTES[is.na(Agregado_bogota$MUERTES)]<-0
Agregado_bogota<-merge(Agregado_bogota, biciusuarios_bogota, by=c("GR_EDAD","AÑO"),all.x=TRUE)
Agregado_bogota<-merge(Agregado_bogota, stndpop, by=c("GR_EDAD"),all.x=TRUE)
rm(AccidentesBiciTotal_NLOC,biciusuarios_bogota,Poblacion_NLOC)

#Calculo de las tasas de mortalidad para cada mes por género
# Age-specific death rates de la población total de bogotá
Agregado_bogota$ASDRbog<-(Agregado_bogota$DEFUNCIONES/Agregado_bogota$POBLACION)*100000
# Age-specific death rates de los biciusuarios
Agregado_bogota$ASDRbic<-(Agregado_bogota$MUERTES/Agregado_bogota$BICIUSRSINT)*100000
Agregado_bogota$ASDRbic[is.nan(Agregado_bogota$ASDRbic)]<-0
Agregado_bogota$ASDRbic[is.infinite(Agregado_bogota$ASDRbic)]<-0
# Varianza de las ASDR's
Agregado_bogota$VarASDRbog<-Agregado_bogota$ASDRbog*(100000-Agregado_bogota$ASDRbog)/Agregado_bogota$POBLACION
Agregado_bogota$VarASDRbic<-Agregado_bogota$ASDRbic*(100000-Agregado_bogota$ASDRbic)/Agregado_bogota$BICIUSRSINT
Agregado_bogota$VarASDRbic[is.nan(Agregado_bogota$VarASDRbic)]<-0
# Pesos de los grupos de edad en la población estándar
Agregado_bogota$w<-Agregado_bogota$STNDPOP/sum(stndpop$STNDPOP)
# Contribución de varianzas a los ADR's de cada grupo de edad en la población estandar
Agregado_bogota$VarBog<-(Agregado_bogota$w**2)*Agregado_bogota$VarASDRbog
Agregado_bogota$VarBic<-(Agregado_bogota$w**2)*Agregado_bogota$VarASDRbic

#Calculo de la mortalidad para cada grupo en ADR e IADR
# Muertes esperadas en los biciusuarios para IADR suponiendo la población total de bogotá como población estándar
Agregado_bogota$Exp<-Agregado_bogota$ASDRbog*(Agregado_bogota$BICIUSRSINT/100000)
# Muertes esperadas de los biciusuarios en la población estandar
Agregado_bogota$ADRbic<-Agregado_bogota$ASDRbic*Agregado_bogota$w
# Muertes esperadas de la población total de bogotá en la población estandar
Agregado_bogota$ADRbog<-Agregado_bogota$ASDRbog*Agregado_bogota$w

# Agregación de los valores de muertes esperadas en las tasas de mortalidad
WorkTable_bogotaAg_prop<-aggregate(Agregado_bogota[,c("MUERTES","Exp","BICIUSRSINT","DEFUNCIONES","POBLACION","ADRbic","VarBic","ADRbog","VarBog")],by=list(Agregado_bogota$FECHA),FUN=sum)
names(WorkTable_bogotaAg_prop)[1]<-c("FECHA")
rm(Agregado_bogota)
WorkTable_bogotaAg_prop$CDRbog<-(WorkTable_bogotaAg_prop$DEFUNCIONES/WorkTable_bogotaAg_prop$POBLACION)*100000
WorkTable_bogotaAg_prop$CDRbic<-(WorkTable_bogotaAg_prop$MUERTES/WorkTable_bogotaAg_prop$BICIUSRSINT)*100000
WorkTable_bogotaAg_prop$SMRbic_bog<-WorkTable_bogotaAg_prop$MUERTES/WorkTable_bogotaAg_prop$Exp
WorkTable_bogotaAg_prop$IADRbic_bog<-WorkTable_bogotaAg_prop$CDRbog*WorkTable_bogotaAg_prop$SMR

####### Calculo de las tosas de mortalidad para toda bogota - estimado de poblacion oficial (Sexo)----

# Lectura de las estimaciones de poblacion oficiales para bogota (Sexo y grupos de edad)
Poblacion_NLOC<-read.csv(paste0(carpetaRAS,"/BASES DE DATOS/DANE/VisorCertificaPPO_Oct11_SinProtección.csv"),skip=2) #Base de datos nacional
Poblacion_NLOC<-Poblacion_NLOC[Poblacion_NLOC$DPMP==11,1:251] #Filtrado para la ciudad de bogotá
Poblacion_NLOC<-Poblacion_NLOC[,5:251] #Poblacion_NLOC$Año>=2011
Poblacion_NLOC<-reshape(data=Poblacion_NLOC,varying = names(Poblacion_NLOC)[-1],timevar="Edad",idvar = "Año",direction = "long",sep=".")
Poblacion_NLOC<-Poblacion_NLOC[Poblacion_NLOC$Edad!="Total",]
Poblacion_NLOC<-Poblacion_NLOC[,-3]
names(Poblacion_NLOC)<-c("ANO","EDAD","Male","Female")
Poblacion_NLOC<-melt(Poblacion_NLOC,id = c("ANO","EDAD"),value.name="POBLACION")
names(Poblacion_NLOC)[3]<-"SEXO"
Poblacion_NLOC$EDAD<-as.numeric(Poblacion_NLOC$EDAD)
Poblacion_NLOC$POBLACION<-as.numeric(gsub(",","",Poblacion_NLOC$POBLACION))
Poblacion_NLOC$GR_EDAD<-factor(apply(as.data.frame(Poblacion_NLOC$EDAD),MARGIN=1,FUN=encodeEdad2,g=gruposEdad),levels = paste0(gruposEdad$ED_MIN,"-",gruposEdad$ED_MAX))
Poblacion_NLOC<-aggregate(Poblacion_NLOC$POBLACION,by=list(Poblacion_NLOC$GR_EDAD,Poblacion_NLOC$SEXO,Poblacion_NLOC$ANO),FUN=sum)
names(Poblacion_NLOC)<-c("GR_EDAD","SEXO","AÑO","POBLACION")

# Eliminación del grupo de edad 0-4 años por limitación de la información disponible
Poblacion_NLOC<-Poblacion_NLOC[Poblacion_NLOC$GR_EDAD!="0-4",]

# Consolidación de las muertes de biciusuarios por edad y por sexo
AccidentesBiciTotal_NLOC<-aggregate(AccidentesBiciTotal$EDAD,by=list(AccidentesBiciTotal$GR_EDAD,AccidentesBiciTotal$SEXO,getYear(AccidentesBiciTotal$FECHA)),FUN=length)
names(AccidentesBiciTotal_NLOC)<-c("GR_EDAD","SEXO","FECHA","MUERTES")

#Consolidación de los biciusuarios por grupos de edad y sexo
biciusuarios_bogota<-aggregate(biciusuarios$BICIUSRSINT,by=list(biciusuarios$GR_EDAD,biciusuarios$SEXO,biciusuarios$AÑO),FUN=sum)
names(biciusuarios_bogota)<-c("GR_EDAD","SEXO","AÑO","BICIUSRSINT")

#Consolidación de la tabla de defunciones totales, población total, muertes de biciusuarios, bicisusuarios y poblacion estandar por grupo de edad, sexo y més
Agregado_bogota<-merge(Agregado %>% 
                         mutate(FECHA = getYear(FECHA)) %>% 
                         group_by(GR_EDAD, SEXO, ANO, FECHA) %>% 
                         summarise(DEFUNCIONES = sum(DEFUNCIONES)),AccidentesBiciTotal_NLOC, by=c("GR_EDAD","SEXO","FECHA"),all.x =TRUE)
Agregado_bogota$AÑO<-as.numeric(Agregado_bogota$FECHA)
Agregado_bogota<-merge(Agregado_bogota,Poblacion_NLOC,by=c("GR_EDAD","SEXO","AÑO"),all.x=TRUE)
Agregado_bogota$MUERTES[is.na(Agregado_bogota$MUERTES)]<-0
Agregado_bogota<-merge(Agregado_bogota, biciusuarios_bogota, by=c("GR_EDAD","SEXO","AÑO"),all.x=TRUE)
Agregado_bogota<-merge(Agregado_bogota, stndpop, by=c("GR_EDAD"),all.x=TRUE)
rm(AccidentesBiciTotal_NLOC,biciusuarios_bogota,Poblacion_NLOC)

#Calculo de las tasas de mortalidad para cada mes por género
# Age-specific death rates de la población total de bogotá
Agregado_bogota$ASDRbog<-(Agregado_bogota$DEFUNCIONES/Agregado_bogota$POBLACION)*100000
TasasMortalidad_bogota_of<-Agregado_bogota %>% select(FECHA,GR_EDAD,DEFUNCIONES,SEXO,ASDRbog)
# Age-specific death rates de los biciusuarios
Agregado_bogota$ASDRbic<-(Agregado_bogota$MUERTES/Agregado_bogota$BICIUSRSINT)*100000
Agregado_bogota$ASDRbic[is.nan(Agregado_bogota$ASDRbic)]<-0
Agregado_bogota$ASDRbic[is.infinite(Agregado_bogota$ASDRbic)]<-0
# Varianza de las ASDR's
Agregado_bogota$VarASDRbog<-Agregado_bogota$ASDRbog*(100000-Agregado_bogota$ASDRbog)/Agregado_bogota$POBLACION
Agregado_bogota$VarASDRbic<-Agregado_bogota$ASDRbic*(100000-Agregado_bogota$ASDRbic)/Agregado_bogota$BICIUSRSINT
Agregado_bogota$VarASDRbic[is.nan(Agregado_bogota$VarASDRbic)]<-0
# Pesos de los grupos de edad en la población estándar
Agregado_bogota$w<-Agregado_bogota$STNDPOP/sum(stndpop$STNDPOP)
# Contribución de varianzas a los ADR's de cada grupo de edad en la población estandar
Agregado_bogota$VarBog<-(Agregado_bogota$w**2)*Agregado_bogota$VarASDRbog
Agregado_bogota$VarBic<-(Agregado_bogota$w**2)*Agregado_bogota$VarASDRbic

#Calculo de la mortalidad para cada grupo en ADR e IADR
# Muertes esperadas en los biciusuarios para IADR suponiendo la población total de bogotá como población estándar
Agregado_bogota$Exp<-Agregado_bogota$ASDRbog*(Agregado_bogota$BICIUSRSINT/100000)
# Muertes esperadas de los biciusuarios en la población estandar
Agregado_bogota$ADRbic<-Agregado_bogota$ASDRbic*Agregado_bogota$w
# Muertes esperadas de la población total de bogotá en la población estandar
Agregado_bogota$ADRbog<-Agregado_bogota$ASDRbog*Agregado_bogota$w

# Agregación de los valores de muertes esperadas en las tasas de mortalidad
WorkTable_bogota_of<-aggregate(Agregado_bogota[,c("MUERTES","Exp","BICIUSRSINT","DEFUNCIONES","POBLACION","ADRbic","VarBic","ADRbog","VarBog")],by=list(Agregado_bogota$FECHA,Agregado_bogota$SEXO),FUN=sum)
names(WorkTable_bogota_of)[1:2]<-c("FECHA","SEXO")
rm(Agregado_bogota)
WorkTable_bogota_of$CDRbog<-(WorkTable_bogota_of$DEFUNCIONES/WorkTable_bogota_of$POBLACION)*100000
WorkTable_bogota_of$CDRbic<-(WorkTable_bogota_of$MUERTES/WorkTable_bogota_of$BICIUSRSINT)*100000
WorkTable_bogota_of$SMRbic_bog<-WorkTable_bogota_of$MUERTES/WorkTable_bogota_of$Exp
WorkTable_bogota_of$IADRbic_bog<-WorkTable_bogota_of$CDRbog*WorkTable_bogota_of$SMR

####### Calculo de las tosas de mortalidad para toda bogota - estimado de poblacion propuesto (Sexo)----

# Lectura de las estimaciones de poblacion oficiales para bogota (Sexo y grupos de edad)
Poblacion_NLOC<-aggregate(Poblacion$POBLACION,by=list(Poblacion$GR_EDAD,Poblacion$SEXO,Poblacion$AÑO),FUN=sum)
names(Poblacion_NLOC)<-c("GR_EDAD","SEXO","AÑO","POBLACION")

# Consolidación de las muertes de biciusuarios por edad y por sexo
AccidentesBiciTotal_NLOC<-aggregate(AccidentesBiciTotal$EDAD,by=list(AccidentesBiciTotal$GR_EDAD,AccidentesBiciTotal$SEXO,AccidentesBiciTotal$FECHA),FUN=length)
names(AccidentesBiciTotal_NLOC)<-c("GR_EDAD","SEXO","FECHA","MUERTES")

#Consolidación de los biciusuarios por grupos de edad y sexo
biciusuarios_bogota<-aggregate(biciusuarios$BICIUSRSINT,by=list(biciusuarios$GR_EDAD,biciusuarios$SEXO,biciusuarios$AÑO),FUN=sum)
names(biciusuarios_bogota)<-c("GR_EDAD","SEXO","AÑO","BICIUSRSINT")

#Consolidación de la tabla de defunciones totales, población total, muertes de biciusuarios, bicisusuarios y poblacion estandar por grupo de edad, sexo y més
Agregado_bogota<-merge(Agregado,AccidentesBiciTotal_NLOC, by=c("GR_EDAD","SEXO","FECHA"),all.x =TRUE)
Agregado_bogota$AÑO<-as.numeric(getYear(Agregado_bogota$FECHA))
Agregado_bogota<-merge(Agregado_bogota,Poblacion_NLOC,by=c("GR_EDAD","SEXO","AÑO"),all.x=TRUE)
Agregado_bogota$MUERTES[is.na(Agregado_bogota$MUERTES)]<-0
Agregado_bogota<-merge(Agregado_bogota, biciusuarios_bogota, by=c("GR_EDAD","SEXO","AÑO"),all.x=TRUE)
Agregado_bogota<-merge(Agregado_bogota, stndpop, by=c("GR_EDAD"),all.x=TRUE)
rm(AccidentesBiciTotal_NLOC,biciusuarios_bogota,Poblacion_NLOC)

#Calculo de las tasas de mortalidad para cada mes por género
# Age-specific death rates de la población total de bogotá
Agregado_bogota$ASDRbog<-(Agregado_bogota$DEFUNCIONES/Agregado_bogota$POBLACION)*100000
TasasMortalidad_bogota_prop<-Agregado_bogota %>% select(FECHA,GR_EDAD,DEFUNCIONES,SEXO,ASDRbog)
# Age-specific death rates de los biciusuarios
Agregado_bogota$ASDRbic<-(Agregado_bogota$MUERTES/Agregado_bogota$BICIUSRSINT)*100000
Agregado_bogota$ASDRbic[is.nan(Agregado_bogota$ASDRbic)]<-0
Agregado_bogota$ASDRbic[is.infinite(Agregado_bogota$ASDRbic)]<-0
# Varianza de las ASDR's
Agregado_bogota$VarASDRbog<-Agregado_bogota$ASDRbog*(100000-Agregado_bogota$ASDRbog)/Agregado_bogota$POBLACION
Agregado_bogota$VarASDRbic<-Agregado_bogota$ASDRbic*(100000-Agregado_bogota$ASDRbic)/Agregado_bogota$BICIUSRSINT
Agregado_bogota$VarASDRbic[is.nan(Agregado_bogota$VarASDRbic)]<-0
# Pesos de los grupos de edad en la población estándar
Agregado_bogota$w<-Agregado_bogota$STNDPOP/sum(stndpop$STNDPOP)
# Contribución de varianzas a los ADR's de cada grupo de edad en la población estandar
Agregado_bogota$VarBog<-(Agregado_bogota$w**2)*Agregado_bogota$VarASDRbog
Agregado_bogota$VarBic<-(Agregado_bogota$w**2)*Agregado_bogota$VarASDRbic

#Calculo de la mortalidad para cada grupo en ADR e IADR
# Muertes esperadas en los biciusuarios para IADR suponiendo la población total de bogotá como población estándar
Agregado_bogota$Exp<-Agregado_bogota$ASDRbog*(Agregado_bogota$BICIUSRSINT/100000)
# Muertes esperadas de los biciusuarios en la población estandar
Agregado_bogota$ADRbic<-Agregado_bogota$ASDRbic*Agregado_bogota$w
# Muertes esperadas de la población total de bogotá en la población estandar
Agregado_bogota$ADRbog<-Agregado_bogota$ASDRbog*Agregado_bogota$w

# Agregación de los valores de muertes esperadas en las tasas de mortalidad
WorkTable_bogota_prop<-aggregate(Agregado_bogota[,c("MUERTES","Exp","BICIUSRSINT","DEFUNCIONES","POBLACION","ADRbic","VarBic","ADRbog","VarBog")],by=list(Agregado_bogota$FECHA,Agregado_bogota$SEXO),FUN=sum)
names(WorkTable_bogota_prop)[1:2]<-c("FECHA","SEXO")
rm(Agregado_bogota)
WorkTable_bogota_prop$CDRbog<-(WorkTable_bogota_prop$DEFUNCIONES/WorkTable_bogota_prop$POBLACION)*100000
WorkTable_bogota_prop$CDRbic<-(WorkTable_bogota_prop$MUERTES/WorkTable_bogota_prop$BICIUSRSINT)*100000
WorkTable_bogota_prop$SMRbic_bog<-WorkTable_bogota_prop$MUERTES/WorkTable_bogota_prop$Exp
WorkTable_bogota_prop$IADRbic_bog<-WorkTable_bogota_prop$CDRbog*WorkTable_bogota_prop$SMR

####### Calculo de las tosas de mortalidad para toda bogota - estimado de población oficial (Sexo - localidad)----

# Consolidación de las muertes de biciusuarios por edad, sexo y localidad
AccidentesBiciTotal_FULL<-aggregate(AccidentesBiciTotal$EDAD,by=list(AccidentesBiciTotal$GR_EDAD,AccidentesBiciTotal$SEXO,AccidentesBiciTotal$FECHA,AccidentesBiciTotal$LOCALIDAD),FUN=length)
names(AccidentesBiciTotal_FULL)<-c("GR_EDAD","SEXO","FECHA","LOCALIDAD","MUERTES")

#Consolidación de los biciusuarios por grupos de edad, sexo y localidad
biciusuarios_localidad<-aggregate(biciusuarios$BICIUSRSINT,by=list(biciusuarios$GR_EDAD,biciusuarios$SEXO,biciusuarios$AÑO,biciusuarios$LOCALIDAD),FUN=sum)
names(biciusuarios_localidad)<-c("GR_EDAD","SEXO","AÑO","LOCALIDAD","BICIUSRSINT")

#Tabla de combinaciones posibles de sexo,grupo de edad, localidad y fecha por meses
Agregado_localidad<-expand.grid(unique(Agregado$SEXO),unique(Agregado$GR_EDAD),unique(Poblacion$LOCALIDAD),unique(Agregado$FECHA))
names(Agregado_localidad)<-c("SEXO","GR_EDAD","LOCALIDAD","FECHA")
Agregado_localidad$AÑO<-as.numeric(getYear(Agregado_localidad$FECHA))

#Consolidación de las ASDR de bogota, población total, muertes de biciusuarios, bicisusuarios y poblacion estandar por grupo de edad, sexo, més y localidad
Agregado_localidad<-merge(Agregado_localidad,TasasMortalidad_bogota_of,by=c("FECHA","SEXO","GR_EDAD"),all.x=TRUE)
Agregado_localidad<-merge(Agregado_localidad,Poblacion, by=c("GR_EDAD","SEXO","AÑO","LOCALIDAD"),all.x=TRUE)
Agregado_localidad<-merge(Agregado_localidad,AccidentesBiciTotal_FULL,by=c("GR_EDAD","SEXO","FECHA","LOCALIDAD"),all.x=TRUE)
Agregado_localidad$MUERTES[is.na(Agregado_localidad$MUERTES)]<-0
Agregado_localidad<-merge(Agregado_localidad, biciusuarios_localidad, by=c("GR_EDAD","SEXO","AÑO","LOCALIDAD"),all.x=TRUE)
Agregado_localidad<-merge(Agregado_localidad, stndpop, by=c("GR_EDAD"),all.x=TRUE)
rm(AccidentesBiciTotal_FULL, biciusuarios_localidad)

#Calculo de las tasas de mortalidad para cada mes por género
# Age-specific death rates de los biciusuarios
Agregado_localidad$ASDRbic<-(Agregado_localidad$MUERTES/Agregado_localidad$BICIUSRSINT)*100000
Agregado_localidad$ASDRbic[is.nan(Agregado_localidad$ASDRbic)]<-0
Agregado_localidad$ASDRbic[is.infinite(Agregado_localidad$ASDRbic)]<-0
# Varianza de la ASDR
Agregado_localidad$VarASDRbic<-Agregado_localidad$ASDRbic*(100000-Agregado_localidad$ASDRbic)/Agregado_localidad$BICIUSRSINT
Agregado_localidad$VarASDRbic[is.nan(Agregado_localidad$VarASDRbic)]<-0
Agregado_localidad$VarASDRbic[is.infinite(Agregado_localidad$VarASDRbic)]<-0
# Pesos de los grupos de edad en la población estándar
Agregado_localidad$w<-Agregado_localidad$STNDPOP/sum(stndpop$STNDPOP)
# Contribución de varianza al ADR de cada grupo de edad en la población estandar
Agregado_localidad$VarBic<-(Agregado_localidad$w**2)*Agregado_localidad$VarASDRbic

#Calculo de la mortalidad para cada grupo en ADR e IADR
# Muertes esperadas en los biciusuarios para IADR suponiendo la población total de bogotá como población estándar
Agregado_localidad$Exp<-Agregado_localidad$ASDRbog*(Agregado_localidad$BICIUSRSINT/100000)
# Muertes esperadas de los biciusuarios en la población estandar
Agregado_localidad$ADRbic<-Agregado_localidad$ASDRbic*Agregado_localidad$w

# Agregación de los valores de muertes esperadas en las tasas de mortalidad
WorkTable_localidad_of<-aggregate(Agregado_localidad[,c("DEFUNCIONES","POBLACION","MUERTES","Exp","BICIUSRSINT","ADRbic","VarBic")],by=list(Agregado_localidad$FECHA,Agregado_localidad$SEXO,Agregado_localidad$LOCALIDAD),FUN=sum)
names(WorkTable_localidad_of)[1:3]<-c("FECHA","SEXO","LOCALIDAD")
rm(Agregado_localidad)
WorkTable_localidad_of$CDRbog<-(WorkTable_localidad_of$DEFUNCIONES/WorkTable_localidad_of$POBLACION)*100000
WorkTable_localidad_of$CDRbic<-(WorkTable_localidad_of$MUERTES/WorkTable_localidad_of$BICIUSRSINT)*100000
WorkTable_localidad_of$CDRbic[is.nan(WorkTable_localidad_of$CDRbic)]<-0
WorkTable_localidad_of$CDRbic[is.infinite(WorkTable_localidad_of$CDRbic)]<-0
WorkTable_localidad_of$SMRbic_bog<-WorkTable_localidad_of$MUERTES/WorkTable_localidad_of$Exp
WorkTable_localidad_of$SMRbic_bog[is.nan(WorkTable_localidad_of$SMRbic_bog)]<-0
WorkTable_localidad_of$SMRbic_bog[is.infinite(WorkTable_localidad_of$SMRbic_bog)]<-0
WorkTable_localidad_of$IADRbic_bog<-WorkTable_localidad_of$CDRbog*WorkTable_localidad_of$SMRbic_bog

####### Calculo de las tosas de mortalidad para toda bogota - estimado de población propuesto (Sexo - localidad)----

# Consolidación de las muertes de biciusuarios por edad, sexo y localidad
AccidentesBiciTotal_FULL<-aggregate(AccidentesBiciTotal$EDAD,by=list(AccidentesBiciTotal$GR_EDAD,AccidentesBiciTotal$SEXO,AccidentesBiciTotal$FECHA,AccidentesBiciTotal$LOCALIDAD),FUN=length)
names(AccidentesBiciTotal_FULL)<-c("GR_EDAD","SEXO","FECHA","LOCALIDAD","MUERTES")

#Consolidación de los biciusuarios por grupos de edad, sexo y localidad
biciusuarios_localidad<-aggregate(biciusuarios$BICIUSRSINT,by=list(biciusuarios$GR_EDAD,biciusuarios$SEXO,biciusuarios$AÑO,biciusuarios$LOCALIDAD),FUN=sum)
names(biciusuarios_localidad)<-c("GR_EDAD","SEXO","AÑO","LOCALIDAD","BICIUSRSINT")

#Tabla de combinaciones posibles de sexo,grupo de edad, localidad y fecha por meses
Agregado_localidad<-expand.grid(unique(Agregado$SEXO),unique(Agregado$GR_EDAD),unique(Poblacion$LOCALIDAD),unique(Agregado$FECHA))
names(Agregado_localidad)<-c("SEXO","GR_EDAD","LOCALIDAD","FECHA")
Agregado_localidad$AÑO<-as.numeric(getYear(Agregado_localidad$FECHA))

#Consolidación de las ASDR de bogota, población total, muertes de biciusuarios, bicisusuarios y poblacion estandar por grupo de edad, sexo, més y localidad
Agregado_localidad<-merge(Agregado_localidad,TasasMortalidad_bogota_prop,by=c("FECHA","SEXO","GR_EDAD"),all.x=TRUE)
Agregado_localidad<-merge(Agregado_localidad,Poblacion, by=c("GR_EDAD","SEXO","AÑO","LOCALIDAD"),all.x=TRUE)
Agregado_localidad<-merge(Agregado_localidad,AccidentesBiciTotal_FULL,by=c("GR_EDAD","SEXO","FECHA","LOCALIDAD"),all.x=TRUE)
Agregado_localidad$MUERTES[is.na(Agregado_localidad$MUERTES)]<-0
Agregado_localidad<-merge(Agregado_localidad, biciusuarios_localidad, by=c("GR_EDAD","SEXO","AÑO","LOCALIDAD"),all.x=TRUE)
Agregado_localidad<-merge(Agregado_localidad, stndpop, by=c("GR_EDAD"),all.x=TRUE)
rm(AccidentesBiciTotal_FULL, biciusuarios_localidad)

#Calculo de las tasas de mortalidad para cada mes por género
# Age-specific death rates de los biciusuarios
Agregado_localidad$ASDRbic<-(Agregado_localidad$MUERTES/Agregado_localidad$BICIUSRSINT)*100000
Agregado_localidad$ASDRbic[is.nan(Agregado_localidad$ASDRbic)]<-0
Agregado_localidad$ASDRbic[is.infinite(Agregado_localidad$ASDRbic)]<-0
# Varianza de la ASDR
Agregado_localidad$VarASDRbic<-Agregado_localidad$ASDRbic*(100000-Agregado_localidad$ASDRbic)/Agregado_localidad$BICIUSRSINT
Agregado_localidad$VarASDRbic[is.nan(Agregado_localidad$VarASDRbic)]<-0
Agregado_localidad$VarASDRbic[is.infinite(Agregado_localidad$VarASDRbic)]<-0
# Pesos de los grupos de edad en la población estándar
Agregado_localidad$w<-Agregado_localidad$STNDPOP/sum(stndpop$STNDPOP)
# Contribución de varianza al ADR de cada grupo de edad en la población estandar
Agregado_localidad$VarBic<-(Agregado_localidad$w**2)*Agregado_localidad$VarASDRbic

#Calculo de la mortalidad para cada grupo en ADR e IADR
# Muertes esperadas en los biciusuarios para IADR suponiendo la población total de bogotá como población estándar
Agregado_localidad$Exp<-Agregado_localidad$ASDRbog*(Agregado_localidad$BICIUSRSINT/100000)
# Muertes esperadas de los biciusuarios en la población estandar
Agregado_localidad$ADRbic<-Agregado_localidad$ASDRbic*Agregado_localidad$w

# Agregación de los valores de muertes esperadas en las tasas de mortalidad
WorkTable_localidad_prop<-aggregate(Agregado_localidad[,c("DEFUNCIONES","POBLACION","MUERTES","Exp","BICIUSRSINT","ADRbic","VarBic")],by=list(Agregado_localidad$FECHA,Agregado_localidad$SEXO,Agregado_localidad$LOCALIDAD),FUN=sum)
names(WorkTable_localidad_prop)[1:3]<-c("FECHA","SEXO","LOCALIDAD")
rm(Agregado_localidad)
WorkTable_localidad_prop$CDRbog<-(WorkTable_localidad_prop$DEFUNCIONES/WorkTable_localidad_prop$POBLACION)*100000
WorkTable_localidad_prop$CDRbic<-(WorkTable_localidad_prop$MUERTES/WorkTable_localidad_prop$BICIUSRSINT)*100000
WorkTable_localidad_prop$CDRbic[is.nan(WorkTable_localidad_prop$CDRbic)]<-0
WorkTable_localidad_prop$CDRbic[is.infinite(WorkTable_localidad_prop$CDRbic)]<-0
WorkTable_localidad_prop$SMRbic_bog<-WorkTable_localidad_prop$MUERTES/WorkTable_localidad_prop$Exp
WorkTable_localidad_prop$SMRbic_bog[is.nan(WorkTable_localidad_prop$SMRbic_bog)]<-0
WorkTable_localidad_prop$SMRbic_bog[is.infinite(WorkTable_localidad_prop$SMRbic_bog)]<-0
WorkTable_localidad_prop$IADRbic_bog<-WorkTable_localidad_prop$CDRbog*WorkTable_localidad_prop$SMRbic_bog

#Almacenamiento de las tasas de mortalidad----
save(WorkTable_bogotaAg_of, WorkTable_bogotaAg_prop, 
     WorkTable_bogota_of, WorkTable_bogota_prop,
     WorkTable_localidad_of, WorkTable_localidad_prop,
     file=paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/7. MortalityRates.Rdata"))
