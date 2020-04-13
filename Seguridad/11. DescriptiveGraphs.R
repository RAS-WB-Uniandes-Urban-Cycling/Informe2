#Preparación del entorno de trabajo----
rm(list=ls())
carpetaRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/Uniandes - RAS - SDM"
gitRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/proBikePolicies"
require(pacman)
pacman::p_load(readxl)
pacman::p_load(gdata)
pacman::p_load(tidyverse)
pacman::p_load(ggplot2)
pacman::p_load(ggpmisc)
pacman::p_load(RColorBrewer)
pacman::p_load(tidyr)
pacman::p_load(padr)
pacman::p_load(lubridate)
pacman::p_load(showtext)
pacman::p_load(sf)
pacman::p_load(tmap)
pacman::p_load(viridis)
pacman::p_load(lwgeom)
pacman::p_load(ggpubr)
font_add("Helvetica Light",paste(gitRAS,"/Seguridad/Helvetica Light.ttf",sep=""))
setwd(paste(carpetaRAS,"/RESULTADOS/SEGURIDAD/Resultados-Inglés",sep=""))
cmHeight<-7
cmWidth<-10
capture.output(now() %>% as.character(), file = "./GRAFICOS/estadisticas.txt")

#Leer bases de datos preprocesada----
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/3. AccidentesBiciTotalGeoData.Rdata"))
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/3. AccidentesTotal2011_2017.Rdata"))
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/4. Edad-Sexo-Defunciones-Bogota.Rdata"))
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/5. biciusuarios.Rdata"))
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/6. Poblacion.Rdata"))
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/7. MortalityRates.Rdata"))
#load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/9. DistZat2Zat.Rdata"))
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/10. Viajes_KilometrosRecorridos.Rdata"))
ciclTime<-read.csv(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/TABLAS/8. lengthCiclTime.csv"),stringsAsFactors = FALSE)
ciclTime$Fecha<-as.Date(ciclTime$FECHA,format="%F")
ciclTime$X<-NULL

#Lectura de bases de datos originales auxiliares----
Localidades<-st_read(paste(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR0318.gdb",sep=""),layer="Loca",crs=4326)
zat_loca<-st_read(paste0(carpetaRAS,"/RESULTADOS/GENERAL/GEO-DATA/ZATs_Localidad.shp"),crs=4326)
red_cicloruta<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR0318.gdb"),layer = "RBic",type = 5,crs=4326) %>% st_transform(4326) %>% st_cast("LINESTRING",do_split = TRUE)
red_cicloruta$LENGTH_GEO<-as.numeric(st_length(red_cicloruta))

#Eliminación del grupo de edad de 0-4 años por limitaciones de información----
AccidentesBiciTotal<-AccidentesBiciTotal %>% filter(EdadCategorica!="0-4")
AccidentesTotal<-AccidentesTotal %>% filter(EdadCategorica!="0-4")
Agregado<-Agregado %>% filter(GR_EDAD!="0-4")
biciusuarios<-biciusuarios %>% filter(GR_EDAD!="0-4")
Poblacion<-Poblacion %>% filter(GR_EDAD!="0-4")

########### INFRAESTRUCTURA ##########################################################################
#Kilómetros de infraestructura y biciusuarios por localidad en el tiempo----
aux<-aggregate(ciclTime$Mts,by=list(ciclTime$FECHA,ciclTime$LocNombre),FUN=sum) %>% select(Mts=x,Fecha=Group.1, LocNombre=Group.2) %>% mutate(Fecha=as.Date(Fecha),Año=getYear(Fecha))
aux<-left_join(aux, aggregate(biciusuarios$BICIUSRSINT,by=list(biciusuarios$FECHA,biciusuarios$LOCALIDAD),FUN=sum) %>% select(bici=x,Fecha=Group.1,LocNombre=Group.2) %>% mutate(Año=getYear(as.Date(Fecha))) %>% select(Año,LocNombre,bici), by=c("LocNombre","Año"))
svg("./GRAFICOS/Bikeway_Cyclists_Localidad_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux[getYear(aux$Fecha)<2019,],aes(x=Fecha))+scale_x_date(date_labels="%Y",limits=as.Date(c("2012-11-01","2018-04-01")))+xlab("Year")+
    geom_line(aes(y=Mts/1000,colour="Bikeway"),show.legend = TRUE)+ylab("Bikeway (Km)")+
    geom_line(aes(y=bici/1000,colour="Cyclists"),show.legend = TRUE)+scale_y_continuous(limits=c(0,90),sec.axis = sec_axis(~.*1, name = "Cyclists (x 1.000)"))+
    scale_colour_manual("", breaks = c("Bikeway", "Cyclists"),values = c("#4e78a6", "limegreen"))+labs(colour="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.position = c(0.9, 0.125),legend.text=element_text(colour="black",size=10,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"),axis.text.x = element_text(angle = 90,vjust=0.5))+
    facet_wrap(~LocNombre)
)
showtext_end()
dev.off()

#Kilómetros de infraestructura en el tiempo----
aux<-aggregate(ciclTime$Mts,by=list(ciclTime$FECHA),FUN=sum) %>% select(Mts=x,Fecha=Group.1) %>% mutate(Fecha=as.Date(Fecha))
svg("./GRAFICOS/Bikeway_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=Fecha,y=Mts/1000))+scale_x_date(date_labels="%Y")+xlab("Year")+
    geom_point(color="#4e78a6",size = 3)+geom_line(color="#4e78a6",linetype="dashed")+ylab("Bikeway (KM)")+
    geom_text(aes(label=round(Mts/1000,1)),size=3.2,hjust=1,vjust=-1,family="Helvetica Light",check_overlap = TRUE)+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))
  )
showtext_end()
dev.off()

#Kilómetros de infraestructura por localidad en diciembre de 2017----
aux<-aggregate(ciclTime$Mts,by=list(ciclTime$LocNombre,ciclTime$FECHA),FUN=sum) %>% select(Mts=x,LocNombre=Group.1,Fecha=Group.2) %>% filter(getYear(as.Date(Fecha))==2017) %>% mutate(Km=Mts/1000)
aux$LocNombre<-factor(aux$LocNombre, aux$LocNombre[order(aux$Km,decreasing=FALSE)])
svg("./GRAFICOS/Bikeway_Localidad_2017.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data=aux,aes(x=LocNombre,y=Km))+xlab("Locality (District)")+
    geom_bar(stat="identity", fill = "#4e78a6")+ylab("Bikeway (Km)")+ylim(c(0,87))+
    geom_text(aes(label=paste(round(Km,2),"Km")),size=3,hjust=-0.05,family="Helvetica Light")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    coord_flip()
    )
showtext_end()
dev.off()

#Kilometros de infraestructura por localidad estandarizado por población en diciembre de 2017----
aux<-aggregate(ciclTime$Mts,by=list(ciclTime$LocNombre,ciclTime$FECHA),FUN=sum) %>% select(Mts=x,LocNombre=Group.1,Fecha=Group.2) %>% filter(getYear(as.Date(Fecha))==2017) %>% mutate(Km=Mts/1000)
aux<-left_join(aux, aggregate(Poblacion$POBLACION,by=list(Poblacion$AÑO,Poblacion$LOCALIDAD),FUN=sum) %>% filter(Group.1==2017) %>% select(Poblacion=x,LocNombre=Group.2) , by="LocNombre") %>% mutate(EstanPop=Km*100000/Poblacion)
aux$LocNombre<-factor(aux$LocNombre, aux$LocNombre[order(aux$EstanPop,decreasing=FALSE)])
svg("./GRAFICOS/BikewayPerPopulation_Localidad_2017.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data=aux,aes(x=LocNombre,y=EstanPop))+xlab("Locality (District)")+
    geom_bar(stat="identity", fill = "#4e78a6")+ylab("Bikeway per 100.000 people (Km)")+ylim(c(0,27))+
    geom_text(aes(label=paste(round(EstanPop,2),"Km")),size=3,hjust=-0.05,family="Helvetica Light")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    coord_flip()
  )
showtext_end()
dev.off()

#Kilometros de infraestructura por localidad estandarizado por biciusuarios en diciembre de 2017----
aux<-aggregate(ciclTime$Mts,by=list(ciclTime$LocNombre,ciclTime$FECHA),FUN=sum) %>% select(Mts=x,LocNombre=Group.1,Fecha=Group.2) %>% filter(getYear(as.Date(Fecha))==2017) %>% mutate(Km=Mts/1000)
aux<-left_join(aux, aggregate(Poblacion$POBLACION,by=list(Poblacion$AÑO,Poblacion$LOCALIDAD),FUN=sum) %>% filter(Group.1==2017) %>% select(Poblacion=x,LocNombre=Group.2) , by="LocNombre") %>% mutate(EstanPop=Km*100000/Poblacion)
aux<-left_join(aux, aggregate(biciusuarios$BICIUSRSINT,by=list(biciusuarios$FECHA,biciusuarios$LOCALIDAD),FUN=sum) %>% filter(getYear(as.Date(Group.1))==2017) %>% select(bici=x,LocNombre=Group.2) , by="LocNombre") %>% mutate(EstanBici=Km*1000/bici)
aux$LocNombre<-factor(aux$LocNombre, aux$LocNombre[order(aux$EstanBici,decreasing=FALSE)])
svg("./GRAFICOS/BikewayPerCyclists_Localidad_2017.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data=aux,aes(x=LocNombre,y=EstanBici))+xlab("Locality (District)")+
    geom_bar(stat="identity", fill = "#4e78a6")+ylim(c(0,7))+ylab("Bikeway per 1.000 Cyclists (Km)")+
    geom_text(aes(label=paste(round(EstanBici,2),"Km")),size=3,hjust=-0.05,family="Helvetica Light")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    coord_flip()
  )
showtext_end()
dev.off()

########### BICIUSUARIOS #############################################################################
#Gráfica de tendencias de biciusuarios por género y grupo de edad----
aux<-aggregate(biciusuarios[,c("BICIUSRS")],by=list(biciusuarios$FECHA,biciusuarios$GR_EDAD,biciusuarios$SEXO),FUN=sum) %>% na.omit()
names(aux)<-c("FECHA","GR_EDAD","SEXO","BICIUSRS")
svg("./GRAFICOS/Cyclists_Age_Gender_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=FECHA,y=BICIUSRS,colour=SEXO))+xlab("Year")+scale_x_date(breaks = as.Date(c("2005-01-01","2011-01-01","2014-01-01","2015-01-01","2017-01-01")),date_labels="%Y")+
    geom_point()+geom_line(linetype="dashed")+ylab("Cyclists")+
    scale_colour_manual(values = c("Male"="lightsteelblue3", "Female"="lightpink2"))+labs(colour="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.position = c(0.9, 0.125),legend.text=element_text(colour="black",size=10,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"),axis.text.x = element_text(angle = 90,vjust=0.5))+
    facet_wrap(~GR_EDAD)
)
showtext_end()
dev.off()

#Gráfica de tendencias de biciusuarios por género y grupo de edad, como porcentaje de la población----
aux<-aggregate(biciusuarios[,c("BICIUSRS")],by=list(biciusuarios$FECHA,biciusuarios$GR_EDAD,biciusuarios$SEXO),FUN=sum) %>% na.omit()
names(aux)<-c("FECHA","GR_EDAD","SEXO","BICIUSRS")
aux<-left_join(aux,aggregate(Poblacion$POBLACION,by=list(Poblacion$AÑO,Poblacion$GR_EDAD,Poblacion$SEXO),FUN=sum) %>% select(Poblacion=x,SEXO=Group.3,GR_EDAD=Group.2,FECHA=Group.1) %>% mutate(FECHA=as.Date(paste0(FECHA,"-01-01"))),by=c("GR_EDAD","SEXO","FECHA")) %>% mutate(BICIPERC=BICIUSRS/Poblacion)
svg("./GRAFICOS/CyclistsPerPopulation_Age_Gender_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=FECHA,y=BICIPERC,colour=SEXO))+xlab("Year")+scale_x_date(breaks = as.Date(c("2005-01-01","2011-01-01","2014-01-01","2015-01-01","2017-01-01")),date_labels="%Y")+
    geom_point()+geom_line(linetype="dashed")+ylab("Cyclists as proportion of population")+scale_y_continuous(labels = scales::percent,limits=c(0,0.15))+
    scale_colour_manual(values = c("Male"="lightsteelblue3", "Female"="lightpink2"))+labs(colour="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.position = c(0.9, 0.125),legend.text=element_text(colour="black",size=10,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"),axis.text.x = element_text(angle = 90,vjust=0.5))+
    facet_wrap(~GR_EDAD)
)
showtext_end()
dev.off()

#Gráfica de tendencia de biciusuarios por localidad----
aux<-aggregate(biciusuarios$BICIUSRS,by=list(getYear(biciusuarios$FECHA),biciusuarios$LOCALIDAD),FUN=sum,na.rm=TRUE) %>% mutate(x=replace(x,x==0,NA)) %>% na.omit %>% mutate(Group.1=as.Date(paste0(Group.1,"-01-01")))  %>% filter(!Group.2%in%c("SUMAPAZ","OTRA LOCALIDAD RURAL"))
svg("./GRAFICOS/Cyclists_Localidad_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=Group.1,y=x))+xlab("Year")+scale_x_date(breaks = as.Date(c("2005-01-01","2011-01-01","2014-01-01","2015-01-01","2017-01-01")),date_labels="%Y")+
    geom_point(color="#4e78a6")+geom_line(color="#4e78a6",linetype="dashed")+ylab("Cyclists")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.text=element_text(colour="black",size=10,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"),axis.text.x = element_text(angle = 90,vjust=0.5))+
    facet_wrap(~Group.2)
)
showtext_end()
dev.off()

#Gráfica de tendencia de biciusuarios por localidad, como porcentaje de la población----
aux<-aggregate(biciusuarios$BICIUSRS,by=list(getYear(biciusuarios$FECHA),biciusuarios$LOCALIDAD),FUN=sum,na.rm=TRUE) %>% mutate(x=replace(x,x==0,NA)) %>% na.omit %>% mutate(Group.1=as.Date(paste0(Group.1,"-01-01")))  %>% filter(!Group.2%in%c("SUMAPAZ","OTRA LOCALIDAD RURAL"))
aux<-left_join(aux,aggregate(Poblacion$POBLACION,by=list(Poblacion$AÑO,Poblacion$LOCALIDAD),FUN=sum) %>% mutate(Group.1=as.Date(paste0(Group.1,"-01-01"))),by=c("Group.1","Group.2")) %>% mutate(x=x.x/x.y)
svg("./GRAFICOS/CyclistsPerPopulation_Localidad_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=Group.1,y=x))+xlab("Year")+scale_x_date(breaks = as.Date(c("2005-01-01","2011-01-01","2014-01-01","2015-01-01","2017-01-01")),date_labels="%Y")+
    geom_point(color="#4e78a6")+geom_line(color="#4e78a6",linetype="dashed")+ylab("Cyclists as proportion of population")+scale_y_continuous(labels = scales::percent,limits=c(0,0.07))+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.text=element_text(colour="black",size=10,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"),axis.text.x = element_text(angle = 90,vjust=0.5))+
    facet_wrap(~Group.2)
)
showtext_end()
dev.off()

#Gráfica de tendencias de biciusuarios----
aux<-aggregate(biciusuarios[,c("BICIUSRS")],by=list(biciusuarios$FECHA),FUN=sum) %>% na.omit()
svg("./GRAFICOS/Cyclists_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=Group.1,y=BICIUSRS))+xlab("Year")+scale_x_date(date_labels="%Y",limits = as.Date(c("2004-01-01","2018-02-01")))+
    geom_point(color="#4e78a6",size = 3)+ylim(80000,330000)+ylab("Cyclists")+
    geom_line(color="#4e78a6",linetype="dashed")+
    geom_label(label.size = NA,aes(label=paste(getYear(Group.1),round(BICIUSRS),sep=" - ")),fill="white",size=3.5,hjust=0.8,vjust=-0.5,family="Helvetica Light",color="gray0")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))
)
showtext_end()
dev.off()

#Gráfica de tendencias de biciusuarios, como porcentaje de la población----
aux<-aggregate(biciusuarios[,c("BICIUSRS")],by=list(biciusuarios$FECHA),FUN=sum) %>% na.omit()
aux<-left_join(aux,aggregate(Poblacion$POBLACION,by=list(Poblacion$AÑO),FUN=sum) %>% mutate(Group.1=as.Date(paste0(Group.1,"-01-01"))) %>% rename(POBLACION=x),by=c("Group.1")) %>% mutate(x=BICIUSRS/POBLACION)
svg("./GRAFICOS/CyclistsPerPopulation_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=Group.1,y=x))+xlab("Year")+scale_x_date(date_labels="%Y",limits = as.Date(c("2004-01-01","2018-02-01")))+
    geom_point(color="#4e78a6",size = 3)+ylab("Cyclists as proportion of population")+scale_y_continuous(labels = scales::percent,limits=c(0,0.044))+
    geom_line(color="#4e78a6",linetype="dashed")+
    geom_label(label.size = NA,aes(label=paste0(getYear(Group.1)," - ",round(x,4)*100," %")),fill="white",size=3.5,hjust=0.5,vjust=-0.5,family="Helvetica Light",color="gray0")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))
)
showtext_end()
dev.off()

########### VIAJES Y KILÓMETROS RECORRIDOS ###########################################################
#Kilómetros recorridos totales por año y validación----
aux<-aggregate(viajes[,c("DIST_TOTAL_KM","DIST_TOTAL_KM_EST","DIST_TOTAL_KMINT")],by=list(viajes$AÑO),FUN=sum,na.rm=TRUE) %>% na_if(0) %>% mutate(Fecha=as.Date(paste0(Group.1,"-01-01"))) %>% select(Fecha,DIST_TOTAL_KM_EST,DIST_TOTAL_KM,DIST_TOTAL_KMINT)
svg("./GRAFICOS/TraveledKM_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=Fecha))+scale_x_date(date_labels="%Y",limits=as.Date(c("2004-09-01","2017-04-01")))+xlab("Year")+
    geom_point(aes(y=DIST_TOTAL_KM_EST/1e+06,colour="Estimated"),size = 3)+ylab("Kilometers cycled per day (Million Km)")+
    geom_line(aes(y=DIST_TOTAL_KMINT/1e+06,colour="Estimated"),linetype="dashed")+
    geom_label(label.size = NA,aes(y=DIST_TOTAL_KM_EST/1e+06,colour="Estimated",label=round(DIST_TOTAL_KM_EST,1)),fill="white",size=3.5,hjust=0.5,vjust=-0.5,family="Helvetica Light",show.legend = FALSE)+
    geom_point(aes(y=DIST_TOTAL_KM/1e+06,colour="Actual"),size = 3)+
    geom_label(label.size = NA,aes(y=DIST_TOTAL_KM/1e+06,colour="Actual",label=round(DIST_TOTAL_KM,1)),fill="white",size=3.5,hjust=0.5,vjust=1.5,family="Helvetica Light",show.legend = FALSE)+
    scale_colour_manual(breaks=c("Estimated","Actual"),values = c("darkorange3","#4e78a6"))+labs(colour="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.text=element_text(colour="black",size=10,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"))
)
showtext_end()
dev.off()

#Kilómetros recorridos y viajes por año----
aux<-aggregate(viajes[,c("VIAJES","VIAJESINT","DIST_TOTAL_KM_EST","DIST_TOTAL_KMINT")],by=list(viajes$AÑO),FUN=sum) %>%  mutate(Fecha=as.Date(paste0(Group.1,"-01-01")))
svg("./GRAFICOS/TraveledKM_Trips_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=Fecha))+scale_x_date(date_labels="%Y",limits=as.Date(c("2004-09-01","2017-04-01")))+xlab("Year")+
    geom_line(aes(y=DIST_TOTAL_KMINT/1e+6,colour="Kilometers"),show.legend = TRUE)+ylab("Kilometers cycled per day (Million Km)")+
    geom_line(aes(y=VIAJESINT/150000,colour="Trips"),show.legend = TRUE)+scale_y_continuous(limits=c(1,5.5),sec.axis = sec_axis(~.*1.5, name = "Trips (x100.000)"))+
    scale_colour_manual("", breaks = c("Kilometers", "Trips"),values = c("#4e78a6", "darkorange3"))+labs(colour="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.text=element_text(colour="black",size=10,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"),axis.text.x = element_text(angle = 90,vjust=0.5))
)
showtext_end()
dev.off()

#Kilómetros recorridos y biciusuarios por año----
aux<-aggregate(viajes[,c("VIAJESINT","DIST_TOTAL_KMINT")],by=list(viajes$AÑO),FUN=sum) %>%  mutate(Fecha=as.Date(paste0(Group.1,"-01-01")))
aux<-left_join(aux, aggregate(biciusuarios$BICIUSRSINT,by=list(biciusuarios$FECHA),FUN=sum) %>% dplyr::select(bici=x,Fecha=Group.1), by=c("Fecha"))
svg("./GRAFICOS/TraveledKM_Cyclists_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=Fecha))+scale_x_date(date_labels="%Y",limits=as.Date(c("2004-09-01","2017-04-01")))+xlab("Year")+
    geom_line(aes(y=DIST_TOTAL_KMINT/1e+6,colour="Kilometers"),show.legend = TRUE)+ylab("Kilometers cycled per day (Million Km)")+
    geom_line(aes(y=bici/60000,colour="Cyclists"),show.legend = TRUE)+scale_y_continuous(limits=c(1,5.5),sec.axis = sec_axis(~.*60, name = "Cyclists (x 1.000)"))+
    scale_colour_manual("", breaks = c("Kilometers", "Cyclists"),values = c("limegreen","#4e78a6"))+labs(colour="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.text=element_text(colour="black",size=10,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"),axis.text.x = element_text(angle = 90,vjust=0.5))
)
showtext_end()
dev.off()

#Viajes y biciusuarios por año----
aux<-aggregate(viajes[,c("VIAJESINT","DIST_TOTAL_KMINT")],by=list(viajes$AÑO),FUN=sum) %>%  mutate(Fecha=as.Date(paste0(Group.1,"-01-01"))) %>% na.omit
aux<-left_join(aux, aggregate(biciusuarios$BICIUSRSINT,by=list(biciusuarios$FECHA),FUN=sum) %>% select(bici=x,Fecha=Group.1), by=c("Fecha"))
svg("./GRAFICOS/Cyclists_Trips_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=Fecha))+scale_x_date(date_labels="%Y",limits=as.Date(c("2004-09-01","2017-04-01")))+xlab("Year")+
    geom_line(aes(y=bici/1000,colour="Cyclists"),show.legend = TRUE)+ylab("Cyclists (x 1.000)")+
    geom_line(aes(y=VIAJESINT*60/150000,colour="Trips"),show.legend = TRUE)+scale_y_continuous(limits=c(60,330),sec.axis = sec_axis(~./40, name = "Trips (x100.000)"))+
    scale_colour_manual("", breaks = c("Trips", "Cyclists"),values = c("limegreen","darkorange3"))+labs(colour="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.text=element_text(colour="black",size=10,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"),axis.text.x = element_text(angle = 90,vjust=0.5))
)
showtext_end()
dev.off()

#Kilómetros recorridos por viaje----
aux<-aggregate(viajes[,c("VIAJESINT","DIST_TOTAL_KMINT")],by=list(viajes$AÑO),FUN=sum) %>%  mutate(Fecha=as.Date(paste0(Group.1,"-01-01"))) %>% na.omit
aux<-left_join(aux, aggregate(biciusuarios$BICIUSRSINT,by=list(biciusuarios$FECHA),FUN=sum) %>% select(bici=x,Fecha=Group.1), by=c("Fecha"))
aux$DIST_PER_VIAJE<-aux$DIST_TOTAL_KM/aux$VIAJES
svg("./GRAFICOS/KMperTrip_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=Fecha))+scale_x_date(date_labels="%Y",limits=as.Date(c("2004-09-01","2017-04-01")))+xlab("Year")+
    geom_line(aes(y=DIST_PER_VIAJE),colour="#4e78a6",show.legend = TRUE)+ylim(4.7,7.5)+ylab("Kilometers cycled per trip (Km)")+
    geom_label(label.size = NA,aes(y=DIST_PER_VIAJE,label=round(DIST_PER_VIAJE,2)),fill="white",size=3.5,hjust=0.5,vjust=-0.5,family="Helvetica Light",color="gray0")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.text=element_text(colour="black",size=10,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"),axis.text.x = element_text(angle = 90,vjust=0.5))
)
showtext_end()
dev.off()

#Kilómetros recorridos por biciusuario----
aux<-aggregate(viajes[,c("VIAJESINT","DIST_TOTAL_KMINT")],by=list(viajes$AÑO),FUN=sum) %>%  mutate(Fecha=as.Date(paste0(Group.1,"-01-01"))) %>% na.omit
aux<-left_join(aux, aggregate(biciusuarios$BICIUSRSINT,by=list(biciusuarios$FECHA),FUN=sum) %>% dplyr::select(bici=x,Fecha=Group.1), by=c("Fecha"))
aux$DIST_PER_CYC<-aux$DIST_TOTAL_KM/aux$bici
write_rds(aux, paste(carpetaRAS, "/RESULTADOS/SEGURIDAD/Bases de datos/11. kilometrosPorCiclista.rds"))
svg("./GRAFICOS/KMperCyclist_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=Fecha))+scale_x_date(date_labels="%Y",limits=as.Date(c("2004-09-01","2017-04-01")))+xlab("Year")+
    geom_line(aes(y=DIST_PER_CYC),colour="#4e78a6",show.legend = TRUE)+ylim(10,18)+ylab("Kilometers cycled per cyclist (Km)")+
    geom_label(label.size = NA,aes(y=DIST_PER_CYC,label=round(DIST_PER_CYC,2)),fill="white",size=3.5,hjust=0.5,vjust=-0.5,family="Helvetica Light",color="gray0")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.text=element_text(colour="black",size=10,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"),axis.text.x = element_text(angle = 90,vjust=0.5))
)
showtext_end()
dev.off()

#Viajes por biciusuario----
aux<-aggregate(viajes[,c("VIAJESINT","DIST_TOTAL_KMINT")],by=list(viajes$AÑO),FUN=sum) %>%  mutate(Fecha=as.Date(paste0(Group.1,"-01-01"))) %>% na.omit
aux<-left_join(aux, aggregate(biciusuarios$BICIUSRSINT,by=list(biciusuarios$FECHA),FUN=sum) %>% select(bici=x,Fecha=Group.1), by=c("Fecha"))
aux$TRIPS_PER_CYC<-aux$VIAJES/aux$bici
svg("./GRAFICOS/TripsperCyclist_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=Fecha))+scale_x_date(date_labels="%Y",limits=as.Date(c("2004-09-01","2017-04-01")))+xlab("Year")+
    geom_line(aes(y=TRIPS_PER_CYC),colour="#4e78a6",show.legend = TRUE)+ylim(1.9,2.4)+ylab("Trips per cyclist")+
    geom_label(label.size = NA,aes(y=TRIPS_PER_CYC,label=round(TRIPS_PER_CYC,2)),fill="white",size=3.5,hjust=0.5,vjust=-0.5,family="Helvetica Light",color="gray0")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.text=element_text(colour="black",size=10,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"),axis.text.x = element_text(angle = 90,vjust=0.5))
)
showtext_end()
dev.off()

########### TASAS DE MORTALIDAD ######################################################################
#Crude death rate de los ciclistas y la población de Bogotá----
aux<-WorkTable_bogotaAg_prop[,c("FECHA","CDRbic","CDRbog")] %>% mutate(FECHA=as.Date(FECHA))
svg("./GRAFICOS/CDR_BogotaCyclists_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=FECHA))+scale_x_date(date_labels="%Y",limits=as.Date(c("2010-11-01","2017-03-01")))+xlab("Year")+
    geom_line(aes(y=CDRbic,colour="Cyclists"))+ylab("Crude death rate per 100.000 people")+
    geom_line(aes(y=CDRbog,colour="Bogotá"))+
    scale_colour_manual("",breaks=c("Cyclists","Bogotá"),values = c("lightsteelblue3", "gray40"))+labs(colour="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.text=element_text(colour="black",size=10,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"),axis.text.x = element_text(angle = 90,vjust=0.5))
)
showtext_end()
dev.off()

#Age-Adjusted death rate de los ciclistas y la población de Bogotá----
aux<-WorkTable_bogotaAg_prop[,c("FECHA","ADRbic","ADRbog")] %>% mutate(FECHA=as.Date(FECHA))
svg("./GRAFICOS/ADR_BogotaCyclists_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=FECHA))+scale_x_date(date_labels="%Y",limits=as.Date(c("2010-11-01","2017-03-01")))+xlab("Year")+
    geom_line(aes(y=ADRbic,colour="Cyclists"))+ylab("Age-adjusted death rate per 100.000 people")+
    geom_line(aes(y=ADRbog,colour="Bogotá"))+
    scale_colour_manual("",breaks=c("Cyclists","Bogotá"),values = c("lightsteelblue3", "gray40"))+labs(colour="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.text=element_text(colour="black",size=10,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"),axis.text.x = element_text(angle = 90,vjust=0.5))
)
showtext_end()
dev.off()

#SMR de los ciclistas contra la población de Bogotá----
aux<-WorkTable_bogotaAg_of[,c("FECHA","SMRbic_bog")] %>% mutate(FECHA=as.Date(paste0(FECHA,"-12-31")))
summary(lm(formula = SMRbic_bog~FECHA, data =aux))
svg("./GRAFICOS/SMR_BogotaCyclists_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=FECHA))+scale_x_date(date_labels="%Y",limits=as.Date(c("2010-11-01","2017-03-01")))+xlab("Year")+
    geom_line(aes(y=SMRbic_bog),color="lightsteelblue3")+ylab("Standardized mortality ratio")+
    stat_smooth(aes(y = SMRbic_bog),method="lm",formula = y~x,linetype="dashed",size=0.5,alpha=0.2,fill="navy",show.legend = FALSE,se = FALSE)+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.text=element_text(colour="black",size=10,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"),axis.text.x = element_text(angle = 90,vjust=0.5))
)
showtext_end()
dev.off()

#IADR de los ciclistas contra la población de Bogotá----
aux<-WorkTable_bogotaAg_of[,c("FECHA","IADRbic_bog")] %>% mutate(FECHA=as.Date(FECHA))
svg("./GRAFICOS/IADR_BogotaCyclists_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=FECHA))+scale_x_date(date_labels="%Y",limits=as.Date(c("2010-11-01","2017-03-01")))+xlab("Year")+
    geom_line(aes(y=IADRbic_bog),color="lightsteelblue3")+ylab("Indirect age-adjusted death rate per 100.000 people")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.text=element_text(colour="black",size=10,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"),axis.text.x = element_text(angle = 90,vjust=0.5))
)
showtext_end()
dev.off()

#Crude death rate de los clistas y referencia de Bogotá por género----
aux<-WorkTable_bogota_of[,c("FECHA","SEXO","CDRbic","CDRbog")] %>% mutate(FECHA=as.Date(FECHA))
svg("./GRAFICOS/CDR_GenderBogota_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=FECHA))+scale_x_date(date_labels="%Y",limits=as.Date(c("2010-11-01","2017-03-01")))+xlab("Year")+
    geom_line(aes(y=CDRbic,colour=SEXO,group=SEXO))+ylab("Crude death rate per 100.000 people")+
    geom_line(aes(y=CDRbog,colour=SEXO,group=SEXO),alpha=0.6)+
    scale_colour_manual(values = c("Male"="lightsteelblue3","Female"="lightpink2"))+labs(colour="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.text=element_text(colour="black",size=10,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"),axis.text.x = element_text(angle = 90,vjust=0.5))
)
showtext_end()
dev.off()

#Age-Adjusted death rate de los ciclistas hombres y referencia de Bogotá----
confidence<-0.05
aux<-WorkTable_bogota_of[,c("FECHA","SEXO","ADRbic","VarBic","ADRbog")] %>% mutate(FECHA=as.Date(FECHA))
aux$lowerBic<-pmax(aux$ADRbic-qnorm(1-confidence/2)*sqrt(aux$VarBic),0)
aux$upperBic<-pmin(aux$ADRbic+qnorm(1-confidence/2)*sqrt(aux$VarBic),80)
svg("./GRAFICOS/ADR_MaleBogotá_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux[aux$SEXO=="Male",],aes(x=FECHA))+scale_x_date(date_labels="%Y",limits=as.Date(c("2010-11-01","2017-03-01")))+xlab("Year")+
    geom_line(aes(y=ADRbic,colour="Male cyclists"))+ylim(0,80)+geom_ribbon(aes(ymin=lowerBic,ymax=upperBic),fill="lightsteelblue3",linetype=0,alpha=0.2,show.legend = FALSE)+ylab("Age-adjusted death rate per 100.000 people")+
    geom_line(aes(y=ADRbog,colour="Male Bogota"))+
    scale_colour_manual("",breaks=c("Male Bogota","Male cyclists"),values = c("gray40","lightsteelblue3"))+labs(colour="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.text=element_text(colour="black",size=10,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"),axis.text.x = element_text(angle = 90,vjust=0.5))
)
showtext_end()
dev.off()

#Age-Adjusted death rate de los ciclistas mujeres y referencia de Bogotá----
confidence<-0.05
aux<-WorkTable_bogota_of[,c("FECHA","SEXO","ADRbic","VarBic","ADRbog")] %>% mutate(FECHA=as.Date(FECHA))
aux$lowerBic<-pmax(aux$ADRbic-qnorm(1-confidence/2)*sqrt(aux$VarBic),0)
aux$upperBic<-pmin(aux$ADRbic+qnorm(1-confidence/2)*sqrt(aux$VarBic),80)
svg("./GRAFICOS/ADR_FemaleBogotá_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux[aux$SEXO=="Female",],aes(x=FECHA))+scale_x_date(date_labels="%Y",limits=as.Date(c("2010-11-01","2017-03-01")))+xlab("Year")+
    geom_line(aes(y=ADRbic,colour="Female cyclists"))+ylim(0,80)+geom_ribbon(aes(ymin=lowerBic,ymax=upperBic),fill="lightpink2",linetype=0,alpha=0.2,show.legend = FALSE)+ylab("Age-adjusted death rate per 100.000 people")+
    geom_line(aes(y=ADRbog,colour="Female Bogota"))+
    scale_colour_manual("",breaks=c("Female Bogota","Female cyclists"),values = c("gray40","lightpink2"))+labs(colour="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.text=element_text(colour="black",size=10,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"),axis.text.x = element_text(angle = 90,vjust=0.5))
)
showtext_end()
dev.off()

#Age-Adjusted death rate por sexo----
confidence<-0.05
aux<-WorkTable_bogota_of[,c("FECHA","SEXO","ADRbic","VarBic","ADRbog")] %>% mutate(FECHA=as.Date(FECHA))
aux$lowerBic<-pmax(aux$ADRbic-qnorm(1-confidence/2)*sqrt(aux$VarBic),0)
aux$upperBic<-pmin(aux$ADRbic+qnorm(1-confidence/2)*sqrt(aux$VarBic),80)
svg("./GRAFICOS/ADR_gender_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=FECHA,color=SEXO,group=SEXO))+scale_x_date(date_labels="%Y",limits=as.Date(c("2010-11-01","2017-03-01")))+xlab("Year")+
    geom_line(aes(y=ADRbic))+geom_ribbon(aes(ymin=lowerBic,ymax=upperBic,colour=factor(SEXO),fill=SEXO),linetype=0,alpha=0.2,show.legend = FALSE)+ylab("Age-adjusted death rate per 100.000 people")+
    scale_colour_manual(values = c("Male"="lightsteelblue3", "Female"="lightpink2"))+labs(colour="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.text=element_text(colour="black",size=10,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"),axis.text.x = element_text(angle = 90,vjust=0.5))
)
showtext_end()
dev.off()

#SMR de los ciclistas contra la población de Bogotá por género----
aux<-WorkTable_bogota_of[,c("FECHA","SEXO","SMRbic_bog")] %>% mutate(FECHA=as.Date(paste0(FECHA, "-12-31")))
summary(lm(formula = SMRbic_bog~FECHA, data =aux %>% filter(SEXO=="Male")))
summary(lm(formula = SMRbic_bog~FECHA, data =aux %>% filter(SEXO=="Female")))
svg("./GRAFICOS/SMR_Cyclists_Gender_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=FECHA,group=SEXO))+scale_x_date(date_labels="%Y",limits=as.Date(c("2010-11-01","2017-03-01")))+xlab("Year")+
    geom_line(aes(y=SMRbic_bog,colour=SEXO))+ylab("Standardized mortality ratio")+
    scale_colour_manual(values = c("Male"="lightsteelblue3", "Female"="lightpink2"))+labs(colour="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.text=element_text(colour="black",size=10,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"),axis.text.x = element_text(angle = 90,vjust=0.5))
)
showtext_end()
dev.off()

#IADR de los ciclistas contra la población de Bogotá por género----
aux<-WorkTable_bogota_of[,c("FECHA","SEXO","IADRbic_bog")] %>% mutate(FECHA=as.Date(FECHA))
svg("./GRAFICOS/IADR_Cyclists_Gender_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=FECHA,group=SEXO))+scale_x_date(date_labels="%Y",limits=as.Date(c("2010-11-01","2017-03-01")))+xlab("Year")+
    geom_line(aes(y=IADRbic_bog,colour=SEXO))+ylab("Indirect age-adjusted death rate per 100.000 people")+
    scale_colour_manual(values = c("Male"="lightsteelblue3", "Female"="lightpink2"))+labs(colour="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.text=element_text(colour="black",size=10,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"),axis.text.x = element_text(angle = 90,vjust=0.5))
)
showtext_end()
dev.off()

#Crude death rate de los clistas por localidad----
aux<-WorkTable_localidad_of[,c("FECHA","SEXO","CDRbic","LOCALIDAD")] %>% mutate(FECHA=as.Date(FECHA)) %>% filter(LOCALIDAD!="SUMAPAZ")
svg("./GRAFICOS/CDR_Gender_Localidad_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=FECHA))+scale_x_date(date_labels="%Y",limits=as.Date(c("2011-01-01","2016-12-01")))+xlab("Year")+
    geom_line(aes(y=CDRbic,colour=SEXO,group=SEXO))+ylab("Crude death rate per 100.000 people")+
    scale_colour_manual(values = c("Male"="lightsteelblue3","Female"="lightpink2"))+labs(colour="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(axis.text.y  = element_text(size=5),legend.text=element_text(colour="black",size=10,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"),strip.text.y = element_text(angle = 0))+
    facet_grid(LOCALIDAD~.,scales="free_y")
)
showtext_end()
dev.off()

#Age-adjusted death rate de los clistas por localidad----
aux<-WorkTable_localidad_of[,c("FECHA","SEXO","ADRbic","LOCALIDAD")] %>% mutate(FECHA=as.Date(FECHA)) %>% filter(LOCALIDAD!="SUMAPAZ")
svg("./GRAFICOS/ADR_Gender_Localidad_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=FECHA))+scale_x_date(date_labels="%Y",limits=as.Date(c("2011-01-01","2016-12-01")))+xlab("Year")+
    geom_line(aes(y=ADRbic,colour=SEXO,group=SEXO))+ylab("Age-adjusted death rate per 100.000 people")+
    scale_colour_manual(values = c("Male"="lightsteelblue3","Female"="lightpink2"))+labs(colour="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(axis.text.y  = element_text(size=5),legend.text=element_text(colour="black",size=10,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"),strip.text.y = element_text(angle = 0))+
    facet_grid(LOCALIDAD~.,scales="free_y")
)
showtext_end()
dev.off()

#SMR de los clistas por localidad----
aux<-WorkTable_localidad_of[,c("FECHA","SEXO","SMRbic_bog","LOCALIDAD")] %>% mutate(FECHA=as.Date(FECHA)) %>% filter(LOCALIDAD!="SUMAPAZ")
svg("./GRAFICOS/SMR_Gender_Localidad_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=FECHA))+scale_x_date(date_labels="%Y",limits=as.Date(c("2011-01-01","2016-12-01")))+xlab("Year")+
    geom_line(aes(y=SMRbic_bog,colour=SEXO,group=SEXO))+ylab("Standardized mortality ratio")+
    scale_colour_manual(values = c("Male"="lightsteelblue3","Female"="lightpink2"))+labs(colour="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(axis.text.y  = element_text(size=5),legend.text=element_text(colour="black",size=10,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"),strip.text.y = element_text(angle = 0))+
    facet_grid(LOCALIDAD~.,scales="free_y")
)
showtext_end()
dev.off()

#IADR de los clistas por localidad----
aux<-WorkTable_localidad_of[,c("FECHA","SEXO","IADRbic_bog","LOCALIDAD")] %>% mutate(FECHA=as.Date(FECHA)) %>% filter(LOCALIDAD!="SUMAPAZ")
svg("./GRAFICOS/IADR_Gender_Localidad_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=FECHA))+scale_x_date(date_labels="%Y",limits=as.Date(c("2011-01-01","2016-12-01")))+xlab("Year")+
    geom_line(aes(y=IADRbic_bog,colour=SEXO,group=SEXO))+ylab("Indirect age-adjusted death rate per 100.000 people")+
    scale_colour_manual(values = c("Male"="lightsteelblue3","Female"="lightpink2"))+labs(colour="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(axis.text.y  = element_text(size=5),legend.text=element_text(colour="black",size=10,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"),strip.text.y = element_text(angle = 0))+
    facet_grid(LOCALIDAD~.,scales="free_y")
)
showtext_end()
dev.off()

########### TENDENCIAS ACCIDENTES ####################################################################
#Accidentes y biciusuarios por año----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(getYear(AccidentesBiciTotal$Accidentes.Fecha)),FUN=length) %>%  mutate(Fecha=as.Date(paste0(Group.1,"-01-01")))
aux<-left_join(aux, aggregate(biciusuarios$BICIUSRSINT,by=list(biciusuarios$FECHA),FUN=sum) %>% select(bici=x,Fecha=Group.1), by=c("Fecha"))
svg("./GRAFICOS/Accidents_Cyclists_Time.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=Fecha))+scale_x_date(date_labels="%Y",limits=as.Date(c("2010-11-01","2017-03-01")))+xlab("Year")+
    geom_line(aes(y=x,colour="Casualties"),show.legend = TRUE)+ylab("Cyclists' casualties")+
    geom_label(label.size = NA,aes(y=x,label=x),fill="white",size=3.5,hjust=0.5,vjust=-0.5,family="Helvetica Light",color="gray0")+
    geom_line(aes(y=bici/231,colour="Cyclists"),show.legend = TRUE)+scale_y_continuous(limits=c(400,2000),sec.axis = sec_axis(~.*(0.231), name = "Cyclists (x 1.000)"))+
    geom_label(label.size = NA,aes(y=bici/231,label=round(bici)),fill="white",size=3.5,hjust=0.5,vjust=1.5,family="Helvetica Light",color="gray0")+
    scale_colour_manual("", breaks = c("Casualties", "Cyclists"),values = c("#4e78a6","limegreen"))+labs(colour="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey40"))+
    theme(legend.text=element_text(colour="black",size=10,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"),axis.text.x = element_text(angle = 0,vjust=0.5))
)
showtext_end()
dev.off()

#Total de accidentes y mortalidad durante los años----
aux <- aggregate(AccidentesBiciTotal$Accidente,by=list(getYear(AccidentesBiciTotal$Accidentes.Fecha),AccidentesBiciTotal$Gravedad2),FUN=length) %>% mutate(Group.1=as.Date(paste0(Group.1,"-01-01")))
aux<-reshape(aux,idvar = "Group.1",timevar = "Group.2",direction= "wide")
svg("./GRAFICOS/Accidents_deaths_TimeYear.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data = aux, aes(x=Group.1)) + xlab("Year")+ scale_x_date(date_labels = "%Y")+
    geom_line(aes(y = x.Dead,colour="Dead")) + ylab("Cyclists’ casualties")+
    geom_text(aes(y = x.Dead,label=x.Dead),size=3,vjust=-2,family="Helvetica Light")+ylim(0,2000)+
    stat_smooth(aes(y = x.Dead,colour="Dead"),method="lm",linetype="dashed",size=0.5,alpha=0.2,fill="orangered3",show.legend = FALSE)+
    geom_line(aes(y = `x.Not Dead`,colour="Not Dead")) + ylab("Cyclists’ casualties")+
    geom_text(aes(y = `x.Not Dead`,label=`x.Not Dead`),size=3,vjust=-2,family="Helvetica Light")+
    stat_smooth(aes(y = `x.Not Dead`,colour="Not Dead"),method="lm",formula = y~x,linetype="dashed",size=0.5,alpha=0.2,fill="navy",show.legend = FALSE)+
    scale_colour_manual(breaks=c("Dead","Not Dead"),values = c("orangered3","navy"),labels=c("Fatal","Nonfatal"))+labs(colour="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey40"))
)
showtext_end()
dev.off()

#Total de accidentes y mortalidad durante los años estandarizado por biciusuarios----
aux <- aggregate(AccidentesBiciTotal$Accidente,by=list(getYear(AccidentesBiciTotal$Accidentes.Fecha),AccidentesBiciTotal$Gravedad2),FUN=length)
aux <- left_join(aux,aggregate(biciusuarios$BICIUSRSINT,by=list(getYear(biciusuarios$FECHA)),FUN=sum) %>% rename(BICIUSRS=x),by="Group.1") %>% mutate(Group.1=as.Date(paste0(Group.1,"-01-01")),EstanBici=x*1000/BICIUSRS)
aux<-reshape(aux,idvar = "Group.1",timevar = "Group.2",direction= "wide")
aux$`EstanBici.Not Dead`[is.na(aux$`x.Not Dead`)]<-0
aux$EstanBici.Dead[is.na(aux$x.Dead)]<-0

summary(lm(formula = `EstanBici.Not Dead`~Group.1, data=aux))
summary(lm(formula = EstanBici.Dead~Group.1, data=aux))

aux <- aux %>% 
  mutate(y_lab_1 = ifelse(Group.1=="2014-01-01", 5.7, NA),
         lab_1 = ifelse(Group.1=="2014-01-01", "p = 0.013", NA),
         y_lab_2 = ifelse(Group.1=="2014-01-01", 1.7, NA),
         lab_2 = ifelse(Group.1=="2014-01-01", "p = 0.513", NA))

graph11 <-  ggplot(data = aux, aes(x=Group.1)) + xlab("Year")+ scale_x_date(date_labels = "%Y")+
  geom_line(aes(y = EstanBici.Dead,colour="Dead",linetype="Tipo1"),show.legend = FALSE) + ylab("Bicyclists’ collisions")+
  geom_text(aes(y = EstanBici.Dead,label=round(EstanBici.Dead,1)),size=3,vjust=-2,family="Helvetica Light",show.legend = FALSE)+ylim(0,14)+
  stat_smooth(aes(y = EstanBici.Dead,colour="Dead"),method="lm",linetype="dashed",size=0.5,alpha=0.2,fill="orangered3",show.legend = FALSE)+
  geom_line(aes(y = `EstanBici.Not Dead`,colour="Not Dead",linetype="Tipo2"),show.legend = TRUE) + ylab("Collisions per 1,000 bicyclists")+
  geom_text(aes(y = `EstanBici.Not Dead`,label=round(`EstanBici.Not Dead`,1)),size=3,vjust=-2,family="Helvetica Light",show.legend = FALSE)+
  stat_smooth(aes(y = `EstanBici.Not Dead`,colour="Not Dead"),method="lm",formula = y~x,linetype="dashed",size=0.5,alpha=0.2,fill="navy",show.legend = FALSE) +
  geom_text(aes(y = y_lab_1, label = lab_1),size=3.5,vjust=-2,family="Helvetica Light",show.legend = FALSE,color="black") +
  geom_text(aes(y = y_lab_2, label = lab_2),size=3.5,vjust=-2,family="Helvetica Light",show.legend = FALSE,color="black") +
  scale_colour_manual(breaks=c("Dead","Not Dead"),values = c("orangered3","navy"),labels=c("Fatal","Nonfatal"))+
  scale_linetype_manual(breaks=c("Tipo1","Tipo2"),values=c(1,3),labels=c("Fatal","Nonfatal"))+labs(colour="",linetype="")+
  theme_minimal()+
  theme(text=element_text(family="Helvetica Light",size=12,color="grey40")) 

pdf("./GRAFICOS/AccidentsDeathsPerCyclists_TimeYear.pdf",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  graph11
)
showtext_end()
dev.off()

#Total de accidentes y mortalidad durante los meses colapsado por años estandarizado por biciusuarios----
aux <- AccidentesBiciTotal %>% 
  filter(year(Accidentes.Fecha)<=2017,
         !is.na(Gravedad2)) %>% 
  st_set_geometry(NULL) %>%
  group_by(Accidentes.Fecha = floor_date(Accidentes.Fecha, unit = "month"),
           Gravedad2) %>% 
  summarise(x = n()) %>% 
  group_by(Gravedad2) %>% 
  pad(interval = "month") %>% 
  mutate(x = ifelse(is.na(x), 0, x),
         Group.1 = getYear(Accidentes.Fecha)) %>%
  left_join(aggregate(biciusuarios$BICIUSRSINT,by=list(getYear(biciusuarios$FECHA)),FUN=sum) %>% 
              rename(BICIUSRS=x), by="Group.1") %>% 
  mutate(Group.1=as.Date(paste0(Group.1,"-01-01")),EstanBici=x*1000/BICIUSRS) %>% {
    data <- .
    
    capture.output({
      suppressWarnings({
        print("Biciusurios - total - Dead")
        data %>% 
          filter(year(Accidentes.Fecha) %in% c(2011, 2017),
                 Gravedad2 == "Dead") %>% 
          t.test(EstanBici~Group.1, data = .) %>% 
          print()
        print("Biciusurios - total - Not Dead")
        data %>% 
          filter(year(Accidentes.Fecha) %in% c(2011, 2017),
                 Gravedad2 == "Not Dead") %>% 
          t.test(EstanBici~Group.1, data = .) %>% 
          print()
      })
    }, file = "./GRAFICOS/estadisticas.txt", append = T)
    
    data
  } %>% 
  group_by(Group.1 = floor_date(Accidentes.Fecha, unit = "year"),
           Group.2 = Gravedad2) %>% 
  summarise(n = n(),
            sd_EstanBici = sd(EstanBici),
            EstanBici = mean(EstanBici)) %>%
  mutate(lwr = EstanBici - qt(0.975, df = n - 1) * sd_EstanBici / sqrt(n),
         upr = EstanBici + qt(0.975, df = n - 1) * sd_EstanBici / sqrt(n),
         n = NULL,
         sd_EstanBici = NULL) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  reshape(idvar = "Group.1", timevar = "Group.2", direction= "wide")

graph11_2 <- ggplot(data = aux, aes(x=Group.1)) + xlab("Year") + scale_x_date(date_labels = "%Y")+
  geom_point(aes(y = EstanBici.Dead,colour="Dead"),show.legend = FALSE) +
  geom_errorbar(aes(ymin = lwr.Dead,ymax=upr.Dead,colour="Dead"),show.legend = FALSE) +
  geom_text(aes(y = upr.Dead,label=sprintf('%.3f',EstanBici.Dead)),size=3,vjust=-1,family="Helvetica Light",show.legend = FALSE) +
  geom_point(aes(y = `EstanBici.Not Dead`,colour="Not Dead"),show.legend = TRUE) + 
  geom_errorbar(aes(ymin = `lwr.Not Dead`,ymax=`upr.Not Dead`,colour="Not Dead"),show.legend = FALSE) + ylab("Collisions per 1,000 bicyclists")+
  geom_text(aes(y = `upr.Not Dead`,label=sprintf('%.3f',`EstanBici.Not Dead`)),size=3,vjust=-1,family="Helvetica Light",show.legend = FALSE)+
  scale_colour_manual(breaks=c("Dead","Not Dead"),values = c("orangered3","navy"),labels=c("Fatal","Nonfatal"))+
  scale_linetype_manual(breaks=c("Tipo1","Tipo2"),values=c(1,3),labels=c("Fatal","Nonfatal"))+labs(colour="",linetype="")+
  scale_y_continuous(expand = expand_scale(mult = 0.2)) +
  theme_minimal()+
  theme(text=element_text(family="Helvetica Light",size=12,color="grey40")) 

#Total de accidentes y mortalidad durante los años estandarizado por kilómetros recorridos----
aux <- aggregate(AccidentesBiciTotal$Accidente,by=list(getYear(AccidentesBiciTotal$Accidentes.Fecha),AccidentesBiciTotal$Gravedad2),FUN=length)
aux <- left_join(aux,aggregate(viajes$DIST_TOTAL_KMINT,by=list(viajes$AÑO),FUN=sum) %>% rename(KM=x),by="Group.1") %>% mutate(Group.1=as.Date(paste0(Group.1,"-01-01")),EstanKm=x*100e6/(KM*365))
aux<-reshape(aux,idvar = "Group.1",timevar = "Group.2",direction= "wide")
aux$`EstanKm.Not Dead`[is.na(aux$`x.Not Dead`)]<-0
aux$EstanKm.Dead[is.na(aux$x.Dead)]<-0

summary(lm(formula = `EstanKm.Not Dead`~Group.1, data=aux))
summary(lm(formula = EstanKm.Dead~Group.1, data=aux))

aux <- aux %>% 
  mutate(y_lab_1 = ifelse(Group.1=="2014-01-01", 600, NA),
         lab_1 = ifelse(Group.1=="2014-01-01", "p = 0.002", NA),
         y_lab_2 = ifelse(Group.1=="2014-01-01", 200, NA),
         lab_2 = ifelse(Group.1=="2014-01-01", "p = 0.132", NA))

graph21 <- ggplot(data = aux, aes(x=Group.1)) + xlab("Year")+ scale_x_date(date_labels = "%Y")+
  geom_line(aes(y = EstanKm.Dead,colour="Dead",linetype="Tipo1"),show.legend = FALSE) + 
  geom_text(aes(y = EstanKm.Dead,label=round(EstanKm.Dead,2)),size=3,vjust=-2,family="Helvetica Light",show.legend = FALSE)+
  stat_smooth(aes(y = EstanKm.Dead,colour="Dead"),method="lm",linetype="dashed",size=0.5,alpha=0.2,fill="orangered3",show.legend = FALSE)+ylim(0, 1100) +
  geom_line(aes(y = `EstanKm.Not Dead`,colour="Not Dead",linetype="Tipo2"),show.legend = TRUE) + ylab("Collisions per 100-million VKmT")+
  geom_text(aes(y = `EstanKm.Not Dead`,label=round(`EstanKm.Not Dead`,1)),size=3,vjust=-2,family="Helvetica Light",show.legend = FALSE)  +
  geom_text(aes(y = y_lab_1, label = lab_1),size=3.5,vjust=-2,family="Helvetica Light",show.legend = FALSE,color="black") +
  geom_text(aes(y = y_lab_2, label = lab_2),size=3.5,vjust=-2,family="Helvetica Light",show.legend = FALSE,color="black") +
  stat_smooth(aes(y = `EstanKm.Not Dead`,colour="Not Dead"),method="lm",formula = y~x,linetype="dashed",size=0.5,alpha=0.2,fill="navy",show.legend = FALSE)+
  scale_colour_manual(breaks=c("Dead","Not Dead"),values = c("orangered3","navy"),labels=c("Fatal","Nonfatal"))+
  scale_linetype_manual(breaks=c("Tipo1","Tipo2"),values=c(1,3),labels=c("Fatal","Nonfatal"))+labs(colour="",linetype="")+
  theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey40"))

pdf("./GRAFICOS/AccidentsDeathsPerKm_TimeYear.pdf",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  graph21
)
showtext_end()
dev.off()

#Total de accidentes y mortalidad durante los meses colapsado por años estandarizado por kilometros recorridos ----
aux <- AccidentesBiciTotal %>% 
  filter(year(Accidentes.Fecha)<=2017,
         !is.na(Gravedad2)) %>% 
  st_set_geometry(NULL) %>%
  group_by(Accidentes.Fecha = floor_date(Accidentes.Fecha, unit = "month"),
           Gravedad2) %>% 
  summarise(x = n()) %>% 
  group_by(Gravedad2) %>% 
  pad(interval = "month") %>% 
  mutate(x = ifelse(is.na(x), 0, x),
         Group.1 = getYear(Accidentes.Fecha)) %>%
  left_join(aggregate(viajes$DIST_TOTAL_KMINT,by=list(viajes$AÑO),FUN=sum) %>% 
              rename(KM=x), by="Group.1") %>% 
  mutate(days = days_in_month(Accidentes.Fecha),
         Group.1=as.Date(paste0(Group.1,"-01-01")),
         EstanKm=(x/(KM*days))*100e6) %>% {
    data <- .
    
    capture.output({
      suppressWarnings({
        print("VKmT - total - Dead")
        data %>% 
          filter(year(Accidentes.Fecha) %in% c(2011, 2017),
                 Gravedad2 == "Dead") %>% 
          t.test(EstanKm~Group.1, data = .) %>% 
          print()
        print("VKmT - total - Not Dead")
        data %>% 
          filter(year(Accidentes.Fecha) %in% c(2011, 2017),
                 Gravedad2 == "Not Dead") %>% 
          t.test(EstanKm~Group.1, data = .) %>% 
          print()
      })
    }, file = "./GRAFICOS/estadisticas.txt", append = T)
    
    data
  } %>% 
  group_by(Group.1 = floor_date(Accidentes.Fecha, unit = "year"),
           Group.2 = Gravedad2) %>% 
  summarise(n = n(),
            sd_EstanKm = sd(EstanKm),
            EstanKm = mean(EstanKm)) %>%
  mutate(lwr = EstanKm - qt(0.975, df = n - 1) * sd_EstanKm / sqrt(n),
         upr = EstanKm + qt(0.975, df = n - 1) * sd_EstanKm / sqrt(n),
         n = NULL,
         sd_EstanKm = NULL) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  reshape(idvar = "Group.1", timevar = "Group.2", direction= "wide")

graph21_2 <- ggplot(data = aux, aes(x=Group.1)) + xlab("Year") + scale_x_date(date_labels = "%Y")+
  geom_point(aes(y = EstanKm.Dead,colour="Dead"),show.legend = FALSE) +
  geom_errorbar(aes(ymin = lwr.Dead,ymax=upr.Dead,colour="Dead"),show.legend = FALSE) +
  geom_text(aes(y = upr.Dead,label=sprintf('%.3f', EstanKm.Dead)),size=3,vjust=-1,family="Helvetica Light",show.legend = FALSE) +
  geom_point(aes(y = `EstanKm.Not Dead`,colour="Not Dead"),show.legend = TRUE) + 
  geom_errorbar(aes(ymin = `lwr.Not Dead`,ymax=`upr.Not Dead`,colour="Not Dead"),show.legend = FALSE) + ylab("Collisions per 100-million VKmT") +
  geom_text(aes(y = `upr.Not Dead`,label=sprintf('%.3f', `EstanKm.Not Dead`)),size=3,vjust=-1,family="Helvetica Light",show.legend = FALSE)+
  scale_colour_manual(breaks=c("Dead","Not Dead"),values = c("orangered3","navy"),labels=c("Fatal","Nonfatal"))+
  scale_linetype_manual(breaks=c("Tipo1","Tipo2"),values=c(1,3),labels=c("Fatal","Nonfatal"))+labs(colour="",linetype="")+
  scale_y_continuous(expand = expand_scale(mult = 0.2)) +
  theme_minimal() +
  theme(text=element_text(family="Helvetica Light",size=12,color="grey40")) 

#Total de accidentes durante los meses----
aux <- aggregate(AccidentesBiciTotal$Accidente,by=list(getYear(AccidentesBiciTotal$Accidentes.Fecha),getMonth(AccidentesBiciTotal$Accidentes.Fecha),AccidentesBiciTotal$Gravedad2),FUN=length) %>% mutate(Mes=as.Date(paste(Group.1,Group.2,"01",sep="/"),"%Y/%m/%d"))
aux$Important <-ifelse(aux$Mes%in%as.Date(c("2012-02-01","2015-04-01","2015-09-01","2017-05-01","2017-09-01")),"Important","NA")
aux<-reshape(aux,idvar = c("Mes","Important"),timevar = "Group.3",direction= "wide")
aux$`x.Not Dead`[is.na(aux$`x.Not Dead`)]<-0
aux$x.Dead[is.na(aux$x.Dead)]<-0
svg("./GRAFICOS/Accidents_TimeMonth.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data = aux, aes(x=Mes)) + xlab("Year")+ scale_x_date(date_labels = "%Y")+
    geom_line(aes(y = x.Dead,colour="Dead")) + ylab("Cyclists’ casualties")+ylim(0,190)+
    stat_smooth(aes(y = x.Dead,colour="Dead"),method="lm",linetype="dashed",size=0.5,alpha=0.2,fill="orangered3",show.legend = FALSE)+
    geom_line(aes(y = `x.Not Dead`,colour="Not Dead")) + ylab("Cyclists’ casualties")+
    stat_smooth(aes(y = `x.Not Dead`,colour="Not Dead"),method="lm",formula = y~x,linetype="dashed",size=0.5,alpha=0.2,fill="navy",show.legend = FALSE)+
    scale_colour_manual(breaks=c("Dead","Not Dead"),values = c("orangered3","navy"),labels=c("Fatal","Nonfatal"))+labs(colour="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey40"))
)
showtext_end()
dev.off()

#Total de accidentes durante los meses estandarizado por biciusuarios----
aux <- aggregate(AccidentesBiciTotal$Accidente,by=list(getYear(AccidentesBiciTotal$Accidentes.Fecha),getMonth(AccidentesBiciTotal$Accidentes.Fecha),AccidentesBiciTotal$Gravedad2),FUN=length) %>% mutate(Mes=as.Date(paste(Group.1,Group.2,"01",sep="/"),"%Y/%m/%d"))
aux <- left_join(aux,aggregate(biciusuarios$BICIUSRSINT,by=list(getYear(biciusuarios$FECHA)),FUN=sum) %>% rename(BICIUSRS=x),by="Group.1") %>% mutate(EstanBici=x*1000/BICIUSRS)
aux$Important <-ifelse(aux$Mes%in%as.Date(c("2012-02-01","2015-04-01","2015-09-01","2017-05-01","2017-09-01")),"Important","NA")
aux<-reshape(aux,idvar = c("Mes","Important"),timevar = "Group.3",direction= "wide")
aux$`EstanBici.Not Dead`[is.na(aux$`x.Not Dead`)]<-0
aux$EstanBici.Dead[is.na(aux$x.Dead)]<-0
svg("./GRAFICOS/AccidentsPerCyclist_TimeMonth.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data = aux, aes(x=Mes)) + xlab("Year")+ scale_x_date(date_labels = "%Y")+
    geom_line(aes(y = EstanBici.Dead,colour="Dead")) + ylab("Cyclists’ casualties")+ylim(0,1.4)+
    stat_smooth(aes(y = EstanBici.Dead,colour="Dead"),method="lm",linetype="dashed",size=0.5,alpha=0.2,fill="orangered3",show.legend = FALSE)+
    geom_line(aes(y = `EstanBici.Not Dead`,colour="Not Dead")) + ylab("Cyclists’ casualties per 1.000 cyclists")+
    stat_smooth(aes(y = `EstanBici.Not Dead`,colour="Not Dead"),method="lm",formula = y~x,linetype="dashed",size=0.5,alpha=0.2,fill="navy",show.legend = FALSE)+
    scale_colour_manual(breaks=c("Dead","Not Dead"),values = c("orangered3","navy"),labels=c("Fatal","Nonfatal"))+labs(colour="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey40"))
)
showtext_end()
dev.off()

#Total de accidentes durante los meses estandarizado por kilómetros recorridos----
aux <- aggregate(AccidentesBiciTotal$Accidente,by=list(getYear(AccidentesBiciTotal$Accidentes.Fecha),getMonth(AccidentesBiciTotal$Accidentes.Fecha),AccidentesBiciTotal$Gravedad2),FUN=length) %>% mutate(Mes=as.Date(paste(Group.1,Group.2,"01",sep="/"),"%Y/%m/%d"))
aux <- left_join(aux,aggregate(viajes$DIST_TOTAL_KMINT,by=list(viajes$AÑO),FUN=sum) %>% rename(KM=x),by="Group.1") %>% mutate(EstanKm=x*1000000/KM)
aux$Important <-ifelse(aux$Mes%in%as.Date(c("2012-02-01","2015-04-01","2015-09-01","2017-05-01","2017-09-01")),"Important","NA")
aux<-reshape(aux,idvar = c("Mes","Important"),timevar = "Group.3",direction= "wide")
aux$`EstanKm.Not Dead`[is.na(aux$`x.Not Dead`)]<-0
aux$EstanKm.Dead[is.na(aux$x.Dead)]<-0
svg("./GRAFICOS/AccidentsPerKm_TimeMonth.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data = aux, aes(x=Mes)) + xlab("Year")+ scale_x_date(date_labels = "%Y")+
    geom_line(aes(y = EstanKm.Dead,colour="Dead")) + ylab("Cyclists’ casualties")+ylim(0,0.11)+
    stat_smooth(aes(y = EstanKm.Dead,colour="Dead"),method="lm",linetype="dashed",size=0.5,alpha=0.2,fill="orangered3",show.legend = FALSE)+
    geom_line(aes(y = `EstanKm.Not Dead`,colour="Not Dead")) + ylab("Cyclists’ casualties per million Km cycled per day")+
    stat_smooth(aes(y = `EstanKm.Not Dead`,colour="Not Dead"),method="lm",formula = y~x,linetype="dashed",size=0.5,alpha=0.2,fill="navy",show.legend = FALSE)+
    scale_colour_manual(breaks=c("Dead","Not Dead"),values = c("orangered3","navy"),labels=c("Fatal","Nonfatal"))+labs(colour="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey40"))
)
showtext_end()
dev.off()

#Total de accidentes y mortalidad durante los años por género----
aux <- aggregate(AccidentesBiciTotal$Accidente,by=list(getYear(AccidentesBiciTotal$Accidentes.Fecha),AccidentesBiciTotal$Gravedad2,AccidentesBiciTotal$Sexo),FUN=length) %>% mutate(Group.1=as.Date(paste0(Group.1,"-01-01")))
aux<-reshape(aux,idvar = c("Group.3","Group.1"),timevar = "Group.2",direction= "wide")
aux$`x.Not Dead`[is.na(aux$`x.Not Dead`)]<-0
aux$x.Dead[is.na(aux$x.Dead)]<-0
svg("./GRAFICOS/Accidents_deaths_Gender_TimeYear.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data = aux, aes(x=Group.1)) + xlab("Year")+ scale_x_date(date_labels = "%Y")+
    geom_line(aes(y = x.Dead,colour="Dead")) + ylab("Cyclists’ casualties")+
    geom_text(aes(y = x.Dead,label=x.Dead),size=3,vjust=-0.5,family="Helvetica Light")+ylim(0,1700)+
    stat_smooth(aes(y = x.Dead,colour="Dead"),method="lm",linetype="dashed",size=0.5,alpha=0.2,fill="orangered3",show.legend = FALSE)+
    geom_line(aes(y = `x.Not Dead`,colour="Not Dead")) + ylab("Cyclists’ casualties")+
    geom_text(aes(y = `x.Not Dead`,label=`x.Not Dead`),size=3,vjust=-2,family="Helvetica Light")+
    stat_smooth(aes(y = `x.Not Dead`,colour="Not Dead"),method="lm",formula = y~x,linetype="dashed",size=0.5,alpha=0.2,fill="navy",show.legend = FALSE)+
    scale_colour_manual(breaks=c("Dead","Not Dead"),values = c("orangered3","navy"),labels=c("Fatal","Nonfatal"))+labs(colour="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey40"))+
    facet_grid(Group.3~.)
)
showtext_end()
dev.off()

#Total de accidentes y mortalidad durante los años estandarizado por biciusuarios por género----
aux <- aggregate(AccidentesBiciTotal$Accidente,by=list(getYear(AccidentesBiciTotal$Accidentes.Fecha),AccidentesBiciTotal$Gravedad2,AccidentesBiciTotal$Sexo),FUN=length)
aux <- left_join(aux,aggregate(biciusuarios$BICIUSRSINT,by=list(getYear(biciusuarios$FECHA),biciusuarios$SEXO),FUN=sum) %>% rename(BICIUSRS=x,Group.3=Group.2),by=c("Group.1","Group.3")) %>% mutate(Group.1=as.Date(paste0(Group.1,"-01-01")),EstanBici=x*1000/BICIUSRS)
aux<-reshape(aux,idvar = c("Group.1","Group.3"),timevar = "Group.2",direction= "wide")
aux$`EstanBici.Not Dead`[is.na(aux$`x.Not Dead`)]<-0
aux$EstanBici.Dead[is.na(aux$x.Dead)]<-0

summary(lm(formula = `EstanBici.Not Dead`~Group.1, data=aux %>% filter(Group.3=="Female")))
summary(lm(formula = EstanBici.Dead~Group.1, data=aux%>% filter(Group.3=="Female")))
summary(lm(formula = `EstanBici.Not Dead`~Group.1, data=aux %>% filter(Group.3=="Male")))
summary(lm(formula = EstanBici.Dead~Group.1, data=aux%>% filter(Group.3=="Male")))

aux<- aux %>% 
  mutate(y_lab_1 = ifelse(Group.1=="2014-01-01" & Group.3=="Female", 6.5, NA),
         lab_1 = ifelse(Group.1=="2014-01-01" & Group.3=="Female", "p = 0.728", NA),
         y_lab_2 = ifelse(Group.1=="2014-01-01" & Group.3=="Female", 0.6, NA),
         lab_2 = ifelse(Group.1=="2014-01-01" & Group.3=="Female", "p = 0.166", NA),
         y_lab_1 = ifelse(Group.1=="2014-01-01" & Group.3=="Male", 4, y_lab_1),
         lab_1 = ifelse(Group.1=="2014-01-01" & Group.3=="Male", "p = 0.021", lab_1),
         y_lab_2 = ifelse(Group.1=="2014-01-01" & Group.3=="Male", 1, y_lab_2),
         lab_2 = ifelse(Group.1=="2014-01-01" & Group.3=="Male", "p = 0.412", lab_2))

graph12<-  ggplot(data = aux, aes(x=Group.1)) + xlab("Year")+ scale_x_date(date_labels = "%Y")+
  geom_line(aes(y = EstanBici.Dead,colour="Dead",linetype="Tipo1"),show.legend = FALSE) + ylab("Bicyclists’ collisions")+
  geom_text(aes(y = EstanBici.Dead,label=round(EstanBici.Dead,1)),size=3,vjust=-0.5,family="Helvetica Light",show.legend = FALSE)+ylim(0,17)+
  stat_smooth(aes(y = EstanBici.Dead,colour="Dead"),method="lm",linetype="dashed",size=0.5,alpha=0.2,fill="orangered3",show.legend = FALSE)+
  geom_line(aes(y = `EstanBici.Not Dead`,colour="Not Dead",linetype="Tipo2"),show.legend = TRUE) + ylab("Collisions per 1,000 bicyclists")+
  geom_text(aes(y = `EstanBici.Not Dead`,label=round(`EstanBici.Not Dead`,1)),size=3,vjust=-2,family="Helvetica Light",show.legend = FALSE)+
  geom_text(aes(y = y_lab_1, label = lab_1),size=3.5,vjust=-2,family="Helvetica Light",show.legend = FALSE,color="black") +
  geom_text(aes(y = y_lab_2, label = lab_2),size=3.5,vjust=-2,family="Helvetica Light",show.legend = FALSE,color="black") +
  stat_smooth(aes(y = `EstanBici.Not Dead`,colour="Not Dead"),method="lm",formula = y~x,linetype="dashed",size=0.5,alpha=0.2,fill="navy",show.legend = FALSE)+
  scale_colour_manual(breaks=c("Dead","Not Dead"),values = c("orangered3","navy"),labels=c("Fatal","Nonfatal"))+
  scale_linetype_manual(breaks=c("Tipo1","Tipo2"),values=c(1,3),labels=c("Fatal","Nonfatal"))+labs(colour="",linetype="")+
  theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey40"))+
  facet_grid(Group.3~.)

svg("./GRAFICOS/AccidentsDeathsPerCyclists_Gender_TimeYear.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  graph12
)
showtext_end()
dev.off()

#Total de accidentes y mortalidad durante los meses colapsado por años estandarizado por biciusuarios por genero----
aux <- AccidentesBiciTotal %>% 
  filter(year(Accidentes.Fecha)<=2017,
         !is.na(Gravedad2),
         !is.na(Sexo)) %>% 
  st_set_geometry(NULL) %>%
  group_by(Accidentes.Fecha = floor_date(Accidentes.Fecha, unit = "month"),
           Gravedad2,
           Sexo) %>% 
  summarise(x = n()) %>% 
  group_by(Gravedad2, Sexo) %>% 
  pad(interval = "month",
      start_val = as.Date("2011-01-01"), 
      end_val = as.Date("2017-12-01")) %>% 
  mutate(x = ifelse(is.na(x), 0, x),
         Group.1 = getYear(Accidentes.Fecha)) %>%
  left_join(aggregate(biciusuarios$BICIUSRSINT,by=list(getYear(biciusuarios$FECHA)),FUN=sum) %>% 
              rename(BICIUSRS=x), by="Group.1") %>% 
  mutate(Group.1=as.Date(paste0(Group.1,"-01-01")),EstanBici=x*1000/BICIUSRS) %>% {
    data <- .
    
    capture.output({
      suppressWarnings({
        print("Biciusuarios - Female - Dead")
        data %>% 
          filter(year(Accidentes.Fecha) %in% c(2011, 2017),
                 Gravedad2 == "Dead",
                 Sexo=="Female") %>% 
          t.test(EstanBici~Group.1, data = .) %>% 
          print()
        print("Biciusuarios - Female - Not Dead")
        data %>% 
          filter(year(Accidentes.Fecha) %in% c(2011, 2017),
                 Gravedad2 == "Not Dead",
                 Sexo=="Female") %>% 
          t.test(EstanBici~Group.1, data = .) %>% 
          print()
        print("Biciusuarios - Male - Dead")
        data %>% 
          filter(year(Accidentes.Fecha) %in% c(2011, 2017),
                 Gravedad2 == "Dead",
                 Sexo=="Male") %>% 
          t.test(EstanBici~Group.1, data = .) %>% 
          print()
        print("Biciusuarios - Male - Not Dead")
        data %>% 
          filter(year(Accidentes.Fecha) %in% c(2011, 2017),
                 Gravedad2 == "Not Dead",
                 Sexo=="Female") %>% 
          t.test(EstanBici~Group.1, data = .) %>% 
          print()
      })
    }, file = "./GRAFICOS/estadisticas.txt", append = T)
    
    data
  } %>% 
  group_by(Group.1 = floor_date(Accidentes.Fecha, unit = "year"),
           Group.2 = Gravedad2,
           Group.3 = Sexo) %>% 
  summarise(n = n(),
            sd_EstanBici = sd(EstanBici),
            EstanBici = mean(EstanBici)) %>%
  mutate(lwr = EstanBici - qt(0.975, df = n - 1) * sd_EstanBici / sqrt(n),
         upr = EstanBici + qt(0.975, df = n - 1) * sd_EstanBici / sqrt(n),
         n = NULL,
         sd_EstanKm = NULL) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  reshape(idvar = c("Group.1","Group.3"), timevar = "Group.2", direction= "wide")

graph12_2 <- ggplot(data = aux, aes(x=Group.1)) + xlab("Year") + scale_x_date(date_labels = "%Y")+
  geom_point(aes(y = EstanBici.Dead,colour="Dead"),show.legend = FALSE) +
  geom_errorbar(aes(ymin = lwr.Dead,ymax=upr.Dead,colour="Dead"),show.legend = FALSE) +
  geom_text(aes(y = upr.Dead,label=sprintf('%.3f', EstanBici.Dead)),size=3,vjust=-1,family="Helvetica Light",show.legend = FALSE) +
  geom_point(aes(y = `EstanBici.Not Dead`,colour="Not Dead"),show.legend = TRUE) + 
  geom_errorbar(aes(ymin = `lwr.Not Dead`,ymax=`upr.Not Dead`,colour="Not Dead"),show.legend = FALSE) + ylab("Bicyclists’ collisions per 1,000 bicyclists") +
  geom_text(aes(y = `upr.Not Dead`,label=sprintf('%.3f',`EstanBici.Not Dead`)),size=3,vjust=-1,family="Helvetica Light",show.legend = FALSE)+
  scale_colour_manual(breaks=c("Dead","Not Dead"),values = c("orangered3","navy"),labels=c("Fatal","Nonfatal"))+
  scale_linetype_manual(breaks=c("Tipo1","Tipo2"),values=c(1,3),labels=c("Fatal","Nonfatal"))+labs(colour="",linetype="")+
  scale_y_continuous(expand = expand_scale(mult = 0.2)) +
  theme_minimal() +
  theme(text=element_text(family="Helvetica Light",size=12,color="grey40")) +
  facet_grid(Group.3~., scales = "free_y")

#Total de accidentes y mortalidad durante los años estandarizado por kilómetros recorridos por género----
aux <- aggregate(AccidentesBiciTotal$Accidente,by=list(getYear(AccidentesBiciTotal$Accidentes.Fecha),AccidentesBiciTotal$Gravedad2,AccidentesBiciTotal$Sexo),FUN=length)
aux <- left_join(aux,aggregate(viajes$DIST_TOTAL_KMINT,by=list(viajes$AÑO,viajes$SEXO),FUN=sum) %>% rename(KM=x,Group.3=Group.2),by=c("Group.1","Group.3")) %>% mutate(Group.1=as.Date(paste0(Group.1,"-01-01")),EstanKm=x*100e6/(KM*365))
aux<-reshape(aux,idvar = c("Group.1","Group.3"),timevar = "Group.2",direction= "wide")
aux$`EstanKm.Not Dead`[is.na(aux$`x.Not Dead`)]<-0
aux$EstanKm.Dead[is.na(aux$x.Dead)]<-0

summary(lm(formula = `EstanKm.Not Dead`~Group.1, data=aux %>% filter(Group.3=="Female")))
summary(lm(formula = EstanKm.Dead~Group.1, data=aux%>% filter(Group.3=="Female")))
summary(lm(formula = `EstanKm.Not Dead`~Group.1, data=aux %>% filter(Group.3=="Male")))
summary(lm(formula = EstanKm.Dead~Group.1, data=aux%>% filter(Group.3=="Male")))

aux<- aux %>% 
  mutate(y_lab_1 = ifelse(Group.1=="2014-01-01" & Group.3=="Female", 800, NA),
         lab_1 = ifelse(Group.1=="2014-01-01" & Group.3=="Female", "p = 0.036", NA),
         y_lab_2 = ifelse(Group.1=="2014-01-01" & Group.3=="Female", 150, NA),
         lab_2 = ifelse(Group.1=="2014-01-01" & Group.3=="Female", "p = 0.942", NA),
         y_lab_1 = ifelse(Group.1=="2014-01-01" & Group.3=="Male", 850, y_lab_1),
         lab_1 = ifelse(Group.1=="2014-01-01" & Group.3=="Male", "p = 0.001", lab_1),
         y_lab_2 = ifelse(Group.1=="2014-01-01" & Group.3=="Male", 150, y_lab_2),
         lab_2 = ifelse(Group.1=="2014-01-01" & Group.3=="Male", "p = 0.082", lab_2))

graph22 <-  ggplot(data = aux, aes(x=Group.1)) + xlab("Year")+ scale_x_date(date_labels = "%Y")+
  geom_line(aes(y = EstanKm.Dead,colour="Dead",linetype="Tipo1"),show.legend = FALSE) + ylab("Cyclists’ casualties")+
  geom_text(aes(y = EstanKm.Dead,label=round(EstanKm.Dead,2)),size=3,vjust=-0.5,family="Helvetica Light",show.legend = FALSE)+ylim(0,1400)+
  stat_smooth(aes(y = EstanKm.Dead,colour="Dead"),method="lm",linetype="dashed",size=0.5,alpha=0.2,fill="orangered3",show.legend = FALSE)+
  geom_line(aes(y = `EstanKm.Not Dead`,colour="Not Dead",linetype="Tipo2"),show.legend = TRUE) + ylab("Collisions per 100-million VKmT")+
  geom_text(aes(y = `EstanKm.Not Dead`,label=round(`EstanKm.Not Dead`,1)),size=3,vjust=-2,family="Helvetica Light",show.legend = FALSE)+
  geom_text(aes(y = y_lab_1, label = lab_1),size=3.5,vjust=-2,family="Helvetica Light",show.legend = FALSE,color="black") +
  geom_text(aes(y = y_lab_2, label = lab_2),size=3.5,vjust=-2,family="Helvetica Light",show.legend = FALSE,color="black") +
  stat_smooth(aes(y = `EstanKm.Not Dead`,colour="Not Dead"),method="lm",formula = y~x,linetype="dashed",size=0.5,alpha=0.2,fill="navy",show.legend = FALSE)+
  scale_colour_manual(breaks=c("Dead","Not Dead"),values = c("orangered3","navy"),labels=c("Fatal","Nonfatal"))+
  scale_linetype_manual(breaks=c("Tipo1","Tipo2"),values=c(1,3),labels=c("Fatal","Nonfatal"))+labs(colour="",linetype="")+
  theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey40"))+
  facet_grid(Group.3~.)

svg("./GRAFICOS/AccidentsDeathsPerKm_Gender_TimeYear.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  graph22
)
showtext_end()
dev.off()

#Total de accidentes y mortalidad durante los meses colapsado por años estandarizado por kilometros recorridos por genero----
aux <- AccidentesBiciTotal %>% 
  filter(year(Accidentes.Fecha)<=2017,
         !is.na(Gravedad2),
         !is.na(Sexo)) %>% 
  st_set_geometry(NULL) %>%
  group_by(Accidentes.Fecha = floor_date(Accidentes.Fecha, unit = "month"),
           Gravedad2,
           Sexo) %>% 
  summarise(x = n()) %>% 
  group_by(Gravedad2, Sexo) %>% 
  pad(interval = "month",
      start_val = as.Date("2011-01-01"), 
      end_val = as.Date("2017-12-01")) %>% 
  mutate(x = ifelse(is.na(x), 0, x),
         Group.1 = getYear(Accidentes.Fecha)) %>%
  left_join(aggregate(viajes$DIST_TOTAL_KMINT,by=list(viajes$AÑO),FUN=sum) %>% 
              rename(KM=x), by="Group.1") %>% 
  mutate(days = days_in_month(Accidentes.Fecha),
         Group.1=as.Date(paste0(Group.1,"-01-01")),
         EstanKm=(x/(KM*days))*100e6)  %>% {
    data <- .
    
    capture.output({
      suppressWarnings({
        print("Km - Female - Dead")
        data %>% 
          filter(year(Accidentes.Fecha) %in% c(2011, 2017),
                 Gravedad2 == "Dead",
                 Sexo=="Female") %>% 
          t.test(EstanKm~Group.1, data = .) %>% 
          print()
        print("Km - Female - Not Dead")
        data %>% 
          filter(year(Accidentes.Fecha) %in% c(2011, 2017),
                 Gravedad2 == "Not Dead",
                 Sexo=="Female") %>% 
          t.test(EstanKm~Group.1, data = .) %>% 
          print()
        print("Km - Male - Dead")
        data %>% 
          filter(year(Accidentes.Fecha) %in% c(2011, 2017),
                 Gravedad2 == "Dead",
                 Sexo=="Male") %>% 
          t.test(EstanKm~Group.1, data = .) %>% 
          print()
        print("Km - Male - Not Dead")
        data %>% 
          filter(year(Accidentes.Fecha) %in% c(2011, 2017),
                 Gravedad2 == "Not Dead",
                 Sexo=="Female") %>% 
          t.test(EstanKm~Group.1, data = .) %>% 
          print()
      })
    }, file = "./GRAFICOS/estadisticas.txt", append = T)
    
    data
  } %>% 
  group_by(Group.1 = floor_date(Accidentes.Fecha, unit = "year"),
           Group.2 = Gravedad2,
           Group.3 = Sexo) %>% 
  summarise(n = n(),
            sd_EstanKm = sd(EstanKm),
            EstanKm = mean(EstanKm)) %>%
  mutate(lwr = EstanKm - qt(0.975, df = n - 1) * sd_EstanKm / sqrt(n),
         upr = EstanKm + qt(0.975, df = n - 1) * sd_EstanKm / sqrt(n),
         n = NULL,
         sd_EstanKm = NULL) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  reshape(idvar = c("Group.1","Group.3"), timevar = "Group.2", direction= "wide")

graph22_2 <- ggplot(data = aux, aes(x=Group.1)) + xlab("Year") + scale_x_date(date_labels = "%Y")+
  geom_point(aes(y = EstanKm.Dead,colour="Dead"),show.legend = FALSE) +
  geom_errorbar(aes(ymin = lwr.Dead,ymax=upr.Dead,colour="Dead"),show.legend = FALSE) +
  geom_text(aes(y = upr.Dead,label=sprintf('%.3f', EstanKm.Dead)),size=3,vjust=-1,family="Helvetica Light",show.legend = FALSE) +
  geom_point(aes(y = `EstanKm.Not Dead`,colour="Not Dead"),show.legend = TRUE) + 
  geom_errorbar(aes(ymin = `lwr.Not Dead`,ymax=`upr.Not Dead`,colour="Not Dead"),show.legend = FALSE) + ylab("Collisions per 100-million VKmT") +
  geom_text(aes(y = `upr.Not Dead`,label=sprintf('%.3f', `EstanKm.Not Dead`)),size=3,vjust=-1,family="Helvetica Light",show.legend = FALSE)+
  scale_colour_manual(breaks=c("Dead","Not Dead"),values = c("orangered3","navy"),labels=c("Fatal","Nonfatal"))+
  scale_linetype_manual(breaks=c("Tipo1","Tipo2"),values=c(1,3),labels=c("Fatal","Nonfatal"))+labs(colour="",linetype="")+
  scale_y_continuous(expand = expand_scale(mult = 0.2)) +
  theme_minimal() +
  theme(text=element_text(family="Helvetica Light",size=12,color="grey40"))  +
  facet_grid(Group.3~., scales = "free_y")

#Total de accidentes durante los meses por género----
aux <- aggregate(AccidentesBiciTotal$Accidente,by=list(getYear(AccidentesBiciTotal$Accidentes.Fecha),getMonth(AccidentesBiciTotal$Accidentes.Fecha),AccidentesBiciTotal$Gravedad2,AccidentesBiciTotal$Sexo),FUN=length) %>% mutate(Mes=as.Date(paste(Group.1,Group.2,"01",sep="/"),"%Y/%m/%d"))
aux$Important <-ifelse(aux$Mes%in%as.Date(c("2012-02-01","2015-04-01","2015-09-01","2017-05-01","2017-09-01")),"Important","NA")
aux<-reshape(aux,idvar = c("Mes","Group.4","Important"),timevar = "Group.3",direction= "wide")
aux$`x.Not Dead`[is.na(aux$`x.Not Dead`)]<-0
aux$x.Dead[is.na(aux$x.Dead)]<-0
svg("./GRAFICOS/Accidents_Gender_TimeMonth.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data = aux, aes(x=Mes)) + xlab("Year")+ scale_x_date(date_labels = "%Y")+
    geom_line(aes(y = x.Dead,colour="Dead")) + ylab("Cyclists’ casualties")+ylim(0,180)+
    stat_smooth(aes(y = x.Dead,colour="Dead"),method="lm",linetype="dashed",size=0.5,alpha=0.2,fill="orangered3",show.legend = FALSE)+
    geom_line(aes(y = `x.Not Dead`,colour="Not Dead")) + ylab("Cyclists’ casualties")+
    stat_smooth(aes(y = `x.Not Dead`,colour="Not Dead"),method="lm",formula = y~x,linetype="dashed",size=0.5,alpha=0.2,fill="navy",show.legend = FALSE)+
    scale_colour_manual(breaks=c("Dead","Not Dead"),values = c("orangered3","navy"),labels=c("Fatal","Nonfatal"))+labs(colour="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey40"))+
    facet_grid(Group.4~.)
)
showtext_end()
dev.off()

#Total de accidentes durante los meses estandarizado por biciusuarios por género----
aux <- aggregate(AccidentesBiciTotal$Accidente,by=list(getYear(AccidentesBiciTotal$Accidentes.Fecha),getMonth(AccidentesBiciTotal$Accidentes.Fecha),AccidentesBiciTotal$Gravedad2,AccidentesBiciTotal$Sexo),FUN=length) %>% mutate(Mes=as.Date(paste(Group.1,Group.2,"01",sep="/"),"%Y/%m/%d"))
aux <- left_join(aux,aggregate(biciusuarios$BICIUSRSINT,by=list(getYear(biciusuarios$FECHA),biciusuarios$SEXO),FUN=sum) %>% rename(BICIUSRS=x,Group.4=Group.2),by=c("Group.1","Group.4")) %>% mutate(EstanBici=x*1000/BICIUSRS)
aux$Important <-ifelse(aux$Mes%in%as.Date(c("2012-02-01","2015-04-01","2015-09-01","2017-05-01","2017-09-01")),"Important","NA")
aux<-reshape(aux,idvar = c("Mes","Group.4","Important"),timevar = "Group.3",direction= "wide")
aux$`EstanBici.Not Dead`[is.na(aux$`x.Not Dead`)]<-0
aux$EstanBici.Dead[is.na(aux$x.Dead)]<-0
svg("./GRAFICOS/AccidentsPerCyclist_Gender_TimeMonth.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data = aux, aes(x=Mes)) + xlab("Year")+ scale_x_date(date_labels = "%Y")+
    geom_line(aes(y = EstanBici.Dead,colour="Dead")) + ylab("Cyclists’ casualties")+ylim(0,1.7)+
    stat_smooth(aes(y = EstanBici.Dead,colour="Dead"),method="lm",linetype="dashed",size=0.5,alpha=0.2,fill="orangered3",show.legend = FALSE)+
    geom_line(aes(y = `EstanBici.Not Dead`,colour="Not Dead")) + ylab("Cyclists’ casualties per 1.000 cyclists")+
    stat_smooth(aes(y = `EstanBici.Not Dead`,colour="Not Dead"),method="lm",formula = y~x,linetype="dashed",size=0.5,alpha=0.2,fill="navy",show.legend = FALSE)+
    scale_colour_manual(breaks=c("Dead","Not Dead"),values = c("orangered3","navy"),labels=c("Fatal","Nonfatal"))+labs(colour="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey40"))+
    facet_grid(Group.4~.)
)
showtext_end()
dev.off()

#Total de accidentes durante los meses estandarizado por kilómetros recorridos por género----
aux <- aggregate(AccidentesBiciTotal$Accidente,by=list(getYear(AccidentesBiciTotal$Accidentes.Fecha),getMonth(AccidentesBiciTotal$Accidentes.Fecha),AccidentesBiciTotal$Gravedad2,AccidentesBiciTotal$Sexo),FUN=length) %>% mutate(Mes=as.Date(paste(Group.1,Group.2,"01",sep="/"),"%Y/%m/%d"))
aux <- left_join(aux,aggregate(viajes$DIST_TOTAL_KMINT,by=list(viajes$AÑO,viajes$SEXO),FUN=sum) %>% rename(KM=x,Group.4=Group.2),by=c("Group.1","Group.4")) %>% mutate(EstanKm=x*1000/KM)
aux$Important <-ifelse(aux$Mes%in%as.Date(c("2012-02-01","2015-04-01","2015-09-01","2017-05-01","2017-09-01")),"Important","NA")
aux<-reshape(aux,idvar = c("Mes","Group.4","Important"),timevar = "Group.3",direction= "wide")
aux$`EstanKm.Not Dead`[is.na(aux$`x.Not Dead`)]<-0
aux$EstanKm.Dead[is.na(aux$x.Dead)]<-0
svg("./GRAFICOS/AccidentsPerKm_Gender_TimeMonth.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data = aux, aes(x=Mes)) + xlab("Year")+ scale_x_date(date_labels = "%Y")+
    geom_line(aes(y = EstanKm.Dead,colour="Dead")) + ylab("Cyclists’ casualties")+ylim(0,0.16)+
    stat_smooth(aes(y = EstanKm.Dead,colour="Dead"),method="lm",linetype="dashed",size=0.5,alpha=0.2,fill="orangered3",show.legend = FALSE)+
    geom_line(aes(y = `EstanKm.Not Dead`,colour="Not Dead")) + ylab("Cyclists’ casualties per million Km cycled per day")+
    stat_smooth(aes(y = `EstanKm.Not Dead`,colour="Not Dead"),method="lm",formula = y~x,linetype="dashed",size=0.5,alpha=0.2,fill="navy",show.legend = FALSE)+
    scale_colour_manual(breaks=c("Dead","Not Dead"),values = c("orangered3","navy"),labels=c("Fatal","Nonfatal"))+labs(colour="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey40"))+
    facet_grid(Group.4~.)
)
showtext_end()
dev.off()

#Tendencias de accidentalidad para publicación----
pdf("./GRAFICOS/AccidentTrends_Paper.pdf",width = cmWidth, height = cmHeight*1.2)
showtext_begin()
print(
  ggarrange(graph11,graph12,graph21,graph22,labels=c("A","B","C","D"),common.legend = TRUE,legend = "bottom")
)
showtext_end()
dev.off()

pdf("./GRAFICOS/AccidentTrends_Paper_2.pdf",width = cmWidth, height = cmHeight*1.2)
showtext_begin()
print(
  ggarrange(graph11_2,graph12_2,graph21_2,graph22_2,labels=c("A","B","C","D"),common.legend = TRUE,legend = "bottom")
)
showtext_end()
dev.off()

########### TEMPORALIDAD ACCIDENTES ##################################################################
#Total de accidentes durante los dias----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(as.Date(AccidentesBiciTotal$Accidentes.Fecha)),FUN=length)
aux<-pad(aux, interval="day") %>% mutate(x = replace(x, is.na(x), 0))
svg("./GRAFICOS/Accidents_TimeDay.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data = aux, aes(x=Group.1,y = x))+xlab("Day")+
    geom_line(color="navy")+scale_x_date(date_labels = "%Y",date_breaks = "1 year")+ ylab("Cyclists’ casualties")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))
)
showtext_end()
dev.off()

#Total de accidentes durante los dias por biciusuarios----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(as.Date(AccidentesBiciTotal$Accidentes.Fecha)),FUN=length)
aux<-pad(aux, interval="day") %>% mutate(x = replace(x, is.na(x), 0),AÑO=getYear(Group.1))
aux <- merge(aux,aggregate(biciusuarios$BICIUSRSINT,by=list(getYear(biciusuarios$FECHA)),FUN=sum) %>% rename(BICIUSRS=x),by.x="AÑO",by.y="Group.1",all.x=TRUE) %>% mutate(EstanPop=x*1000/BICIUSRS)
svg("./GRAFICOS/AccidentsPerCyclists_TimeDay.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data = aux, aes(x=Group.1,y = EstanPop))+xlab("Day")+
    geom_line(color="navy")+scale_x_date(date_labels = "%Y",date_breaks = "1 year")+ ylab("Cyclists’ casualties per 1.000 cyclists")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))
)
showtext_end()
dev.off()

#Total accidentes por día de la semana y por año----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(as.Date(AccidentesBiciTotal$Accidentes.Fecha)),FUN=length)
aux<-pad(aux, interval="day") %>% mutate(x = replace(x, is.na(x), 0),AÑO=getYear(Group.1),WDAY=weekdays(Group.1))
aux <- aggregate(aux$x,by=list(aux$AÑO,aux$WDAY),FUN=mean)
aux$Group.2<-factor(as.factor(aux$Group.2),c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
svg("./GRAFICOS/AccidentsAverageLines_TimeWeekdayYear.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data = aux, aes(x=Group.2,y = x,group=Group.1,colour=Group.1)) + xlab("Weekday")+
    geom_line() + ylab("Average cyclists’ casualties")+
    scale_color_brewer(palette="Accent")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.text=element_text(colour="black"),legend.title=element_text(colour="black"))+labs(colour="Year")
)
showtext_end()
dev.off()

#Total accidentes por día de la semana y por año estandarizado por biciusuarios----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(as.Date(AccidentesBiciTotal$Accidentes.Fecha)),FUN=length)
aux<-pad(aux, interval="day") %>% mutate(x = replace(x, is.na(x), 0),AÑO=getYear(Group.1),WDAY=weekdays(Group.1))
aux <- merge(aggregate(aux$x,by=list(aux$AÑO,aux$WDAY),FUN=mean),aggregate(biciusuarios$BICIUSRSINT,by=list(getYear(biciusuarios$FECHA)),FUN=sum) %>% rename(BICIUSRS=x),by="Group.1",all.x=TRUE) %>% mutate(EstanPop=x*1000/BICIUSRS)
aux$Group.2<-factor(as.factor(aux$Group.2),c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
svg("./GRAFICOS/AccidentsAverageLinesPerCyclists_TimeWeekdayYear.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data = aux, aes(x=Group.2,y = EstanPop,group=Group.1,colour=Group.1)) + xlab("Weekday")+
    geom_line() + ylab("Average Cyclists’ casualties per 1.000 cyclists")+
    scale_color_brewer(palette="Accent")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.text=element_text(colour="black"),legend.title=element_text(colour="black"))+labs(colour="Year")
)
showtext_end()
dev.off()

#Total accidentes por día de la semana y por año (Boxplot)----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(as.Date(AccidentesBiciTotal$Accidentes.Fecha)),FUN=length)
aux<-pad(aux, interval="day") %>% mutate(x = replace(x, is.na(x), 0),AÑO=getYear(Group.1),WDAY=weekdays(Group.1))
aux <- aggregate(aux$x,by=list(aux$AÑO,aux$WDAY),FUN=mean)
aux$Group.2<-factor(as.factor(aux$Group.2),c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
svg("./GRAFICOS/AccidentsAverageBox_TimeWeekdayYear.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data = aux, aes(x=Group.2,y = x,colour=Group.2)) + xlab("Weekday")+
    geom_boxplot() + ylab("Average Cyclists’ casualties")+
    scale_color_brewer(palette="Dark2")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.position = "none")
)
showtext_end()
dev.off()

#Total accidentes por día de la semana y por año estandarizado por biciusuarios (Boxplot)----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(as.Date(AccidentesBiciTotal$Accidentes.Fecha)),FUN=length)
aux<-pad(aux, interval="day") %>% mutate(x = replace(x, is.na(x), 0),AÑO=getYear(Group.1),WDAY=weekdays(Group.1))
aux <- merge(aggregate(aux$x,by=list(aux$AÑO,aux$WDAY),FUN=mean),aggregate(biciusuarios$BICIUSRSINT,by=list(getYear(biciusuarios$FECHA)),FUN=sum) %>% rename(BICIUSRS=x),by="Group.1",all.x=TRUE) %>% mutate(EstanPop=x*1000/BICIUSRS)
aux$Group.2<-factor(as.factor(aux$Group.2),c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
svg("./GRAFICOS/AccidentsAverageBoxPerCyclists_TimeWeekdayYear.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data = aux, aes(x=Group.2,y = EstanPop,colour=Group.2)) + xlab("Weekday")+
    geom_boxplot() + ylab("Average Cyclists’ casualties per 1.000 cyclists")+
    scale_color_brewer(palette="Dark2")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.position = "none")
)
showtext_end()
dev.off()

#Accidentes promedio por día de la semana y por año----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(as.Date(AccidentesBiciTotal$Accidentes.Fecha)),FUN=length)
aux<-pad(aux, interval="day") %>% mutate(x = replace(x, is.na(x), 0),AÑO=as.factor(getYear(Group.1)),WDAY=weekdays(Group.1))
aux$WDAY<-factor(aux$WDAY,c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
svg("./GRAFICOS/PromedioAccidentesDiasSemanaBoxplot.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data=aux,aes(x=WDAY,y=x,colour=WDAY))+xlab("Weekday")+
    geom_boxplot(outlier.size = 1)+ylim(0,21)+ylab("Average Cyclists’ casualties")+
    scale_color_brewer(palette="Dark2")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(axis.text.x = element_text(angle = 90,hjust=1,vjust=0.5))+theme(legend.position = "none")+
    facet_grid(~AÑO)
)
showtext_end()
dev.off()

#Total accidentes por mes y por año----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(as.Date(AccidentesBiciTotal$Accidentes.Fecha)),FUN=length)
aux<-pad(aux, interval="day") %>% mutate(x = replace(x, is.na(x), 0),AÑO=getYear(Group.1),MONTH=getMonth(Group.1,"%b"))
aux <- aggregate(aux$x,by=list(aux$AÑO,aux$MONTH),FUN=mean)
aux$Group.2<-factor(aux$Group.2,c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
svg("./GRAFICOS/AccidentsAverageLines_TimeMonthYear.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data = aux, aes(x=Group.2,y = x,group=Group.1,colour=Group.1)) + xlab("Weekday")+
    geom_line() + ylab("Average cyclists’ casualties")+
    scale_color_brewer(palette="Accent")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.text=element_text(colour="black"),legend.title=element_text(colour="black"))+labs(colour="Year")
)
showtext_end()
dev.off()

#Total accidentes por mes y por año estandarizado por biciusuarios----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(as.Date(AccidentesBiciTotal$Accidentes.Fecha)),FUN=length)
aux<-pad(aux, interval="day") %>% mutate(x = replace(x, is.na(x), 0),AÑO=getYear(Group.1),MONTH=getMonth(Group.1,"%b"))
aux <- merge(aggregate(aux$x,by=list(aux$AÑO,aux$MONTH),FUN=mean),aggregate(biciusuarios$BICIUSRSINT,by=list(getYear(biciusuarios$FECHA)),FUN=sum) %>% rename(BICIUSRS=x),by="Group.1",all.x=TRUE) %>% mutate(EstanPop=x*1000/BICIUSRS)
aux$Group.2<-factor(aux$Group.2,c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
svg("./GRAFICOS/AccidentsAverageLinesPerCyclists_TimeMonthYear.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data = aux, aes(x=Group.2,y = EstanPop,group=Group.1,colour=Group.1)) + xlab("Weekday")+
    geom_line() + ylab("Average Cyclists’ casualties per 1.000 cyclists")+
    scale_color_brewer(palette="Accent")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.text=element_text(colour="black"),legend.title=element_text(colour="black"))+labs(colour="Year")
)
showtext_end()
dev.off()

#Total accidentes por mes y por año (Boxplot)----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(as.Date(AccidentesBiciTotal$Accidentes.Fecha)),FUN=length)
aux<-pad(aux, interval="day") %>% mutate(x = replace(x, is.na(x), 0),AÑO=getYear(Group.1),MONTH=getMonth(Group.1,"%b"))
aux <- aggregate(aux$x,by=list(aux$AÑO,aux$MONTH),FUN=mean)
aux$Group.2<-factor(aux$Group.2,c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
svg("./GRAFICOS/AccidentsAverageBox_TimeMonthYear.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data = aux, aes(x=Group.2,y = x,colour=Group.2)) + xlab("Weekday")+
    geom_boxplot() + ylab("Average Cyclists’ casualties")+
    scale_color_viridis(discrete = TRUE)+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.position = "none")
)
showtext_end()
dev.off()

#Total accidentes por mes y por año estandarizado por biciusuarios (Boxplot)----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(as.Date(AccidentesBiciTotal$Accidentes.Fecha)),FUN=length)
aux<-pad(aux, interval="day") %>% mutate(x = replace(x, is.na(x), 0),AÑO=getYear(Group.1),MONTH=getMonth(Group.1,"%b"))
aux <- merge(aggregate(aux$x,by=list(aux$AÑO,aux$MONTH),FUN=mean),aggregate(biciusuarios$BICIUSRSINT,by=list(getYear(biciusuarios$FECHA)),FUN=sum) %>% rename(BICIUSRS=x),by="Group.1",all.x=TRUE) %>% mutate(EstanPop=x*1000/BICIUSRS)
aux$Group.2<-factor(aux$Group.2,c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
svg("./GRAFICOS/AccidentsAverageBoxPerCyclists_TimeMonthYear.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data = aux, aes(x=Group.2,y = EstanPop,colour=Group.2)) + xlab("Weekday")+
    geom_boxplot() + ylab("Average Cyclists’ casualties per 1.000 cyclists")+
    scale_color_viridis(discrete = TRUE)+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.position = "none")
)
showtext_end()
dev.off()

#Análisis de la hora de acurrencia del total de accidentes para un día (Wednesday)----
aux<-AccidentesBiciTotal[weekdays(AccidentesBiciTotal$Accidentes.Fecha)=="Wednesday",]
aux<-aggregate(aux$Accidente,by=list(as.POSIXlt(aux$Accidentes.HoraOcurrencia)$hour),FUN=length)
aux[23,]<-c(1,0)
svg("./GRAFICOS/AccidentsOnWednesday.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data=aux,aes(x=Group.1,y=x))+xlim(c(0,23))+xlab("Day hours")+
    geom_line(color="navy")+ylab("Cyclists’ casualties")+
    geom_text(aes(label=x),family="Helvetica Light",size=3,vjust=0,hjust=0)+
    geom_vline(xintercept=6, colour="red", linetype="dotted")+
    geom_vline(xintercept=18, colour="red", linetype="dotted")+
    geom_errorbarh(aes(xmin=6.1,xmax=17.9,y=97.5,colour="red"))+
    geom_text(aes(x=12.5,y=97.5,label="Work hours\n6:00 - 18:00"),family="Helvetica Light",size=5,vjust=0.5,hjust=0.5,color="red")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.position="none")
)
showtext_end()
dev.off()

#Análisis de la hora de acurrencia de accidentes promedio por dia por año----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(floor_date(AccidentesBiciTotal$Accidentes.HoraOcurrencia,"hour")),FUN=length)
aux<-pad(aux, interval="hour",start_val = as.POSIXct("2011-01-01 00:00:00",format="%Y-%m-%d %H:%M:%S"),end_val = as.POSIXct("2017-12-31 23:00:00",format="%Y-%m-%d %H:%M:%S")) %>% mutate(x = replace(x, is.na(x), 0))
aux$Group.2<-weekdays(aux$Group.1)
aux$hora<-as.POSIXlt(aux$Group.1)$hour
aux$año<-getYear(aux$Group.1)
aux$Group.2<-factor(as.factor(aux$Group.2),c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
aux$Group.1<-as.Date(aux$Group.1)
aux<-aggregate(aux$x,by=list(aux$año,aux$Group.2,aux$hora),FUN=mean)
svg("./GRAFICOS/Accidents_TimeYearWdayHour.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=Group.3,y=x))+xlab("Day hours")+xlim(c(0,23))+
    geom_line(color="navy")+ylab("Average Cyclists’ casualties")+
    geom_vline(xintercept=6, colour="red", linetype="dotted")+
    geom_vline(xintercept=18, colour="red", linetype="dotted")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.position="none")+
    facet_grid(Group.2~Group.1)
)
showtext_end()
dev.off()

########### UBICACIÓN ACCIDENTES ####################################################################
#Total de accidentes por localidad por año----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(getYear(AccidentesBiciTotal$Accidentes.Fecha),AccidentesBiciTotal$Accidentes.Localidad),FUN=length)
names(aux)<-c("Group.1","LocNombre","Accidentes")
aux$Group.1<-as.numeric(aux$Group.1)
aux$LocNombre<-factor(aux$LocNombre)
svg("./GRAFICOS/Accidentes_Localidad_TimeAño.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data=aux,aes(x=LocNombre,y=Accidentes))+xlab("Locality (District)")+
    geom_bar(stat="identity",fill="#4e78a6")+ylab("Cyclists’ casualties")+ylim(0,340)+
    geom_text(aes(label=Accidentes),size=3,hjust=0.5,vjust=-0.2,family="Helvetica Light")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))+
    facet_grid(Group.1~.)
)
showtext_end()
dev.off()

#Total de accidentes por localidad por año estandarizadas por biciusuarios----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(getYear(AccidentesBiciTotal$Accidentes.Fecha),AccidentesBiciTotal$Accidentes.Localidad),FUN=length)
names(aux)<-c("Group.1","LocNombre","Accidentes")
aux$Group.1<-as.numeric(aux$Group.1)
aux$LocNombre<-factor(aux$LocNombre)
aux<-merge(aux,aggregate(biciusuarios$BICIUSRSINT,by=list(getYear(biciusuarios$FECHA),biciusuarios$LOCALIDAD),FUN=sum) %>% rename(BICIUSRS=x,LocNombre=Group.2),by=c("Group.1","LocNombre"))
aux$EstanPop<-aux$Accidentes*1000/aux$BICIUSRS
svg("./GRAFICOS/AccidentesPerCyclists_Localidad_TimeAño.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data=aux,aes(x=LocNombre,y=EstanPop))+xlab("Locality (District)")+
    geom_bar(stat="identity",fill="#4e78a6")+ylab("Cyclists’ casualties per 1.000 cyclists")+ylim(0,160)+
    geom_text(aes(label=round(EstanPop,1)),size=3,hjust=0.5,vjust=-0.2,family="Helvetica Light")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))+
    facet_grid(Group.1~.)
)
showtext_end()
dev.off()

########### GRAVEDAD ACCIDENTES #####################################################################
#Gravedad del accidente para el conductor (Todos los Niveles)----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(getYear(AccidentesBiciTotal$Accidentes.Fecha),AccidentesBiciTotal$Gravedad),FUN=length)
aux %<>%
  group_by(Group.1) %>%
  summarise(Sum=sum(x)) %>%
  left_join(aux,by="Group.1")
aux$x<-aux$x/aux$Sum
svg("./GRAFICOS/severidadTotalAccidentes.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data=aux,aes(x=Group.1,y=x,fill=Group.2))+xlab("")+
    geom_bar(stat="identity")+scale_y_continuous(labels = scales::percent)+ylab("Distribution of severity of injuries")+
    geom_text(aes(label=ifelse(round(x,2)>0,paste(round(x*100,1)," %"),NA)),check_overlap = TRUE, position = position_stack(vjust = 0.5),family="Helvetica Light",size=3.5)+
    geom_text(aes(x=Group.1,y=1.02,label=paste("N ",Sum)),family="Helvetica Light",size=3,color="grey40")+theme(legend.position="top")+
    scale_fill_brewer(palette="Paired")+labs(fill="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey40"))+
    theme(legend.position = "top")
)
showtext_end()
dev.off()


#Gravedad del accidente para el conductor (Binomial - Muerto/No Muerto)-----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(getYear(AccidentesBiciTotal$Accidentes.Fecha),AccidentesBiciTotal$Gravedad2),FUN=length)
aux %<>%
  group_by(Group.1) %>%
  summarise(Sum=sum(x)) %>%
  left_join(aux,by="Group.1")
aux$Group.2<-as.character(aux$Group.2)
aux$x<-aux$x/aux$Sum
svg("./GRAFICOS/severidadBinomialAccidentes.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data=aux,aes(x=Group.1,y=x))+xlab("")+
    geom_bar(stat="identity",aes(fill=Group.2))+scale_y_continuous(labels = scales::percent)+ylab("Distribution of severity of injuries")+
    geom_text(aes(label=ifelse(round(x,2)>0,paste(round(x*100,1)," %"),NA)),check_overlap = TRUE, position = position_stack(vjust = 0.5),family="Helvetica Light",size=3.5)+
    geom_text(aes(x=Group.1,y=1.02,label=paste("N ",Sum)),family="Helvetica Light",size=3,color="grey40")+
    scale_fill_manual(values=c("Not Dead"="lightblue","Dead"="lightpink1"))+labs(fill="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey40"))+
    theme(legend.position = "top")
)
showtext_end()
dev.off()

#Tipos de accidente registrados----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(getYear(AccidentesBiciTotal$Accidentes.Fecha),AccidentesBiciTotal$Accidentes.ClaseNombre),FUN=length)
aux %<>%
  group_by(Group.1) %>%
  summarise(Sum=sum(x)) %>%
  left_join(aux,by="Group.1")
aux$x<-aux$x/aux$Sum
svg("./GRAFICOS/TiposAccidentes.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data=aux,aes(x=Group.1,y=x,fill=Group.2))+xlab("")+
    geom_bar(stat="identity")+scale_y_continuous(labels = scales::percent)+ylab("Distribution of types of incidents")+
    geom_text(aes(label=ifelse(round(x,2)>0.01,paste(round(x*100,1)," %",sep=""),NA)),check_overlap = TRUE, position = position_stack(vjust = 0.5),family="Helvetica Light")+
    geom_text(aes(x=Group.1,y=1.02,label=paste("N ",Sum)),family="Helvetica Light",size=3,color="grey40")+
    scale_fill_brewer(palette="Paired")+labs(fill="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey40"))+
    theme(legend.position="top")
)
showtext_end()
dev.off()

########### FACTORES INDIVIDUALES ###################################################################
#Sexo de los involucrados en accidentes----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(AccidentesBiciTotal$Sexo),FUN=length)
aux[,2]<-aux[,2]*100/sum(aux[,2])
aux <- aux %>% mutate(pos = cumsum(x)- x/2)
aux$Group.1<-factor(aux$Group.1,c("Male","Female"))
svg("./GRAFICOS/sexoGraph.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data=aux,aes(x="",y=x,fill=Group.1))+xlab("")+
    geom_bar(width=1,stat="identity")+ ylab("")+
    coord_polar(theta="y")+
    scale_fill_brewer(palette="Paired")+
    geom_text(aes(y = pos,label=c(paste(Group.1,"\n",round(x,2),"%",sep="")[1:2])),family="Helvetica Light",size=5,color="white")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(axis.text.x=element_blank(),panel.grid=element_blank(),axis.ticks = element_blank())+theme(legend.position = "none")
)
showtext_end()
dev.off()

#Género y gravedad del accidente----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(AccidentesBiciTotal$Sexo,AccidentesBiciTotal$Gravedad),FUN=length)
aux<-aux[aux$Group.1!="NA",]
aux %<>%
  group_by(Group.1) %>%
  summarise(Sum=sum(x)) %>%
  left_join(aux,by="Group.1") %>%
  mutate(y=x/Sum)
svg("./GRAFICOS/SexoSeveridad.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=Group.2,y=Group.1))+xlab("Severity")+
    geom_tile(aes(fill=y),colour = "white") + ylab("Gender")+
    scale_fill_gradient(low = "white",high = "steelblue")+
    geom_text(aes(label=paste(x,"\n",round(y*100,0)," %",sep=""),family="Helvetica Light"))+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.position = "none")
)
showtext_end()
dev.off()

#Género y gravedad binomial del accidente----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(AccidentesBiciTotal$Sexo,AccidentesBiciTotal$Gravedad2),FUN=length)
aux<-aux[aux$Group.1!="NA",]
aux %<>%
  group_by(Group.1) %>%
  summarise(Sum=sum(x)) %>%
  left_join(aux,by="Group.1") %>%
  mutate(y=x/Sum)
svg("./GRAFICOS/SexoSeveridadBinominal.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=Group.2,y=Group.1))+xlab("Severity")+
    geom_tile(aes(fill=x),colour = "white") + ylab("Gender")+
    scale_fill_gradient(low = "white",high = "steelblue")+
    geom_text(aes(label=paste(x,"\n",round(y*100,0)," %",sep=""),family="Helvetica Light"))+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.position = "none")
)
showtext_end()
dev.off()

#Edad del conductor involucrado----
svg("./GRAFICOS/histogramaEdad.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot()+xlab("Age")+
    geom_histogram(data=AccidentesBiciTotal,aes(Edad),fill="#4e78a6",color="navy",binwidth=5)+stat_count()+ylab("Frecuency")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))
)
showtext_end()
dev.off()

#Tile de severidad y edad en grupos quinquenales por género----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(AccidentesBiciTotal$EdadCategorica,AccidentesBiciTotal$Gravedad,AccidentesBiciTotal$Sexo),FUN=length)
aux<-aux[aux$Group.3!="NA",]
svg("./GRAFICOS/EdadSeveridadSexo.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=Group.2,y=Group.1))+xlab("Severity")+
    geom_tile(aes(fill=x),colour = "white") + ylab("Age")+
    scale_fill_gradient(low = "white",high = "steelblue")+
    geom_text(aes(label=x),family="Helvetica Light")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.position = "none")+
    facet_grid(~Group.3)
)
showtext_end()
dev.off()

#Tile de severidad binomial y edad en grupos quinquenales por género----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(AccidentesBiciTotal$EdadCategorica,AccidentesBiciTotal$Gravedad2,AccidentesBiciTotal$Sexo),FUN=length)
aux<-aux[aux$Group.3!="NA",]
aux %<>%
  group_by(Group.1,Group.3) %>%
  summarise(Sum=sum(x)) %>%
  left_join(aux,by=c("Group.1","Group.3")) %>%
  mutate(y=x/Sum)
svg("./GRAFICOS/EdadSeveridadBinomialSexo.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=Group.2,y=Group.1))+xlab("Severity")+
    geom_tile(aes(fill=x),colour = "white") + ylab("Age")+
    scale_fill_gradient(low = "white",high = "steelblue")+
    geom_text(aes(label=paste0(round(y,2)*100," %")),family="Helvetica Light")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.position = "none")+
    facet_grid(~Group.3)
)
showtext_end()
dev.off()

#Uso del casco y gravedad (Todos los niveles)----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(AccidentesBiciTotal$LLevaCasco,AccidentesBiciTotal$Gravedad),FUN="length")
aux<-aux[aux$Group.1!="SIN INFORMACION",]
aux %<>%
  group_by(Group.1) %>%
  summarise(Sum=sum(x)) %>%
  left_join(aux,by="Group.1") %>%
  mutate(y=x/Sum)
svg("./GRAFICOS/UsoCascoSeveridad.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=Group.2,y=Group.1))+xlab("Severity")+
    geom_tile(aes(fill=y),colour = "white") + ylab("Helmet use")+
    scale_fill_gradient(low = "white",high = "steelblue")+
    geom_text(aes(label=paste(x,"\n",paste(round(y*100,0),"%",sep=""),sep="")),family="Helvetica Light")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.position = "none")
)
showtext_end()
dev.off()

#Uso del casco y gravedad (Binomial)----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(AccidentesBiciTotal$LLevaCasco,AccidentesBiciTotal$Gravedad2),FUN="length")
aux<-aux[aux$Group.1!="SIN INFORMACION",]
aux %<>%
  group_by(Group.1) %>%
  summarise(Sum=sum(x)) %>%
  left_join(aux,by="Group.1") %>%
  mutate(y=x/Sum)
svg("./GRAFICOS/UsoCascoSeveridadBinomial.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(aux,aes(x=Group.2,y=Group.1))+xlab("Severity")+
    geom_tile(aes(fill=y),colour = "white") + ylab("Helmet use")+
    scale_fill_gradient(low = "white",high = "steelblue")+
    geom_text(aes(label=paste(x,"\n",paste(round(y*100,0),"%",sep=""),sep="")),family="Helvetica Light")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    theme(legend.position = "none")
)
showtext_end()
dev.off()

########### INFRAESTRUCTURA Y VEHUCILOS #############################################################
#Accidentes por tipo de infraestructura/Diseño----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(AccidentesBiciTotal$Accidentes.TipoDiseno2),FUN="length")
aux$y<-aux$x/sum(aux$x)
aux$Group.1<-factor(aux$Group.1, aux$Group.1[order(aux$x,decreasing=FALSE)])
svg("./GRAFICOS/AccidenteTipoInfraestructura.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data=aux,aes(x=Group.1,y=x))+xlab("Type of infraestructure")+
    geom_bar(stat="identity", fill = "#4e78a6")+ylim(c(0,4250))+ylab("Cyclists’ casualties")+
    geom_text(aes(label=paste(x," (",round(y*100,1)," %",")",sep="")),size=3,hjust=-0.05,family="Helvetica Light")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    coord_flip()
)
showtext_end()
dev.off()

#Muertes por tipo de infraestructura/Diseño----
aux<-AccidentesBiciTotal[AccidentesBiciTotal$Gravedad2=="Dead",]
aux<-aggregate(aux$Accidente,by=list(aux$Accidentes.TipoDiseno2),FUN=length)
aux$y<-aux$x/sum(aux$x)
aux$Group.1<-factor(aux$Group.1, aux$Group.1[order(aux$x,decreasing=FALSE)])
svg("./GRAFICOS/MuerteTipoInfraestructura.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data=aux,aes(x=Group.1,y=x))+xlab("Type of infraestructure")+
    geom_bar(stat="identity", fill = "#4e78a6")+ylim(c(0,160))+ylab("Cyclists’ deaths")+
    geom_text(aes(label=paste(x," (",round(y*100,1)," %",")",sep="")),size=3,hjust=-0.05,family="Helvetica Light")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    coord_flip()
)
showtext_end()
dev.off()

#Tipos de vehiculos relacionados con accidentes de biciusuarios----
aux<-AccidentesBiciTotal[duplicated(AccidentesBiciTotal$KEY),c("KEY","ClaseVehiculo2")] %>% st_set_geometry(NULL)
relacionados<-as.data.frame(rbind(AccidentesTotal[AccidentesTotal$KEY %in% unique(AccidentesBiciTotal$KEY) & AccidentesTotal$ClaseVehiculo!="Bicicleta",c("KEY","ClaseVehiculo2")],aux))
aux<-aggregate(relacionados$KEY,by=list(relacionados$ClaseVehiculo2),FUN=length)
rm(relacionados)
aux$Group.1<-factor(aux$Group.1, aux$Group.1[order(aux$x,decreasing=FALSE)])
aux$y<-aux$x/sum(aux$x)
svg("./GRAFICOS/vehiclesAtIncident.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data=aux,aes(x=Group.1,y=x))+xlab("Type of vehicles")+
    geom_bar(stat="identity", fill = "#4e78a6")+ylab("Cyclists’ casualties")+ylim(c(0,2800))+
    geom_text(aes(label=paste(x," (",round(y*100,1)," %",")",sep="")),size=3,hjust=-0.05,family="Helvetica Light")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    coord_flip()
)
showtext_end()
dev.off()
rm(relacionados)

#Tipos de vehiculos relacionados con muertes de biciusuarios----
aux1<-AccidentesBiciTotal[AccidentesBiciTotal$Gravedad2=="Dead",] %>% st_set_geometry(NULL)
aux<-aux1[duplicated(aux1$KEY),c("KEY","ClaseVehiculo2")]
relacionados<-as.data.frame(rbind(AccidentesTotal[AccidentesTotal$KEY %in% unique(aux1$KEY) & AccidentesTotal$ClaseVehiculo!="Bicicleta",c("KEY","ClaseVehiculo2")],aux))
aux<-aggregate(relacionados$KEY,by=list(relacionados$ClaseVehiculo2),FUN=length)
rm(relacionados,aux1)
aux$Group.1<-factor(aux$Group.1, aux$Group.1[order(aux$x,decreasing=FALSE)])
aux$y<-aux$x/sum(aux$x)
svg("./GRAFICOS/vehiclesAtDeath.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot(data=aux,aes(x=Group.1,y=x))+xlab("Type of vehicles")+
    geom_bar(stat="identity", fill = "#4e78a6")+ylab("Cyclists’ deaths")+ylim(c(0,80))+
    geom_text(aes(label=paste(x," (",round(y*100,1)," %",")",sep="")),size=3,hjust=-0.05,family="Helvetica Light")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+
    coord_flip()
)
showtext_end()
dev.off()
rm(relacionados)

########### MAPAS ACCIDENTES ########################################################################
#Visualización de los accidentes integrados----
mapAccidents<-tm_shape(Localidades[-9,])+tm_polygons(col="LocNombre",style="cat",alpha=0.1,palette=brewer.pal(9,"Greens"),legend.show = FALSE)+tm_shape(red_cicloruta)+tm_lines(col="#111E6C")+tm_shape(AccidentesBiciTotal)+tm_dots(col="Source",style="cat",title = "Source",palette=c("#7c7b7d","#4e78a6","#f4b843"),popup.vars=c("Date"="Accidentes.HoraOcurrencia", "Severity"="Gravedad", "Adress"="Accidentes.Direccion","Gender"="Sexo","Age"="Edad"))
tmap_leaflet(mapAccidents)
save(mapAccidents,file="mapAccidents.Rdata")
rm(mapAccidents)

#Visualización de la mortalidad----
mapDeaths<-tm_shape(Localidades[-9,])+tm_polygons(col="LocNombre",style="cat",alpha=0.1,palette=brewer.pal(9,"Greens"),legend.show = FALSE)+tm_shape(red_cicloruta)+tm_lines(col="#111E6C")+tm_shape(AccidentesBiciTotal)+tm_dots(col="Gravedad2",style="cat",title = "Severity",showNA=FALSE,palette=c("#C0C0C0","#FF0000","#FFFFFF"),popup.vars=c("Date"="Accidentes.HoraOcurrencia", "Severity"="Gravedad", "Adress"="Accidentes.Direccion","Gender"="Sexo","Age"="Edad"))
tmap_leaflet(mapDeaths)
save(mapDeaths,file="mapDeaths.Rdata")
rm(mapDeaths)

#Heat-map de accidentes por ZAT----
aux1<-st_join(AccidentesBiciTotal,zat_loca[,c("Zn_Nm_N")],left=TRUE,largest=TRUE)
aux<-aux1 %>% group_by(Zn_Nm_N) %>% summarise(Accidentes=n()) %>% st_set_geometry(NULL)
aux<-merge(zat_loca,aux,by="Zn_Nm_N",all.x=TRUE)
aux$Accidentes[is.na(aux$Accidentes)]<-0
colfunc <- colorRampPalette(c("white", "yellow2","red4"))
hmapAccidents<-tm_shape(aux[aux$LocNmbr!="SUMAPAZ",])+tm_polygons(border.alpha = 0,title="Casualties",col="Accidentes",style="cont",palette=colfunc(length(unique(aux$Accidentes))),alpha=0.5,popup.vars=c("ZAT"="Zn_Nm_N", "Casualties"="Accidentes"))
tmap_leaflet(hmapAccidents)
save(hmapAccidents,file="hmapAccidents.Rdata")
rm(hmapAccidents)

#Heat-map de muertes por ZAT----
if(!"aux1"%in%ls()){aux1<-st_join(AccidentesBiciTotal,zat_loca[,c("Zn_Nm_N")],left=TRUE,largest=TRUE)}
aux<-aux1[aux1$Gravedad2=="Dead",] %>% group_by(Zn_Nm_N) %>% summarise(Accidentes=n()) %>% st_set_geometry(NULL)
aux<-merge(zat_loca,aux,by="Zn_Nm_N",all.x=TRUE)
aux$Accidentes[is.na(aux$Accidentes)]<-0
colfunc <- colorRampPalette(c("white", "yellow2","red4"))
hmapDeaths<-tm_shape(aux[aux$LocNmbr!="SUMAPAZ",],name = "ZAT")+tm_polygons(border.alpha = 0,title="Deaths",col="Accidentes",style="cont",palette=colfunc(length(unique(aux$Accidentes))),alpha=0.5,popup.vars=c("ZAT"="Zn_Nm_N", "Deaths"="Accidentes"))
tmap_leaflet(hmapDeaths)
save(hmapDeaths,file="hmapDeaths.Rdata")
rm(hmapDeaths)

#Visualización de las rutas----
# zat_src=561
# zat_dst=259
# tmap_leaflet(tm_shape(zat_loca)+tm_polygons(col="lightgray",alpha=0.3,size=1)+tm_shape(zat_loca)+tm_dots(col="gray40",size=0.01)+tm_shape(zat_loca[zat_loca$Zn_Nm_N==zat_src|zat_loca$Zn_Nm_N==zat_dst,])+tm_dots(col="red",size=0.03)+tm_shape(bike_dist_zat2zat[bike_dist_zat2zat$id==paste(zat_src,zat_dst,sep="_"),])+tm_lines(col="blue",lwd=3))

########### OTROS ###################################################################################
#Número de vehiculos relacionados con accidentes de biciusuarios----
aux1<-AccidentesBiciTotal[AccidentesBiciTotal$Gravedad2=="Dead",] %>% st_set_geometry(NULL)
aux<-aux1[duplicated(aux1$KEY),c("KEY","ClaseVehiculo2")]
relacionados<-as.data.frame(rbind(AccidentesTotal[AccidentesTotal$KEY %in% unique(aux1$KEY) & AccidentesTotal$ClaseVehiculo!="Bicicleta",c("KEY","ClaseVehiculo2")],aux))
relaDead<-aggregate(relacionados$KEY,by=list(relacionados$KEY),FUN=length)
aux1<-AccidentesBiciTotal[AccidentesBiciTotal$Gravedad2=="Not Dead",] %>% st_set_geometry(NULL)
aux<-aux1[duplicated(aux1$KEY),c("KEY","ClaseVehiculo2")]
relacionados<-as.data.frame(rbind(AccidentesTotal[AccidentesTotal$KEY %in% unique(aux1$KEY) & AccidentesTotal$ClaseVehiculo!="Bicicleta",c("KEY","ClaseVehiculo2")],aux))
relaNDead<-aggregate(relacionados$KEY,by=list(relacionados$KEY),FUN=length)
rm(aux1)
svg("./GRAFICOS/vehiclesPerIncident.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(
  ggplot()+stat_count()+xlab("Number of vehicles at casualty")+scale_y_continuous(labels = scales::percent,limits=c(0,1))+ylab("Frecuency")+
    geom_histogram(data=relaNDead,aes(x = x,y=..density..,fill="Not Dead",color="Not Dead"),binwidth=1,alpha=0.3)+
    geom_histogram(data=relaDead,aes(x,y=..density..,fill="Dead",color="Dead"),binwidth=1,alpha=0.3)+
    scale_colour_manual(breaks=c("Dead","Not Dead"),values = c("orangered3","navy"),labels=c("Fatal","Nonfatal"))+labs(colour="")+
    scale_fill_manual(breaks=c("Dead","Not Dead"),values = c("orangered3","navy"),labels=c("Fatal","Nonfatal"))+labs(fill="")+
    theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey40"))
)
showtext_end()
dev.off()
rm(relaDead,relaNDead,relacionados)
