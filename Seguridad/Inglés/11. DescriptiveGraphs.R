#Preparación del entorno de trabajo----
rm(list=ls())
carpetaRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/Uniandes - RAS - SDM"
gitRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/proBikePolicies"
require(pacman)
pacman::p_load(readxl)
pacman::p_load(gdata)
pacman::p_load(plyr)
pacman::p_load(dplyr)
pacman::p_load(ggplot2)
pacman::p_load(ggpmisc)
pacman::p_load(RColorBrewer)
pacman::p_load(tidyr)
pacman::p_load(reshape)
pacman::p_load(padr)
pacman::p_load(lubridate)
pacman::p_load(showtext)
pacman::p_load(sf)
pacman::p_load(tmap)
font_add("Helvetica Light",paste(gitRAS,"/Seguridad/Helvetica Light.ttf",sep=""))
setwd(paste(carpetaRAS,"/RESULTADOS/SEGURIDAD/Resultados-Inglés",sep=""))
cmHeight<-7
cmWidth<-10

#Lectura de bases de datos originales auxiliares----
Localidades<-st_read(paste(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR0318.gdb",sep=""),layer="Loca",crs=4326)
zat_loca<-st_read(paste0(carpetaRAS,"/RESULTADOS/GENERAL/GEO-DATA/ZATs_Localidad.shp"),crs=4326)
red_cicloruta<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR0318.gdb"),layer = "RBic",type = 5,crs=4326) %>% st_transform(4326) %>% st_cast("LINESTRING",do_split = TRUE)
red_cicloruta$LENGTH_GEO<-as.numeric(st_length(red_cicloruta))
Bikeway<-st_join(red_cicloruta,Localidades,join=st_intersects,left=TRUE) %>% st_set_geometry(NULL)
poblacion <- read_excel(paste(carpetaRAS,"/BASES DE DATOS/DANE/DICE015A-ProyeccionesLocalidades-2016.xls.xlsx",sep=""), sheet = "Serie_regularizada_SDP", range = "A6:AK26")
names(poblacion)[names(poblacion)=="Localidad"]<-"LocNombre"
auxPopYear<-as.data.frame(cbind(as.numeric(names(poblacion[,2:37])),colSums(poblacion[,2:37])))
names(auxPopYear)<-c("Group.1","Population")
viajesbici<-read.csv(paste(carpetaRAS,"/RESULTADOS/GENERAL/TABLAS/Viajes_Localidad.csv",sep=""),encoding = "UTF-8")
viajesbici<-viajesbici[,c("LocCodigo","VIAJES_ORIGEN_zona","VIAJES_DEST_zona","LocNombre")]

#Leer bases de datos preprocesada----
load(paste(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/1. AccidentesDespacio2011_2015.Rdata",sep=""))
rm(AccidentesBici)
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/3. AccidentesBiciTotalGeoData.Rdata"))
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/4. Edad-Sexo-Defunciones-Poblacion-Bogota.Rdata"))
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/4. GruposEdadCodificacion.Rdata"))
load(paste0(carpetaRAS,"/RESULTADOS/GENERAL/TABLAS/6. biciusuarios.Rdata"))
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/7. IADR.Rdata"))
ciclTime<-read.csv(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Resultados/TABLAS/5. lengthCiclTime.csv"),encoding="Latin1",fileEncoding = "Latin1",stringsAsFactors = FALSE)
ciclTime$Fecha<-as.Date(ciclTime$Fecha)
ciclTime$X<-NULL

#Visualización de los accidentes integrados----
mapAccidents<-tm_shape(Localidades[-9,])+tm_polygons(col="LocNombre",style="cat",alpha=0.1,palette=brewer.pal(19,"Greens"),legend.show = FALSE)+tm_shape(red_cicloruta)+tm_lines(col="#111E6C")+tm_shape(AccidentesBiciTotal)+tm_dots(col="Source",style="cat",title = "Source",palette=c("#7c7b7d","#4e78a6","#f4b843"),popup.vars=c("Date"="Accidentes.HoraOcurrencia", "Severity"="Gravedad", "Adress"="Accidentes.Direccion","Gender"="Sexo","Age"="Edad"))
save(mapAccidents,file="mapAccidents.Rdata")
tmap_leaflet(mapAccidents)
rm(mapAccidents)

#Visualización de las rutas----
zat_src=110
zat_dst=907
w<-tm_shape(zat_loca)+tm_polygons(col="lightgray",alpha=0.3)+tm_shape(zat_loca[zat_loca$Zn_Nm_N==zat_src|zat_loca$Zn_Nm_N==zat_dst,])+tm_dots(col="red")+tm_shape(bike_dist_zat2zat[bike_dist_zat2zat$id==paste(zat_src,zat_dst,sep="_"),])+tm_lines(col="blue")
tmap_leaflet(w)

#Kilómetros de infraestructura en el tiempo----
evol<-ggplot(ciclTime,aes(x=Fecha,y=Mts/1000))+geom_point(color="#4e78a6",size = 3)+geom_line(color="#4e78a6",linetype="dashed")+geom_text(aes(label=round(Mts/1000,1)),size=3.2,hjust=1,vjust=-1,family="Helvetica Light",check_overlap = TRUE)+theme_minimal()+ylab("Bikeway KM")+xlab("Year")+scale_x_date(date_labels="%Y")+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))
svg("./GRAFICOS/Bikeway_evol.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(evol)
showtext_end()
dev.off()
rm(evol)

#Kilómetros de infraestructura por localidad----
KmxLocalidad<-Bikeway[,c("LocNombre","LENGTH_GEO")]
KmxLocalidad<-aggregate(KmxLocalidad$LENGTH_GEO, by=list(KmxLocalidad$LocNombre),FUN=sum)
KmxLocalidad$x<-KmxLocalidad$x/1000
colnames(KmxLocalidad)<-c("LocNombre","Km")
KmxLocalidad[1,1]<-"ANTONIO NARIÑO"
KmxLocalidad$LocNombre<-factor(KmxLocalidad$LocNombre, KmxLocalidad$LocNombre[order(KmxLocalidad$Km,decreasing=FALSE)])
KmxLocalidadGraph<-ggplot(data=KmxLocalidad,aes(x=LocNombre,y=Km))+geom_bar(stat="identity", fill = "#4e78a6")+coord_flip()+theme_minimal()+ylab("Bikeway kilometers (Km)")+geom_text(aes(label=paste(round(Km,2),"Km")),size=3,hjust=-0.05,family="Helvetica Light")+theme(text=element_text(family="Helvetica Light",color="grey"))+ylim(c(0,87))+xlab("Locality (District)")
svg("./GRAFICOS/Kilometros de infraestructura por localidad.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(KmxLocalidadGraph)
showtext_end()
dev.off()
rm(KmxLocalidadGraph)

#Kilometros de infraestructura por localidad estandarizado por población----
KmxLocalidad<-merge(KmxLocalidad,poblacion,by="LocNombre",all.x=TRUE)
KmxLocalidad$EstanPop<-KmxLocalidad$Km*100000/KmxLocalidad$`2017`
KmxLocalidad$LocNombre<-factor(KmxLocalidad$LocNombre, KmxLocalidad$LocNombre[order(KmxLocalidad$EstanPop,decreasing=FALSE)])
KmxLocalidadGraphPop<-ggplot(data=KmxLocalidad,aes(x=LocNombre,y=EstanPop))+geom_bar(stat="identity", fill = "#4e78a6")+coord_flip()+theme_minimal()+ylab("Bikeway kilometers (Km) per 100.000 people")+geom_text(aes(label=paste(round(EstanPop,2),"Km")),size=3,hjust=-0.05,family="Helvetica Light")+theme(text=element_text(family="Helvetica Light",color="grey"))+ylim(c(0,27))+xlab("Locality (District)")
svg("./GRAFICOS/Kilometros de infraestructura por localidad Pop.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(KmxLocalidadGraphPop)
showtext_end()
dev.off()
rm(KmxLocalidadGraphPop)

#Kilometros de infraestructura por localidad estandarizado por biciusuarios----
biciAux<-aggregate(biciusuarios$BICIUSRSINT,by=list(biciusuarios$LOCALIDAD,getYear(biciusuarios$FECHA)),FUN=sum) %>% filter(Group.2==2017) %>% select(LocNombre=Group.1,f_bicicleta=x)
KmxLocalidad<-merge(KmxLocalidad,biciAux,by="LocNombre",all.x=TRUE)
KmxLocalidad$EstanBici<-KmxLocalidad$Km*1000/KmxLocalidad$f_bicicleta
KmxLocalidad$LocNombre<-factor(KmxLocalidad$LocNombre, KmxLocalidad$LocNombre[order(KmxLocalidad$EstanBici,decreasing=FALSE)])
KmxLocalidadGraphBici<-ggplot(data=KmxLocalidad,aes(x=LocNombre,y=EstanBici))+geom_bar(stat="identity", fill = "#4e78a6")+coord_flip()+theme_minimal()+ylab("Bikeway kilometers (Km) per 1.000 bicycle users")+geom_text(aes(label=paste(round(EstanBici,2),"Km")),size=3,hjust=-0.05,family="Helvetica Light")+theme(text=element_text(family="Helvetica Light",color="grey"))+ylim(c(0,7))+xlab("Locality (District)")
svg("./GRAFICOS/Kilometros de infraestructura por localidad Bici.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(KmxLocalidadGraphBici)
showtext_end()
dev.off()
rm(KmxLocalidadGraphBici)

#Gráfica de tendencias de biciusuarios----
biciAux<-aggregate(biciusuarios[,c("BICIUSRS","BICIUSRSINT")],by=list(biciusuarios$FECHA),FUN=sum)
evol<-ggplot(biciAux[!is.na(biciAux$BICIUSRS),],aes(x=Group.1,y=BICIUSRS))+geom_point(color="#4e78a6",size = 3)+geom_line(color="#4e78a6",linetype="dashed")+geom_text(aes(label=paste(getYear(Group.1),round(BICIUSRS),sep=" - ")),size=3.2,hjust=0.5,vjust=-1.5,family="Helvetica Light",check_overlap = TRUE)+theme_minimal()+ylab("Cyclists")+xlab("Year")+scale_x_date(date_labels="%Y")+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))
svg("./GRAFICOS/Bikeusers_evol.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(evol)
showtext_end()
dev.off()
rm(evol,biciAux)

#Gráfica de tendencia de biciusuarios por género----
biciusuariosGender<-aggregate(biciusuarios$BICIUSRS,by=list(getYear(biciusuarios$FECHA),biciusuarios$SEXO),FUN=sum,na.rm=TRUE) %>% mutate(x=replace(x,x==0,NA)) %>% na.omit %>% mutate(Group.1=as.Date(paste0(Group.1,"-01-01")))
trends<-ggplot(biciusuariosGender,aes(x=Group.1,y=x,colour=Group.2))+geom_point()+geom_line(linetype="dashed")+theme_minimal()+ylab("Bike users")+xlab("Year")+scale_x_date(breaks = as.Date(c("2005-01-01","2011-01-01","2014-01-01","2015-01-01","2017-01-01")),date_labels="%Y")+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+scale_color_brewer(palette="Accent")+theme(legend.text=element_text(colour="black",size=12,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"),axis.text.x = element_text(angle = 90,vjust=0.5))+labs(colour="Gender") 
svg("./GRAFICOS/Bikeusers_gender_evol.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(trends)
showtext_end()
dev.off()
rm(biciusuariosGender,trends)

#Gráfica de tendencias de biciusuarios por género y grupo de edad----
biciAux<-aggregate(biciusuarios[,c("BICIUSRS")],by=list(biciusuarios$FECHA,biciusuarios$GR_EDAD,biciusuarios$SEXO),FUN=sum) %>% na.omit()
names(biciAux)<-c("FECHA","GR_EDAD","SEXO","BICIUSRS")
trends<-ggplot(biciAux,aes(x=FECHA,y=BICIUSRS/10000,colour=SEXO))+geom_point()+geom_line(linetype="dashed")+facet_wrap(~GR_EDAD)+ theme_minimal()+ylab("Bike users (x 10.000)")+xlab("Year")+scale_x_date(breaks = as.Date(c("2005-01-01","2011-01-01","2014-01-01","2015-01-01","2017-01-01")),date_labels="%Y")+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+scale_color_brewer(palette="Accent")+theme(legend.position = c(0.9, 0.07),legend.text=element_text(colour="black",size=12,family="Helvetica Light"),legend.title=element_text(colour="black",family="Helvetica Light"),axis.text.x = element_text(angle = 90,vjust=0.5))+labs(colour="Gender") 
svg("./GRAFICOS/Bikeusers_age_gender_evol.svg",width = cmWidth,height = cmHeight)
showtext_begin()
print(trends)
showtext_end()
dev.off()
rm(trends,biciAux)

#Total de accidentes durante los años----
TotalAccidentesAño <- aggregate(AccidentesBiciTotal$Accidente,by=list(getYear(AccidentesBiciTotal$Accidentes.Fecha)),FUN=length)
TotalAccidentesAño$Group.1<-as.numeric(TotalAccidentesAño$Group.1)
TotalAccidentesAñoGraph <- ggplot(data = TotalAccidentesAño, aes(x=Group.1,y = x)) + geom_line(group=1, color="navy") + theme_minimal()+ylab("Cyclists’ deaths and injuries")+xlab("Year")+theme(axis.text.x = element_text(angle = 0),legend.position = c(0.84,0.2))+geom_text(aes(label=x),size=3,vjust=-2,family="Helvetica Light")+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+ylim(c(1000,1600))+scale_x_continuous(limits=c(2011,2015))
svg("./GRAFICOS/TotalAccidentesAño.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(TotalAccidentesAñoGraph)
showtext_end()
dev.off()
rm(TotalAccidentesAñoGraph)

#Total de accidentes durante los años estandarizado por población----
TotalAccidentesAño <- merge(TotalAccidentesAño,auxPopYear,by="Group.1",all.x=TRUE)
TotalAccidentesAño$EstanPop<-TotalAccidentesAño$x*100000/TotalAccidentesAño$Population
TotalAccidentesAñoPopGraph <- ggplot(data = TotalAccidentesAño, aes(x=Group.1,y = EstanPop)) + geom_line(group=1, color="navy") + theme_minimal()+ylab("Cyclists’ deaths and injuries per 100.000 people")+xlab("Year")+theme(axis.text.x = element_text(angle = 0),legend.position = c(0.84,0.2))+geom_text(aes(label=round(EstanPop,1)),size=3,vjust=-2,family="Helvetica Light")+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+ylim(c(12,21))+scale_x_continuous(limits=c(2011,2015))
svg("./GRAFICOS/TotalAccidentesAñoEstanPop.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(TotalAccidentesAñoPopGraph)
showtext_end()
dev.off()
rm(TotalAccidentesAño,TotalAccidentesAñoPopGraph)

#Total de accidentes durante los meses----
TotalAccidentesMes <- aggregate(AccidentesBiciTotal$Accidente,by=list(getYear(AccidentesBiciTotal$Accidentes.Fecha),getMonth(AccidentesBiciTotal$Accidentes.Fecha)),FUN=length)
TotalAccidentesMes$Mes<-as.Date(paste(TotalAccidentesMes$Group.1,TotalAccidentesMes$Group.2,"01",sep="/"),"%Y/%m/%d")
TotalAccidentesMes$Important<-ifelse(TotalAccidentesMes$Mes%in%as.Date(c("2012-02-01","2015-04-01","2015-08-01")),"Important","NA")
TotalAccidentesGraphMes <- ggplot(data = TotalAccidentesMes, aes(x=Mes,y = x)) + geom_line(group=1, color="navy") + theme_minimal()+ylab("Cyclists’ deaths and injuries")+xlab("Month-Year")+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+scale_x_date(date_labels = "%b-%Y",date_breaks = "1 year")+stat_smooth(method="loess")+geom_point(data=TotalAccidentesMes[TotalAccidentesMes$Important=="Important",],color="red",size=1.5)+geom_text(aes(label=ifelse(TotalAccidentesMes$Important=="Important",paste(getMonth(TotalAccidentesMes$Mes,"%b"),TotalAccidentesMes$x,sep=" - "),NA)),color="red",size=3,vjust=-1,family="Helvetica Light")
svg("./GRAFICOS/TotalAccidentesMes.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(TotalAccidentesGraphMes)
showtext_end()
dev.off()
rm(TotalAccidentesGraphMes)

#Total de accidentes durante los meses estandarizado por población----
TotalAccidentesMes <- merge(TotalAccidentesMes,auxPopYear,by="Group.1",all.x=TRUE)
TotalAccidentesMes$EstanPop<-TotalAccidentesMes$x*100000/TotalAccidentesMes$Population
TotalAccidentesGraphMesPop <- ggplot(data = TotalAccidentesMes, aes(x=Mes,y = EstanPop)) + geom_line(group=1, color="navy") + theme_minimal()+ylab("Cyclists’ deaths and injuries per 100.000 people")+xlab("Month-Year")+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+scale_x_date(date_labels = "%b-%Y",date_breaks = "1 year")+stat_smooth(method="loess")+geom_point(data=TotalAccidentesMes[TotalAccidentesMes$Important=="Important",],color="red",size=1.5)+geom_text(aes(label=ifelse(TotalAccidentesMes$Important=="Important",paste(getMonth(TotalAccidentesMes$Mes,"%b"),round(TotalAccidentesMes$EstanPop,1),sep=" - "),NA)),color="red",size=3,vjust=-1,family="Helvetica Light")
svg("./GRAFICOS/TotalAccidentesMesPop.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(TotalAccidentesGraphMesPop)
showtext_end()
dev.off()
rm(TotalAccidentesMes,TotalAccidentesGraphMesPop)

#Total de accidentes durante los dias----
TotalAccidentesDias <- aggregate(AccidentesBiciTotal$Accidente,by=list(AccidentesBiciTotal$Accidentes.Fecha),FUN=length)
TotalAccidentesDias<-pad(TotalAccidentesDias, interval="day") %>% mutate(x = replace(x, is.na(x), 0))
TotalAccidentesDias$Group.1<-as.Date(TotalAccidentesDias$Group.1)
TotalAccidentesGraphDia <- ggplot(data = TotalAccidentesDias, aes(x=Group.1,y = x))+geom_line(color="navy")+scale_x_date(date_labels = "%Y",date_breaks = "1 year")+ theme_minimal()+ylab("Cyclists’ deaths and injuries")+xlab("Day")+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))
svg("./GRAFICOS/TotalAccidentesDia.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(TotalAccidentesGraphDia)
showtext_end()
dev.off()
rm(TotalAccidentesGraphDia)

#Total de accidentes durante los dias estandarizado por población----
TotalAccidentesDias$Fecha<-as.Date(TotalAccidentesDias$Group.1)
TotalAccidentesDias$Group.1<-getYear(TotalAccidentesDias$Group.1)
TotalAccidentesDias <- merge(TotalAccidentesDias,auxPopYear,by="Group.1",all.x=TRUE)
TotalAccidentesDias$EstanPop<-TotalAccidentesDias$x*100000/TotalAccidentesDias$Population
TotalAccidentesGraphDiasPop <- ggplot(data = TotalAccidentesDias, aes(x=Fecha,y = EstanPop))+geom_line(color="navy")+scale_x_date(date_labels = "%Y",date_breaks = "1 year")+ theme_minimal()+ylab("Average Cyclists’ deaths and injuries per 100.000 people")+xlab("Day")+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))
svg("./GRAFICOS/TotalAccidentesDiasPop.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(TotalAccidentesGraphDiasPop)
showtext_end()
dev.off()
rm(TotalAccidentesDias,TotalAccidentesGraphDiasPop)

#Total accidentes por día de la semana y por año----
TotalAccidentesDias<-aggregate(AccidentesBiciTotal$Accidente,by=list(getYear(AccidentesBiciTotal$Accidentes.Fecha),weekdays(AccidentesBiciTotal$Accidentes.Fecha)),FUN=length)
TotalAccidentesDias$Group.2<-factor(as.factor(TotalAccidentesDias$Group.2),c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
TotalAccidentesGraphDias <- ggplot(data = TotalAccidentesDias, aes(x=Group.2,y = x,group=Group.1,colour=Group.1)) + geom_line() + theme_minimal()+ylab("Cyclists’ deaths and injuries")+xlab("Weekday")+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+scale_color_brewer(palette="Accent")+theme(legend.position = c(0.85, 0.82),legend.text=element_text(colour="black"),legend.title=element_text(colour="black"))+labs(colour="Year")
svg("./GRAFICOS/TotalAccidentesDiasSemana.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(TotalAccidentesGraphDias)
showtext_end()
dev.off()
rm(TotalAccidentesGraphDias)

#Total accidentes por día de la semana y por año estandarizado por poblacion----
TotalAccidentesDias <- merge(TotalAccidentesDias,auxPopYear,by="Group.1",all.x=TRUE)
TotalAccidentesDias$EstanPop<-TotalAccidentesDias$x*100000/TotalAccidentesDias$Population
TotalAccidentesGraphDias <- ggplot(data = TotalAccidentesDias, aes(x=Group.2,y = EstanPop,group=Group.1,colour=Group.1)) + geom_line() + theme_minimal()+ylab("Average Cyclists’ deaths and injuries per 100.000 people")+xlab("Weekday")+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+scale_color_brewer(palette="Accent")+theme(legend.position = c(0.85, 0.82),legend.text=element_text(colour="black"),legend.title=element_text(colour="black"))+labs(colour="Year")
svg("./GRAFICOS/TotalAccidentesDiasSemanaPop.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(TotalAccidentesGraphDias)
showtext_end()
dev.off()
rm(TotalAccidentesGraphDias)

#Total accidentes por día de la semana y por año en boxplot----
TotalAccidentesGraphDiasBoxplot <- ggplot(data = TotalAccidentesDias, aes(x=Group.2,y = x,colour=Group.2)) + geom_boxplot() + theme_minimal()+ylab("Cyclists’ deaths and injuries")+xlab("Weekday")+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+scale_color_brewer(palette="Dark2")+theme(legend.position = "none")
svg("./GRAFICOS/TotalAccidentesDiasSemanaBoxplot.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(TotalAccidentesGraphDiasBoxplot)
showtext_end()
dev.off()

#Total accidentes por día de la semana y por año en boxplot estandarizado por población----
TotalAccidentesGraphDiasBoxplot <- ggplot(data = TotalAccidentesDias, aes(x=Group.2,y = EstanPop,colour=Group.2)) + geom_boxplot() + theme_minimal()+ylab("Average Cyclists’ deaths and injuries per 100.000 people")+xlab("Weekday")+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+scale_color_brewer(palette="Dark2")+theme(legend.position = "none")
svg("./GRAFICOS/TotalAccidentesDiasSemanaBoxplotPop.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(TotalAccidentesGraphDiasBoxplot)
showtext_end()
dev.off()
rm(TotalAccidentesGraphDiasBoxplot,TotalAccidentesDias)

#Accidentes promedio por día de la semana y por año----
TotalAccidentesDias2 <- aggregate(AccidentesBiciTotal$Accidente,by=list(AccidentesBiciTotal$Accidentes.Fecha),FUN=length)
TotalAccidentesDias2<-pad(TotalAccidentesDias2, interval="day") %>% mutate(x = replace(x, is.na(x), 0))
TotalAccidentesDias2$Group.2<-weekdays(TotalAccidentesDias2$Group.1)
TotalAccidentesDias2$Group.2<-factor(as.factor(TotalAccidentesDias2$Group.2),c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
TotalAccidentesDias2$Year<-factor(getYear(TotalAccidentesDias2$Group.1))
svg("./GRAFICOS/PromedioAccidentesDiasSemanaBoxplot.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(ggplot(data=TotalAccidentesDias2,aes(x=Group.2,y=x,colour=Group.2))+geom_boxplot(outlier.size = -1)+facet_grid(~Year)+ylim(0,10)+theme_minimal()+theme(axis.text.x = element_text(angle = 90,hjust=1,vjust=0.5))+ylab("Average Cyclists’ deaths and injuries")+xlab("Weekday")+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+scale_color_brewer(palette="Dark2")+theme(legend.position = "none"))
showtext_end()
dev.off()
rm(TotalAccidentesDias2)

#Análisis de la hora de acurrencia del total de accidentes para un día (Wednesday)----
AccidentesMiercoles<-AccidentesBiciTotal[weekdays(AccidentesBiciTotal$Accidentes.Fecha)=="Wednesday",]
AccidentesDuranteDia<-aggregate(AccidentesMiercoles$Accidente,by=list(as.POSIXlt(AccidentesMiercoles$Accidentes.HoraOcurrencia)$hour),FUN=length)
AccidentesDuranteDia[23,]<-c(1,0)
AccidentesDuranteDiaGraph<-ggplot(data=AccidentesDuranteDia,aes(x=Group.1,y=x))+geom_line(color="navy")+theme_minimal()+ylab("Cyclists’ deaths and injuries")+theme(text=element_text(family="Helvetica Light",color="grey"))+xlab("Day hours")+xlim(c(0,23))+geom_text(aes(label=x),family="Helvetica Light",size=3,vjust=0,hjust=0)+geom_vline(xintercept=6, colour="red", linetype="dotted")+geom_vline(xintercept=18, colour="red", linetype="dotted")+geom_errorbarh(aes(xmin=6.1,xmax=17.9,y=97.5,colour="red"))+geom_text(aes(x=12.5,y=97.5,label="Work hours\n6:00 - 18:00"),family="Helvetica Light",size=5,vjust=0.5,hjust=0.5,color="red")+theme(legend.position="none")
svg("./GRAFICOS/AccidentsOnWednesday.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(AccidentesDuranteDiaGraph)
showtext_end()
dev.off()
rm(AccidentesMiercoles,AccidentesDuranteDia,AccidentesDuranteDiaGraph)

#Análisis de la hora de acurrencia de accidentes promedio por dia por año----
TotalAccidentesDias2 <- aggregate(AccidentesBiciTotal$Accidente,by=list(floor_date(AccidentesBiciTotal$Accidentes.HoraOcurrencia,"hour")),FUN=length)
TotalAccidentesDias2<-pad(TotalAccidentesDias2, interval="hour",start_val = as.POSIXct("2011-01-01 00:00:00",format="%Y-%m-%d %H:%M:%S"),end_val = as.POSIXct("2015-12-31 23:00:00",format="%Y-%m-%d %H:%M:%S")) %>% mutate(x = replace(x, is.na(x), 0))
TotalAccidentesDias2$Group.2<-weekdays(TotalAccidentesDias2$Group.1)
TotalAccidentesDias2$hora<-as.POSIXlt(TotalAccidentesDias2$Group.1)$hour
TotalAccidentesDias2$año<-getYear(TotalAccidentesDias2$Group.1)
TotalAccidentesDias2$Group.2<-factor(as.factor(TotalAccidentesDias2$Group.2),c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
TotalAccidentesDias2$Group.1<-as.Date(TotalAccidentesDias2$Group.1)
AccidentesDuranteDia<-aggregate(TotalAccidentesDias2$x,by=list(TotalAccidentesDias2$año,TotalAccidentesDias2$Group.2,TotalAccidentesDias2$hora),FUN=mean)
svg("./GRAFICOS/AccidentsbyWeekdayHour.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(ggplot(AccidentesDuranteDia,aes(x=Group.3,y=x))+geom_line(color="navy")+facet_grid(Group.2~Group.1)+theme_minimal()+ylab("Average Cyclists’ deaths and injuries")+theme(text=element_text(family="Helvetica Light",color="grey"))+xlab("Day hours")+xlim(c(0,23))+geom_vline(xintercept=6, colour="red", linetype="dotted")+geom_vline(xintercept=18, colour="red", linetype="dotted")+theme(legend.position="none"))
showtext_end()
dev.off()
rm(TotalAccidentesDias2,AccidentesDuranteDia)

#Total de accidentes por localidad por año----
TotalAccidentesLocalidad<-aggregate(AccidentesBiciTotal$Accidente,by=list(getYear(AccidentesBiciTotal$Accidentes.Fecha),AccidentesBiciTotal$Accidentes.Localidad),FUN=length)
names(TotalAccidentesLocalidad)<-c("Group.1","LocNombre","Accidentes")
TotalAccidentesLocalidad$Group.1<-as.numeric(TotalAccidentesLocalidad$Group.1)
TotalAccidentesLocalidad$LocNombre<-factor(TotalAccidentesLocalidad$LocNombre)
TotalAccidentesLocalidadGraph<-ggplot(data=TotalAccidentesLocalidad,aes(x=LocNombre,y=Accidentes))+geom_bar(stat="identity",fill="#4e78a6")+facet_grid(Group.1~.)+theme_minimal()+ylab("Cyclists’ deaths and injuries")+theme(text=element_text(family="Helvetica Light",color="grey"))+xlab("Locality (District)")+theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))+geom_text(aes(label=Accidentes),size=3,hjust=0.5,vjust=0,family="Helvetica Light")+ylim(c(0,300))
svg("./GRAFICOS/TotalAccidentesLocalidadPorAño.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(TotalAccidentesLocalidadGraph)
showtext_end()
dev.off()
rm(TotalAccidentesLocalidadGraph)

#Total de accidentes por localidad por año estandarizadas por población----
poblacionLong<-poblacion %>% gather(Group.1,Population,names(poblacion)[2:37])
TotalAccidentesLocalidad<-merge(TotalAccidentesLocalidad,poblacionLong,by=c("LocNombre","Group.1"),all.x=TRUE)
TotalAccidentesLocalidad$EstanPop<-TotalAccidentesLocalidad$Accidentes*100000/TotalAccidentesLocalidad$Population
TotalAccidentesLocalidadPopGraph<-ggplot(data=TotalAccidentesLocalidad,aes(x=LocNombre,y=EstanPop))+geom_bar(stat="identity",fill="#4e78a6")+facet_grid(Group.1~.)+theme_minimal()+ylab("Cyclists’ deaths and injuries per 100.000 people")+theme(text=element_text(family="Helvetica Light",color="grey"))+xlab("Locality (District)")+theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))+geom_text(aes(label=round(EstanPop,1)),size=3,hjust=0.5,vjust=0,family="Helvetica Light")+ylim(c(0,45))
svg("./GRAFICOS/TotalAccidentesLocalidadPorAñoPop.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(TotalAccidentesLocalidadPopGraph)
showtext_end()
dev.off()
rm(TotalAccidentesLocalidadPopGraph)

#Total de accidentes por localidad por año estandarizadas por biciusuarios----
biciAux<-aggregate(biciusuarios$BICIUSRSINT,by=list(biciusuarios$LOCALIDAD,getYear(biciusuarios$FECHA)),FUN=sum) %>% select(LocNombre=Group.1,Group.1=Group.2,f_bicicleta=x)
TotalAccidentesLocalidad<-merge(TotalAccidentesLocalidad,biciAux,by=c("LocNombre","Group.1"),all.x=TRUE)
TotalAccidentesLocalidad$EstanBici<-TotalAccidentesLocalidad$Accidentes*1000/TotalAccidentesLocalidad$f_bicicleta
TotalAccidentesLocalidadBiciGraph<-ggplot(data=TotalAccidentesLocalidad,aes(x=LocNombre,y=EstanBici))+geom_bar(stat="identity",fill="#4e78a6")+facet_grid(Group.1~.)+theme_minimal()+ylab("Cyclists’ deaths and injuries per 1.000 bycicle users")+theme(text=element_text(family="Helvetica Light",color="grey"))+xlab("Locality (District)")+theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))+geom_text(aes(label=round(EstanBici,1)),size=3,hjust=0.5,vjust=-0.2,family="Helvetica Light")+ylim(c(0,54))
svg("./GRAFICOS/TotalAccidentesLocalidadPorAñoBici.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(TotalAccidentesLocalidadBiciGraph)
showtext_end()
dev.off()
rm(TotalAccidentesLocalidadBiciGraph)

#Total de accidentes por localidad por año estandarizadas por kilómetros de infraestructura----
TotalAccidentesLocalidad<-merge(TotalAccidentesLocalidad,KmxLocalidad[,c(1:2)],by="LocNombre",all.x=TRUE)
TotalAccidentesLocalidad$EstanBici2<-TotalAccidentesLocalidad$Accidentes/TotalAccidentesLocalidad$Km
TotalAccidentesLocalidadBiciGraph<-ggplot(data=TotalAccidentesLocalidad,aes(x=LocNombre,y=EstanBici2))+geom_bar(stat="identity",fill="#4e78a6")+facet_grid(Group.1~.)+theme_minimal()+ylab("Cyclists’ deaths and injuries per bikeway km")+theme(text=element_text(family="Helvetica Light",color="grey"))+xlab("Locality (District)")+theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))+geom_text(aes(label=round(EstanBici2,1)),size=3,hjust=0.5,vjust=0,family="Helvetica Light")+ylim(c(0,15))
svg("./GRAFICOS/TotalAccidentesLocalidadPorAñoKm.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(TotalAccidentesLocalidadBiciGraph)
showtext_end()
dev.off()
rm(TotalAccidentesLocalidadBiciGraph)

#Tabla con los datos de resumen de accidentalidad para el año 2015----
Top<-TotalAccidentesLocalidad[TotalAccidentesLocalidad$Group.1==2015,][,c(1,4,5,6,7,8,9)]
names(Top)<-c("Locality","Population","Injuries per 100.000 people","Bicycle users","Injuries per 1.000 users","Bikeway Km","Injuries per bikeway km")
write.csv(Top,paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Resultados/TABLAS/11. AccidentalidadLocalidad.csv"))
rm(Top,TotalAccidentesLocalidad)

#Sexo de los involucrados en accidentes----
AccidentesBiciTotal$Sexo<-factor(AccidentesBiciTotal$Sexo,levels = c(levels(AccidentesBiciTotal$Sexo),"Female","Male","NA"))
AccidentesBiciTotal$Sexo[AccidentesBiciTotal$Sexo=="FEMENINO"]<-"Female"
AccidentesBiciTotal$Sexo[AccidentesBiciTotal$Sexo=="MASCULINO"]<-"Male"
AccidentesBiciTotal$Sexo[AccidentesBiciTotal$Sexo!="Female" & AccidentesBiciTotal$Sexo!="Male"]<-"NA"
sexoTotal<-aggregate(AccidentesBiciTotal$Accidente,by=list(AccidentesBiciTotal$Sexo),FUN=length)
sexoTotal[,2]<-sexoTotal[,2]*100/sum(sexoTotal[,2])
sexoTotal <- sexoTotal %>% mutate(pos = cumsum(x)- x/2.4)
sexoTotal$Group.1<-factor(sexoTotal$Group.1,c("Male","Female"))
sexoGraph<-ggplot(data=sexoTotal,aes(x="",y=x,fill=Group.1))+geom_bar(width=1,stat="identity")+ coord_polar(theta="y")+theme_minimal()+xlab("")+ylab("")+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+geom_text(aes(y = pos,label=c(paste(Group.1,"\n",round(x,2),"%",sep="")[1:2],NA)),family="Helvetica Light",size=5,color="white")+theme(axis.text.x=element_blank(),panel.grid=element_blank(),axis.ticks = element_blank())+theme(legend.position = "none")+scale_fill_brewer(palette="Paired")
svg("./GRAFICOS/sexoGraph.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(sexoGraph)
showtext_end()
dev.off()
rm(sexoTotal,sexoGraph)

#Gravedad del accidente para el conductor (Todos los Niveles)----
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
TotalAccidentesGravedad<-aggregate(AccidentesBiciTotal$Accidente,by=list(getYear(AccidentesBiciTotal$Accidentes.Fecha),AccidentesBiciTotal$Gravedad),FUN=length)
TotalAccidentesGravedad %<>%
  group_by(Group.1) %>%
  summarise(Sum=sum(x)) %>%
  left_join(TotalAccidentesGravedad,by="Group.1")
TotalAccidentesGravedad$x<-TotalAccidentesGravedad$x/TotalAccidentesGravedad$Sum
AccidentesSeveridad<-ggplot(data=TotalAccidentesGravedad,aes(x=Group.1,y=x,fill=Group.2))+geom_bar(stat="identity")+theme_minimal()+geom_text(aes(label=ifelse(round(x,2)>0,paste(round(x*100,1)," %"),NA)),check_overlap = TRUE, position = position_stack(vjust = 0.5),family="Helvetica Light",size=3.5)+ scale_y_continuous(labels = scales::percent)+theme(legend.position = "right")+labs(fill="")+xlab("")+ylab("Distribution of severity of injuries")+theme(text=element_text(family="Helvetica Light",size=12,color="grey40"))+scale_fill_brewer(palette="Paired")+geom_text(aes(x=Group.1,y=1.02,label=paste("N ",Sum)),family="Helvetica Light",size=3,color="grey40")+theme(legend.position="top")
svg("./GRAFICOS/severidadTotalAccidentes.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(AccidentesSeveridad)
showtext_end()
dev.off()
rm(TotalAccidentesGravedad,AccidentesSeveridad)
h<-aggregate(AccidentesBiciTotal$Accidente,by=list(AccidentesBiciTotal$Gravedad),FUN="length")
h$y<-h$x/sum(h$x)
write.csv(h,paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Resultados/TABLAS/11. SeveridadAccidentesTotal.csv"))
rm(h)

#Gravedad del accidente para el conductor (Binomial - Muerto/No Muerto)-----
AccidentesBiciTotal$Gravedad2<-factor(ifelse(AccidentesBiciTotal$Gravedad=="Dead","Dead","Not Dead"))
TotalAccidentesGravedad<-aggregate(AccidentesBiciTotal$Accidente,by=list(getYear(AccidentesBiciTotal$Accidentes.Fecha),AccidentesBiciTotal$Gravedad2),FUN=length)
TotalAccidentesGravedad %<>%
  group_by(Group.1) %>%
  summarise(Sum=sum(x)) %>%
  left_join(TotalAccidentesGravedad,by="Group.1")
TotalAccidentesGravedad$x<-TotalAccidentesGravedad$x/TotalAccidentesGravedad$Sum
AccidentesSeveridad<-ggplot(data=TotalAccidentesGravedad,aes(x=Group.1,y=x,fill=Group.2))+geom_bar(stat="identity")+theme_minimal()+geom_text(aes(label=ifelse(round(x,2)>0,paste(round(x*100,1)," %",sep=""),NA)),check_overlap = TRUE, position = position_stack(vjust = 0.5),family="Helvetica Light",size=4)+ scale_y_continuous(labels = scales::percent)+labs(fill="")+xlab("")+ylab("Distribution of severity of injuries")+theme(text=element_text(family="Helvetica Light",size=12,color="grey40"))+scale_fill_brewer(palette="Paired")+geom_text(aes(x=Group.1,y=1.02,label=paste("N ",Sum)),family="Helvetica Light",size=3,color="grey40")+theme(legend.position="top")
svg("./GRAFICOS/severidadBinomialAccidentes.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(AccidentesSeveridad)
showtext_end()
dev.off()
rm(TotalAccidentesGravedad,AccidentesSeveridad)

#Tipos de accidente registrados----
AccidentesBiciTotal$Accidentes.ClaseNombre<-as.character(AccidentesBiciTotal$Accidentes.ClaseNombre)
AccidentesBiciTotal$Accidentes.ClaseNombre[AccidentesBiciTotal$Accidentes.ClaseNombre=="CHOQUE"]<-"Choque"
AccidentesBiciTotal$Accidentes.ClaseNombre[AccidentesBiciTotal$Accidentes.ClaseNombre=="ATROPELLO"]<-"Atropello"
AccidentesBiciTotal$Accidentes.ClaseNombre[AccidentesBiciTotal$Accidentes.ClaseNombre=="VOLCAMIENTO"]<-"Volcamiento"
AccidentesBiciTotal$Accidentes.ClaseNombre[AccidentesBiciTotal$Accidentes.ClaseNombre=="CAIDA DE OCUPANTE"]<-"Caida Ocupante"
AccidentesBiciTotal$Accidentes.ClaseNombre[AccidentesBiciTotal$Accidentes.ClaseNombre=="OTRO"]<-"Otro"
AccidentesBiciTotal$Accidentes.ClaseNombre[AccidentesBiciTotal$Accidentes.ClaseNombre=="AUTOLESION"]<-"Autolesion"
AccidentesBiciTotal$Accidentes.ClaseNombre[AccidentesBiciTotal$Accidentes.ClaseNombre=="Caida de ocupante"]<-"Caida Ocupante"
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(getYear(AccidentesBiciTotal$Accidentes.Fecha),AccidentesBiciTotal$Accidentes.ClaseNombre),FUN=length)
aux %<>%
  group_by(Group.1) %>%
  summarise(Sum=sum(x)) %>%
  left_join(aux,by="Group.1")
aux$x<-aux$x/aux$Sum
AccidentesSeveridad<-ggplot(data=aux,aes(x=Group.1,y=x,fill=Group.2))+geom_bar(stat="identity")+theme_minimal()+geom_text(aes(label=ifelse(round(x,2)>0.01,paste(round(x*100,1)," %",sep=""),NA)),check_overlap = TRUE, position = position_stack(vjust = 0.5),family="Helvetica Light")+ scale_y_continuous(labels = scales::percent)+labs(fill="")+xlab("")+ylab("Distribution of types of incidents")+theme(text=element_text(family="Helvetica Light",size=12,color="grey40"))+scale_fill_brewer(palette="Paired")+geom_text(aes(x=Group.1,y=1.02,label=paste("N ",Sum)),family="Helvetica Light",size=3,color="grey40")+theme(legend.position="top")
svg("./GRAFICOS/TiposAccidentes.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(AccidentesSeveridad)
showtext_end()
dev.off()
rm(aux,AccidentesSeveridad)

#Género y gravedad del accidente----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(AccidentesBiciTotal$Sexo,AccidentesBiciTotal$Gravedad),FUN=length)
aux<-aux[aux$Group.1!="NA",]
aux$y<-aux$x/sum(aux$x)
svg("./GRAFICOS/SexoSeveridad.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(ggplot(aux,aes(x=Group.2,y=Group.1))+theme_minimal()+geom_tile(aes(fill=x),colour = "white") + scale_fill_gradient(low = "white",high = "steelblue")+geom_text(aes(label=paste(x,"\n",round(y*100,0)," %",sep=""),family="Helvetica Light"))+theme(legend.position = "none")+xlab("Severity")+ylab("Gender")+theme(text=element_text(family="Helvetica Light",size=12,color="grey")))
showtext_end()
dev.off()
rm(aux)

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
print(ggplot(aux,aes(x=Group.2,y=Group.1))+theme_minimal()+geom_tile(aes(fill=x),colour = "white") + scale_fill_gradient(low = "white",high = "steelblue")+geom_text(aes(label=paste(x,"\n",round(y*100,0)," %",sep=""),family="Helvetica Light"))+theme(legend.position = "none")+xlab("Severity")+ylab("Gender")+theme(text=element_text(family="Helvetica Light",size=12,color="grey")))
showtext_end()
dev.off()
rm(aux)

#Edad del conductor involucrado----
AccidentesBiciTotal$Edad[AccidentesBiciTotal$Edad=="SIN INFORMACION"]<-NA
AccidentesBiciTotal$Edad<-as.numeric(AccidentesBiciTotal$Edad)
histogramaEdad<-ggplot()+geom_histogram(data=AccidentesBiciTotal,aes(Edad),fill="#4e78a6",color="navy",binwidth=5)+stat_count()+xlab("Age")+ylab("Count")+theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))
svg("./GRAFICOS/histogramaEdad.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(histogramaEdad)
showtext_end()
dev.off()
rm(histogramaEdad)
write.csv(mean(AccidentesBiciTotal$Edad,na.rm=TRUE),paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Resultados/TABLAS/11. EdadPromedioConducAccidentes.csv"))

#Tile de severidad y edad en grupos quinquenales por género----
AccidentesBiciTotal$EdadCategorica<-factor(apply(as.data.frame(AccidentesBiciTotal$Edad),MARGIN=1,FUN=encodeEdad2,g=gruposEdad),levels = paste0(gruposEdad$ED_MIN,"-",gruposEdad$ED_MAX))
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(AccidentesBiciTotal$EdadCategorica,AccidentesBiciTotal$Gravedad,AccidentesBiciTotal$Sexo),FUN=length)
aux<-aux[aux$Group.3!="NA",]
svg("./GRAFICOS/EdadSeveridadSexo.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(ggplot(aux,aes(x=Group.2,y=Group.1))+theme_minimal()+geom_tile(aes(fill=x),colour = "white") + scale_fill_gradient(low = "white",high = "steelblue")+geom_text(aes(label=x),family="Helvetica Light")+theme(legend.position = "none")+xlab("Severity")+ylab("Age")+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+facet_grid(~Group.3))
showtext_end()
dev.off()
rm(aux)

#Tile de severidad binomial y edad en grupos quinquenales por género----
aux<-aggregate(AccidentesBiciTotal$Accidente,by=list(AccidentesBiciTotal$EdadCategorica,AccidentesBiciTotal$Gravedad2,AccidentesBiciTotal$Sexo),FUN=length)
aux<-aux[aux$Group.3!="NA",]
svg("./GRAFICOS/EdadSeveridadBinomialSexo.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(ggplot(aux,aes(x=Group.2,y=Group.1))+theme_minimal()+geom_tile(aes(fill=x),colour = "white") + scale_fill_gradient(low = "white",high = "steelblue")+geom_text(aes(label=x),family="Helvetica Light")+theme(legend.position = "none")+xlab("Severity")+ylab("Age")+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))+facet_grid(~Group.3))
showtext_end()
dev.off()
rm(aux)

#Uso del casco y gravedad (Todos los niveles)----
AccidentesBiciTotal$LLevaCasco[AccidentesBiciTotal$LLevaCasco=="S"]<-"Yes"
AccidentesBiciTotal$LLevaCasco[AccidentesBiciTotal$LLevaCasco=="N"]<-"No"
h<-aggregate(AccidentesBiciTotal$Accidente,by=list(AccidentesBiciTotal$LLevaCasco,AccidentesBiciTotal$Gravedad),FUN="length")
h<-h[h$Group.1!="SIN INFORMACION",]
h %<>%
  group_by(Group.1) %>%
  summarise(Sum=sum(x)) %>%
  left_join(h,by="Group.1") %>%
  mutate(y=x/Sum)
svg("./GRAFICOS/UsoCascoSeveridad.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(ggplot(h,aes(x=Group.2,y=Group.1))+theme_minimal()+geom_tile(aes(fill=y),colour = "white") + scale_fill_gradient(low = "white",high = "steelblue")+geom_text(aes(label=paste(x,"\n",paste(round(y*100,0),"%",sep=""),sep="")),family="Helvetica Light")+theme(legend.position = "none")+xlab("Severity")+ylab("Helmet use")+theme(text=element_text(family="Helvetica Light",size=12,color="grey")))
showtext_end()
dev.off()
rm(h)

#Uso del casco y gravedad (Binomial)----
h<-aggregate(AccidentesBiciTotal$Accidente,by=list(AccidentesBiciTotal$LLevaCasco,AccidentesBiciTotal$Gravedad2),FUN="length")
h<-h[h$Group.1!="SIN INFORMACION",]
h %<>%
  group_by(Group.1) %>%
  summarise(Sum=sum(x)) %>%
  left_join(h,by="Group.1") %>%
  mutate(y=x/Sum)
svg("./GRAFICOS/UsoCascoSeveridadBinomial.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(ggplot(h,aes(x=Group.2,y=Group.1))+theme_minimal()+geom_tile(aes(fill=y),colour = "white") + scale_fill_gradient(low = "white",high = "steelblue")+geom_text(aes(label=paste(x,"\n",paste(round(y*100,0),"%",sep=""),sep="")),family="Helvetica Light")+theme(legend.position = "none")+xlab("Severity")+ylab("Helmet use")+theme(text=element_text(family="Helvetica Light",size=12,color="grey")))
showtext_end()
dev.off()
rm(h)

#Accidentes por tipo de infraestructura/Diseño----
AccidentesBiciTotal$Accidentes.TipoDiseno[AccidentesBiciTotal$Accidentes.TipoDiseno=="Interseccion"]<-"Intersección"
AccidentesBiciTotal$Accidentes.TipoDiseno[AccidentesBiciTotal$Accidentes.TipoDiseno=="Tramo de Via"]<-"Tramo de Vía"
AccidentesBiciTotal$Accidentes.TipoDiseno[AccidentesBiciTotal$Accidentes.TipoDiseno=="Via peatonal"]<-"Vía peatonal"
h<-aggregate(AccidentesBiciTotal$Accidente,by=list(AccidentesBiciTotal$Accidentes.TipoDiseno),FUN="length")
h$y<-h$x/sum(h$x)
h$Group.1<-factor(h$Group.1, h$Group.1[order(h$x,decreasing=FALSE)])
svg("./GRAFICOS/AccidenteTipoInfraestructura.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(ggplot(data=h,aes(x=Group.1,y=x))+geom_bar(stat="identity", fill = "#4e78a6")+coord_flip()+theme_minimal()+ylab("Cyclists’ deaths and injuries")+geom_text(aes(label=paste(x," (",round(y*100,1)," %",")",sep="")),size=3,hjust=-0.05,family="Helvetica Light")+theme(text=element_text(family="Helvetica Light",color="grey"))+ylim(c(0,2700))+xlab("Type of infraestructure"))
showtext_end()
dev.off()
rm(h)

#Tipos de vehiculos relacionados con accidentes de biciusuarios----
AccidentesTotales$ClaseVehiculo[AccidentesTotales$ClaseVehiculo=="Automovil"]<-"Automóvil"
AccidentesTotales$ClaseVehiculo[AccidentesTotales$ClaseVehiculo=="Bus Alimentador"]<-"Bus"
AccidentesTotales$ClaseVehiculo[AccidentesTotales$ClaseVehiculo=="Buseta"]<-"Bus"
AccidentesTotales$ClaseVehiculo[AccidentesTotales$ClaseVehiculo=="Camion, Furgon"]<-"Camion, Furgón"
AccidentesTotales$ClaseVehiculo[AccidentesTotales$ClaseVehiculo=="Camion, Furgón"]<-"Camión, Furgón"
AccidentesTotales$ClaseVehiculo[AccidentesTotales$ClaseVehiculo=="Microbus"]<-"Bus"
AccidentesTotales$ClaseVehiculo[AccidentesTotales$ClaseVehiculo=="Motociclo"]<-"Motocicleta"
AccidentesTotales$ClaseVehiculo[AccidentesTotales$ClaseVehiculo=="No identificado"]<-"Otro"
AccidentesTotales$ClaseVehiculo[AccidentesTotales$ClaseVehiculo=="null"]<-"Otro"
AccidentesTotales$ClaseVehiculo[AccidentesTotales$ClaseVehiculo=="Tractocamion"]<-"Tractocamión"
AccidentesTotales$ClaseVehiculo[AccidentesTotales$ClaseVehiculo=="Traccion animal"]<-"Tracción animal"
AccidentesTotales$KEY<-paste0(AccidentesTotales$Accidentes.HoraOcurrencia," - ",AccidentesTotales$Accidentes.Direccion)
aux1<-AccidentesBiciTotal[duplicated(AccidentesBiciTotal$KEY),c("KEY","ClaseVehiculo")]
st_geometry(aux1)<-NULL
relacionados<-as.data.frame(rbind(AccidentesTotales[AccidentesTotales$KEY %in% unique(AccidentesBiciTotal$KEY) & AccidentesTotales$ClaseVehiculo!="Bicicleta",c("KEY","ClaseVehiculo")],aux1))
h<-aggregate(relacionados$KEY,by=list(relacionados$ClaseVehiculo),FUN=length)
h$Group.1<-factor(h$Group.1, h$Group.1[order(h$x,decreasing=FALSE)])
h$y<-h$x/sum(h$x)
svg("./GRAFICOS/vehiclesAtIncident.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(ggplot(data=h,aes(x=Group.1,y=x))+geom_bar(stat="identity", fill = "#4e78a6")+coord_flip()+theme_minimal()+ylab("Cyclists’ deaths and injuries")+geom_text(aes(label=paste(x," (",round(y*100,1)," %",")",sep="")),size=3,hjust=-0.05,family="Helvetica Light")+theme(text=element_text(family="Helvetica Light",color="grey"))+ylim(c(0,2100))+xlab("Type of vehicles"))
showtext_end()
dev.off()
rm(h,aux1)

#Número de vehiculos relacionados con accidentes de biciusuarios----
h<-as.data.frame(table(relacionados$KEY))
h%<>%
  group_by(Freq) %>%
  summarise(sum=sum(Freq))
h$sum<-h$sum/sum(h$sum)
m<-as.data.frame(table(relacionados$KEY))
w<-ggplot()+geom_histogram(data=m,aes(Freq),fill="#4e78a6",color="navy",binwidth=1)+stat_count()+xlab("Number of vehicles per incident")+ylab("Count")+theme_minimal()+theme(text=element_text(family="Helvetica Light",size=12,color="grey"))
wd<-ggplot_build(w)
w<-w+geom_line(data=h,aes(x=Freq,y=cumsum(sum*max(wd$data[[1]]$y)),colour="#HFD6A02"))+scale_y_continuous(sec.axis = sec_axis(~./max(wd$data[[1]]$y), name = "Cumulative frequency",labels = scales::percent))+geom_text(data=h,aes(x=Freq,y=(cumsum(sum*max(wd$data[[1]]$y))-500),label=ifelse(Freq==3,paste(Freq,"\n",round(cumsum(sum)*100,0),"%"),NA),colour="#HFD6A02"),family="Helvetica Light")+theme(legend.position = "none")
svg("./GRAFICOS/vehiclesPerIncident.svg",width = cmWidth, height = cmHeight)
showtext_begin()
print(w)
showtext_end()
dev.off()
rm(m,h,w,wd,relacionados)

#--------
h<-aggregate(AccidentesBiciTotal$Accidente,by=list(AccidentesBiciTotal$Accidentes.TipoTiempo),FUN="length")
h$y<-h$x/sum(h$x)
h
#Almacenamiento de la base de datos con las correcciones inducidas durante los gráficos-----
save(AccidentesBiciTotal,file=paste(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/11. AccidentesBiciTotalGeoData_Standartized.Rdata",sep=""))
