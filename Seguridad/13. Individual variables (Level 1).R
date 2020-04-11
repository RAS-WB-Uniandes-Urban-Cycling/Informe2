#Preparación del entorno de trabajo----
rm(list=ls())
carpetaRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/Uniandes - RAS - SDM"
gitRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/proBikePolicies"
require(pacman)
pacman::p_load(gdata)
pacman::p_load(tidyverse)
pacman::p_load(lubridate)
pacman::p_load(sf)
pacman::p_load(dummies)
pacman::p_load(raster)

#Leer bases de datos preprocesada----
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/3. AccidentesBiciTotalGeoData.Rdata"))
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/3. AccidentesTotal2011_2017.Rdata"))

#Lectura de bases de datos auxiliares----
Holidays<-read.csv(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/TABLAS/12. Holidays.csv"),stringsAsFactors = FALSE)
Holidays<-as.Date(Holidays$X2011.07.01)
slope<-raster(paste0(carpetaRAS,"/RESULTADOS/GENERAL/GEO-DATA/SLOPES_bogota.tif"))
load(paste0(carpetaRAS,"/RESULTADOS/LTS/Bases de Datos/8-Capa_Predicción_LTS_Logit.Rdata"))
Capa_Variables_Prediccion<-Capa_Variables_Prediccion %>% st_transform(3116) %>% st_buffer(.,dist=10+Capa_Variables_Prediccion$Ancho/2) %>% st_transform(4326)
Capa_Variables_Prediccion<-Capa_Variables_Prediccion[,-c(1,11:12)]
Capa_Variables_Prediccion<-Capa_Variables_Prediccion %>% mutate(LTS=ifelse(X4>=0.5,1,ifelse(X3>=0.5,2,ifelse(X2>=0.5,3,4))))
Capa_Variables_Prediccion<-Capa_Variables_Prediccion[,-c(10:13)]

#Eliminación del grupo de edad de 0-4 años por limitaciones de información----
AccidentesBiciTotal<-AccidentesBiciTotal %>% filter(EdadCategorica!="0-4")
AccidentesTotal<-AccidentesTotal %>% filter(EdadCategorica!="0-4")

############### SELECCIÓN DE VARIABLES ##############################################################

# Reduccion a variables de accidentes a utilizar
AccidentesBiciTotal$Hora<-as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$hour+(as.POSIXlt(AccidentesBiciTotal$Accidentes.HoraOcurrencia)$min/60)
AccidentesBiciTotal$Dia<-factor(weekdays(AccidentesBiciTotal$Accidentes.HoraOcurrencia),c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
AccidentesBiciTotal$Mes<-factor(getMonth(AccidentesBiciTotal$Accidentes.HoraOcurrencia,"%b"),c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
AccidentesBiciTotal$Year<-factor(getYear(AccidentesBiciTotal$Accidentes.HoraOcurrencia))
AccidentesBiciTotal$Festivo<-(as.Date(AccidentesBiciTotal$Accidentes.HoraOcurrencia) %in% Holidays)
AccidentesBiciTotal<-AccidentesBiciTotal[,c("Accidente","Accidentes.Localidad","KEY","UPlCodigo","Year","Gravedad2","Sexo","Edad","PortaLicencia","LLevaCasco","AntiguedadBici","Hora","Dia","Mes","Festivo","Clima","Accidentes.TipoDiseno2")]
AccidentesBiciTotal<- AccidentesBiciTotal %>% rename(Localidad=Accidentes.Localidad,Diseno=Accidentes.TipoDiseno2) %>% mutate(Accidente=1:n())
rm(Holidays)

# Vehiculos relacionados
h<-as.data.frame(dummy(AccidentesTotal$ClaseVehiculo2))
names(h)<-substr(names(h),16,10000)
h[c("NA","Bicycle")]<-NULL
h$KEY<-AccidentesTotal$KEY
h %<>% group_by(KEY) %>%  summarize_all(,.funs=sum)
h[,-1]<-ifelse(h[,-1]>=1,1,0)
AccidentesBiciTotal <-left_join(x = AccidentesBiciTotal,y=h,by="KEY")
rm(h,AccidentesTotal)

# Inclusión de la pendiente
AccidentesBiciTotal$Slope<-raster::extract(slope,as_Spatial(AccidentesBiciTotal))
rm(slope)

# Inclusión de variables de LTS
AccidentesBiciTotal<-st_join(AccidentesBiciTotal %>% st_transform(3116),Capa_Variables_Prediccion[,-1] %>% st_transform(3116),left=TRUE,largest=TRUE) %>% st_transform(4326)
AccidentesBiciTotal$Ancho<-AccidentesBiciTotal$Ancho/AccidentesBiciTotal$Carriles
AccidentesBiciTotal$Diseno[(AccidentesBiciTotal$Diseno=="Bikeway")&(AccidentesBiciTotal$CicloRuta==0)]<-"Other"
rm(Capa_Variables_Prediccion)

# tmap_leaflet(tm_shape(AccidentesBiciTotal[AccidentesBiciTotal$Localidad=="KENNEDY" & is.na(AccidentesBiciTotal$SITP),])+tm_dots()+tm_shape(Capa_Variables_Prediccion[Capa_Variables_Prediccion$LocNombre=="KENNEDY",])+tm_polygons())

#Transormación de variables a tipos adecuados
AccidentesBiciTotal<-within(AccidentesBiciTotal,{
  UPlCodigo<-as.factor(UPlCodigo)
  Gravedad2<-ifelse(Gravedad2=="Dead",1,0) # 1=Dead
  Sexo<-ifelse(Sexo=="Male",1,0) #1=Male
  PortaLicencia<-as.numeric(PortaLicencia=="Yes") #1=Yes
  LLevaCasco<-as.numeric(LLevaCasco=="Yes") #1=Yes
  Festivo<-as.numeric(Festivo)
  Clima<-as.factor(Clima)
  Diseno<-as.factor(Diseno)
  LTS<-as.factor(LTS)
})
#Almacenamiento de la base de datos de nivel 1
save(AccidentesBiciTotal,file=paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/13. Level1.Rdata"))
