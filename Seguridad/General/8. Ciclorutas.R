#Preparacion del entorno de trabajo----
rm(list=ls())
carpetaRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/UNIANDES - RAS - SDM"
gitRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/proBikePolicies"
require(pacman)
pacman::p_load(sf)
pacman::p_load(tidyverse)

#Lectura de la base de datos de localidades----
Localidades<-st_read(paste(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR0318.gdb",sep=""),layer="Loca",crs=4326)

#Lectura de las bases de datos de ciclorutas----
rcic2012<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR1212.gdb"),layer = "Cicl",crs=4326)
rcic2012$LENGTH_GEO<-as.numeric(st_length(rcic2012))
rcic2012<-st_join(rcic2012,Localidades,join=st_intersects,left=TRUE) %>% st_set_geometry(NULL)
rcic2012$FECHA<-as.POSIXct("2012-12-01",format="%F")
rcic2013<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR1213.gdb"),layer = "Cicl",crs=4326)
rcic2013$LENGTH_GEO<-as.numeric(st_length(rcic2013))
rcic2013<-st_join(rcic2013,Localidades,join=st_intersects,left=TRUE) %>% st_set_geometry(NULL)
rcic2013$FECHA<-as.POSIXct("2013-12-01",format="%F")
rcic2014<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR1214.gdb"),layer = "Cicl",crs=4326)
rcic2014$LENGTH_GEO<-as.numeric(st_length(rcic2014))
rcic2014<-st_join(rcic2014,Localidades,join=st_intersects,left=TRUE) %>% st_set_geometry(NULL)
rcic2014$FECHA<-as.POSIXct("2014-12-01",format="%F")
rcic2015<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR1215.gdb"),layer = "Cicl",crs=4326)
rcic2015$LENGTH_GEO<-as.numeric(st_length(rcic2015))
rcic2015<-st_join(rcic2015,Localidades,join=st_intersects,left=TRUE) %>% st_set_geometry(NULL)
rcic2015$FECHA<-as.POSIXct("2015-12-01",format="%F")
rcic2016<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR1216.gdb"),layer = "Cicl",type = 5,crs=4326) %>% st_transform(4326) %>% st_cast("LINESTRING",do_split = TRUE) 
rcic2016$LENGTH_GEO<-as.numeric(st_length(rcic2016))
rcic2016<-st_join(rcic2016,Localidades,join=st_intersects,left=TRUE) %>% st_set_geometry(NULL)
rcic2016$FECHA<-as.POSIXct("2016-12-01",format="%F")
rcic2017<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR1217.gdb"),layer = "RBic",type = 5,crs=4326) %>% st_transform(4326) %>% st_cast("LINESTRING",do_split = TRUE) 
rcic2017$LENGTH_GEO<-as.numeric(st_length(rcic2017))
rcic2017<-st_join(rcic2017,Localidades,join=st_intersects,left=TRUE) %>% st_set_geometry(NULL)
rcic2017$FECHA<-as.POSIXct("2017-12-01",format="%F")
rcic2018<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR0318.gdb"),layer = "RBic",type = 5,crs=4326) %>% st_transform(4326) %>% st_cast("LINESTRING",do_split = TRUE)
rcic2018$LENGTH_GEO<-as.numeric(st_length(rcic2018))
rcic2018<-st_join(rcic2018,Localidades,join=st_intersects,left=TRUE) %>% st_set_geometry(NULL)
rcic2018$FECHA<-as.POSIXct("2018-03-01",format="%F")
rcic2019_add<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Shapefiles/PlanImplementacionCicloruta2018_2019.shp"),type = 5) %>% st_zm(drop=TRUE) %>% st_transform(4326) %>% st_cast("LINESTRING",do_split = TRUE)
rcic2019_add$LENGTH_GEO<-as.numeric(st_length(rcic2019_add))
rcic2019_add<-st_join(rcic2019_add,Localidades,join=st_intersects,left=TRUE) %>% st_set_geometry(NULL)
rcic2019_add$FECHA<-as.POSIXct("2019-12-01",format="%F")

#Longitudes de cicloruta por localidad por año----
rcic2012<-aggregate(rcic2012$LENGTH_GEO,by=list(rcic2012$FECHA,rcic2012$LocNombre),FUN=sum) %>% select(FECHA=Group.1,LocNombre=Group.2,Mts=x)
rcic2013<-aggregate(rcic2013$LENGTH_GEO,by=list(rcic2013$FECHA,rcic2013$LocNombre),FUN=sum) %>% select(FECHA=Group.1,LocNombre=Group.2,Mts=x)
rcic2014<-aggregate(rcic2014$LENGTH_GEO,by=list(rcic2014$FECHA,rcic2014$LocNombre),FUN=sum) %>% select(FECHA=Group.1,LocNombre=Group.2,Mts=x)
rcic2015<-aggregate(rcic2015$LENGTH_GEO,by=list(rcic2015$FECHA,rcic2015$LocNombre),FUN=sum) %>% select(FECHA=Group.1,LocNombre=Group.2,Mts=x)
rcic2016<-aggregate(rcic2016$LENGTH_GEO,by=list(rcic2016$FECHA,rcic2016$LocNombre),FUN=sum) %>% select(FECHA=Group.1,LocNombre=Group.2,Mts=x)
rcic2017<-aggregate(rcic2017$LENGTH_GEO,by=list(rcic2017$FECHA,rcic2017$LocNombre),FUN=sum) %>% select(FECHA=Group.1,LocNombre=Group.2,Mts=x)
rcic2018<-aggregate(rcic2018$LENGTH_GEO,by=list(rcic2018$FECHA,rcic2018$LocNombre),FUN=sum) %>% select(FECHA=Group.1,LocNombre=Group.2,Mts=x)
rcic2019<-aggregate(rcic2019_add$LENGTH_GEO,by=list(rcic2019_add$FECHA,rcic2019_add$LocNombre),FUN=sum) %>% select(FECHA=Group.1,LocNombre=Group.2,Mts=x) %>% merge(.,rcic2018,by="LocNombre") %>% mutate(Mts=Mts.x+Mts.y) %>% select(FECHA=FECHA.x,LocNombre=LocNombre,Mts=Mts)

#Integración de la información----
longCiclTime<-rbind(rcic2012,rcic2013,rcic2014,rcic2015,rcic2016,rcic2017,rcic2018,rcic2019)

#Almacenamiento de resultados----
write.csv(longCiclTime,paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Resultados/TABLAS/8. lengthCiclTime.csv"))

