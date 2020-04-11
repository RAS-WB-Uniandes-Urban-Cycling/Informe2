#Preparación del entorno de trabajo----
rm(list=ls())
carpetaRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/Uniandes - RAS - SDM"
gitRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/proBikePolicies"
require(pacman)
pacman::p_load(gdata)
pacman::p_load(tidyverse)
pacman::p_load(dplyr)
pacman::p_load(lubridate)
pacman::p_load(sf)
pacman::p_load(dummies)
pacman::p_load(reshape2)
pacman::p_load(tmap)
pacman::p_load(zoo)
pacman::p_load(lwgeom)

#Leer bases de datos preprocesada----
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/5. biciusuariosUPZ.Rdata"))
biciusuarios<-biciusuarios %>% 
  ungroup() %>% 
  mutate(UPZ=paste0("UPZ",UPZ)) %>% 
  group_by(UPZ,Year=as.numeric(getYear(FECHA))) %>% 
  dplyr::summarise(BICIUSURS=sum(BICIUSRSINT))
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/13. Level1.Rdata"))
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/15.  Kilometros_Ruta por UPZ.Rdata"))
Entropia<-read.csv(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/TABLAS/14. EntropiaUPZ.csv"),fileEncoding = "Latin1",encoding = "Latin1",stringsAsFactors = FALSE) %>% 
  rename(UPlCodigo=UPZ) %>% replace(.,is.na(.),0)
TimeVarying<-read.csv(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/TABLAS/14.5 TimeVarying.csv"),fileEncoding = "Latin1",encoding = "Latin1",stringsAsFactors = FALSE)

############### VARIABLES PREPROCESADAS ####################################################################
AccidentesBiciTotal <- AccidentesBiciTotal %>% 
  st_set_geometry(NULL) %>% 
  group_by(UPlCodigo, Year=as.numeric(levels(AccidentesBiciTotal$Year))[AccidentesBiciTotal$Year], Gravedad2) %>% 
  summarise(Accidentes=n()) %>% 
  filter(!is.na(UPlCodigo)) %>% 
  ungroup() %>% 
  spread(value = "Accidentes",key = "Gravedad2") %>% 
  dplyr::select(-`<NA>`) %>% 
  rename(Fatal=`1`,Nonfatal=`0`) %>% 
  replace(.,is.na(.),0)
Table <- Table %>% 
  st_set_geometry(NULL) %>% 
  group_by(Year,UPlCodigo) %>% 
  summarise(TraveledKm=sum(Km*VIAJESINT))

Level2<-left_join(AccidentesBiciTotal,Entropia,by=c("UPlCodigo","Year"))
rm(AccidentesBiciTotal,Entropia)
Level2<-left_join(Level2,Table,by=c("UPlCodigo","Year"))
rm(Table)
Level2$TraveledKm[is.na(Level2$TraveledKm)]<-0
Level2<-left_join(Level2,biciusuarios,by=c("UPlCodigo"="UPZ","Year"))
rm(biciusuarios)
Level2$BICIUSURS[is.na(Level2$BICIUSURS)]<-0
Level2<-left_join(Level2,TimeVarying,by=c("UPlCodigo","Year"))
rm(TimeVarying)

############### VARIABLES DINAMICAS ####################################################################

Level3<- Level2 %>% 
  dplyr::select(-Year,-Fatal,-Nonfatal) %>% 
  group_by(UPlCodigo) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))

Level2<-left_join(Level2,Level3,suffix=c(".T",".Avg"),by="UPlCodigo")

Level2<-Level2 %>% 
          transmute(UPlCodigo,Year,Nonfatal,Fatal,
                Entropia.D=Entropia.T-Entropia.Avg,
                PrcntComercial.D=PrcntComercial.T-PrcntComercial.Avg,
                PrcntOficinas.D=PrcntOficinas.T-PrcntOficinas.Avg,
                PrcntResidencial.D=PrcntResidencial.T-PrcntResidencial.Avg,
                PrcntIndustrial.D=PrcntIndustrial.T-PrcntIndustrial.Avg,
                TraveledKm.D=TraveledKm.T-TraveledKm.Avg,
                BICIUSURS.D=BICIUSURS.T-BICIUSURS.Avg,
                Valor.D=Valor.T-Valor.Avg,
                CicloRuta.D=CicloRuta.T-CicloRuta.Avg
                )

############### VARIABLES ESTATICAS ####################################################################

#Caracteristicas generales----
Level3<-Level3 %>% mutate(Rural=0)
Level3$Rural[contains("UPR",vars=Level3$UPlCodigo)]<-1
upz <- st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR1213.gdb"),layer='UPla', stringsAsFactors = FALSE) %>% st_transform(4326)
upz <- upz %>%  mutate(Area=as.numeric(st_area(.))) %>% dplyr::select(Area, UPlCodigo) %>% st_set_geometry(NULL)
Level3<-left_join(Level3,upz)
rm(upz)

# Catastro Bogotá ----
#Espacio público
espPub<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Catastro Bogotá/Indicador espacio público 2017"), stringsAsFactors = FALSE) %>% st_transform(4326)
upz17 <- st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR1213.gdb"),layer='UPla', stringsAsFactors = FALSE) %>% st_transform(4326)
espPub<-st_join(espPub %>% st_transform(3116),upz17 %>% st_transform(3116),left=TRUE,largest=TRUE) %>% st_transform(4326) %>% st_set_geometry(NULL)
Level3<-left_join(Level3,espPub[,c("UPlCodigo","IUPIEPEFEC")])
rm(espPub)

# Lote de uso público 2017
espPub<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Catastro Bogotá/Lote de uso público 2017"), stringsAsFactors = FALSE) %>% st_transform(4326)
espPub<-st_join(espPub %>% st_transform(3116),upz17 %>% st_transform(3116),left=TRUE,largest=TRUE) %>% st_transform(4326)
espPub<- espPub %>% 
  mutate(Area=as.numeric(st_area(.))) %>% 
  st_set_geometry(NULL) %>% 
  dplyr::select(Area, UPlCodigo) %>% 
  group_by(UPlCodigo) %>% 
  summarise(AreaPublica=sum(Area,na.rm=TRUE))
Level3<-left_join(Level3,espPub)
rm(espPub, upz17)

#Safetipin----
Safeti<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Safetipin.gdb"), stringsAsFactors = FALSE) %>% st_transform(4326)
upz15 <- st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR1215.gdb"),layer='UPla', stringsAsFactors = FALSE) %>% st_transform(4326)
Safeti<-st_join(Safeti %>% st_transform(crs=3116),upz15 %>% st_transform(crs=3116),left=TRUE,largest=TRUE) %>% st_transform(4326) %>% st_set_geometry(NULL)
Safeti<-Safeti  %>% 
  mutate(LIGTH=LIGTH/max(LIGTH),CROWD=CROWD/max(CROWD)) %>% 
  dplyr::dplyr::select(LIGTH, CROWD, UPlCodigo) %>% 
  group_by(UPlCodigo) %>% 
  summarise_all(funs(mean))
Level3<-left_join(Level3,Safeti)
rm(upz15,Safeti)

#Señalizacion vial----
señales <- st_read(paste0(carpetaRAS,"/BASES DE DATOS/Shapefiles/Inventario_Senal_Vertical.shp"), stringsAsFactors = FALSE) %>% st_transform(4326)
señales<-señales %>% 
  filter(CLASE_SENA=="SENAL REGLAMENTARIA DE CICLO RUTA"|CLASE_SENA=="SENAL PREVENTIVA DE CICLO RUTA"|
         CLASE_SENA=="SENAL INFORMATIVA DE CICLO RUTA"|CLASE_SENA=="SENAL INFORMATIVA CICLORRUTA") %>% 
  mutate(CLASE_SENA=ifelse(CLASE_SENA=="SENAL REGLAMENTARIA DE CICLO RUTA","REGLAMENTARIA",
                       ifelse(CLASE_SENA=="SENAL PREVENTIVA DE CICLO RUTA","PREVENTIVA",
                          ifelse(CLASE_SENA=="SENAL INFORMATIVA DE CICLO RUTA","INFORMATIVA",
                             "INFORMATIVA"))))
upz17 <- st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR1217.gdb"),layer='UPla', stringsAsFactors = FALSE) %>% st_transform(4326)
señales<-st_join(señales %>% st_transform(crs=3116),upz17 %>% st_transform(crs=3116),left=TRUE,largest=TRUE) %>% st_transform(crs=4326)
señales<-señales %>% 
  st_set_geometry(NULL) %>% 
  dplyr::dplyr::select(CLASE_SENA, UPlCodigo) %>% 
  group_by(UPlCodigo,CLASE_SENA) %>% 
  summarise(n=n()) %>% 
  ungroup() %>% 
  spread(CLASE_SENA,n) %>% 
  mutate(TotalSeñales=INFORMATIVA+PREVENTIVA+REGLAMENTARIA)
Level3<-left_join(Level3,señales)
Level3$INFORMATIVA[is.na(Level3$INFORMATIVA)]<-0
Level3$PREVENTIVA[is.na(Level3$PREVENTIVA)]<-0
Level3$REGLAMENTARIA[is.na(Level3$REGLAMENTARIA)]<-0
Level3$TotalSeñales[is.na(Level3$TotalSeñales)]<-0
rm(señales)

#Semaforización----
semaforos <- st_read(paste0(carpetaRAS,"/BASES DE DATOS/Shapefiles/REDSEM.shp"), stringsAsFactors = FALSE) %>% st_transform(4326)
semaforos <- semaforos %>% 
  filter(INFRA_CICL!="SIN")
semaforos<-st_join(semaforos %>% st_transform(crs=3116), upz17 %>% st_transform(crs=3116) ,left=TRUE,largest=TRUE) %>% st_transform(crs=4326)
semaforos <- semaforos %>% 
  st_set_geometry(NULL) %>% 
  group_by(UPlCodigo) %>% 
  summarize(SEMAFOROS=n())
Level3<-left_join(Level3, semaforos)
Level3$SEMAFOROS[is.na(Level3$SEMAFOROS)]<-0
rm(upz17, semaforos)

#Huecos en la vía
huecos<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/IDU/Capas_IDU/Huecos2016"), stringsAsFactors = FALSE) %>% st_transform(4326)
upz<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR1216.gdb"),layer='UPla', stringsAsFactors = FALSE) %>% st_transform(4326)
huecos<-st_join(huecos %>% st_transform(3116),upz %>% st_transform(3116),left=TRUE,largest=TRUE) %>% st_transform(4326)
huecos<- huecos %>% 
  group_by(UPlCodigo) %>% 
  summarise(HUECOS=n()) %>% 
  st_set_geometry(NULL)
Level3<-left_join(Level3,huecos)
Level3$HUECOS[is.na(Level3$HUECOS)]<-0
rm(upz,huecos)

#Almacenamiento de variables de nivel 2----
save(Level2,Level3,file=paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/16. Level2.Rdata"))

