# Recodificación de variables encuesta movilidad 2015
library(tidyverse)
library(sf)
library(mlr)
library(stplanr)
library(lwgeom)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Ruta relativa a la base de datos
path <- "C:/Users/pa.uriza274/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/"

# Lectura de las tablas maestras de la encuesta
Personas <- read_csv2(paste0(path,"BASES DE DATOS/Encuesta de Movilidad/2015/Encuesta/encuesta 2015 - personas.csv"),locale = locale(encoding = stringi::stri_enc_get())) %>% `names<-`(str_to_lower(names(.)))
Encuesta <- read_csv2(paste0(path,"BASES DE DATOS/Encuesta de Movilidad/2015/Encuesta/encuesta 2015 - encuestas.csv"),locale = locale(encoding = stringi::stri_enc_get())) %>% `names<-`(str_to_lower(names(.)))
Vehiculos <- read_csv2(paste0(path,"BASES DE DATOS/Encuesta de Movilidad/2015/Encuesta/encuesta 2015 - vehiculos.csv"),locale = locale(encoding = stringi::stri_enc_get())) %>% `names<-`(str_to_lower(names(.)))
Viajes <- read_csv2(paste0(path,"BASES DE DATOS/Encuesta de Movilidad/2015/Encuesta/encuesta 2015 - viajes.csv"),locale = locale(encoding = stringi::stri_enc_get())) %>% `names<-`(str_to_lower(names(.)))
Etapas <- read_csv2(paste0(path,"BASES DE DATOS/Encuesta de Movilidad/2015/Encuesta/encuesta 2015 - etapas.csv"),locale = locale(encoding = stringi::stri_enc_get())) %>% `names<-`(str_to_lower(names(.)))

zats <- read_sf(paste0(path,"BASES DE DATOS/ZATs/ZATs_2012_MAG.shp"),stringsAsFactors = FALSE,quiet = TRUE) %>% st_transform(4326) %>% select(Zona_Num_N)

Personas <- Personas %>% mutate(education_level = case_when(nivel_educativo %in% c("Ninguno","Preescolar","Primaria incompleta","Primaria Completa") ~ "< middle school",
                                                            nivel_educativo %in% c("Media incompleta","Media Completa") ~ "Middle or highschool",
                                                            nivel_educativo %in% c("Tecnico Tegnologico incompleto","Tecnico Tegnologo completo","Pregrado incompleto","Pregrado completo","Postgreado incompleto","Postgrado Completo") ~ "University and above",
                                                            TRUE ~ str_to_title(nivel_educativo)),
                                ageRank = case_when(edad < 4 ~ "< 16",
                                                    between(edad,14,29) ~ "14-29",
                                                    between(edad,30,49) ~ "30-49",
                                                    edad >= 50 ~ ">= 50"),
                                economic_activity = if_else(!(actividad_principal %in% c("Esta incapacitado permanentemente para trabajar","-")),
                                                            case_when(actividad_principal %in% c("Estudia","Estudia y trabaja") ~ "Student",
                                                                      actividad_principal %in% c("Trabajar") ~ "Employed",
                                                                      actividad_principal %in% c("Busca Trabajo","Oficios del Hogar","Pensionado / jubilado","Otra Actividad") ~ "Other"),
                                                            actividad_principal),
                                license = case_when(licenciaconduccion1 %in% c("No tiene licencia de conduccion.","-") ~ "No license",
                                                    licenciaconduccion1 == "Para Moto" ~ "Motorcycle",
                                                    licenciaconduccion1 %in% c("Para automovil","Para otros vehiculos") ~ "Other vehicles"))

Encuesta <- Encuesta %>% left_join(group_by(Vehiculos,id_encuesta) %>% summarise(NumMotorVehic=n(),MotorizedVehicles = NumMotorVehic > 0)) %>%
  mutate(MotorizedVehicles = if_else(is.na(MotorizedVehicles),F,MotorizedVehicles),
         socioeconomic_status = case_when(estrato %in% c(1,2) ~ "Low",
                                          estrato == 3 ~ "Middle",
                                          estrato %in% c(4,5,6) ~ "High"))

Viajes <- Viajes %>% unite("id",c("id_encuesta","numero_persona","numero_viaje"),remove = F) %>% 
  left_join(select(Etapas,id_encuesta,numero_persona,numero_viaje,mediotrasporte),by = c("id_encuesta","numero_persona","numero_viaje")) %>% 
  mutate(BicycleCommuting = mediotrasporte %in% c("Bicicleta","Bicicleta con motor", "Bicicletas publicas")) %>% 
  group_by_(.dots = c(names(Viajes),"id")) %>% summarise(BicycleCommuting = any(BicycleCommuting))

zatsCoord <- zats %>% st_centroid() %>% bind_cols(st_coordinates(.) %>% as.tibble() %>% `names<-`(c("ZATlong","ZATlat"))) %>% st_set_geometry(NULL)

CommutingTrips <- Personas %>% filter(edad>=14)%>% 
  # Filter regarding tranport mode (identify commuters) for workdays
  inner_join(Viajes %>% filter(motivoviaje %in% c("Estudiar","Trabajar","Asuntos de Trabajo","Buscar trabajo") & 
                                 dia_habil == "S"), by = c("id_encuesta", "numero_persona")) %>% 
  # Impute economic_activity for "missing" classes using travel information
  mutate(economic_activity = if_else(economic_activity %in% c("Esta incapacitado permanentemente para trabajar","-"),
                                     case_when(motivoviaje == "Estudiar"  ~ "Student",
                                               motivoviaje %in% c("Trabajar","Asuntos de Trabajo") ~ "Employed",
                                               motivoviaje %in% c("Buscar trabajo") ~ "Employed"),
                                     economic_activity)) %>% 
  # Impute trips coordinates for non-valid origin destination coordinates based on TAZ centroid
  left_join(zatsCoord %>% select(zat_origen = Zona_Num_N,zat_origen_lat = ZATlat,zat_origen_lon = ZATlong)) %>% 
  left_join(zatsCoord %>% select(zat_destino = Zona_Num_N,zat_destino_lat = ZATlat,zat_destino_lon = ZATlong)) %>% 
  mutate(latitud_origen = if_else(is.na(latitud_origen)|!between(latitud_origen,3.66,5.83)|latitud_origen == latitud_destino,
                                  zat_origen_lat,latitud_origen),
         longitud_origen = if_else(is.na(longitud_origen)|!between(longitud_origen,-74.88,-73.05)|longitud_origen == longitud_destino,
                                   zat_origen_lon,longitud_origen),
         latitud_destino = if_else(is.na(latitud_destino)|!between(latitud_destino,3.66,5.83)|latitud_origen == latitud_destino,
                                   zat_destino_lat,latitud_destino),
         longitud_destino = if_else(is.na(longitud_destino)|!between(longitud_destino,-74.88,-73.05)|longitud_origen == longitud_destino,
                                    zat_destino_lon,longitud_destino)) %>% 
  # Filter missings after imputation
  filter(!is.na(latitud_origen),!is.na(longitud_origen),!is.na(latitud_destino),!is.na(longitud_destino)) %>% 
  # Filter for people living in Bogota
  inner_join(Encuesta %>% filter(municipio == "BOGOTA-DC 11001"),by = c("id_encuesta")) 

desireLines <- CommutingTrips %>% rowwise() %>% 
  mutate(geometry = st_sfc(st_linestring(rbind(c(longitud_origen,latitud_origen),c(longitud_destino,latitud_destino))))) %>% 
  st_as_sf(crs = 4326) %>% ungroup() %>%  mutate(Distancia = as.numeric(st_length(.)))

routes <- line2route(desireLines,"route_osrm",l_id = "id",osrmurl = "http://localhost:5000/",profile = "biking")

source("./mabr.R")

buffer_origen <- CommutingTrips %>% select(id_encuesta,numero_persona,numero_viaje,longitud_origen,latitud_origen) %>% st_as_sf(coords = c("longitud_origen","latitud_origen"),crs = 4326) %>% 
  st_transform(3116) %>% st_buffer(dist = units::set_units(500,m)) %>% mutate(Area = as.numeric(units::set_units(st_area(.),"km^2"))) %>% st_transform(4326)
buffer_destino <- CommutingTrips %>% select(id_encuesta,numero_persona,numero_viaje,longitud_destino,latitud_destino) %>% st_as_sf(coords = c("longitud_destino","latitud_destino"),crs = 4326) %>% 
  st_transform(3116) %>% st_buffer(dist = units::set_units(500,m))%>% mutate(Area = as.numeric(units::set_units(st_area(.),"km^2"))) %>% st_transform(4326)
buffer_ruta <- separate(routes,id,into = c("id_encuesta","numero_persona","numero_viaje"),convert = T) %>% select(-error) %>% mabr() %>% mutate(Area = as.numeric(units::set_units(st_area(.),"km^2")))


CR <- inner_join(separate(routes,id,into = c("id_encuesta","numero_persona","numero_viaje"),convert = T),CommutingTrips)

Individuo <- CR %>% st_set_geometry(NULL) %>% group_by(id_encuesta,numero_persona,BicycleCommuting,sexo,ageRank,economic_activity,socioeconomic_status,MotorizedVehicles,license) %>% summarise(age = mean (edad),AvgDistance = mean (distance))

plot(select(left_join(separate(routes,id,into = c("id_encuesta","numero_persona","numero_viaje"),convert = T),CommutingTrips),BicycleCommuting))


a <- filter(CR,BicycleCommuting == T)
sum(routes$distance*a$ponderador_calibrado_viajes,na.rm = T)

CommutingTrips %>% filter(is.na(latitud_origen),is.na(longitud_origen),is.na(latitud_destino),is.na(longitud_destino)) %>%glimpse()

capa_LTS <- Capa_Variables_Prediccion %>% st_as_sf() %>% mutate(Cluster=ifelse(X4>=0.5,1,ifelse(X3>=0.5,2,ifelse(X2>=0.5,3,4))))

#### Accidentes ####
load(paste0(path,"RESULTADOS/SEGURIDAD/Bases de datos/3. AccidentesBiciTotalGeoData.Rdata"))

# Origen
Accidentes_origen <- AccidentesBiciTotal %>% st_join(buffer_origen,left = FALSE,largest = FALSE) %>% st_set_geometry(NULL) %>% 
  group_by(id_encuesta,numero_persona,numero_viaje) %>% 
  summarise(NAcc = n())

# Destino
Accidentes_destino <- AccidentesBiciTotal %>% st_join(buffer_destino,left = FALSE,largest = FALSE) %>% st_set_geometry(NULL) %>% 
  group_by(id_encuesta,numero_persona,numero_viaje) %>% 
  summarise(NAcc = n())

# Ruta
Accidentes_ruta <- AccidentesBiciTotal %>% st_join(buffer_ruta,left = FALSE,largest = FALSE) %>% st_set_geometry(NULL) %>% 
  group_by(id_encuesta,numero_persona,numero_viaje) %>% 
  summarise(NAcc = n())

#### Paraderos SiTP ####
capa_SiTP <- read_sf(paste0(path,"BASES DE DATOS/Mapas de Referencia IDECA/MR0318.gdb"),layer = "NTra",stringsAsFactors = FALSE) %>% filter(NTrTipo == 4) %>% st_transform(4326)

# Origen
SiTP_origen <- capa_SiTP %>% st_join(buffer_origen,left = FALSE,largest = FALSE) %>% st_set_geometry(NULL) %>% 
  group_by(id_encuesta,numero_persona,numero_viaje) %>% 
  summarise(NSiTP = n())

# Destino
SiTP_destino <- capa_SiTP %>% st_join(buffer_destino,left = FALSE,largest = FALSE) %>% st_set_geometry(NULL) %>% 
  group_by(id_encuesta,numero_persona,numero_viaje) %>% 
  summarise(NSiTP = n())

# Ruta
SiTP_ruta <- capa_SiTP %>% st_join(buffer_ruta,left = FALSE,largest = FALSE) %>% st_set_geometry(NULL) %>% 
  group_by(id_encuesta,numero_persona,numero_viaje) %>% 
  summarise(NSiTP = n())

#### TransMilenio ####

capa_TM <- read_sf(paste0(path,"BASES DE DATOS/Mapas de Referencia IDECA/MR0318.gdb"),layer = "NTra_P",type = 6,stringsAsFactors = FALSE) %>% filter(NTrTipo %in% c(2,3)) %>% st_transform(4326) %>% st_centroid()

# Origen
TM_origen <- capa_TM %>% st_join(buffer_origen,left = FALSE) %>% st_set_geometry(NULL) %>% 
  group_by(id_encuesta,numero_persona,numero_viaje) %>% 
  summarise(NTM = n())

# Destino
TM_destino <- capa_TM %>% st_join(buffer_destino,left = FALSE) %>% st_set_geometry(NULL) %>% 
  group_by(id_encuesta,numero_persona,numero_viaje) %>% 
  summarise(NTM = n())

# Ruta
TM_ruta <- capa_TM %>% st_join(buffer_ruta,left = FALSE) %>% st_set_geometry(NULL) %>% 
  group_by(id_encuesta,numero_persona,numero_viaje) %>% 
  summarise(NTM = n())

#### Cicloparqueaderos ####
capa_BP <- read_sf(paste0(path,"BASES DE DATOS/Shapefiles/Cicloparqueadero.shp"),stringsAsFactors = FALSE) %>% st_transform(4326)

# Origen
BP_origen <- capa_BP %>% st_join(buffer_origen,left = FALSE,largest = FALSE) %>% st_set_geometry(NULL) %>% 
  group_by(id_encuesta,numero_persona,numero_viaje) %>% 
  summarise(NCPark = n())

# Destino
BP_destino <- capa_BP %>% st_join(buffer_destino,left = FALSE,largest = FALSE) %>% st_set_geometry(NULL) %>% 
  group_by(id_encuesta,numero_persona,numero_viaje) %>% 
  summarise(NCPark = n())

# Ruta
BP_ruta <- capa_BP %>% st_join(buffer_ruta,left = FALSE,largest = FALSE) %>% st_set_geometry(NULL) %>% 
  group_by(id_encuesta,numero_persona,numero_viaje) %>% 
  summarise(NCPark = n())

#### LTS ####
load(paste0(path, "RESULTADOS/LTS/Bases de Datos/8-Capa_Predicción_LTS_Logit.Rdata"))

# Origen
t <- Sys.time()
LTS_origen <- c()
for (i in 1:nrow(buffer_ruta)) {
  LTS_origen <- rbind(LTS_origen,
                      buffer_origen[i,] %>% st_intersection(y = capa_LTS) %>% 
                        mutate(Longitud = as.numeric(st_length(.))) %>% st_set_geometry(NULL) %>%
                        group_by(id_encuesta,numero_persona,numero_viaje,Cluster) %>% 
                        summarise(Prop = sum(Longitud)) %>% mutate(Prop = Prop/sum(Prop),Cluster = paste0("LTS",Cluster)) %>% 
                        spread(Cluster,Prop))
}
Sys.time()-t

# Destino
t <- Sys.time()
LTS_destino <- c()
for (i in 1:nrow(buffer_ruta)) {
  LTS_destino <- rbind(LTS_destino,
                       buffer_destino[i,] %>% st_intersection(y = capa_LTS) %>% 
                         mutate(Longitud = as.numeric(st_length(.))) %>% st_set_geometry(NULL) %>%
                         group_by(id_encuesta,numero_persona,numero_viaje,Cluster) %>% 
                         summarise(Prop = sum(Longitud)) %>% mutate(Prop = Prop/sum(Prop),Cluster = paste0("LTS",Cluster)) %>% 
                         spread(Cluster,Prop))
}
Sys.time()-t

# Ruta
t <- Sys.time()
LTS_ruta <- c()
for (i in 1:nrow(buffer_ruta)) {
  LTS_ruta <- rbind(LTS_ruta,
                    buffer_ruta[i,] %>% st_intersection(y = capa_LTS) %>% 
                      mutate(Longitud = as.numeric(st_length(.))) %>% st_set_geometry(NULL) %>%
                      group_by(id_encuesta,numero_persona,numero_viaje,Cluster) %>% 
                      summarise(Prop = sum(Longitud)) %>% mutate(Prop = Prop/sum(Prop),Cluster = paste0("LTS",Cluster)) %>% 
                      spread(Cluster,Prop))
}
Sys.time()-t

#### CicloRuta ####
capa_CiclR <- read_sf(paste0(path,"BASES DE DATOS/Mapas de Referencia IDECA/MR0318.gdb"),layer = "RBic",stringsAsFactors = FALSE,type=5) %>% st_transform(4326)

# Origen
t <- Sys.time()
CiclR_origen <- c()
for (i in 1:nrow(buffer_ruta)) {
  CiclR_origen <- rbind(CiclR_origen,
                        buffer_origen[i,] %>% st_intersection(y = capa_CiclR) %>% 
                          mutate(Longitud = as.numeric(st_length(.))) %>% st_set_geometry(NULL) %>%
                          group_by(id_encuesta,numero_persona,numero_viaje) %>% 
                          summarise(CiclR = sum(Longitud)))
}
Sys.time()-t

# Destino
t <- Sys.time()
CiclR_destino <- c()
for (i in 1:nrow(buffer_ruta)) {
  CiclR_destino <- rbind(CiclR_destino,
                         buffer_destino[i,] %>% st_intersection(y = capa_CiclR) %>% 
                           mutate(Longitud = as.numeric(st_length(.))) %>% st_set_geometry(NULL) %>%
                           group_by(id_encuesta,numero_persona,numero_viaje) %>% 
                           summarise(CiclR = sum(Longitud)))
}
Sys.time()-t

# Ruta
t <- Sys.time()
CiclR_ruta <- c()
for (i in 1:nrow(buffer_ruta)) {
  CiclR_ruta <- rbind(CiclR_ruta,
                      buffer_ruta[i,] %>% st_intersection(y = capa_CiclR) %>% 
                        mutate(Longitud = as.numeric(st_length(.))) %>% st_set_geometry(NULL) %>%
                        group_by(id_encuesta,numero_persona,numero_viaje) %>% 
                        summarise(CiclR = sum(Longitud)))
}
Sys.time()-t

#### Pendiente ####
load(paste0(path,"RESULTADOS/PROPENSION/GEO-DATA/Slopes.RData"))

#### Índice de Entropía de uso del suelo ####
cat_usos <- read_csv(paste0(path,"BASES DE DATOS/Mapas de Referencia IDECA/UsosLote.csv"),locale = locale(encoding = stringi::stri_enc_get()))
usos_lote <- read_sf(paste0(path,"BASES DE DATOS/Mapas de Referencia IDECA/MR0318.gdb"),layer = "Uso",stringsAsFactors = FALSE)
capa_lote <- read_sf(paste0(path,"BASES DE DATOS/Mapas de Referencia IDECA/MR0318.gdb"),layer = "Lote",type = 3,stringsAsFactors = FALSE) %>% st_transform(4326) %>% 
  inner_join(usos_lote,by=c("LotCodigo"="UsoCLote")) %>% left_join(cat_usos) %>% select(UsoArea,Cat) %>% filter(!is.na(Cat))

t <- Sys.time()
entropy_origen <- capa_lote %>% st_join(buffer_origen[1:200,]) %>% group_by(id_encuesta,numero_persona,numero_viaje,Cat) %>% 
  st_set_geometry(NULL) %>% summarise_all(sum) %>% group_by(id_encuesta,numero_persona,numero_viaje) %>% 
  mutate(Prop=UsoArea/sum(UsoArea),entropyIndex=-Prop*log(Prop)/log(n())) %>% summarise_at("entropyIndex",sum)
Sys.time()-t

t <- Sys.time()
entropy_destino <- capa_lote %>% st_join(buffer_origen[1:10,]) %>%
  filter(!is.na(UsoTUso)) %>% group_by(id_encuesta,numero_persona,numero_viaje,Cat) %>% 
  st_set_geometry(NULL) %>% summarise_all(sum) %>% group_by(id_encuesta,numero_persona,numero_viaje) %>% 
  mutate(Prop=UsoArea/sum(UsoArea),entropyIndex=-Prop*log(Prop)/log(n())) %>% summarise_at("entropyIndex",sum)
Sys.time()-t

#### Datasets ####
Individual <- select(CommutingTrips,id_encuesta,numero_persona,numero_viaje,BicycleCommuting,sex=sexo,age=edad,ageRank,license,economic_activity,socioeconomic_status,motorized_vehicles=MotorizedVehicles) %>% replace(.,is.na(.),FALSE)
Environmental_O <-  CR %>%  st_set_geometry(NULL) %>% select(id_encuesta,numero_persona,numero_viaje,distance) %>% left_join(buffer_origen %>% st_set_geometry(NULL)) %>%  left_join(Accidentes_origen) %>% left_join(SiTP_origen) %>% 
  left_join(TM_origen) %>% left_join(BP_origen) %>% left_join(CiclR_origen) %>% left_join(LTS_origen) %>% left_join(slope_origen) %>% replace(.,is.na(.),0)
Environmental_D <-  CR %>% st_set_geometry(NULL) %>% select(id_encuesta,numero_persona,numero_viaje,distance) %>% left_join(buffer_destino %>% st_set_geometry(NULL)) %>% left_join(Accidentes_destino) %>% left_join(SiTP_destino) %>% 
  left_join(TM_destino) %>% left_join(BP_destino) %>% left_join(CiclR_destino) %>% left_join(LTS_destino) %>% left_join(slope_destino) %>% replace(.,is.na(.),0)
Environmental_R <-  CR %>% st_set_geometry(NULL) %>% select(id_encuesta,numero_persona,numero_viaje,distance) %>% left_join(buffer_ruta %>% st_set_geometry(NULL)) %>% left_join(Accidentes_ruta) %>% left_join(SiTP_ruta) %>% 
  left_join(TM_ruta) %>% left_join(BP_ruta) %>% left_join(CiclR_ruta) %>% left_join(LTS_ruta) %>% left_join(slope_ruta) %>% replace(.,is.na(.),0)

Data <- Individual %>% left_join(Environmental_O,by=c("id_encuesta","numero_persona","numero_viaje")) %>% 
  left_join(Environmental_D,by=c("id_encuesta","numero_persona","numero_viaje"),suffix = c(".O", ".D")) %>% 
  left_join(Environmental_R,by=c("id_encuesta","numero_persona","numero_viaje")) %>% 
  mutate(distance=distance/1000,
         NAccidentes = NAccidentes/distance,
         NSiTP = NSiTP/distance,
         NTM = NTM/distance,
         NCPark = NCPark/distance,
         CiclR = CiclR/distance,
         sex=factor(sex,levels = c("Hombre","Mujer")),
         license=factor(license,levels = c("No license","Motorcycle","Other vehicles")),
         economic_activity=factor(economic_activity,levels = c("Student","Employed","Other")),
         socioeconomic_status=factor(socioeconomic_status,levels = c("Low","Middle","High")),
         LTS1.O = `LTS 1.O`, LTS1.D = `LTS 1.D`, LTS1 = `LTS 1`,
         LTS2.O = `LTS 2.O`, LTS2.D = `LTS 2.D`, LTS2 = `LTS 2`,
         LTS3.O = `LTS 3.O`, LTS3.D = `LTS 3.D`, LTS3 = `LTS 3`,
         LTS4.O = `LTS 4.O`, LTS4.D = `LTS 4.D`, LTS4 = `LTS 4`)
DataI <- Individuo %>% ungroup()%>% 
  mutate(sexo=factor(sexo,levels = c("Hombre","Mujer")),
         license=factor(license,levels = c("No license","Motorcycle","Other vehicles")),
         economic_activity=factor(economic_activity,levels = c("Student","Employed","Other")),
         socioeconomic_status=factor(socioeconomic_status,levels = c("Low","Middle","High")))

fit <- glm(BicycleCommuting ~ AvgSlope, family = "binomial",data = DataI)
cbind(broom::tidy(fit),broom::confint_tidy(fit))

fit <- glm(BicycleCommuting ~ sexo + age + license + economic_activity + 
             socioeconomic_status + MotorizedVehicles + AvgDistance, family = "binomial",data = DataI)
cbind(broom::tidy(fit),broom::confint_tidy(fit))

fitT <- glm(BicycleCommuting ~ sexo + age + license + economic_activity + 
              socioeconomic_status + MotorizedVehicles + AvgDistance +
              , family = "binomial",data = Data)
summary(fitT)

fit <- glm(BicycleCommuting ~ distance, family = "binomial",data = Data)
cbind(exp(cbind(coef(fit), confint(fit))) ,coef(summary(fit))[,4])
ggplot(Data,aes(x = NSiTP.O,y = as.numeric(BicycleCommuting),color = sex, fill = sex)) + 
  stat_smooth(aes(linetype = "loess"),method = "loess", formula = y~x, alpha = 0.2) +
  stat_smooth(aes(linetype = "logit"),method = "glm", method.args = list(family = "binomial"), formula = y~x, alpha = 0.2) +
  #geom_point(alpha = 0.3,position = position_jitter(height = 0.03, width = 0))+
  scale_color_manual(labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_fill_manual(labels = c("Male", "Female"), values = c("blue", "red"))+
  scale_linetype_manual(name = "Fit Type", values = c(1, 3))+
  labs(x=varX,y="Bycycle Commuting",title = "Route buffer")+
  theme_light()


cbind(broom::tidy(fit),broom::confint_tidy(fit))


fit <- glm(BicycleCommuting ~ NAccidentes.O + NSiTP.O + NTM.O + NCPark.O + CiclR.O +
             `LTS 1.O` + `LTS 2.O` + `LTS 3.O` + `LTS 4.O` + Avgslope.O,
           family = "binomial",data = Data)
cbind(exp(cbind(coef(fit), confint(fit))) ,coef(summary(fit))[,4])

fit <- glm(BicycleCommuting ~ NAccidentes.D + NSiTP.D + NTM.D + NCPark.D + CiclR.D +
             `LTS 1.D` + `LTS 2.D` + `LTS 3.D` + `LTS 4.D` + Avgslope.D,
           family = "binomial",data = Data)
cbind(exp(cbind(coef(fit), confint(fit))) ,coef(summary(fit))[,4])

fit <- glm(BicycleCommuting ~ NAccidentes + NSiTP + NTM + NCPark + CiclR +
             `LTS 1` + `LTS 2` + `LTS 3` + `LTS 4` + Avgslope + distance,
           family = "binomial",data = Data)
cbind(exp(cbind(coef(fit), confint(fit))) ,coef(summary(fit))[,4])
