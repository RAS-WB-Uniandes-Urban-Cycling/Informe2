# Script cálculo commuters a partir de encuesta de movilidad 2015
library(tidyverse)
library(sf)
library(mlr)

# Ruta relativa a la base de datos
path <- "C:/Users/pa.uriza274/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/"

# Lectura de las tablas maestras de la encuesta
Personas <- read_csv2(paste0(path,"BASES DE DATOS/Encuesta de Movilidad/2015/Encuesta/encuesta 2015 - personas.csv"),locale = locale(encoding = stringi::stri_enc_get())) %>% `names<-`(str_to_lower(names(.)))
Encuesta <- read_csv2(paste0(path,"BASES DE DATOS/Encuesta de Movilidad/2015/Encuesta/encuesta 2015 - encuestas.csv"),locale = locale(encoding = stringi::stri_enc_get())) %>% `names<-`(str_to_lower(names(.)))
Vehiculos <- read_csv2(paste0(path,"BASES DE DATOS/Encuesta de Movilidad/2015/Encuesta/encuesta 2015 - vehiculos.csv"),locale = locale(encoding = stringi::stri_enc_get())) %>% `names<-`(str_to_lower(names(.)))
Viajes <- read_csv2(paste0(path,"BASES DE DATOS/Encuesta de Movilidad/2015/Encuesta/encuesta 2015 - viajes.csv"),locale = locale(encoding = stringi::stri_enc_get())) %>% `names<-`(str_to_lower(names(.)))
Etapas <- read_csv2(paste0(path,"BASES DE DATOS/Encuesta de Movilidad/2015/Encuesta/encuesta 2015 - etapas.csv"),locale = locale(encoding = stringi::stri_enc_get())) %>% `names<-`(str_to_lower(names(.)))

# Se crea la variable booleana BicycleCommuting que es verdadera si en alguna etapa del viaje se usa bicicleta como medio de transporte
ViajesBicicleta <- Viajes %>% left_join(Etapas,by = c("id_encuesta","numero_persona","numero_viaje")) %>% 
  mutate(BicycleCommuting = mediotrasporte %in% c("Bicicleta","Bicicleta con motor", "Bicicletas publicas")) %>% 
  group_by(id_encuesta,numero_persona,numero_viaje) %>% summarise(BicycleCommuting = any(BicycleCommuting))

# Se agregan los resultados a nivel de viajes
Viajes %>%  left_join(ViajesBicicleta) %>% 
  # Motivo de viaje, estudio o trabajo (Commuters)
  mutate(Commute = motivoviaje %in% c("Estudiar","Trabajar","Asuntos de Trabajo","Buscar trabajo"))

# Partimos de la tabla personas
Commuters <- Personas %>% 
  # Se filtran las personas de acuerdo a características de sus viajes
  inner_join(left_join(Viajes,ViajesBicicleta) %>% 
               # Motivo de viaje, estudio o trabajo (Commuters)
               filter(motivoviaje %in% c("Estudiar","Trabajar","Asuntos de trabajo","Buscar trabajo") & 
                        # Viaje en día hábil únicamente
                        dia_habil == "S"), by = c("id_encuesta", "numero_persona")) %>% 
  group_by(id_encuesta,numero_persona) %>% top_n(6,wt = numero_viaje) %>% 
  # Se filtran las personas que vivan en Bogotá
  inner_join(Encuesta %>% filter(municipio == "BOGOTA-DC 11001"),by = c("id_encuesta")) 

# Se reduce la información a nivel de persona. Es decir si una persona reporta al menos un viaje con etapas en bicicleta, se clasifica como "biciusuario"
# Se calcula con los factores de expansión la cantidad de commuters que usan y no usan bicicleta.
Commuters %>% group_by(id_encuesta,numero_persona,ponderador_calibrado) %>% 
  summarise(BicycleCommuting = any(BicycleCommuting)) %>% group_by(BicycleCommuting) %>% 
  summarise(Cant = sum(ponderador_calibrado))      

Data <- Commuters %>% group_by(id_encuesta,numero_persona,ponderador_calibrado,moviliza_bicicleta) %>% 
  summarise(BicycleCommuting = any(BicycleCommuting))


ggplot(Data, aes(x = BicycleCommuting,y = ..count..,fill= BicycleCommuting))+geom_bar()
ggplot(Data, aes(x = BicycleCommuting,y = ..count..,fill= moviliza_bicicleta))+geom_bar()
ggplot(Data, aes(x = BicycleCommuting,y = ponderador_calibrado/1e3,fill= moviliza_bicicleta))+geom_bar(stat = "identity")+
  geom_text(aes(x = BicycleCommuting,y = ponderador_calibrado/1e3,label = ..count..))+
  theme_light() + labs(title = "Dependent variable comparison", x = "Commuting by bicycle", y = "Expanded number of commuters (Thousands)", fill = "Answer to \n'Travels by bicycle\n commonly?'")


# Regresi�n log�stica

# Datos ZAT

distCicloRutaZat <- read_csv(paste0(path,"RESULTADOS/PROPENSION/TABLAS/distCicloRutaZat.csv"))
puntajesSafetipinZat <- read_csv(paste0(path,"RESULTADOS/PROPENSION/TABLAS/puntajesSafetipinZat.csv"))
NAccidentesZat <- read_csv(paste0(path,"RESULTADOS/PROPENSION/TABLAS/NAccidentesZat.csv"))
Vehiculos <- read_csv2(paste0(path,"BASES DE DATOS/Encuesta de Movilidad/2015/Encuesta/encuesta 2015 - vehiculos.csv"),locale = locale(encoding = stringi::stri_enc_get())) %>% `names<-`(str_to_lower(names(.)))
Viajes <- read_sf(paste0(path,"RESULTADOS/PROPENSION/GEO-DATA/LineasViajesManhattan.shp"))

DistanciaL1Viajes <- Viajes %>% transmute(id_encuesta=id_ncst,numero_persona=nmr_prs,Distancia = as.numeric(st_length(.))) %>% st_set_geometry(NULL) %>% group_by(id_encuesta, numero_persona) %>% summarise(DistL1Prom = mean(Distancia))

Data <- Commuters %>% mutate(Zona_Num_N = zat_hogar) %>% left_join(group_by(Vehiculos,id_encuesta) %>% summarise(NumVehiculosM=n())) %>% 
  mutate(NumVehiculosM = ifelse(is.na(NumVehiculosM),0,NumVehiculosM),NumVehiculosNM=pmax(0,vehic_personaidonea-NumVehiculosM)) %>%
  left_join(distCicloRutaZat) %>% left_join(puntajesSafetipinZat) %>% left_join(NAccidentesZat) %>% left_join(DistanciaL1Viajes) %>% 
  mutate_if(is.integer,coalesce,0L)%>% mutate_if(is.double,coalesce,0) %>% ungroup()

mlrData <- Data %>% transmute(BicycleCommute=factor(BicycleCommuting),
                              sexo=factor(sexo),edad,nivel_educativo = factor(nivel_educativo),
                              actividad_principal = factor(actividad_principal),licenciaconduccion1 = factor(licenciaconduccion1),
                              estrato = factor(estrato), vehic_personaidonea, LongitudCicloRuta, LIGTH, CROWD, SECURITY, TRANSPORT,
                              GENDER_DIV, CantidadAccidentes, DistL1Prom)

# Task creation
task = makeClassifTask(data = mlrData, target = "BicycleCommute",
                       positive = "TRUE")

# # Create Learner
lrn <- makeLearner(cl = "classif.binomial",
                      link = "logit",
                      predict.type = "prob",
                      fix.factors.prediction = TRUE)

perf_level = makeResampleDesc(method = "RepCV", folds = 5, reps = 10) 

lrn.under <- makeOversampleWrapper(lrn,osw.rate = 20,osw.cl = "TRUE")

CV_Bin <- resample(learner = lrn.under, task = task,
                   resampling = perf_level, measures = list(mmce,ber,auc))


Commuters %>% mutate(nivelEducativo = if_else())%>% group_by(nivel_educativo,BicycleCommuting) %>% summarise(N=n()) %>% ungroup() %>% mutate(prop = N/sum(N)) %>% as.data.frame()