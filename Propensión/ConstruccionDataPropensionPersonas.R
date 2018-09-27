library(tidyverse)
library(mlr)
library(sf)

# Ruta relativa a la base de datos
path <- "C:/Users/pa.uriza274/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/"

distCicloRutaZat <- read_csv(paste0(path,"RESULTADOS/PROPENSION/TABLAS/distCicloRutaZat.csv"))
puntajesSafetipinZat <- read_csv(paste0(path,"RESULTADOS/PROPENSION/TABLAS/puntajesSafetipinZat.csv"))
NAccidentesZat <- read_csv(paste0(path,"RESULTADOS/PROPENSION/TABLAS/NAccidentesZat.csv"))
Personas <- read_csv(paste0(path,"RESULTADOS/GENERAL/TABLAS/encuesta 2015 - personasTipoDia.csv"))
Encuesta <- read_csv2(paste0(path,"BASES DE DATOS/Encuesta de Movilidad/2015/Encuesta/encuesta 2015 - encuestas.csv"),locale = locale(encoding = stringi::stri_enc_get())) %>% `names<-`(str_to_lower(names(.)))
Vehiculos <- read_csv2(paste0(path,"BASES DE DATOS/Encuesta de Movilidad/2015/Encuesta/encuesta 2015 - vehiculos.csv"),locale = locale(encoding = stringi::stri_enc_get())) %>% `names<-`(str_to_lower(names(.)))
Viajes <- read_csv2(paste0(path,"BASES DE DATOS/Encuesta de Movilidad/2015/Encuesta/encuesta 2015 - viajes.csv"),locale = locale(encoding = stringi::stri_enc_get())) %>% `names<-`(str_to_lower(names(.)))

Viajes <- read_sf(paste0(path,"RESULTADOS/PROPENSION/GEO-DATA/LineasViajesManhattan.shp"))


DistanciaL1Viajes <- Viajes %>% transmute(id_encuesta=id_ncst,numero_persona=nmr_prs,Distancia = as.numeric(st_length(.))) %>% st_set_geometry(NULL) %>% group_by(id_encuesta, numero_persona) %>% summarise(DistL1Prom = mean(Distancia))

Data <- Personas %>% filter(tipo_dia == "dia_habil",!is.na(moviliza_bicicleta)) %>% select(moviliza_bicicleta,id_encuesta,numero_persona,sexo,edad,nivel_educativo,actividad_principal,actividad_economica,licenciaconduccion1) %>% 
  inner_join(select(Encuesta,municipio,id_encuesta,estrato,Zona_Num_N=zat_hogar,vehic_personaidonea) %>% filter(municipio == "BOGOTA-DC 11001")) %>% left_join(group_by(Vehiculos,id_encuesta) %>% summarise(NumVehiculosM=n())) %>% 
  mutate(NumVehiculosM = ifelse(is.na(NumVehiculosM),0,NumVehiculosM),NumVehiculosNM=pmax(0,vehic_personaidonea-NumVehiculosM)) %>%
  left_join(distCicloRutaZat) %>% left_join(puntajesSafetipinZat) %>% left_join(NAccidentesZat) %>% left_join(DistanciaL1Viajes) %>% 
  mutate_if(is.integer,coalesce,0L)%>% mutate_if(is.double,coalesce,0)

mlrData <- Data %>% transmute(moviliza_bicicleta=factor(moviliza_bicicleta),
                              sexo=factor(sexo),edad,nivel_educativo = factor(nivel_educativo),
                              actividad_principal = factor(actividad_principal),licenciaconduccion1 = factor(licenciaconduccion1),
                              estrato = factor(estrato), vehic_personaidonea, LongitudCicloRuta, LIGTH, CROWD, SECURITY, TRANSPORT,
                              GENDER_DIV, CantidadAccidentes, DistL1Prom)

# Faltan coordenadas

# Task creation
task = makeClassifTask(data = mlrData, target = "moviliza_bicicleta",
                       positive = "S")


a <- listLearners(task, warn.missing.packages = FALSE) %>%
  dplyr::select(class, name, short.name, package)

# Create Learners
lrnBin <- makeLearner(cl = "classif.binomial",
                     link = "logit",
                     predict.type = "prob",
                     fix.factors.prediction = TRUE)

lrnRF <- makeLearner(cl = "classif.randomForest",
                    predict.type = "prob",
                    fix.factors.prediction = TRUE)

lrnSVM <- makeLearner(cl = "classif.svm",
                     predict.type = "prob",
                     fix.factors.prediction = TRUE)

perf_level = makeResampleDesc(method = "RepCV", folds = 5, reps = 100) 

CV_Bin <- resample(learner = lrnBin, task = task,
                   resampling = perf_level, measures = auc)

CV_RF <- resample(learner = lrnRF, task = task,
                   resampling = perf_level, measures = auc)

CV_SVM <- resample(learner = lrnSVM, task = task,
                  resampling = cv5, measures = auc)
