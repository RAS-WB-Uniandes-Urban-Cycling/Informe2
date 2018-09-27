library(tidyverse)

# Ruta relativa a la base de datos
path <- "C:/Users/pa.uriza274/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/"

# Se cargan las tablas de viajes y de personas.
Viajes <- read_csv2(paste0(path,"BASES DE DATOS/Encuesta de Movilidad/2015/Encuesta/encuesta 2015 - viajes.csv"),locale = locale(encoding = stringi::stri_enc_get())) %>% `names<-`(str_to_lower(names(.)))
Personas <- read_csv2(paste0(path,"BASES DE DATOS/Encuesta de Movilidad/2015/Encuesta/encuesta 2015 - personas.csv"),locale = locale(encoding = stringi::stri_enc_get())) %>% `names<-`(str_to_lower(names(.)))

# Se extrae de los campos dia_habil y dia_nohabil de la tabla viajes, la información de si para una id_encuesta se respondió en día hábil o no hábil 
encuestaDia <- Viajes %>% select(id_encuesta,dia_habil,dia_nohabil) %>% gather(dia_habil,dia_nohabil,key = "tipo_dia",value="value",na.rm = T) %>% distinct() %>% select(-value)

# Se cruza con la tabla personas para poder discriminar los factores de expansión por tipo de día
Personas2 <- Personas %>% left_join(encuestaDia)

# Se guarda el resultado en la base de datos.
write_csv(Personas2,path = paste0(path,"RESULTADOS/GENERAL/TABLAS/encuesta 2015 - personasTipoDia.csv"))
