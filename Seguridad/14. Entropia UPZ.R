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
pacman::p_load(reshape2)
pacman::p_load(nngeo)
pacman::p_load(lwgeom)

#Lectura de la base deestandarización de usos del suelo----
indice_uso <- read_csv(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/Indice uso de suelo.csv"),col_names = TRUE, col_types = list(col_character(), col_character(), col_character()))

#El procedimiento para cada año corresponde a:
# 1. Importar la capa de UPZ
# 2. Importar la capa de lotes
# 3. Importar la capa de usos del suelo
# 4. Cruzar los centroides de los lotes con las UPZ
# 5. Definir el uso del suelo para cada lote
# 6. Calculo de la entropia por proporciones de areas

#Año 2011----
loteUso2011 <- st_read(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/USO2011"), stringsAsFactors = FALSE)
H <- loteUso2011 %>% st_transform(4326) %>% 
  st_make_valid %>% 
  st_cast("MULTIPOLYGON") %>% 
  filter(!st_is_empty(.)) %>% 
  filter(!is.na(UPLCODIGO)) %>% 
  mutate(Area = as.numeric(st_area(.))) %>% 
  left_join(indice_uso, by=c("CONUSO"="COD")) %>% 
  dplyr::select(Area, Uso = CATEGORIA, UPZ = UPLCODIGO) %>% 
  st_set_geometry(NULL)

#Calculo de entropia de uso de suelo por UPZ
entropia <- H %>%  group_by(UPZ,Uso) %>% 
  summarise(SumArea = sum(Area,na.rm = TRUE)) %>% 
  group_by(UPZ) %>% 
  mutate(Prop = SumArea/ sum(SumArea,na.rm = TRUE),N = n(), e = - Prop * log(Prop) / log(N)) %>% 
  summarise(Entropia = sum(e))

#Calculo de porcentaje de uso comercial
uso_comercial <- H %>% group_by(UPZ,Uso) %>% 
  summarise(SumArea = sum(Area,na.rm = TRUE)) %>% 
  group_by(UPZ) %>% 
  mutate(PrcntComercial = SumArea / sum(SumArea,na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(Uso == "Comercio") %>% 
  subset(select = -c(Uso,SumArea))
entropia<- left_join(entropia,uso_comercial)
rm(uso_comercial)

#Calculo de porcentaje de uso de oficinas
uso_oficinas <- H %>% group_by(UPZ,Uso) %>% 
  summarise(SumArea = sum(Area,na.rm = TRUE)) %>% 
  group_by(UPZ) %>% 
  mutate(PrcntOficinas = SumArea / sum(SumArea,na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(Uso == "Oficinas") %>% 
  subset(select = -c(Uso,SumArea))
entropia<- left_join(entropia,uso_oficinas)
rm(uso_oficinas)

#Calculo de porcentaje de uso residencial
uso_residen <- H %>% group_by(UPZ,Uso) %>% 
  summarise(SumArea = sum(Area,na.rm = TRUE)) %>% 
  group_by(UPZ) %>% 
  mutate(PrcntResidencial = SumArea / sum(SumArea,na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(Uso == "Residencial") %>% 
  subset(select = -c(Uso,SumArea))
entropia<- left_join(entropia,uso_residen)
rm(uso_residen)

#Calculo de porcentaje de uso industrial
uso_ind <- H %>% group_by(UPZ,Uso) %>% 
  summarise(SumArea = sum(Area,na.rm = TRUE)) %>% 
  group_by(UPZ) %>% 
  mutate(PrcntIndustrial = SumArea / sum(SumArea,na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(Uso == "Industria") %>% 
  subset(select = -c(Uso,SumArea))
entropia<- left_join(entropia,uso_ind)
rm(uso_ind)

#Años 2012 a 2017----
Table<-list(NA,NA,NA,NA,NA,NA,NA)
Table[1]<-list(entropia %>% mutate(Year=2011))
rm(H,entropia,loteUso2011)
for(i in 12:17){
#Importar capas de UPZs----
upz <- st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR12",i,".gdb"),layer='UPla', stringsAsFactors = FALSE) %>% st_transform(4326)
#Importar capas de lotes----
lote <- st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR12",i,".gdb"), layer="Lote", stringsAsFactors = FALSE) %>% st_transform(4326) %>% st_make_valid %>% st_cast("MULTIPOLYGON") %>% filter(!st_is_empty(.)) 

#Importar capa de usos de suelo----
uso <- st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR12",i,".gdb"), layer="Uso", stringsAsFactors = FALSE) %>% left_join(.,indice_uso,by = c("UsoTUso" = "COD"))

#Union de lotes con UPZ
lote_upz <- st_join(st_centroid(lote),upz) %>% filter(!is.na(UPlCodigo))%>% st_set_geometry(NULL)
rm(lote, upz)

#Definir el uso de suelo de cada lote
uso_lote <- left_join(lote_upz,uso,by = c("LotCodigo" = "UsoCLote")) %>% filter(!is.na(UsoTUso), UsoArea > 0)%>% select(Area = UsoArea, Uso = CATEGORIA, UPZ = UPlCodigo)
rm(lote_upz,uso)

#Calculo de entropia de uso de suelo por UPZ
entropia <- uso_lote %>%  group_by(UPZ,Uso) %>% 
  summarise(SumArea = sum(Area,na.rm = TRUE)) %>% 
  group_by(UPZ) %>% 
  mutate(Prop = SumArea/ sum(SumArea,na.rm = TRUE),N = n(), e = - Prop * log(Prop) / log(N)) %>% 
  summarise(Entropia = sum(e,na.rm = TRUE))

#Calculo de porcentaje de uso comercial
uso_comercial <- uso_lote %>% group_by(UPZ,Uso) %>% 
  summarise(SumArea = sum(Area,na.rm = TRUE)) %>% 
  group_by(UPZ) %>% 
  mutate(PrcntComercial = SumArea / sum(SumArea,na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(Uso == "Comercio") %>% 
  subset(select = -c(Uso,SumArea))
entropia<- left_join(entropia,uso_comercial)
rm(uso_comercial)

#Calculo de porcentaje de uso de oficinas
uso_oficinas <- uso_lote %>% group_by(UPZ,Uso) %>% 
  summarise(SumArea = sum(Area,na.rm = TRUE)) %>% 
  group_by(UPZ) %>% 
  mutate(PrcntOficinas = SumArea / sum(SumArea,na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(Uso == "Oficinas") %>% 
  subset(select = -c(Uso,SumArea))
entropia<- left_join(entropia,uso_oficinas)
rm(uso_oficinas)

#Calculo de porcentaje de uso residencial
uso_residen <- uso_lote %>% group_by(UPZ,Uso) %>% 
  summarise(SumArea = sum(Area,na.rm = TRUE)) %>% 
  group_by(UPZ) %>% 
  mutate(PrcntResidencial = SumArea / sum(SumArea,na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(Uso == "Residencial") %>% 
  subset(select = -c(Uso,SumArea))
entropia<- left_join(entropia,uso_residen)
rm(uso_residen)

#Calculo de porcentaje de uso industrial
uso_ind <- uso_lote %>% group_by(UPZ,Uso) %>% 
  summarise(SumArea = sum(Area,na.rm = TRUE)) %>% 
  group_by(UPZ) %>% 
  mutate(PrcntIndustrial = SumArea / sum(SumArea,na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(Uso == "Industria") %>% 
  subset(select = -c(Uso,SumArea))
entropia<- left_join(entropia,uso_ind)
rm(uso_ind)

rm(uso_lote)
Table[i-10]<-list(entropia %>% mutate(Year=2000+i))
rm(entropia)
}

#Consolidación----
Table<-do.call("bind_rows",Table)
Table[is.na(Table)]<-0

write_csv(Table,paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/TABLAS/14. EntropiaUPZ.csv"))
