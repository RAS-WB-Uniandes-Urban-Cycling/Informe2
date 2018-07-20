#################################
#RAS_WB - Encuesta de movilidad #
#Programmer: Diana Higuera######
#Last update: 27/05/2018#########
#################################
install.packages("readxl")
library("readxl")
library("foreign")
#Misión: Agregar Matriz OD por Localidad
#1. Abrir base
setwd("C:/Users/di-higue/Dropbox/UNIANDES/World Bank/encuesta_movilidad/datos")
OD<-read_excel("matriz_medio_pico_habil.xlsx")
bici<-OD[!(!is.na(OD$f_bicicleta))]
library(tidyr)
bici<-OD %>% drop_na(f_bicicleta)
ZAT<-read.dbf("LOCA_ZAT_NO_NULL.dbf")
#LO VAMOS A HACER PARA EL ORIGEN PRIMERO
bici$ZAT<-bici$zat_origen
ZAT$ZAT<-ZAT$id #Hay dos variables que parecen de identificación, esta es la primera
BICI<-merge (ZAT, bici,"ZAT")
summary(BICI$LocCodigo)
#Escribimos un archivo describiendo las localidades para cada ZAT según la variable id
write.csv(BICI, file= "viajes_ZAT_loca_id.csv", sep= ";", col.names= TRUE) #No exporta bien, por eso la guardo en dbf
write.dbf(BICI, file= "viajes_ZAT_loca-id-dbf")
#Agregar los viajes por localidad
LOCA_ORIGEN_ID<-xtabs(formula=f_bicicleta~LocCodigo, data=BICI)
localidad<-read.dbf("localidad.dbf")
head(localidad)
LOCA_ORI_ID<-merge (localidad, LOCA_ORIGEN_ID,"LocCodigo")
names(LOCA_ORI_ID)[names(LOCA_ORI_ID)=="Freq"] <- "VIAJES_ORIGEN_id"
#Ahora vamos a hacer lo mismo con la otra variable del shape de ZATs
ZAT$ZAT<-ZAT$Zona_Num_N #Hay dos variables que parecen de identificación, esta es la primera
BICI<-merge (ZAT, bici,"ZAT")
summary(BICI$LocCodigo)
LOCA_ORIGEN_zona<-xtabs(formula=f_bicicleta~LocCodigo, data=BICI)
localidad<-read.dbf("localidad.dbf")
head(localidad)
LOCA_ORI_both<-merge (LOCA_ORI_ID, LOCA_ORIGEN_zona,"LocCodigo")
names(LOCA_ORI_both)[names(LOCA_ORI_both)=="Freq"] <- "VIAJES_ORIGEN_zona"
head(LOCA_ORI_both)
#LO VAMOS A HACER PARA EL DESTINO AHORA
bici$ZAT<-bici$zat_destino
ZAT$ZAT<-ZAT$id #Hay dos variables que parecen de identificación, esta es la primera
BICI<-merge (ZAT, bici,"ZAT")
summary(BICI$LocCodigo)
#Escribimos un archivo describiendo las localidades para cada ZAT según la variable id
write.csv(BICI, file= "viajes_ZAT_loca_id.csv", sep= ";", col.names= TRUE) #No exporta bien, por eso la guardo en dbf
write.dbf(BICI, file= "viajes_ZAT_loca-id-dbf")
#Agregar los viajes por localidad
LOCA_DESTINO_ID<-xtabs(formula=f_bicicleta~LocCodigo, data=BICI)
localidad<-read.dbf("localidad.dbf")
head(localidad)
LOCA_DEST_ID<-merge (localidad, LOCA_DESTINO_ID,"LocCodigo")
names(LOCA_DEST_ID)[names(LOCA_DEST_ID)=="Freq"] <- "VIAJES_DEST_id"
#Ahora vamos a hacer lo mismo con la otra variable del shape de ZATs
ZAT$ZAT<-ZAT$Zona_Num_N #Hay dos variables que parecen de identificación, esta es la primera
BICI<-merge (ZAT, bici,"ZAT")
summary(BICI$LocCodigo)
LOCA_DESTINO_zona<-xtabs(formula=f_bicicleta~LocCodigo, data=BICI)
localidad<-read.dbf("localidad.dbf")
head(localidad)
LOCA_DEST_both<-merge (LOCA_DEST_ID, LOCA_DESTINO_zona,"LocCodigo")
names(LOCA_DEST_both)[names(LOCA_DEST_both)=="Freq"] <- "VIAJES_DEST_zona"
head(LOCA_DEST_both)
#Poner todo en la misma tabla
final<-merge(LOCA_ORI_both, LOCA_DEST_both, by=c("LocCodigo", "LocNombre", "LocAAdmini", "LocArea", "SHAPE_Leng", "SHAPE_Area"))
write.csv(final, file= "viajes_localidad.csv", sep= ";", col.names= TRUE) 
write.dbf(final, file="viajes_localidad.dbf")

#Fin!!! 