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

############### OTHER TIME VARYING VARIABLES ####################################################################

#Lectura de las bases de datos de ciclorutas----

#IDECA y Catastro Bogotá
#Valor de referencia y ciclorutas por localidad
Table<-list(NA,NA,NA,NA,NA,NA)
for(i in 12:17){
  
  val<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Catastro Bogotá/Valor de referencia"),layer=paste0("Val20",i), stringsAsFactors = FALSE) %>% st_transform(4326)
  upz <- st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR12",i,".gdb"),layer='UPla', stringsAsFactors = FALSE) %>% st_transform(4326)
  val<-st_join(val %>% st_transform(3116),upz %>% st_transform(3116),largest=TRUE) %>% 
    st_set_geometry(NULL) %>% 
    group_by(UPlCodigo) %>% 
    summarize(Valor=mean(V_REF))
  
  rcic<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR12",i,".gdb"),layer = ifelse(i<17,"Cicl","RBic"),type = 5) %>% 
    st_transform(3116) %>% 
    st_cast("MULTILINESTRING") %>% 
    st_make_valid
  upz2<- upz %>%
    st_transform(crs=3116) %>% 
    st_cast("MULTILINESTRING", group_or_split=FALSE) %>% 
    st_make_valid
  rcic<-st_split(rcic, st_combine(upz2)) %>% st_collection_extract( type = c("LINESTRING"))
  rcic<-st_join(rcic %>% st_transform(3116),upz %>% st_transform(3116),largest=TRUE) %>% 
    mutate(Cicl=as.numeric(st_length(.))) %>% 
    group_by(UPlCodigo) %>% 
    summarise(CicloRuta=sum(Cicl)) %>% 
    st_set_geometry(NULL)
  val<-left_join(val,rcic)
  
  Table[i-11]<-list(val %>% mutate(Year=2000+i))
}

#Consolidación----
Table<-do.call("bind_rows",Table)
Table[is.na(Table)]<-0
rm(val,upz,upz2,rcic,i)

# Almacenamiento
write_csv(Table,paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/TABLAS/14.5 TimeVarying.csv"))
