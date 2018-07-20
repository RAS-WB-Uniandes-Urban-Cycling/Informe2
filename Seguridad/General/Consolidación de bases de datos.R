#Preparación del entorno de trabajo
rm(list=ls())
carpetaRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/Uniandes - RAS - SDM"
gitRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/proBikePolicies"
pacman::p_load(sf)
pacman::p_load(dplyr)

#Carga de las diferentes bases de datos en memoria
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/AccidentesCoords.Rdata"))
despacio<-AccidentesBici[!(is.na(AccidentesBici$lat)),]
despacio<-st_as_sf(despacio, coords = c("lon", "lat"), crs = 4326)
rm(AccidentesBici)
wri<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/WRI"))
SDM<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Shapefiles"),layer="Accidente")

#Filtrado de los accidentes de ciclistas durante el periodo de estudio
SDM<-SDM[SDM$ANO_OCURRE%in%c(2011:2015),]
wri<-wri[(wri$ANO_OCURRE%in%c(2011:2015))&(wri$CONDICION=="CICLISTA"),]

#Extraccion de las caracteristicas de identificación únicas
#   -SDM: FORMULARIO, CODIGO_ACC, FECHA_OCUR, HORA_OCURR, ANO_OCURRE, MES_OCURRE, DIRECCIÓN
SDM<-SDM[,c("FORMULARIO", "CODIGO_ACC", "FECHA_OCUR", "HORA_OCURR", "ANO_OCURRE", "MES_OCURRE", "DIRECCION")]
#   -wri: OBJECTID, FORMULARIO, CODIGO_ACC, FECHA_OCUR, HORA_OCURR, ANO_OCURRE, MES_OCURRE, DIRECCIÓN
wri<-wri[,c("OBJECTID", "FORMULARIO", "CODIGO_ACC", "FECHA_OCUR", "HORA_OCURR", "ANO_OCURRE", "MES_OCURRE", "DIRECCION")]
#   -despacio: Accidente,Accidentes.HoraOcurrencia,Accidentes.Dirección
despacio<-despacio[,c("Accidente","Accidentes.HoraOcurrencia","Accidentes.Direccion")]

localidad<-st_read("/Users/germancarvajal/OneDrive - Universidad de Los Andes/UNIANDES - RAS - SDM/BASES DE DATOS/IDECA_0318.gdb",layer = "Loca")

pacman::p_load(tmap)

localidad<-localidad[-9,]

tm_shape(localidad)+tm_polygons()+tm_shape(despacio)+tm_dots()
