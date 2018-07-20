#Preparación del entorno de trabajo----
rm(list=ls())
carpetaRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/Uniandes - RAS - SDM"
gitRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/proBikePolicies"
pacman::p_load(sf)
pacman::p_load(tmap)

#Lectura de las bases de datos----
zats<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/ZATs"))
localidad<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/IDECA_0318.gdb"),layer = "Loca")

#Extracción de centroides e intersección de ZATs y localidades
zats_cen<-st_centroid(zats)
zats_loca<-st_join(zats_cen,localidad,st_within)
zats_loca$Bogota<-ifelse(is.na(zats_loca$LocNombre),"lightgray","darkolivegreen3")
st_geometry(zats_loca)<-st_geometry(zats)
rm(zats_cen,zats)

#Eliminación de las ZATs sin intersección
zats_loca<-zats_loca[zats_loca$Bogota=="darkolivegreen3",]

#Visualización de las ZATs dentro de localidades (verde)----
m<-tm_shape(localidad)+tm_polygons(col="lightblue2")+tm_shape(zats_loca)+tm_fill("Bogota")+tm_borders("blue")
tmap_leaflet(m)

#Exportar como capa de simpleFeatures
zats_loca$Bogota<-NULL
st_write(zats_loca,paste0(carpetaRAS,"/RESULTADOS/GENERAL/GEO-DATA/ZATs_Localidad.shp"),driver = "ESRI Shapefile")
