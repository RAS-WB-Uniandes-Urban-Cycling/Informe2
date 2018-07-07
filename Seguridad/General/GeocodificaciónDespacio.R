#Preparacion del entorno de trabajo----
rm(list=ls())
pacman::p_load(googleway)
carpetaRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/Uniandes - RAS - SDM"
load(paste(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/AccidentesDespacio2011_2015.Rdata",sep=""))
AccidentesBici$Adress<-paste(AccidentesBici$Accidentes.Direccion,", Bogota, Colombia",sep="")

#Geocodificación----
key = "Key"
dt<-data.frame(address=AccidentesBici[,56],stringsAsFactors = FALSE)
res <- apply(dt, 1, function(x){google_geocode(address = x[["address"]],key = key,region="co")})
df_coords<-as.data.frame(matrix(0,1427,3))
names(df_coords)<-c("Accidente","lat","lon")

#Consolidación de la base de datos----
for( i in seq(6427)){
  print(i)
  df_coords[i,1]<-AccidentesBici$Accidente[i]
  replacement<-matrix(NA,1,2)
  coords<-res[[i]]$results$geometry$location[1,1:2]
  if (is.null(coords)){
    df_coords[i,2:3]<-replacement
  }else{
  df_coords[i,2:3]<-coords
  }
}
AccidentesBici<-cbind(AccidentesBici,AccidentesCoords)

#Almacenamiento de la tabla de datos----
save(AccidentesBici,file=paste(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/AccidentesCoords.Rdata"))

#Almaceniamiento del Shape geográfico de accidentes----
pacman::p_load(sf)
h<-na.omit(AccidentesBici)
h <- st_as_sf(h, coords = c("lon", "lat"), crs = 4326)
st_write(h,paste(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/GEODATA/AccidentesDespacio2011_2015/AccidentesDespacio.shp",sep=""),layer="Despacio11_15",driver="ESRI Shapefile")
