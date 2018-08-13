#Preparacion del entorno de trabajo----
rm(list=ls())
require(pacman)
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

#Filtrado de los resultados georeferenciados----
AccidentesBici<-AccidentesBici[!(is.na(AccidentesBici$lat)),]
AccidentesBici<-AccidentesBici[(4.2464<=AccidentesBici$lat)&(AccidentesBici$lat<=4.8857),]
AccidentesBici<-AccidentesBici[(-74.4784<=AccidentesBici$lon)&(AccidentesBici$lon<=-73.7306),]

#Almacenamiento de la tabla de datos----
save(AccidentesBici,file=paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/2. AccidentesCoords.Rdata"))
