library(osmar)
library(osmdata)
library(sf)
library(tidyverse)
library(tmap)
library(reshape)
library(readxl)
library(googleway)
library(NISTunits)
library(units)
library(tmaptools)
library(rcompanion)
library(kernlab)
library(stplanr)


#Se importa los archivos de excel consultas GoogleApi
  dist_vel1 <- read_excel("Documents/Universidad de los Andes/Tesis/LTS-Versión 1/DatosGoogleAPI1.xlsx")
  dist_vel2 <- read_excel("Documents/Universidad de los Andes/Tesis/LTS-Versión 1/DatosGoogleAPI2.xlsx")

#Se define la ruta de la base de datos
  ruta_archivo<-"/Users/alejandropalacio/Documents/BancoMundial-Targa/informacion_geografica/"
  ruta_archivo2<-"/Users/alejandropalacio/Desktop/"

#Consulta layers en la base de datos
  st_layers(paste0(ruta_archivo,"IDECA_0318.gdb"))
  st_layers(paste0(ruta_archivo2,"malla vial bogotá.shp"))

#Se almacena los layer en cada variable definida

  malla_vial<-st_read(paste0(ruta_archivo2,"malla vial bogotá.shp"), stringsAsFactors = FALSE)%>% filter(!(highway %in% c("cycleway","service","steps","footway"))) 
  ciclo_rutas<-st_read(paste0(ruta_archivo,"IDECA_0318.gdb"),layer = "RBic",stringsAsFactors = FALSE) %>% st_transform(4326) 
  zats<-st_read(paste0(ruta_archivo,"IDECA_0318.gdb"),layer = "SCAT",stringsAsFactors = FALSE) %>% filter(SCaNombre %in% c("DOCE DE OCTUBRE","CIUDAD SALITRE NOR-ORIENTAL","CIUDAD SALITRE SUR-ORIENTAL")) %>%
    st_transform(4326)
  calzadas<-st_read(paste0(ruta_archivo,"IDECA_0318.gdb"),layer = "Calz",stringsAsFactors = FALSE,promote_to_multi = FALSE) %>% st_transform(4326) %>% filter(st_is_valid(.))
  malla_vial<-st_read(paste0(ruta_archivo,"IDECA_0318.gdb"),layer = "MVI", stringsAsFactors = FALSE)%>% st_transform(4326) 
  
  
#Muestra la informacion almacenada en la variable
  
  malla_vial
  ciclo_rutas
  zats

#Malla Vial
          
          #Se hace un Join espacial entre malla vial y zats
              
            union_malla_vial<-malla_vial %>% st_join(select(zats,SCaNombre),left = FALSE, largest=TRUE)  %>% mutate(ID=row_number())
                
          #Se dividen cada segmento en uno o mas segmentos
                
            segmentos<-tibble(gsection(union_malla_vial)) %>%st_as_sf()
                
            union_malla_vial <- segmentos %>% st_join(union_malla_vial,largest=TRUE) %>% mutate(ID=row_number()) 
                
          #Se hace un Join espacial entre union_malla_vial y calzadas
                
            union_malla_vial<-union_malla_vial %>% st_join(select(calzadas,CalAncho, CalNCarril),left = TRUE,largest=TRUE) 
            
          #Se obtienen las coordenadas (Lat/Long) de punto inicial de cada segmento
          
            coordenadas_incio_seg<-st_coordinates(union_malla_vial) %>% as_tibble()%>% group_by(L1)%>% filter(row_number()==1) %>% 
              ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud Inicio",Y="Latitud Inicio"))
          
            coordenadas_fin_seg<-st_coordinates(union_malla_vial) %>% as_tibble() %>% group_by(L1) %>% filter(row_number()==n()) %>% 
              ungroup()%>%transmute(L1,Y,X)%>% rename(c(L1="ID",X="Longitud Fin",Y="Latitud Fin"))
          
            coordenadas_malla_vial<-coordenadas_incio_seg  %>% left_join(coordenadas_fin_seg, By=ID)
          
          #Se hace join con la capa union_malla_vial
          
            union_malla_vial<-union_malla_vial  %>% left_join(coordenadas_malla_vial, By=ID)

   
#Red-ciclorutas
          
          #Se hace un Join espacial entre malla vial y zats
          
            union_ciclo_rutas<-ciclo_rutas %>% st_join(select(zats,SCaNombre) %>% st_buffer(0.0001347),left = FALSE, largest=TRUE) %>% mutate(ID=row_number())
          
          #Se obtienen las coordenadas (Lat/Long) de punto inicial de cada segmentos
          
            coordenadas_incio_seg<-st_coordinates(union_ciclo_rutas) %>% as_tibble() %>% group_by(L2) %>% filter(row_number()==1) %>% 
              ungroup()%>%transmute(L2,Y,X)%>% rename(c(L2="ID",X="Longitud Inicio C",Y="Latitud Inicio C"))
          
            coordenadas_fin_seg<-st_coordinates(union_ciclo_rutas) %>% as_tibble() %>% group_by(L2) %>% filter(row_number()==n()) %>% 
              ungroup()%>%transmute(L2,Y,X)%>% rename(c(L2="ID",X="Longitud Fin C",Y="Latitud Fin C"))
          
            coordenadas_ciclo_rutas<-coordenadas_incio_seg  %>% left_join(coordenadas_fin_seg, By=ID)
  
          #Se hace join con la capa union_ciclo_rutas
          
            union_ciclo_rutas<-union_ciclo_rutas  %>% left_join(coordenadas_ciclo_rutas, By=ID)
          
        
          #Se hace un buffer de x metros de anchos a la union_ciclo_rutas
          
            buffer_ciclo_rutas<-st_buffer(union_ciclo_rutas,dist = 0.000200)

          
#Join Spacial union_malla_vial y buffer_ciclo_rutas
        
          #Se hace un join espacial entre union_malla_vial y el buffer_ciclo_rutas
            
            union_malla_vial_ciclo_rutas<-union_malla_vial %>% st_join(select(buffer_ciclo_rutas,"Latitud Inicio C","Longitud Inicio C","Latitud Fin C","Longitud Fin C")) 
          
          #Se calcula el la pendiente de cada segmento (union_malla_vial y union_ciclo_rutas), se calcula el angulo que forman dichos segmentos y se eliminan los segmentos cuyo angulo es mayor a 20 grados 
            
            union_malla_vial_ciclo_rutas <-union_malla_vial_ciclo_rutas %>% mutate(p1=((`Longitud Fin`-`Longitud Inicio`)/(`Latitud Fin`-`Latitud Inicio`)),p2=((`Longitud Fin C`-`Longitud Inicio C`)/(`Latitud Fin C`-`Latitud Inicio C`)))%>%
              mutate(angulo=NISTradianTOdeg(atan2(p2-p1,1+p2*p1))) %>% filter( abs(angulo)<=25 )
          
          #Se ordenan los angulos de menor a mayor y se eligen el primer registro de cada segmento
          
            union_malla_vial_ciclo_rutas<-union_malla_vial_ciclo_rutas[order(union_malla_vial_ciclo_rutas$angulo),] %>% group_by(ID) %>% filter(row_number()==1) %>% 
              ungroup() %>% mutate(CicloRuta=1)
          
          #Se hace un left-join entre union_malla_vial y union_malla_vial_ciclo_rutas
 
            union_malla_vial<-union_malla_vial %>% left_join(select( st_set_geometry(union_malla_vial_ciclo_rutas,NULL), CicloRuta, ID)) 
          
          #A los registros que en el campo ciclo ruta tenga valor NULL, se le asigna el valor de 0
          
            union_malla_vial$CicloRuta[is.na(union_malla_vial$CicloRuta)] <- 0
            union_malla_vial$lanes[is.na(union_malla_vial$CicloRuta)] <- 0
            
            
#Google API consultas
            
    numero_registros <- nrow(union_malla_vial)
          
    datos_free_flow<-matrix(nrow=numero_registros,ncol=6)
    datos_hora_pico_manana<-matrix(nrow=numero_registros,ncol=3)
    datos_hora_pico_tarde<-matrix(nrow=numero_registros,ncol=3)
    
    colnames(datos_free_flow) <-c("Longitud_1","TProm_1","TFF_1","Longitud_2","TProm_2","TFF_2")
    colnames(datos_hora_pico_manana) <-c("Longitud","TProm","THPM")
    colnames(datos_hora_pico_tarde) <-c("Longitud","TProm","THPT")
            
            for(i in 1:numero_registros){
              
              origen=paste(toString(union_malla_vial$`Latitud Inicio`[i]) ,"+", toString(union_malla_vial$`Longitud Inicio`[i]))
              destino=paste(toString(union_malla_vial$`Latitud Fin`[i]) ,"+", toString(union_malla_vial$`Longitud Fin`[i]))
              
              #Free Flow
              
                  distancia <- google_distance(origins = c(origen) ,destinations = c(destino) ,
                                               mode="driving",  departure_time =as.POSIXct("09-04-2018 00:00:00", format = "%m-%d-%Y %H:%M:%S"),
                                               traffic_model="best_guess",  key="AIzaSyCB484vIOsEMKJoKAK8Lp4OO4sd6EtM6bk",simplify = TRUE )
                  
                  datos_free_flow[i,1]<-distancia$rows$elements[[1]]$distance$value
                  datos_free_flow[i,2]<-distancia$rows$elements[[1]]$duration$value
                  datos_free_flow[i,3]<-distancia$rows$elements[[1]]$duration_in_traffic$value
                  
                  distancia <- google_distance(origins = c(destino) ,destinations = c(origen) ,
                                               mode="driving",  departure_time =as.POSIXct("09-04-2018 00:00:00", format = "%m-%d-%Y %H:%M:%S"),
                                               traffic_model="best_guess",  key="AIzaSyCB484vIOsEMKJoKAK8Lp4OO4sd6EtM6bk",simplify = TRUE )
                  
                  
                  datos_free_flow[i,4]<-distancia$rows$elements[[1]]$distance$value
                  datos_free_flow[i,5]<-distancia$rows$elements[[1]]$duration$value
                  datos_free_flow[i,6]<-distancia$rows$elements[[1]]$duration_in_traffic$value
                  
              #Hora Pico por la Mañana y tarde  
                   
                  if (datos_free_flow[i,1]<=datos_free_flow[i,4]) {
                    
                      hora_pico_manana <- google_distance(origins = c(origen) ,destinations = c(destino) ,
                                                   mode="driving",  departure_time =as.POSIXct("09-04-2018 7:30:00", format = "%m-%d-%Y %H:%M:%S"),
                                                   traffic_model="best_guess",  key="AIzaSyCB484vIOsEMKJoKAK8Lp4OO4sd6EtM6bk",simplify = TRUE )
                      
                      hora_pico_tarde <- google_distance(origins = c(origen) ,destinations = c(destino) ,
                                                          mode="driving",  departure_time =as.POSIXct("09-04-2018 7:30:00", format = "%m-%d-%Y %H:%M:%S"),
                                                          traffic_model="best_guess",  key="AIzaSyCB484vIOsEMKJoKAK8Lp4OO4sd6EtM6bk",simplify = TRUE )
                      
                  } else {
                    
                      hora_pico_manana <- google_distance(origins = c(destino) ,destinations = c(origen) ,
                                                   mode="driving",  departure_time =as.POSIXct("09-04-2018 18:00:00", format = "%m-%d-%Y %H:%M:%S"),
                                                   traffic_model="best_guess",  key="AIzaSyCB484vIOsEMKJoKAK8Lp4OO4sd6EtM6bk",simplify = TRUE )
                  
                      hora_pico_tarde <- google_distance(origins = c(destino) ,destinations = c(origen) ,
                                                         mode="driving",  departure_time =as.POSIXct("09-04-2018 18:00:00", format = "%m-%d-%Y %H:%M:%S"),
                                                         traffic_model="best_guess",  key="AIzaSyCB484vIOsEMKJoKAK8Lp4OO4sd6EtM6bk",simplify = TRUE )
                  }        
                  
                  
                  datos_hora_pico_manana[i,1]<-hora_pico_manana$rows$elements[[1]]$distance$value
                  datos_hora_pico_manana[i,2]<-hora_pico_manana$rows$elements[[1]]$duration$value
                  datos_hora_pico_manana[i,3]<-hora_pico_manana$rows$elements[[1]]$duration_in_traffic$value
                  
                  datos_hora_pico_tarde[i,1]<-hora_pico_tarde$rows$elements[[1]]$distance$value
                  datos_hora_pico_tarde[i,2]<-hora_pico_tarde$rows$elements[[1]]$duration$value
                  datos_hora_pico_tarde[i,3]<-hora_pico_tarde$rows$elements[[1]]$duration_in_traffic$value
          
            }
            
            
            write.csv(capa_LTS,"Capa.csv")
            write.csv(dist_vel2,"DatosGoogleAPI2.csv")            

            
            
#Procesamiento de datos obtenidos de Google API 
            
            datos_google_API<-matrix(nrow=numero_registros,ncol=8)
            colnames(datos_google_API) <-c("Longitud","TProm","Vprom","TFF","THPM","THPT","Trafico","ID")
            
            for (i in 1:numero_registros) {
              
              datos_google_API[i,8]=i
              
              
              if (datos_free_flow[i,1]<=datos_free_flow[i,4]) {
                
                  datos_google_API[i,1]=datos_free_flow[i,1]
                  datos_google_API[i,2]=datos_free_flow[i,2]
                  datos_google_API[i,4]=datos_free_flow[i,3]
                
              } else {
                
                  datos_google_API[i,1]=datos_free_flow[i,4]
                  datos_google_API[i,2]=datos_free_flow[i,5]
                  datos_google_API[i,4]=datos_free_flow[i,6]
              }      
            
              
              datos_google_API[i,3]=datos_google_API[i,1]*(1/1000)*(3600/1)/datos_google_API[i,2]
              datos_google_API[i,5]=datos_hora_pico_manana[i,2]
              datos_google_API[i,6]=datos_hora_pico_tarde[i,2]
              
              
              
              if (datos_google_API[i,2]==0) {
                for(j in 1:6){
                  datos_google_API[i,j]=0
                }
              }
              
              
              if (datos_google_API[i,4]<min(datos_google_API[i,5],datos_google_API[i,6])) {
                
                datos_google_API[i,7]=1-abs(datos_google_API[i,4]/min(datos_google_API[i,5],datos_google_API[i,6]))
                
              } else {
                
                datos_google_API[i,7]=0
              } 
              
              
            }
            
            datos_google_API<-data.frame(datos_google_API)
            
#Capa LTS
          capa_LTS<- union_malla_vial%>% transmute(ID,CicloRuta,lanes=if_else(is.na(lanes),as.numeric(CalNCarril),as.numeric(lanes)), CalAncho=as.numeric(CalAncho)) %>% left_join(select(datos_google_API, Vprom, Trafico,ID))
          
#Clustering 
          
          
          capa_clusters<-st_set_geometry(capa_LTS,NULL) %>% transmute(Vprom,lanes,CicloRuta, CalAncho, Trafico)
          clusters <- tibble(cluster=as.vector(specc(as.matrix(capa_clusters),centers=4)))
          capa_LTS2 <- capa_LTS %>% bind_cols(clusters) %>% st_as_sf()
          
          mapa<-tm_shape(zats)+tm_polygons(col = "green",alpha = 0.3,title="ZATS")+
            tm_shape(capa_LTS2)+tm_lines(col="cluster",style ="cat" ,scale=5 ,palette = get_brewer_pal("Accent",4,plot = F) ,title.col ="LTS", textNA = "No", colorNA = "Red")+
            tmap_mode("view")+tm_view(alpha = 1, basemaps = "OpenStreetMap.BlackAndWhite")
          mapa
          
          a=4
          estadisticas_vel<-st_set_geometry(capa_LTS2,NULL) %>% filter(cluster==a)%>% transmute(Vprom)%>% summary()
          estadisticas_vel
          estadisticas_ancho<-st_set_geometry(capa_LTS2,NULL) %>% filter(cluster==a)%>% transmute(CalAncho)%>% summary()
          estadisticas_ancho
          ciclo_resumen <- table(st_set_geometry(capa_LTS2,NULL) %>% filter(cluster==a)%>% transmute(CicloRuta))
          ciclo_resumen

          estadisticas_trafico<-st_set_geometry(capa_LTS2,NULL) %>% filter(cluster==a)%>%transmute(lanes)%>% summary()
          estadisticas_trafico
          
          capa_LTS2$cluster[capa_LTS2$cluster==1] <- "LTS 1"
          capa_LTS2$cluster[capa_LTS2$cluster==4] <- "LTS 2"
          capa_LTS2$cluster[capa_LTS2$cluster==2] <- "LTS 3"
          capa_LTS2$cluster[capa_LTS2$cluster==3] <- "LTS 4"
       
#Mapas 
          
          mapa<-tm_shape(zats)+tm_polygons(col = "yellow",alpha = 0.3,title="ZATS")+
            tm_shape(capa_LTS)+tm_lines(lwd =3,col = "red",style ="pretty",title.col ="Malla vial")+
            tm_shape(union_ciclo_rutas)+tm_lines(lwd =3,col = "blue",title.col ="Ciclorutas")+
            tmap_mode("view")+tm_view(alpha = 1, basemaps = "OpenStreetMap.BlackAndWhite")
          mapa
          
          mapa<-tm_shape(zats)+tm_polygons(col = "blue",alpha = 0.3,title="ZATS")+
            tm_shape(capa_LTS)+tm_lines(col="Vprom",style ="pretty" ,scale=5 ,palette = get_brewer_pal("Reds",6,plot = F) ,title.col ="Velocidad Promedio", textNA = "No", colorNA = "Red")+
            tmap_mode("view")+tm_view(alpha = 1, basemaps = "OpenStreetMap.BlackAndWhite")
          mapa
          
          mapa<-tm_shape(zats)+tm_polygons(col = "blue",alpha = 0.3,title="ZATS")+
            tm_shape(capa_LTS)+tm_lines(col="Trafico",style ="kmeans" ,scale=5,n=6 ,palette = get_brewer_pal("Reds",6,plot = F),title.col ="Tráfico", textNA = "No", colorNA = "Red")+
            tmap_mode("view")+tm_view(alpha = 1, basemaps = "OpenStreetMap.BlackAndWhite")
          mapa
          
          mapa<-tm_shape(zats)+tm_polygons(col = "pink",alpha = 0.3,title="ZATS")+
            tm_shape(capa_LTS)+tm_lines(col="CicloRuta",style ="cat" ,scale=5 ,palette = get_brewer_pal("Dark2",6,plot = F) ,title.col ="Presencia Ciclo Ruta", textNA = "No", colorNA = "Red")+
            tm_shape(union_ciclo_rutas)+tm_lines(lwd =3,col = "blue",title.col ="Ciclorutas")+
            tmap_mode("view")+tm_view(alpha = 1, basemaps = "OpenStreetMap.BlackAndWhite")
          mapa
          
          mapa<-tm_shape(zats)+tm_polygons(col = "blue",alpha = 0.3,title="ZATS")+
            tm_shape(capa_LTS)+tm_lines(col="CalAncho",style ="pretty" ,scale=5 ,palette = get_brewer_pal("Reds",6,plot = F) ,title.col ="Ancho malla vial (Metros)")+
            tmap_mode("view")+tm_view(alpha = 1, basemaps = "OpenStreetMap.BlackAndWhite")
          mapa
          
          mapa<-tm_shape(zats)+tm_polygons(col = "blue",alpha = 0.3,title="ZATS")+
            tm_shape(capa_LTS)+tm_lines(col="lanes",style ="cat" ,scale=5 ,palette = get_brewer_pal("Dark2",6,plot = F) ,title.col ="Numero Carriles")+
            tmap_mode("view")+tm_view(alpha = 1, basemaps = "OpenStreetMap.BlackAndWhite")
          mapa
          
          mapa<-tm_shape(zats)+tm_polygons(col = "blue",alpha = 0.3,title="ZATS")+
            tm_shape(union_malla_vial)+tm_lines(col="CalNCarril",style ="cat" ,scale=5 ,palette = get_brewer_pal("Dark2",6,plot = F) ,title.col ="Numero Carriles")+
            tmap_mode("view")+tm_view(alpha = 1, basemaps = "OpenStreetMap.BlackAndWhite")
          mapa
          
          mapa<-tm_shape(zats)+tm_polygons(col = "blue",alpha = 0.3,title="ZATS")+
            tm_shape(capa_LTS_Clustering)+tm_lines(col="clusterCut",style ="cat" ,scale=5 ,palette = get_brewer_pal("Set1",4,plot = F) ,title.col ="LTS", textNA = "No", colorNA = "Red")+
            tm_shape(union_ciclo_rutas)+tm_lines(lwd =3,col = "blue",title.col ="Ciclorutas")+
            tmap_mode("view")+tm_view(alpha = 1, basemaps = "OpenMapSurfer.Roads")
          mapa
          


#################################################################################################

vel_traf<-matrix(nrow=numero_registros,ncol=7)
colnames(vel_traf) <-c("Longitud","TProm","TFF","Vprom","VFF","Trafico","ID")

for (i in 1:numero_registros) {
  
  vel_traf[i,1]=min(dist_vel1[i,1],dist_vel2[i,1])
  vel_traf[i,2]=min(dist_vel1[i,2],dist_vel2[i,2])
  vel_traf[i,3]=min(dist_vel1[i,3],dist_vel2[i,3])

  
  vel_traf[i,4]=vel_traf[i,1]*(1/1000)*(3600/1)/vel_traf[i,2]
  vel_traf[i,5]=vel_traf[i,1]*(1/1000)*(3600/1)/vel_traf[i,3]
  
  if ( vel_traf[i,3]==0 & vel_traf[i,2]!=0 ) {
    vel_traf[i,5]=vel_traf[i,4]
  }
  
  vel_traf[i,6]=(vel_traf[i,5]-vel_traf[i,4])/vel_traf[i,5]
  
  if (vel_traf[i,1]==0||vel_traf[i,2]==0) {
    vel_traf[i,4]=0
    vel_traf[i,5]=0
    vel_traf[i,6]=0
  }
  vel_traf[i,7]=i
  
  if (vel_traf[i,6]<0) {
    vel_traf[i,6]=0
  }
}

vel_traf<-data.frame(vel_traf)

union_malla_vial <- union_malla_vial %>% left_join(select(vel_traf,ID,Vprom,VFF,Trafico))



#Estadísticas descriptivas

#Velocidad

estadisticas_vel<-st_set_geometry(capa_LTS,NULL)%>% transmute(Vprom)%>% summary()
estadisticas_vel

boxplot(capa_LTS$Vprom, col = "lightskyblue", main="Velocidad Promedio",ylab="Velocidad (km/h)")

mean_vprom<-groupwiseMean(data = capa_LTS, var = "Vprom", conf=0.95)
qplot(x="Velocidad Promedio", y=Mean, data=mean_vprom)+geom_errorbar(aes(ymin=Trad.lower, ymax=Trad.upper,width=0.05))

plotDensityHistogram(capa_LTS$Vprom, col="lightskyblue",xlab="Velocidad (km/h)",ylab="freq", main = "Velocidad Promedio")


#Tráfico

estadisticas_trafico<-st_set_geometry(capa_LTS,NULL) %>% transmute(Trafico)%>% summary()

estadisticas_trafico

boxplot(capa_LTS$Trafico, col = "lightskyblue", main="Tráfico", ylab="Índice de Tráfico")

plotDensityHistogram(capa_LTS$Trafico, col="lightskyblue",xlab="Índice de Tráfico",ylab="freq", main = "Tráfico")


#Cicloruta

ciclo_resumen <- table(capa_LTS$CicloRuta)

ciclo_resumen
porcentaje_cobertura = ciclo_resumen[2]/ciclo_resumen[1]
porcentaje_cobertura
pie(ciclo_resumen)
barplot(ciclo_resumen,col=c("white","lightskyblue"))


#CalAncho

estadisticas_ancho<-st_set_geometry(capa_LTS,NULL)  %>% transmute(CalAncho)%>% summary()
estadisticas_ancho

boxplot(capa_LTS$CalAncho, col = "lightskyblue", main="Ancho de la Vía", ylab="Ancho (m)")

plotDensityHistogram(capa_LTS$CalAncho, col="lightskyblue",xlab="Ancho (m)",ylab="freq", main = "Ancho de la Vía")


#Lanes

estadisticas_ancho<-st_set_geometry(capa_LTS,NULL)  %>% transmute(lanes)%>% summary()
estadisticas_ancho

boxplot(capa_LTS$lanes, col = "lightskyblue", main="Número de carriles", ylab="Número de carriles")

hist(capa_LTS$lanes, col = "lightskyblue", xlab="Carriles",ylab="Frecuencia", main = "Número de Carriles") 
lines(density(capa_LTS$Lanes), lwd=1)



c<-st_as_sf(coordenadas_incio_seg,coords = c("Longitud Inicio", "Latitud Inicio"),crs=4326)


st_write(capa_LTS_Clustering, "hola5.shp")
