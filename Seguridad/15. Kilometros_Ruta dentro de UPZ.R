#Preparación del entorno de trabajo----
rm(list=ls())
carpetaRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/Uniandes - RAS - SDM"
gitRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/proBikePolicies"
require(pacman)
pacman::p_load(tidyverse)
pacman::p_load(sf)
pacman::p_load(lwgeom)
pacman::p_load(tmap)
pacman::p_load(parallel)
pacman::p_load(doMC)
pacman::p_load(ggspatial)
pacman::p_load(scales)
pacman::p_load(ggnewscale)
pacman::p_load(colorspace)
pacman::p_load(ggpubr)
pacman::p_load(showtext)
pacman::p_load(pals)
font_add("Helvetica Light",paste(gitRAS,"/Seguridad/Helvetica Light.ttf",sep=""))
setwd(paste(carpetaRAS,"/RESULTADOS/SEGURIDAD/Resultados-Inglés",sep=""))
registerDoMC(4)

#Carga de la base de datos de viajes realizados----
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/10.5 Viajes_Recorridos.Rdata"))
viajesRecorridos<-viajesRecorridos %>% st_transform(crs=3116) %>% st_make_valid

#Creación de la lista de almacenamiento de resultados----
results<-list(NA)

#Splitting de segmentos para el año 2011----
i=11
UPZ<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR10",i,".gdb"),layer="UPla") %>% 
  st_transform(crs=4326) %>% 
  st_transform(crs=3116)
BB<-UPZ %>% st_cast("MULTILINESTRING", group_or_split=FALSE)
A<-(viajesRecorridos %>% filter(AÑO==2000+i) %>% st_cast("MULTILINESTRING"))
results[i-10] <- list(st_split(A, st_combine(BB)) %>% st_collection_extract( type = c("LINESTRING")))
results[i-10]<-list(results[[i-10]] %>% mutate(Year=2000+i))
rm(A,BB)

#Spliting de segmentos para los años 2012-2017----
Resultados<-foreach(i=12:17) %dopar% {
  UPZ<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR12",i,".gdb"),layer='UPla', stringsAsFactors = FALSE) %>% 
    st_transform(crs=4326) %>% 
    st_transform(crs=3116) %>% 
    st_cast("MULTILINESTRING", group_or_split=FALSE)
  A<-(viajesRecorridos %>% filter(AÑO==2000+i) %>% st_cast("MULTILINESTRING"))
  results[i-10] <- list(st_split(A, st_combine(UPZ)) %>% st_collection_extract( type = c("LINESTRING")))
}

for(i in 12:17){
  Resultados[i-11]<-list(Resultados[[i-11]][[1]] %>% mutate(Year=2000+i))
}
rm(i)

#Asignación de UPZ a cada tramo de ruta----
i=11
UPZ<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR10",i,".gdb"),layer="UPla") %>% 
  st_transform(crs=4326) %>% 
  st_transform(crs=3116)
results[i-10]<-list(st_join(results[[i-10]] %>% st_transform(crs=3116),UPZ[,c("UPLCODIGO")],left=TRUE,largest=TRUE))
results[i-10]<-list(results[[i-10]] %>% rename(UPlCodigo=UPLCODIGO))

for(i in 12:17){
  UPZ<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR12",i,".gdb"),layer='UPla', stringsAsFactors = FALSE) %>% 
    st_transform(crs=4326) %>% 
    st_transform(crs=3116) 
  Resultados[i-11]<-list(st_join(Resultados[[i-11]] %>% 
                                   st_transform(crs=3116),
                                 UPZ[,c("UPlCodigo")],
                                 left=TRUE,
                                 largest=TRUE))
}
rm(UPZ,i)

#Consolidación de resultados en una única tabla----
Table<-do.call("rbind",Resultados)
Table<-rbind(Table,results[[1]])
Table$Km<-as.numeric(st_length(Table)/1000)
rm(Resultados,results,viajesRecorridos)

#Almacenamiento de resultados----
save(Table,file = paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/15.  Kilometros_Ruta por UPZ.Rdata"))

#Mapa explicativo del procedimiento----
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/15.  Kilometros_Ruta por UPZ.Rdata"))
zat <- st_read(paste0(carpetaRAS,"/BASES DE DATOs/ZATs")) %>% 
  st_transform(crs=4326) %>% 
  st_transform(crs=3116)

UPZ<-st_read(paste0(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR1215.gdb"),layer='UPla', stringsAsFactors = FALSE) %>% 
  st_transform(crs=4326) %>% 
  st_transform(crs=3116) 

bogota <- UPZ %>% 
  filter(!str_detect(UPlCodigo, "UPR")) %>% 
  st_union()

plotting_route_original <- viajesRecorridos %>% 
  filter(ZAT_SRC==154,
         ZAT_DST==618,
         Año==2015)

plotting_route_split <- Table %>% 
  filter(ZAT_SRC==154,
         ZAT_DST==618,
         Año==2015)

map1 <- ggplot() +
  geom_sf(data = bogota,
          color = "grey20",
          fill = "grey99") +
  geom_sf(data = UPZ %>% 
            slice(st_crosses(plotting_route_original, UPZ)[[1]]),
          fill = "grey90",
          color = "grey20",
          lwd = 0.5) +
  geom_sf(data = zat %>% 
            filter(Zona_Num_N%in%c(154, 618)) %>% 
            mutate(caracter = ifelse(Zona_Num_N==154, "o", "d")),
          mapping = aes(fill = caracter),
          lwd = 0) +
  annotation_scale(location = "bl", 
                   width_hint = 0.4) +
  annotation_north_arrow(location = "bl", 
                         which_north = "true", 
                         pad_x = unit(0, "cm"), 
                         pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering) +
  xlab("Longitud") + 
  ylab("Latitud") +
  scale_fill_manual(breaks = c("d", "o"),
                    values = c("darkred", "forestgreen"),
                    labels = c("Destination" , "Origin"), 
                    name = "ZAT") +
  theme(panel.grid.major = element_line(color = gray(0.5), 
                                        linetype = "dashed", 
                                        size = 0.2), 
        panel.background = element_rect(fill = alpha("forestgreen", 0.2)))

map2 <- ggplot() +
  geom_sf(data = bogota,
          color = "grey20",
          fill = "grey99") +
  geom_sf(data = UPZ %>% 
            slice(st_crosses(plotting_route_original, UPZ)[[1]]),
          fill = "grey90",
          color = "grey20",
          lwd = 0.5) +
  geom_sf(data = zat %>% 
            filter(Zona_Num_N%in%c(154, 618)) %>% 
            mutate(caracter = ifelse(Zona_Num_N==154, "o", "d")),
          mapping = aes(fill = caracter),
          lwd = 0) +
  geom_sf(data = plotting_route_original,
          lwd = 1,
          color = "blue") +
  annotation_scale(location = "bl", 
                   width_hint = 0.4) +
  annotation_north_arrow(location = "bl", 
                         which_north = "true", 
                         pad_x = unit(0, "cm"), 
                         pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering) +
  xlab("Longitud") + 
  ylab("Latitud") +
  scale_fill_manual(breaks = c("d", "o"),
                    values = c("darkred", "forestgreen"),
                    labels = c("Destination" , "Origin"), 
                    name = "ZAT") +
  theme(panel.grid.major = element_line(color = gray(0.5), 
                                        linetype = "dashed", 
                                        size = 0.2), 
        panel.background = element_rect(fill = alpha("forestgreen", 0.2)))

segment_colors <- c("#81C57A", #1
                    "#FFEE33", #2
                    "#8126C0", #3
                    "#575757", #4
                    "#9DAFFF", #5
                    "#0000FF", #6
                    "#00FF00", #7
                    "#FF0000", #8
                    "#FF9233", #9
                    "#AD2323", #10
                    "#814A19", #11
                    "#A0A0A0", #12
                    "#1D6914", #13
                    "#FF00FF", #14
                    "#9DAFFF", #15
                    "#814A19", #16
                    "#29D0D0") #17

pal.bands(segment_colors)

map3 <- ggplot() +
  geom_sf(data = bogota,
          color = "grey20",
          fill = "grey99") +
  geom_sf(data = UPZ %>% 
            slice(st_crosses(plotting_route_original, UPZ)[[1]]),
          mapping = aes(fill = UPlCodigo),
          color = "grey20",
          lwd = 0.5, 
          show.legend = F) +
  scale_fill_manual(values = alpha(lighten(segment_colors, 0.3), 0.4)) +
  new_scale_fill() +
  geom_sf(data = zat %>% 
            filter(Zona_Num_N%in%c(154, 618)) %>% 
            mutate(caracter = ifelse(Zona_Num_N==154, "o", "d")),
          mapping = aes(fill = caracter),
          lwd = 0) +
  scale_fill_manual(breaks = c("d", "o"),
                    values = c("darkred", "forestgreen"),
                    labels = c("Destination" , "Origin"), 
                    name = "ZAT") +
  geom_sf(data = plotting_route_split,
          mapping = aes(colour = UPlCodigo),
          lwd = 1,
          show.legend = F) +
  scale_color_manual(values = darken(segment_colors, 0.2)) +
  annotation_scale(location = "bl", 
                   width_hint = 0.4) +
  annotation_north_arrow(location = "bl", 
                         which_north = "true", 
                         pad_x = unit(0, "cm"), 
                         pad_y = unit(0.5, "cm"),
                         style = north_arrow_fancy_orienteering) +
  xlab("Longitud") + 
  ylab("Latitud") +
  theme(panel.grid.major = element_line(color = gray(0.5), 
                                        linetype = "dashed", 
                                        size = 0.2), 
        panel.background = element_rect(fill = alpha("forestgreen", 0.2)))

pdf("./GRAFICOS/UPZ_distribution_process.pdf",width = 11, height = 6)
showtext_begin()
print(
  ggarrange(map1, map2, map3, labels=c("A","B","C"),common.legend = TRUE,legend = "bottom",ncol = 3)
)
showtext_end()
dev.off()
