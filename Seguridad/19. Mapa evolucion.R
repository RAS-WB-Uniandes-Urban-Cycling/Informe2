# Limpiar espacio de trabajo----
rm(list=ls())
if(!is.null(dev.list())) dev.off()

# Preparación del entorno de trabajo----
carpetaRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/Uniandes - RAS - SDM"
gitRAS="/Users/germancarvajal/OneDrive - Universidad de Los Andes/proBikePolicies"
require(pacman)
pacman::p_load(tidyverse)
pacman::p_load(lubridate)
pacman::p_load(sf)
pacman::p_load(tmap)
pacman::p_load(magrittr)
pacman::p_load(stringr)
pacman::p_load(padr)
pacman::p_load(leaflet)
pacman::p_load(webshot)
pacman::p_load(mapview)
pacman::p_load(leafletCN)
pacman::p_load(mapmate)

# Leer bases de datos preprocesada----
load(paste0(carpetaRAS,"/RESULTADOS/SEGURIDAD/Bases de datos/13. Level1.Rdata"))

# Modificar base de datos para mapear----
AccidentesBiciTotal %<>%
  transmute(Gravedad2 = as.factor(ifelse(Gravedad2==1, "Fatal", "Nonfatal")), 
            fecha = as.Date(str_sub(KEY, end = 10)),
            geometry) 

# Creación del vector de fechas----

fechas <- data.frame(fecha = unique(c(min(AccidentesBiciTotal$fecha), seq(min(AccidentesBiciTotal$fecha), max(AccidentesBiciTotal$fecha), by = "3 month"), max(AccidentesBiciTotal$fecha) + 1))) %>% 
  arrange(fecha) %>% 
  mutate(grupo = row_number(),
         label = paste0(first(fecha), " to ",lead(fecha - as.difftime(1, units = "days"), n = 1))) %>% 
  filter(grupo < nrow(.))

# Adicion del grupo y label a la base de datos de colisiones
AccidentesBiciTotal %<>% 
  ungroup() %>% 
  arrange(fecha) %>% 
  left_join(fechas %>% select(-label), by = "fecha") %>% 
  fill(grupo, .direction = "down") %>% 
  select(-fecha)

# Lectura de la base de localidades----
Localidades<-st_read(paste(carpetaRAS,"/BASES DE DATOS/Mapas de Referencia IDECA/MR0318.gdb",sep=""),layer="Loca") %>% 
  st_transform(crs=4326) %>% 
  filter(LocNombre!="SUMAPAZ")

# Crear secuencia de mapas----

directory <- "/Users/germancarvajal/Desktop"
if(!dir.exists(paste0(directory, "/maps"))){
  dir.create(paste0(directory, "/maps")) 
}
setwd(paste0(directory, "/maps"))

for(g in fechas$grupo){
  
  if(paste0("mapa", str_pad(g, 2, pad = "0"), ".pdf") %in% dir()){
    next
  }
  
  lf <- tmap_leaflet(tm_basemap(leaflet::providers$Esri.WorldTopoMap, alpha = 0.5) + 
                       tm_shape(Localidades, bbox = matrix(c(-74.001922, 4.791285, -74.233526, 4.504612), nrow = 2, ncol = 2)) +
                       tm_polygons(col = "gray85", alpha = 0.2, border.col = "gray40") + 
                       tm_shape(AccidentesBiciTotal %>% filter(grupo<=g))+
                       tm_dots(col = "Gravedad2", style = "cat", title = "Severity", size = 0.01, showNA=FALSE, palette=c("#FF0000","#bbe1ed","#FFFFFF")) + 
                       tm_layout(legend.frame = T), show = F)
  
  lf$x$options$zoomControl <- F
  
  lf$x$options$attributionControl <- F
  
  lf <- removeLayersControl(lf)
  
  lf <- addTitle(lf, fechas %>% filter(grupo==g) %>% pull(label), color = "black")
  
  mapshot(lf, file = paste0("mapa", str_pad(g, 2, pad = "0"), ".pdf"), vwidth = 874/2, vheight = 1172/2) 
  
  cat(paste0("Finished map ", g, "\n"))

}

##### Run automator render PDF Pages as images and extract results----

# Combine images into GIF----

ffmpeg(pattern = "mapa%02d.png", output = "mapa.gif", rate = 2, overwrite = TRUE, dir = paste0(directory, "/maps"))

       