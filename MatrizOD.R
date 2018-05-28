######Análisis Matriz OD Encuesta de movilidad 2015
#Se cargan librerias para transformación y visualizaciones
library(tidyverse)
library(readxl)
library(sf)
library(tmap)
library(tmaptools)
library(stplanr)
library(circlize)

######## Heatmaps demanda#######
# Carga de acap con ZATs, proyección en SRID 4326 longlat WGS84
ZATs_sf <- st_read("Bases de datos/Bases/Bases.gdb",layer = "ZATs",stringsAsFactors = FALSE) %>% st_transform(4326) %>% 
  mutate(id = as.numeric(id),zona_num_n = as.numeric(zona_num_n)) %>% select(zona_num_n ) %>% filter(!st_is_empty(.))

# Polígono zona Bogotá según IDECA quitanto Localidad de Sumapaz
Loc <- st_read("Bases de datos/Bases/IDECA.gdb",layer = "Loca") %>% filter(LocNombre != "SUMAPAZ") %>% st_transform(4326)

# Selección de ZATs dentro de región de Bogotá
sel <- st_within(x = st_centroid(ZATs_sf), y = st_union(Loc), sparse = FALSE)
ZATBOG <- ZATs_sf %>% filter(sel)
ZATLoc <- st_join(x = st_centroid(ZATBOG),y = select(Loc,LocCodigo,LocNombre)) %>% `st_geometry<-`(NULL)
ZATBOG_df <- ZATBOG %>% left_join(ZATLoc) %>% `st_geometry<-`(NULL)

# Matriz OD Multimodal
ODZATMM <- read_xlsx("Bases de datos/Bases/EncuestaMovilidad2015/Matrices EODH/matriz_medio_pico_habil.xlsx",na = "0")
ODZATMM[is.na(ODZATMM)] <- 0
ODZATMM <- ODZATMM %>% filter(zat_origen %in% unique(ZATBOG$zona_num_n) & zat_destino %in% unique(ZATBOG$zona_num_n)) %>% 
  mutate(f_Total = select(.,starts_with("f_")) %>% rowSums())

## Viajes Origen por ZAT
ODZATMM_O <- ODZATMM %>% select(zona_num_n=zat_origen,starts_with("f_")) %>% 
  group_by(zona_num_n) %>% summarise_if(is.numeric,sum) 

## Viajes Destino por ZAT
ODZATMM_D <- ODZATMM %>% select(zona_num_n=zat_destino,starts_with("f_")) %>% 
  group_by(zona_num_n) %>% summarise_if(is.numeric,sum) 

# Asignación de atributos a la capa de ZAT
ZAT_OD <- ZATBOG %>% left_join(ODZATMM_O, by=c("zona_num_n")) %>% left_join(ODZATMM_D, by=c("zona_num_n"))

# Mapa total viajes bicicleta por ZAT
tm_shape(Loc,bbox = bb(ZAT_OD)) + tm_borders(alpha = 0.5)+
  tm_shape(ZAT_OD) + tm_fill(col = c("f_bicicleta.x", "f_bicicleta.y"),
                             palette = get_brewer_pal("YlGnBu",5,plot = F), style = "jenks", n = 5,
                             title = "Total viajes bicicleta",colorNA = "grey") + tm_borders() +
  tm_facets(free.scales = FALSE) +
  tm_layout(panel.labels = c("ZAT Origen", "ZAT Destino"),panel.label.bg.color = "white") +
  tm_scale_bar(position = c("left","top"))+ tm_compass() 


# Mapa total viajes por ZAT
tm_shape(Loc,bbox = bb(ZAT_OD)) + tm_borders(alpha = 0.5)+
  tm_shape(ZAT_OD) + tm_fill(col = c("f_Total.x", "f_Total.y"),
                             palette = get_brewer_pal("YlGnBu",5,plot = F), style = "jenks", n = 5,
                             title = "Total viajes",colorNA = "grey") + tm_borders() +
  tm_facets(free.scales = FALSE) + 
  tm_layout(panel.labels = c("ZAT Origen", "ZAT Destino"),panel.label.bg.color = "white") +
  tm_scale_bar(position = c("left","top"))+ tm_compass() 

############ Chord Diagram
# Lineas de deseo Chord Diagram
flowLines <- od2line(flow = ODZATMM,zones = as_Spatial(ZATBOG)) %>% st_as_sf() %>% mutate(length = st_length(.))  
shortTravels <- flowLines %>% filter(length <= units::as.units(5,value = "km")) %>% `st_geometry<-`(NULL) %>% 
  left_join(ZATBOG_df,by = c("zat_origen" = "zona_num_n")) %>% left_join(ZATBOG_df,by = c("zat_destino" = "zona_num_n")) %>% 
  rename(CodOrigen = LocCodigo.x, NombreOrigen = LocNombre.x, CodDestino = LocCodigo.y,NombreDestino = LocNombre.y) %>% 
  group_by(CodOrigen,NombreOrigen,CodDestino,NombreDestino) %>% summarise_if(is.numeric,sum) %>% rownames_to_column(var = "Pair") %>%
  ungroup()


##Transformación de matriz OD para realizar chord diagram
percentil = 90

#Solo se muestran para cada localidad origen, los destinos que acumulan el 90% de los viajes intermodales con distancias menores a 10km
ODChord <- shortTravels %>% select(c(2,3,5,13)) %>% filter(f_Total > 0) %>% 
  mutate(CodOrigen = as.character(CodOrigen),Total = f_Total) %>% 
  mutate(Total = if_else(Total<quantile(Total,percentil/100),0,Total)) %>% 
  ungroup()

ChordOrd <- ODChord %>% group_by(CodOrigen,NombreOrigen) %>% summarise(Total=sum(Total)) %>%
  arrange(desc(Total)) %>% ungroup() %>% mutate(Color = rainbow(n(),s = 0.75,v = 0.7),legend = glue::glue("[{CodOrigen}]  {NombreOrigen}"))

ChordDF <- ODChord %>% select(-1)

#Construcción Chord Diagram
circos.clear()
circos.par(start.degree = 90, gap.degree = 3, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4),family = "Open Sans")

file <- paste0("Figures/ChordBike",percentil,".pdf")
pdf(file,width=8,height=5.5)

chordDiagram(x = ChordDF, grid.col = ChordOrd$Color, transparency = 0.25,
             order = ChordOrd$NombreOrigen, directional = 1,
             direction.type = c("arrows", "diffHeight"), diffHeight  = -0.04,
             annotationTrack = "grid", annotationTrackHeight = c(0.15, 0.1),
             link.arr.type = "big.arrow", link.sort = TRUE, link.largest.ontop = TRUE)

circos.trackPlotRegion(
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    reg1 = ChordOrd$CodOrigen[ChordOrd$NombreOrigen == sector.index]
    
    circos.text(x = mean(xlim), y = 0.5, labels = reg1, facing = "downward", cex = 0.9)
    
    
  }
)

legend("left", pch = 15, col = ChordOrd$Color, 
       legend = ChordOrd$legend,cex = 0.7,bty = "n",
       pt.cex = 1.5,ncol = 1,text.width = 0.3)

dev.off()
