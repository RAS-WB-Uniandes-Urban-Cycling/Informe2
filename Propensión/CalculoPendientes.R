library(raster)
library(tidyverse)
library(sf)

path <- "C:/Users/pa.uriza274/Universidad de Los Andes/German Augusto Carvajal Murcia - UNIANDES - RAS - SDM/"

DEM1 <- raster(paste0(path,"BASES DE DATOS/Digital Elevation Models/JAXA/N000W075_N005W070/N004W075_AVE_DSM.tif"))
DEM2 <- raster(paste0(path,"BASES DE DATOS/Digital Elevation Models/JAXA/N000W075_N005W070/N004W074_AVE_DSM.tif"))
DEM3 <- raster(paste0(path,"BASES DE DATOS/Digital Elevation Models/JAXA/N005W075_N010W070/N005W075_AVE_DSM.tif"))
DEM4 <- raster(paste0(path,"BASES DE DATOS/Digital Elevation Models/JAXA/N005W075_N010W070/N005W074_AVE_DSM.tif"))

DEM <- raster::merge(DEM1,DEM2,DEM3,DEM4)
rm(DEM1,DEM2,DEM3,DEM4)
gf <- focalWeight(raster(nrows=2126, ncols=1250, xmn=xmin(DEM),resolution=res(DEM)),d = c(0.0005,0.002),type = "Gauss")
DEMsmooth <- focal(DEM,gf)
slope <- terrain(DEMsmooth,unit = "degrees")
slope <- crop(slope, extent(c(-74.35812,-74.01076,4.439076,5.029737)))

AvgSlope_origen <- raster::extract(slope,as_Spatial(buffer_origen),fun = mean)
slope_origen <- buffer_origen %>% st_set_geometry(NULL) %>% bind_cols(Avgslope=as.vector(AvgSlope_origen))
AvgSlope_destino <- raster::extract(slope,as_Spatial(buffer_destino),fun = mean)
slope_destino <- buffer_destino %>% st_set_geometry(NULL) %>% bind_cols(Avgslope=as.vector(AvgSlope_destino))
AvgSlope_ruta <- raster::extract(slope,as_Spatial(buffer_ruta),fun = mean)
slope_ruta <- buffer_ruta %>% st_set_geometry(NULL) %>% bind_cols(Avgslope=as.vector(AvgSlope_ruta))

################

library(rasterVis)
plot3D(DEMsmooth)

rb <- tmaptools::read_GPX("C:/Users/pa.uriza274/Downloads/Ciclismo_por_la_ma_ana.gpx")

# fw <- focalWeight(raster(nrows=2126, ncols=1250, xmn=xmin(DEM),resolution=res(DEM),crs=st_crs(3116)$proj4string),d = 0.002,type = "Gauss")



transect_df = as.data.frame(raster::extract(DEMsmooth, rb$track_points, along = TRUE, cellnumbers = TRUE))

transect = raster::extract(focalDEM, rp$tracks, along = TRUE, cellnumbers = TRUE)
transect_df = map_dfr(transect, as_data_frame, .id = "ID")
transect_coords = xyFromCell(DEMsmooth, transect_df$cell)
transect_df$dist = c(0, cumsum(geosphere::distGeo(transect_coords)))    

plot(transect_df$layer,col = "red",type = "l")
lines(rb$track_points$ele,col = "blue")

sum(pmax(diff(transect_df$layer),0))
plot(x = transect_df$dist,transect_df$layer)

a <- st_as_sf(rb$track_points) %>% cbind(ele2=transect_df$layer)
mapview::mapview(select(a,ele,ele2))

hs = hillShade(slope = terrain(r_m, "slope"), aspect = terrain(DEM, "aspect"))

plot(r_m, col = gray(0:100 / 100), legend = FALSE)

plot(r_m, terrain.colors(25), alpha = 0.5, legend = FALSE, add = TRUE)
# add contour lines
contour(r_m, col = "white", add = TRUE)

Loc <- st_read(paste0(path,"BASES DE DATOS/Mapas de Referencia IDECA/MR0318.gdb"),layer = "Loca") %>% filter(LocNombre != "SUMAPAZ") %>% st_transform(4326)
bog <- mask(DEMsmooth,as_Spatial(Loc))
bog <- crop(bog,as_Spatial(Loc))

levelplot(bog,par.settings=RdBuTheme(), contour=TRUE)

bogsl <- mask(slope,as_Spatial(Loc))
bogsl <- crop(bogsl,as_Spatial(Loc))

levelplot(slope,par.settings=RdBuTheme())
            