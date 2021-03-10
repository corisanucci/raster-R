# Corina Sanucci
# raster-R
# 05/02/2021


# Objetivo: Evaluar cambios en un punto de interés

library(viridis)
library(raster) # paquete sp se debería cargar tmb
library(rgdal)
library(lubridate)
library(magrittr)
library(reshape)
library(ggplot2)
library(sf)


# ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## #

# Tenemos 12 archivos raster con las medias mensuales de NO2 para una región dada durante el 2020
path <- paste(path, '2020_formosa', sep = '')

r_lista <- list.files(path, full.names = T,pattern = '.tif$')

rstack <- stack(r_lista)
nlayers(rstack)
extent(rstack)
res(rstack)
crs(rstack)

# Vamos a cortarlo con nuestra AOI
AOI <- st_read(paste(path, '/mask.shp', sep = ''))
plot(AOI, col = NA) # veamos

# Debe estar en mismo crs que los rasters
rstack <- crop(rstack, AOI) # cortamos

# Generamos mapas con la media anual y la desviación estandar
r_mean <- calc(rstack, mean, na.rm = T) # una capa raster con la media de todos los meses
names(r_mean) <- 'Promedio anual'
r_sd <- calc(rstack, sd, na.rm = T) # una capa raster con la sd para todo el año
names(r_sd) <- 'Desviación estandar'

mean_sd <- stack(r_mean, r_sd)

mean_sd <- as.data.frame(mean_sd, xy = TRUE)%>%
  melt(id.vars = c('x','y')) # Melt une los datos de concentración en una única columna

colnames(mean_sd) <- c('Lon', 'Lat', 'estadistico', 'NO2')
mean_sd$estadistico <- gsub('layer.1', 'Media anual', mean_sd$estadistico)
mean_sd$estadistico <- gsub('layer.2', 'Desviación estándar', mean_sd$estadistico)

# Cargar punto de interés: campo ganadero
punto <- readOGR(path,"punto") 
                  # readOGR() lo carga como SpatialPoint 
                  # ATENCION: ciudar que el path no tenga /$

# Reproyectamos rásters y shapefile
# Coordenadas cartográficas para trabajar en metros
# POSGAR 2007 Argentina Faja 5, EPSG: 5347

# Creamos un objeto de clase CRS
crs_m <- CRS(SRS_string = "EPSG:5347")

# Reproyectamos ráster
rstack <- projectRaster(rstack, crs = crs_m)

# Reproyectamos SpatialPoint
punto <- spTransform(punto, crs_m)

# También podemos crear el SpatialPoint a partir de coordenadas:
punto <- data.frame(5439600, 7235398)
# Deben estar en las unidades de mi crs (m)

# Convertimos el data.frame en SpatialPointsDataFrame
punto <- SpatialPointsDataFrame(punto[,1:2], 
                                # columnas 1 y 2 tienen las coordenadas
                                punto,    
                                # el objeto a convertir a vector espacial
                                proj4string = crs_m)   
                                # asignamos el crs

# Extracción de valores medios del ráster en el punto
# Definimos área buffer alrededor del SpatialPoint
raster_val <- raster::extract(x = rstack, 
                              y = punto, 
                              buffer = 4000,
                              # estamos en m
                              fun = mean,
                              na.rm = T)

# ¿Es necesario reordenar la matriz?
# Veamos
raster_val[1,]

# Para tener una variable por columna
raster_val <- t(raster_val) # trasponer

# Renombramos las filas
row.names(raster_val) <- gsub('NO2_', '', 
                              row.names(raster_val))

# Serie Temporal: Niveles de NO2 en campo ganadero durante el 2020
plot(raster_val,
     type = 'l',#Gráfico de líneas
     main = 'Formosa, Argentina (2020) - Campo ganadero',
     xlab = 'Tiempo (meses)', # etiqueta del eje x
     ylab = '[NO2] (mol/m^2)', # etiqueta del eje y
     adj = 1 # etiquetas ubicadas en los extremos de la gráfica
)


