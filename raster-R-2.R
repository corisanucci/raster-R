# Corina Sanucci
# raster-R
# 05/02/2021


# Objetivo: obtener estadísticos ráster entre dos momentos a comparar

library(viridis)
library(raster) # paquete sp se debería cargar tmb
library(rgdal)
library(lubridate)
library(magrittr)
library(reshape)
library(ggplot2)
library(sf)


# ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## #


# Crear lista con rasters ordenados temporalmente
lista_r <- list.files(path, full.names = T,
                      pattern = '.tif$')
# con full.names=T carga path+archivo

# Verificamos que están ordenados por fecha
head(lista_r)

# Creamos un stack con los rasters
rstack <- stack(lista_r)
nlayers(rstack) # cada archivo es una capa

# Evalúo los metadatos
crs(rstack) # esta en coordenadas geográficas WGS84
extent(rstack) # coordenadas min y max -> el raster es mas extenso de lo que me interesa
res(rstack) # tamaño pixel (en grados, ya que no están proyectados)

# Previsualizamos los datos
plot(rstack, col = viridis(15, direction = -1))
# con direction=-1 invertimos la paleta de color

# Vamos a calcular los niveles medios de NO2 antes y despues del 2020-03-20
# Convertimos nombres de capas a fechas
fechas <- gsub('X','', names(rstack))%>%
  mdy() # %>% pipe: magrittr
# mdy(): lubridate

# Generamos vector de 2 índices según condición
indx <- ifelse(fechas<='2020-03-20', 1, 2)

# Asignamos índice como nombre de capas
names(rstack) <- indx

# Hay días con falta de datos, me conviene quitarlos para no tener un sesgo
thr = ncell(rstack[[1]])*80/100 
rstack <- rstack[[which(cellStats(is.na(rstack), sum) <= thr)]]
# Condición: al menos 20% de datos

# Calculamos los niveles medios de NO2 antes y despúes del 20/03
NO2_cov <- stackApply(rstack, indx, fun = mean, na.rm = T) 
# con na.rm = T le pedimos que no use los NA para calcular las medias

nlayers(NO2_cov) # ¿cuántas capas tiene?

# Graficar rasters on ggplot:
# ggplot trabaja con data frames
NO2cov_df<- raster::as.data.frame(NO2_cov, xy=T)%>%
  melt(id.vars = c('x','y')) # melt(): reshape
# melt une los datos de NO2 en una única columna

NO2cov_df[1,] # Evaluemos las columnas

# Renombramos
colnames(NO2cov_df) <- c('Lon','Lat','Fecha','NO2')

graf1 <- ggplot() +
  geom_raster(data = NO2cov_df , aes(Lon, Lat, 
                                     fill = NO2)) +
  # aes(x, y, datos)
  facet_wrap(~ Fecha) + 
  # con facet_wrap dividimos el plot por variable
  ggtitle('Niveles de NO2 previo y posterior al ASPO', 
          subtitle = 'Ciudad de Buenos Aires, 
          Argentina - 2020') +
  scale_fill_gradientn(name = '[NO2](mol/m^2)', 
                       colours = magma(4, 
                                       direction = -1))

# Reemplazamos index por el período de tiempo

NO2cov_df$Fecha <- gsub('index_1', '19/02 al 20/03',
                        NO2cov_df$Fecha)

NO2cov_df$Fecha <- gsub('index_2', '21/03 al 15/04',
                        NO2cov_df$Fecha)

# Cargamos shapefiles

lim <- st_read(paste(path, 'lin_uni2.shp', sep =''))

rio <- st_read(paste(path, 'rio.shp', sep = ''))

# st_read(): sf --> carga el shp como dataframe

# Armamos una rampa de color de rojos
# con rgb(R,G,B) c/color toma valores e/ 0 y 1
# 0 < alpha < 1 es el grado de opacidad

max.red <- rgb(0.5,0.1,0, alpha = 1)
mid.red1 <- rgb(0.5,0.2,0, alpha = 0.8)
mid.red2 <- rgb(0.5,0.2,0, alpha = 0.5)
min.red <- rgb(0.4,0.2,0, alpha = 0.1)

# Creamos vector de colores
NO2col <- c(min.red, mid.red2, mid.red1, max.red)

graf2 <- graf1 + # le sumamos lo nuevo
  geom_sf(data = lim, color = 'black') +
  # shp de líneas: asignamos color
  geom_sf(data = rio, fill = 'darkturquoise', 
          alpha = 0.5, color = NA) +
  #shp polígono: asignamos relleno y transparencia
  scale_fill_gradientn(name = '[NO2] (mol/m^2)', 
                       colours = NO2col)
# reemplazamos la rampa de color

# orden de términos en ggplot = orden en gráfico

# Creamos un theme personalizado

theme_NO2cov <- function() {
  theme(axis.title = element_blank(),
        plot.title = element_text(face = 'bold'), 
        strip.background = element_rect(fill = 'white'), 
        legend.title = element_text(face = 'bold',
                                    vjust = 0.90), 
        legend.text = element_text(face = 'bold'), 
        legend.key.width = unit(0.5, 'cm'),
        legend.key.height = unit(1, 'cm'),
        panel.background = element_rect(fill = "white",
                                        colour = "grey50"), 
        panel.grid = element_blank())
  
graf2 + theme_NO2cov()


