# Corina Sanucci
# raster-R
# 05/02/2021



# Objetivo: Reordenar archivos temporalmente


          # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## # ## #


# Creamos el path al directorio donde están nuestros archivos
path <- 'D:/Curso_R/Tutorial_useR/Files/'
setwd(path) # Establecemos directorio de trabajo

# Primero vamos a aprender a renombrar archivos en un directorio
# Aveces los nombres de archivos raster no facilitan el trabajo 
# Quizá están desordenados, o nos interesa reordenarlos con un fin en particular

# Generamos un vector de caracteres con los nombres de todos los archivos en la carpeta
originales <- list.files(path, full.names = F, pattern = '.tif$')
# el $ indica al patrón de busqueda que solo cargue los archivos que terminan en .tif
# Así evitamos cargar archivos .aux que se pueden generar con el trabajo de software SIG
# Con la opción full.names indicamos que cargue el nombre junto con el path (TRUE)

# Evaluemos el nombre de nuestros archivos
head(originales) # vemos una muestra de los primeros 6
# El nombre inicia con NO2_ y la fecha está en ddmmyyyy

# Eliminamos el NO2_, el año y la extensión de los archivos
x1 <- gsub('NO2_|2020|.tif', '', originales) # gsub('patron a reemplazar','reemplazo', vector de nombres)

meses <- gsub('^\\d{2}', '', x1) # elimino lo 2 primeros números del nombre (días)
dia_anio <- gsub('\\d{2}$', '-2020.tif', x1) 
# reemplazo los dos últimos números (meses) por el año
# Agrego tmb la extensión de los archivos

fecha <- paste(meses, dia_anio, sep ='-') # compongo nombres con mm-dd-yyyy
head(fecha) # comparemos con los nombres anteriores
head(originales) # el orden de las fechas en ambos vectores coincide

new_files <- paste(path, fecha, sep='')
old_files <- paste(path, originales, sep='')

# Copiamos desde los archivos viejos a los nuevos
file.copy(from = old_files, to = new_files) # TRUE en la salida indica que se copiaron bien

# Movemos los archivos viejos a otra carpeta
dir.create('Old_files') # Creamos la carpeta -> Si no especificamos path se genera en el wd
file.copy(old_files, 'Old_files') # TRUE en la salida indica que se movieron los archivos

# Borramos de la carpeta original los archivos viejos
unlink(old_files)
# Verificamos que no exista más
file.exists(old_files) # FALSE en la salida indica que no se encuentran esos archivos

# borramos todo del environment, nos quedamos con el path para continuar
rm(dia_anio, meses, x1, fecha,
   new_files, old_files, originales)