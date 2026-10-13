# Determinacion de áreas de comportamiento homogéno en Sierra Nevada
# Morales Esau
# 19 de agosto del 2026


# --- Librerías
library(terra)



# --- Bandas Sentinel 2 L2A --- #




# Asigna la ruta de las bandas
ruta_sentinel <- "D:/Esau/Maestria/2027-1/2.Datos/Sentinel/S2B_MSIL2A_20250209T170349_N0511_R069_T14QNG_20250209T215045.SAFE/S2B_MSIL2A_20250209T170349_N0511_R069_T14QNG_20250209T215045.SAFE/GRANULE/L2A_T14QNG_A041422_20250209T171737/IMG_DATA/R20m"

#Guarda todas las bandas en una variable
bandas <- list.files(ruta_sentinel, pattern = ".jp2", full.names = TRUE)

# Concatena las bandas utiles en una variable
bandas_utiles <- c("B02", "B03", "B04", "B8A") #"B05", "B06", "B07", "B08", , "B11", "B12"

# Guarda las bandas utiles en la varaibles selc_bandas
selec_bandas <- bandas[grepl(paste(bandas_utiles, collapse = "|"), basename(bandas))]

# Apila las bandas en una sola variable
apilado <- rast(selec_bandas)

# Extraer solo el nombre de la banda (B02, B03, B8A, etc.)
nombres_simples <- gsub(".*_(B[0-9A]+)_20m.*", "\\1", basename(selec_bandas))
names(apilado) <- nombres_simples

# Plotea 
plot(apilado[[4]], main = names(apilado)[4])




# --- DEM --- #




# Asignar ruta
ruta_dem <- "D:/Esau/Maestria/2027-1/2.Datos/MDE SRTM 1Arc/Editado/MDE_recorte.tif"

# Listar y guardar DEM
DEM <- rast(ruta_dem)

# Plotea
plot(DEM, main = names(DEM))




# --- Comprobando valores --- #




apilado
crs(apilado)

DEM
crs(DEM)

# Reproyectando Bandas a EPSG: 6369
apilado_rep <- project(apilado, crs(DEM))

#Comprobacion de projeccion
apilado_rep
crs(apilado_rep)

# Recortar bandas a DEM
apilado_rec <- crop(apilado_rep, DEM)
plot(apilado_rec)




# --- Resampleo --- # 




# Resampleando DEM a 20m  
DEM_20 <- resample(DEM, apilado_rec, method = "bilinear")
plot(DEM_20)



# --- Calculo de pendiente, exposicion y texturas --- #
pendiente <- terrain(DEM_20, v = "slope", unit = "degrees")
names(pendiente) <- "PENDIENTE"

exposicion <- terrain(DEM_20, v = "aspect", unit = "degrees")
names(exposicion) <- "EXPOSICION"

# texturas <- glcm


# --- Calculo de Indices de Vegetacion --- #





# Asignacion de bandas y formulas

blue <- apilado_rec[["B02"]] #Blue
green <- apilado_rec[["B03"]] #Green
red <- apilado_rec[["B04"]] #Red
# vredg1 <- apilado_rec[["B05"]] #VegRedEdge 20m
# vredg2 <- apilado_rec[["B06"]] #VegRedEdge 20m
# vredg3 <- apilado_rec[["B07"]] #VegRedEdge 20m
# nir <- apilado_rec[["B8"]] #NIR a 10m
 nir20 <- apilado_rec[["B8A"]] #NIR a 20m
# swir1 <- apilado_rec[["B11"]] #SWIR
# swir2 <- apilado_rec[["B12"]] #SWIR

# NDVI
ndvi <- (nir - red) / (nir + red)

# EVI2
evi2 <- 2.5 * ((nir-red)/nir + 2.4 * (red) + 1)

# GNDVI
gndvi <-(nir - green) / (nir + green) 

# GRVI
grvi <- nir / green

# GDI
gdi <- nir - red + green

# GRDI
grdi <- (green - red) / (green + red)

# raster stack de indices
indices_stack <- c(ndvi, evi2, gndvi, grvi, gdi, grdi)
names(indices_stack) <- c("NDVI", "EVI2", "GNDVI", "GRVI", "GDI", "GRDI")
plot(indices_stack, main = names(indices_stack))

# --- Calculo de NDWI y LST --- #
ndwi <- (green - nir) / (green + nir)
names(ndwi) <- "NDWI"
plot(ndwi, main = names(ndwi))


# --- Apilado y estandarizacion --- #


#raster stack de texturas, pendiente, exposicion y humedad
indices_adicionales <- c( ndwi, pendiente, exposicion) #texturas)
plot(indices_adicionales, main= names(indices_adicionales))

# raster stack de indices
# indices_stack <- c(ndvi, evi2, gndvi, grvi, gdi, grdi)
# names(indices_stack) <- c("NDVI", "EVI2", "GNDVI", "GRVI", "GDI", "GRDI")

# Apilando Sentinel + DEM + Indices
datos_class <- c(apilado_rec, DEM_20, indices_stack, indices_adicionales)
names(datos_class) <- c(names(apilado_rec), "DEM", names(indices_stack), names(indices_adicionales))

# Convirtiendo los datos apilados a data.frame y eliminando NA
df_datos <- as.data.frame(datos_class, xy = FALSE, na.rm = TRUE)

# Separando coordenadas de datos
coords <- df_datos[, c("x", "y")]
datos <- df_datos[, !names(df_datos) %in% c("x", "y")]

# Estandarizando los datos
datos_std <- scale(datos)





# --- Aplicar clasificacion no supervisada (k-means) --- #



set.seed(123)
kmeans_res <- kmeans(datos_std, centers = 6, iter.max = 100, nstart = 1, algorithm = "Lloyd")











# Observando el resultado
# print(kmeans_res)
# 
# print(paste("Tamaño de cada cluster:"))
# print(table(kmeans_res$cluster))
# 
# print(paste("Centro de los clusters:"))
# print(kmeans_res$centers)
# 
# # Visualizando los datos
# # Crando una capa raster vacia con una banda 
# clasificacion <- apilado_rec[[1]]
# values(clasificacion)
# 
# # Asignando valores de cluster a las celdas sin NA
# indices <- which(!is.na(values(apilado_rec[[1]])))
# values(clasificacion)[indices] <- kmeans_res$cluster
# 
# names(clasificacion) <- "Clasificacion_Kmeans"
# plot(clasificacion,
#      main = paste("Clasificacion No Sup K-means (K = 3)"), 
#      col = rainbow(7),
#      legend = TRUE)
# 
# # Estadisticas de cada cluster
# df_cluster <- data.frame(
#   cluster = kmeans_res$cluster, 
#   datos_std) 
# 
# cluster_stasts <- aggregate(. ~ cluster, data = df_cluster, FUN = mean)
# print("Características promedio de cada cluster (datos estandarizados):")
# print(cluster_stasts)
# 
# # Estadísticas en valores originales
# df_cluster_orig <- data.frame(
#   cluster = kmeans_res$cluster,
#   datos  # Datos originales sin estandarizar
# )
# 
# cluster_stats_orig <- aggregate(. ~ cluster, data = df_cluster_orig, FUN = mean)
# print("Características promedio de cada cluster (valores originales):")
# print(cluster_stats_orig)


# Aplicar Random Forest