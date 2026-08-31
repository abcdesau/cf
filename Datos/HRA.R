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
bandas_utiles <- c("B02", "B03", "B04", "B05", "B06", "B07", "B08", "B11", "B12")

# Guarda las bandas utiles en la varaibles selc_bandas
selec_bandas <- bandas[grepl(paste(bandas_utiles, collapse = "|"), basename(bandas))]

# Apila las bandas en una sola variable
apilado <- rast(selec_bandas)

# Conserva el nombre original de las bandas
names(apilado) <- basename(bandas)

# Plotea 
plot(apilado[[5]], main = names(apilado)[5])




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

# Apilando Sentinel + DEM 
datos_class <- c(apilado_rec, DEM_20)
names(datos_class) <- c(names(apilado_rec), "DEM")

# Convirtiendo los datos apilados a data.frame y eliminando NA
df_datos <- as.data.frame(datos_class, xy = TRUE, na.rm = TRUE)

# Separando coordenadas de datos
coords <- df_datos[, c("x", "y")]
datos <- df_datos[, !names(df_datos) %in% c("X", "Y")]

# Estandarizando los datos
datos_std <- scale(datos)


# --- Calculo de Indices de Vegetacion --- #

# Asignacion de bandas 

blue <- apilado_rec[["B02"]] #Blue
green <- apilado_rec[["B03"]] #Green
red <- apilado_rec[["B04"]] #Red
vredg1 <- apilado_rec[["B05"]] #VegRedEdge 20m
vredg2 <- apilado_rec[["B06"]] #VegRedEdge 20m
vredg3 <- apilado_rec[["B07"]] #VegRedEdge 20m
nir <- apilado_rec[["B08"]] #NIR
swir1 <- apilado_rec[["B11"]] #SWIR
swir2 <- apilado_rec[["B12"]] #SWIR

# NDVI
ndvi <- (nir - red) / (nir + red)

# EVI2

# GNDVI

# GRVI

# GDI

# GRDI


# --- Aplicar clasificacion no supervisada (k-means) --- #




kmeans_res <- kmeans(datos_std, centers = 6, iter.max = 10, nstart = 1)

# Observando el resultado
print(kmeans_res)

print(paste("Tamaño de cada cluster:"))
print(table(kmeans_res$cluster))

print(paste("Centro de los clusters:"))
print(kmeans_res$centers)

# Visualizando los datos
# Crando una capa raster vacia con una banda 
clasificacion <- apilado_rec[[1]]
values(clasificacion)

# Asignando valores de cluster a las celdas sin NA
indices <- which(!is.na(values(apilado_rec[[1]])))
values(clasificacion)[indices] <- kmeans_res$cluster

names(clasificacion) <- "Clasificacion_Kmeans"
plot(clasificacion,
     main = paste("Clasificacion No Sup K-means (K = 6)"), 
     col = rainbow(7),
     legend = TRUE)

# Estadisticas de cada cluster
df_cluster <- data.frame(
  cluster = kmeans_res$cluster, 
  datos_std) 

cluster_stasts <- aggregate(. ~ cluster, data = df_cluster, FUN = mean)
print("Características promedio de cada cluster (datos estandarizados):")
print(cluster_stasts)

# Estadísticas en valores originales
df_cluster_orig <- data.frame(
  cluster = kmeans_res$cluster,
  datos  # Datos originales sin estandarizar
)

cluster_stats_orig <- aggregate(. ~ cluster, data = df_cluster_orig, FUN = mean)
print("Características promedio de cada cluster (valores originales):")
print(cluster_stats_orig)


# Aplicar Random Forest