# Determinacion de áreas de comportamiento homogéno en Sierra Nevada
# Morales Esau
# 19 de agosto del 2026


# --- Librerías
library(terra)

# --- Primeros pasos

# Bandas Sentinel 2 L2A #

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


# DEM #

# Asignar ruta
ruta_dem <- "D:/Esau/Maestria/2027-1/2.Datos/MDE SRTM 1Arc/Editado/MDE_recorte.tif"

# Listar y guardar DEM
DEM <- rast(ruta_dem)

# Plotea
plot(DEM, main = names(DEM))


# Comprobando valores #

apilado
crs(apilado)

DEM
crs(DEM)


# Aplicar clasificacion no supervisada (k-means) 

# Aplicar Random Forest