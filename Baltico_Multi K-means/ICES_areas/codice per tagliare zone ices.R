library(sf)
library(dplyr)

# Percorso del file .shp (assicurati che ci siano anche .dbf, .shx, ecc. nella stessa cartella)
shp <- st_read("ICES_Areas_20160601_cut_dense_3857.shp")

# Vedi le prime righe della tabella attributi
head(shp)

# Vedi la struttura dei campi (nomi e tipi di dato)
str(shp)

# Visualizza solo i nomi delle colonne
names(shp)

# Vedi il sistema di coordinate
st_crs(shp)








# Filtra solo le zone desiderate
baltic_mask <- shp %>%
  filter(SubDivisio %in% c("21", "22", "23", "24", "25"))
# Trasforma in EPSG:4326
baltic_mask_4326 <- st_transform(baltic_mask, crs = 4326)
# Salva come nuovo shapefile (maschera)
st_write(baltic_mask_4326, "baltic_mask_21_25.shp")




baltic_mask_2 <- shp %>%
  filter(SubDivisio %in% c("21", "22", "23", "24", "25", "26"))
# Trasforma in EPSG:4326
baltic_mask_43 <- st_transform(baltic_mask_2, crs = 4326)
# 3. Salva come nuovo shapefile (maschera)
st_write(baltic_mask_43, "baltic_mask_21_26.shp")


baltic_mask3 <- shp %>%
  filter(SubDivisio %in% c("21", "22", "23", "24", "25", "26", "27"))
# Trasforma in EPSG:4326
baltic_mask_bb <- st_transform(baltic_mask3, crs = 4326)
# 3. Salva come nuovo shapefile (maschera)
st_write(baltic_mask_bb, "baltic_mask_21_27.shp")



baltic_mask5 <- shp %>%
  filter(SubDivisio %in% c("21", "22", "23", "24", "25", "26", "28"))
baltic_mask_cc <- st_transform(baltic_mask5, crs = 4326)
# 3. Salva come nuovo shapefile (maschera)
st_write(baltic_mask_cc, "baltic_mask_21_26_28.shp")




baltic_mask6 <- shp %>%
  filter(SubDivisio %in% c("21", "22", "23", "24", "25", "26", "27", "28"))
baltic_mask_dd <- st_transform(baltic_mask6, crs = 4326)
# 3. Salva come nuovo shapefile (maschera)
st_write(baltic_mask_dd, "baltic_mask_21_28.shp")






baltic_mask7 <- shp %>%
  filter(SubDivisio %in% c("22", "24"))
baltic_mask_dd <- st_transform(baltic_mask7, crs = 4326)
# 3. Salva come nuovo shapefile (maschera)
st_write(baltic_mask_dd, "baltic_mask_22&24.shp")
