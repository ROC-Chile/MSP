# PREPARACIÓN DE DATOS MSP PARA ANÁLSIS
# Cargar paquetes
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)
library(reshape2)

# Cargar datos
df <- read.csv("DatosMSPnov2023.csv", header = TRUE)
df.u <- read.csv('UnidadesMSP.csv')
df.c <- read.csv('MSPcovs.csv')

df$Date <- as.Date(df$Date)
df.c$Date <- as.Date(df.c$Date)

# Seleccionar columnas relevantes
df <- df %>%
  select(Sampling.Unit.Name, Start.Time, End.Time, Spp, Common.Name,
         Scientific.Name, Count, Date)

ds.inv <- df %>% filter(month(Date) >= 5 & month(Date) <= 10)
ds.ver <- df %>% filter(month(Date) <= 3 | month(Date) >= 12)


##INV
# Definir especies de invierno
especies.inv <- ds.inv %>% 
  filter(Spp != "---") %>% 
  select(Spp, Common.Name, Scientific.Name) %>%
  distinct()

# Identificar las filas con '---' en el código de la especie
censos_sin_obs.inv <- ds.inv %>%
  filter(Spp == "---")

# Generar nuevas filas para censos sin observaciones
nuevas_filas <- map_df(1:nrow(censos_sin_obs.inv), function(i) {
  fila_actual <- censos_sin_obs.inv[i,]
  
  especies_observadas <- ds.inv %>%
    filter(Sampling.Unit.Name == fila_actual$Sampling.Unit.Name & 
             Spp != "---") %>%
    select(Spp) %>%
    distinct() %>%
    left_join(especies.inv, by = "Spp")
  
  especies_observadas %>%
    mutate(Count = 0,
           Date = fila_actual$Date,
           Sampling.Unit.Name = fila_actual$Sampling.Unit.Name,
           Start.Time = fila_actual$Start.Time,
           End.Time = fila_actual$End.Time,
           SiteName = fila_actual$SiteName,
           Researcher = fila_actual$Researcher,
           Other.Observers.Count = fila_actual$Other.Observers.Count,
           nresearcher = fila_actual$nresearcher,
           Ha = fila_actual$Ha,
           lha = fila_actual$lha)
})

# Combinar datos originales y nuevas filas
ds.inv2 <- ds.inv %>%
  filter(Spp != "---") %>%
  bind_rows(nuevas_filas)

# Añadir columna presencia/ausencia
ds.inv2$presencia <- ifelse(ds.inv2$Count > 0, 1, 0)

# combinaciones de especie y unidad de muestreo según observaciones
combinaciones <- ds.inv2 %>%
  select(Spp, Sampling.Unit.Name) %>%
  distinct()

# identificar fechas de censos en cada unidad de muestreo
fechas.censos <- ds.inv2 %>%
  select(Sampling.Unit.Name, Date) %>%
  distinct()

# combinar especies con fechas de censos para cada unidad de muestreo
combinaciones_expandidas <- left_join(combinaciones, fechas.censos, by = "Sampling.Unit.Name", relationship = "many-to-many")

# unión con datos originales para completar las ausencias
ds.inv.compl <- left_join(combinaciones_expandidas, ds.inv2, by = c("Spp", "Sampling.Unit.Name", "Date"))%>%
  replace_na(list(presencia = 0))

# Corrección de valores asociados a `Date`:
info_date <- ds.inv2 %>%
  filter(!is.na(Start.Time) & !is.na(End.Time)) %>%
  select(Sampling.Unit.Name, Date, Start.Time, End.Time) %>%
  distinct()

ds.inv.compl <- left_join(ds.inv.compl, info_date, by = c("Sampling.Unit.Name", "Date"))

# Sobrescribimos las columnas originales con las columnas provenientes de info_date y luego las eliminamos
ds.inv.compl$Start.Time.x <- ds.inv.compl$Start.Time.y
ds.inv.compl$End.Time.x <- ds.inv.compl$End.Time.y
ds.inv.compl$Researcher.x <- ds.inv.compl$Researcher.y
ds.inv.compl$Other.Observers.Count.x <- ds.inv.compl$Other.Observers.Count.y

# Corrección de valores asociados a `Spp`:
info_spp <- ds.inv2 %>%
  select(Spp, Common.Name, Scientific.Name) %>%
  distinct()

ds.inv.compl <- left_join(ds.inv.compl, info_spp, by = "Spp")

# Sobrescribimos las columnas originales con las columnas provenientes de info_spp y luego las eliminamos
ds.inv.compl$Common.Name.x <- ds.inv.compl$Common.Name.y
ds.inv.compl$Scientific.Name.x <- ds.inv.compl$Scientific.Name.y
ds.inv.compl <- ds.inv.compl %>% select(-Common.Name.y, -Scientific.Name.y)

ds.inv.compl <- ds.inv.compl %>% 
  rename_with(~ gsub("\\.x$", "", .x))

# Registro único por especie, unidad de muestreo y fecha
MSPinv <- ds.inv.compl %>%
  group_by(Spp, Sampling.Unit.Name, Date) %>%
  summarise(across(everything(), first)) %>%
  distinct()  # Agregar esta línea para eliminar duplicados

# Reemplazar NA con 0 en la columna Count
MSPinv$Count <- replace_na(MSPinv$Count, 0)

MSPinv <- MSPinv %>%
  select(Sampling.Unit.Name, Start.Time, End.Time, Spp, Common.Name,
         Scientific.Name, Count, Date)

MSPinv <- left_join(MSPinv, df.u, by = "Sampling.Unit.Name")

df.c <- df.c %>% distinct(Sampling.Unit.Name, Date, .keep_all = TRUE)
MSPinv <- left_join(MSPinv, df.c, by = c("Sampling.Unit.Name", "Date"))

write.csv(MSPinv, 'MSPinv.csv', row.names = FALSE)

##VER
# Definir especies de verano
especies.ver <- ds.ver %>% 
  filter(Spp != "---") %>% 
  select(Spp, Common.Name, Scientific.Name) %>%
  distinct()

# Identificar las filas con '---' en el código de la especie
censos_sin_obs.ver <- ds.ver %>%
  filter(Spp == "---")

# Generar nuevas filas para censos sin observaciones
nuevas_filas <- map_df(1:nrow(censos_sin_obs.ver), function(i) {
  fila_actual <- censos_sin_obs.ver[i,]
  
  especies_observadas <- ds.ver %>%
    filter(Sampling.Unit.Name == fila_actual$Sampling.Unit.Name & 
             Spp != "---") %>%
    select(Spp) %>%
    distinct() %>%
    left_join(especies.ver, by = "Spp")
  
  especies_observadas %>%
    mutate(Count = 0,
           Date = fila_actual$Date,
           Sampling.Unit.Name = fila_actual$Sampling.Unit.Name,
           Start.Time = fila_actual$Start.Time,
           End.Time = fila_actual$End.Time,
           SiteName = fila_actual$SiteName,
           Researcher = fila_actual$Researcher,
           Other.Observers.Count = fila_actual$Other.Observers.Count,
           nresearcher = fila_actual$nresearcher,
           Ha = fila_actual$Ha,
           lha = fila_actual$lha)
})

# Combinar datos originales y nuevas filas
ds.ver2 <- ds.ver %>%
  filter(Spp != "---") %>%
  bind_rows(nuevas_filas)

# Añadir columna presencia/ausencia
ds.ver2$presencia <- ifelse(ds.ver2$Count > 0, 1, 0)

# combinaciones de especie y unidad de muestreo según observaciones
combinaciones <- ds.ver2 %>%
  select(Spp, Sampling.Unit.Name) %>%
  distinct()

# identificar fechas de censos en cada unidad de muestreo
fechas.censos <- ds.ver2 %>%
  select(Sampling.Unit.Name, Date) %>%
  distinct()

# combinar especies con fechas de censos para cada unidad de muestreo
combinaciones_expandidas <- left_join(combinaciones, fechas.censos, by = "Sampling.Unit.Name", relationship = "many-to-many")

# unión con datos originales para completar las ausencias
ds.ver.compl <- left_join(combinaciones_expandidas, ds.ver2, by = c("Spp", "Sampling.Unit.Name", "Date"))%>%
  replace_na(list(presencia = 0))

# Corrección de valores asociados a `Date`:
info_date <- ds.ver2 %>%
  filter(!is.na(Start.Time) & !is.na(End.Time)) %>%
  select(Sampling.Unit.Name, Date, Start.Time, End.Time) %>%
  distinct()

ds.ver.compl <- left_join(ds.ver.compl, info_date, by = c("Sampling.Unit.Name", "Date"))

# Sobrescribimos las columnas originales con las columnas provenientes de info_date y luego las eliminamos
ds.ver.compl$Start.Time.x <- ds.ver.compl$Start.Time.y
ds.ver.compl$End.Time.x <- ds.ver.compl$End.Time.y
ds.ver.compl <- ds.ver.compl %>% select(-Start.Time.y, -End.Time.y)

# Corrección de valores asociados a `Spp`:
info_spp <- ds.ver2 %>%
  select(Spp, Common.Name, Scientific.Name) %>%
  distinct()

ds.ver.compl <- left_join(ds.ver.compl, info_spp, by = "Spp")

# Sobrescribimos las columnas originales con las columnas provenientes de info_spp y luego las eliminamos
ds.ver.compl$Common.Name.x <- ds.ver.compl$Common.Name.y
ds.ver.compl$Scientific.Name.x <- ds.ver.compl$Scientific.Name.y
ds.ver.compl <- ds.ver.compl %>% select(-Common.Name.y, -Scientific.Name.y)

ds.ver.compl <- ds.ver.compl %>% 
  rename_with(~ gsub("\\.x$", "", .x))

# Registro único por especie, unidad de muestreo y fecha
MSPver <- ds.ver.compl %>%
  group_by(Spp, Sampling.Unit.Name, Date) %>%
  summarise(across(everything(), first)) %>%
  distinct()  # Agregar esta línea para eliminar duplicados

# Reemplazar NA con 0 en la columna Count
MSPver$Count <- replace_na(MSPver$Count, 0)

MSPver <- MSPver %>%
  select(Sampling.Unit.Name, Start.Time, End.Time, Spp, Common.Name,
         Scientific.Name, Count, Date)

MSPver <- left_join(MSPver, df.u, by = "Sampling.Unit.Name")

df.c <- df.c %>% distinct(Sampling.Unit.Name, Date, .keep_all = TRUE)
MSPver <- left_join(MSPver, df.c, by = c("Sampling.Unit.Name", "Date"))

#producir planilla de output
write.csv(MSPver, 'MSPver.csv', row.names = FALSE)

MSPver <- MSPver %>%
  filter(complete.cases(Wind))

MSPver <- MSPver %>%
  select(Sampling.Unit.Name, Spp,Scientific.Name, Count, Date, Longitude, 
         Latitude, Ha, Sitio, Wind, Precipitation)

MSPver$Date <- as.Date(MSPver$Date, format = "%Y-%m-%d")

str(MSPver)

# Modificar las fechas para el año 2021
MSPver <- MSPver %>%
  mutate(Date = case_when(
    year(Date) == 2021 ~ as.Date("2021-02-15"),
    TRUE ~ Date
  ))

# Modificar las fechas para dic 2020
MSPver <- MSPver %>%
  mutate(Date = case_when(
    Date == "2020-12-15" ~ as.Date("2021-02-15"),
    TRUE ~ Date
  ))

MSPver <- MSPver %>%
  mutate(Date = ymd(Date), 
         year = year(Date)) 

# Para sitios con múltiples fechas en 2021, seleccionar el primer conteo
MSPver <- MSPver %>%
  group_by(Sampling.Unit.Name, Spp, Date) %>%
  distinct(Sampling.Unit.Name, Spp, Date, .keep_all = TRUE) %>%
  ungroup()#BORRA MÁS DE 100. REVISAR DESPUES QUE SEA SOLO 2021

# Restar unidades con solo una replica
unidades_a_restar <- c("COQ36", "HT2", "HT3", "HT4", "PU2", "RL1", "RL2", "RL3", "RL5", "RL6")

MSPver <- MSPver %>%
  filter(!(Sampling.Unit.Name %in% unidades_a_restar))

# Filtro por sp
MSPverAMOY <- MSPver %>%
  filter(Spp == "AMOY")

MSPverAMOY$Wind <- as.numeric(MSPverAMOY$Wind)
MSPverAMOY$Precipitation <- as.numeric(MSPverAMOY$Precipitation)
MSPverAMOY$Count <- as.numeric(MSPverAMOY$Count)
MSPverAMOY$Sitio <- as.numeric(MSPverAMOY$Sitio)

# Crear la columna "UnidadAño" combinando la información de "Sampling.Unit.Name" y "year"
MSPverAMOY$UnidadAño <- paste(MSPverAMOY$Sampling.Unit.Name, MSPverAMOY$year, sep = ".")


# Obtener la lista completa de "UnidadAño", las unidades de muestreo y los años
all_UA <- unique(MSPverAMOY$UnidadAño)
all_units <- unique(MSPverAMOY$Sampling.Unit.Name)
all_years <- unique (MSPverAMOY$year)

# Crear un vector numérico del 1 al número total de unidades de muestreo
valores_unit <- 1:length(all_units)

# Crear un dataframe que relacione cada unidad de muestreo con su valor numérico
df_unidades <- data.frame(Sampling.Unit.Name = all_units, Unit = valores_unit)

# Agregar la columna Unit a la base de datos original
MSPverAMOY <- left_join(MSPverAMOY, df_unidades, by = "Sampling.Unit.Name")

MSPverAMOY$SitioUnidad <- paste(MSPverAMOY$Sitio, MSPverAMOY$Unit, sep = "_")
MSPverAMOY$SitioUnidadID <- as.numeric(factor(MSPverAMOY$SitioUnidad))


str(MSPverAMOY)


# Crear la matriz
y <- matrix(NA, nrow = length(all_UA), ncol = 1)
rownames(y) <- all_UA
colnames(y) <- "Count"

# Llenar la matriz con los valores de Count
for (i in 1:nrow(MSPverAMOY)) {
  UA <- MSPverAMOY$UnidadAño[i]
  count <- MSPverAMOY$Count[i]
  
  # Encontrar la posición en la matriz
  row_index <- match(UA, all_UA)
  
  # Llenar la matriz
  y[row_index, ] <- count
}

media_year <- mean(MSPverAMOY$year)
desviacion_year <- sd(MSPverAMOY$year)
MSPverAMOY$years <- (MSPverAMOY$year - media_year) / desviacion_year

# Crear la matriz
abund.covs<- matrix(NA, nrow = length(all_UA), ncol = 5)
rownames(abund.covs) <- all_UA
colnames(abund.covs) <- (c("superficie", "year", "año.s", "sitio", "SUID"))


for (i in 1:nrow(MSPverAMOY)) {
  UA <- MSPverAMOY$UnidadAño[i]
  Ha <- MSPverAMOY$Ha[i]
  Sitio <- MSPverAMOY$Sitio[i]
  year <- MSPverAMOY$year[i]  
  years <- MSPverAMOY$years[i] 
  unidad <- MSPverAMOY$SitioUnidadID[i] 
  
  if (UA %in% all_UA) {
    site_index <- which(all_UA == UA)  
    abund.covs[site_index, "superficie"] <- Ha
    abund.covs[site_index, "year"] <- year
    abund.covs[site_index, "año.s"] <- years
    abund.covs[site_index, "sitio"] <- Sitio
    abund.covs[site_index, "SUID"] <- unidad
  }
}

# Crear las covariables de detección
#PRECIPITACION
# Crear la matriz
precipitacion <- matrix(NA, nrow = length(all_UA), ncol = 1)
rownames(precipitacion) <- all_UA
colnames(precipitacion) <- "precipitacion"

# Llenar la matriz con los valores de Precipitation
for (i in 1:nrow(MSPverAMOY)) {
  UA <- MSPverAMOY$UnidadAño[i]
  precip_value <- MSPverAMOY$Precipitation[i]   
  
  # Encontrar la posición en la matriz
  row_index <- match(UA, all_UA)
  
  # Llenar la matriz
  precipitacion[row_index, ] <- precip_value  
}


#VIENTO
# Crear la matriz
viento <- matrix(NA, nrow = length(all_UA), ncol = 1)
rownames(viento) <- all_UA
colnames(viento) <- "viento"

# Llenar la matriz con los valores de viento
for (i in 1:nrow(MSPverAMOY)) {
  UA <- MSPverAMOY$UnidadAño[i]
  wind_value <- MSPverAMOY$Wind[i]  
  
  # Encontrar la posición en la matriz
  row_index <- match(UA, all_UA)
  
  # Llenar la matriz
  viento[row_index, ] <- wind_value  
}

# Crear la matriz
sitio <- matrix(NA, nrow = length(all_UA), ncol = 1)
rownames(sitio) <- all_UA
colnames(sitio) <- "sitio"

# Llenar la matriz con los valores de sitio
for (i in 1:nrow(MSPverAMOY)) {
  UA <- MSPverAMOY$UnidadAño[i]
  site_value <- MSPverAMOY$Sitio[i]  
  
  # Encontrar la posición en la matriz
  row_index <- match(UA, all_UA)
  
  # Llenar la matriz
  sitio[row_index, ] <- site_value  
}

# Crear la matriz
año <- matrix(NA, nrow = length(all_UA), ncol = 1)
rownames(año) <- all_UA
colnames(año) <- "año"

# Llenar la matriz con los valores de año
for (i in 1:nrow(MSPverAMOY)) {
  UA <- MSPverAMOY$UnidadAño[i]
  year_value <- MSPverAMOY$years[i]  
  
  # Encontrar la posición en la matriz
  row_index <- match(UA, all_UA)
  
  # Llenar la matriz
  año[row_index, ] <- year_value  
}

# Crear la matriz
SUID <- matrix(NA, nrow = length(all_UA), ncol = 1)
rownames(SUID) <- all_UA
colnames(SUID) <- "SUID"

# Llenar la matriz con los valores de SUID
for (i in 1:nrow(MSPverAMOY)) {
  UA <- MSPverAMOY$UnidadAño[i]
  unit_value <- MSPverAMOY$SitioUnidadID[i]  
  
  # Encontrar la posición en la matriz
  row_index <- match(UA, all_UA)
  
  # Llenar la matriz
  SUID[row_index, ] <- unit_value  
}

# Crear el objeto det.covs como una lista
det.covs <- list(
  "precipitacion" = precipitacion,
  "viento" = viento,
  "sitio" = sitio,
  "año" = año,
  "SUID" = SUID
)

str(det.covs)

# Coordenadas
# Extraer las coordenadas y convertirlas a números
coords <- MSPverAMOY %>%
  filter(Sampling.Unit.Name %in% all_units) %>%
  distinct(Sampling.Unit.Name, Latitude, Longitude) %>%
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude)) %>%
  arrange(Sampling.Unit.Name)

# Escalar las coordenadas
scaled_coords <- scale(coords[, c("Latitude", "Longitude")], center = TRUE, scale = TRUE)
rownames(scaled_coords) <- all_units

# Convertir a data frame
scaled_coords_df <- as.data.frame(scaled_coords)

# Cambiar nombres de las columnas en el objeto scaled_coords_df
colnames(scaled_coords_df)[1:2] <- c("X", "Y")

scaled_coords_df <- as.matrix(scaled_coords_df)

#Base de datos NMix
# Crear la lista de 4 objetos
MSPverAMOYNM <- list(y = y,
                     abund.covs = abund.covs,
                     det.covs = det.covs,
                     coords = scaled_coords_df)

# Mostrar la estructura actualizada de la lista
str(MSPverAMOYNM)

save(MSPverAMOYNM, file = 'MSPverAMOYNM.rda')
