# PREPARACIÓN DE DATOS MSP PARA ANÁLSIS
# Cargar paquetes
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)

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
