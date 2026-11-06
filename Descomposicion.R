library(fpp3)
library(tidyverse)
library(here)

archivos <- list.files(
  here("Data"),
  pattern = "^EDV_[0-9]{4}\\.csv$",
  full.names = TRUE,
  ignore.case = TRUE
)

datos_divorcios <- archivos %>%
  lapply(read.csv, sep = ";", colClasses = "character") %>%
  bind_rows()

divorcios_ts <- datos_divorcios %>%
  mutate(
    fecha_insc = as.Date(fecha_insc, format = "%m/%d/%Y"),
    Mes = yearmonth(fecha_insc)
  ) %>%
  count(Mes, prov_insc, cant_insc, parr_insc,
        name = "Observaciones") %>%
  as_tsibble(
    index = Mes,
    key = c(prov_insc, cant_insc, parr_insc)
  )


# `divorcios_ts` está indexado por Mes y tiene como claves la provincia,
# el cantón y la parroquia. Con index_by() cambiamos el índice a Mes y
# sumamos todas las observaciones de cada mes para obtener la serie
# nacional.
divorcios_nacional <- divorcios_ts %>%
  index_by(Mes) %>%
  summarise(Observaciones = sum(Observaciones))

#Aplicamos el desplazamiento
divorcios_nacional_ma <- divorcios_nacional %>%
  mutate(
    `12-MA` = slider::slide_dbl(
      Observaciones,
      mean,
      .before = 5,
      .after = 6,
      .complete = TRUE
    ),
    `2x12-MA` = slider::slide_dbl(
      `12-MA`,
      mean,
      .before = 1,
      .after = 0,
      .complete = TRUE
    )
  )

# T_t: tendencia
divorcios_nacional_ma %>%
  autoplot(Observaciones, colour = "grey60") +
  geom_line(aes(y = `2x12-MA`), colour = "#D55E00", linewidth = 1) +
  labs(
    title = "Divorcios en Ecuador: serie y tendencia",
    x = "Mes",
    y = "Número de divorcios"
  )





# y_t-T_t: Observaciones - 2x12-MA.
divorcios_sin_tendencia <- divorcios_nacional_ma %>%
  mutate(pt = Observaciones - `2x12-MA`)

divorcios_sin_tendencia %>%
  autoplot(pt) +
  labs(
    title = "Divorcios en Ecuador: serie sin tendencia",
    x = "Mes",
    y = "Observaciones - 2x12-MA"
  )

# S_t:  Componente estacional promedio de la serie sin tendencia para
#    cada mes del año.
divorcios_estacional <- divorcios_sin_tendencia %>%
  group_by(month(Mes)) %>%
  mutate(secuencia = mean(pt, na.rm = TRUE))

autoplot(divorcios_estacional, secuencia)+
  labs(
    title = "Divorcios en Ecuador: Estacional",
    x = "Mes",
    y = "Secuence"
  )

# R_t=y_t-T_t-S_t: Residuo.
divorcios_resto <- divorcios_estacional %>%
  mutate(resto = pt - secuencia)

divorcios_resto %>%
  autoplot(resto) +
  labs(
    title = "Residuo de la descomposición de los divorcios en Ecuador",
    x = "Mes",
    y = "Residuo"
  )

#Descomposición
divorcios_desc <- divorcios_nacional %>%
  model(
    classical_decomposition(Observaciones, type = "additive")
  ) %>%
  components()

divorcios_desc %>%
  autoplot() +
  labs(
    title = "Descomposición clásica aditiva de los divorcios en Ecuador"
  )





#-------------------------------------------------------------
# AUDITORÍA
#Ana Aldás Cárdenas

# Frecuencia temporal
interval(divorcios_nacional)

# Comprobar meses faltantes
count_gaps(divorcios_nacional)

# Número de observaciones y valores ausentes
divorcios_nacional |>
  as_tibble() |>
  summarise(
    inicio = min(Mes),
    fin = max(Mes),
    observaciones = n(),
    valores_NA = sum(is.na(Observaciones))
  )

#------------------------------------------------------
#COMPONENTE ESTACIONAL

# Se conserva la idea original de calcular el
# promedio de la serie sin tendencia para cada mes
#1:
# Se crea una variable explícita 'estacion'
# que identifica los 12 meses del año
#2:
# Se añade ungroup() para evitar que los
# agrupamientos afecten los cálculos posteriores

divorcios_estacional <- divorcios_sin_tendencia |>
  mutate(
    estacion = month(Mes)
  ) |>
  group_by(estacion) |>
  mutate(
    secuencia = mean(pt, na.rm = TRUE)
  ) |>
  ungroup()

divorcios_estacional


# ------------------------------------------------------------
# NORMALIZACIÓN DE LOS FACTORES ESTACIONALES
# En el código original se calculaban los promedios
# estacionales, pero no se normalizaban
# En la descomposición aditiva debe cumplirse:
# SUMA(S_j) = 0, para j = 1,...,12.
# Por eso se resta a cada factor estacional el
# promedio de los 12 factores calculados


# Obtener los 12 promedios estacionales
medias <- divorcios_estacional |>
  distinct(estacion, secuencia) |>
  arrange(estacion)

medias


# Calcular el ajuste
ajuste <- mean(medias$secuencia)


#Obtener la componente estacional corregida
divorcios_estacional <- divorcios_estacional |>
  mutate(
    estacional = secuencia - ajuste
  )

divorcios_estacional


#Mostrar los 12 factores estacionales ajustados
divorcios_estacional |>
  distinct(estacion, estacional) |>
  arrange(estacion)


#-----------------------------------------------------------
#COMPROBAR LA NORMALIZACION

divorcios_estacional |>
  as_tibble() |>
  ungroup() |>
  distinct(estacion, estacional) |>
  summarise(
    suma_estacional = sum(estacional)
  )

# RESULTADO:
# -1.78e-15
#Este valor es aproximadamente 0
#Los 12 factores estacionales están correctamente normalizados


# Gráfica de la componente estacional corregida
divorcios_estacional |>
  autoplot(estacional) +
  labs(
    title = "Divorcios en Ecuador: componente estacional",
    x = "Mes",
    y = "Efecto estacional (número de divorcios)"
  )


# -----------------------------------------------------------
#COMPONENTE RESTANTE
#se utiliza la estacionalidad normalizada, 
#en lugar de los promedios sin ajustar.
# R_t = Y_t - T_t - S_t
# R_t = pt - estacional

divorcios_resto<- divorcios_estacional |>
  mutate(
    resto = pt - estacional
  )

divorcios_resto


# Gráfica de la componente restante
divorcios_resto |>
  autoplot(resto) +
  labs(
    title = "Residuo de la descomposición de los divorcios en Ecuador",
    x = "Mes",
    y = "Componente restante (número de divorcios)"
  )


# -------------------------------------------------------
#RECONSTRUCCIÓN DE LA SERIE
# PD: Y_t = T_t + S_t + R_t
# La reconstrucción debe coincidir con la serie original

auditoria_reconstruccion <- divorcios_resto |>
  mutate(
    reconstruccion = `2x12-MA` + estacional + resto,
    error = Observaciones - reconstruccion
  )


#Error máximo y número de observaciones válidas
auditoria_reconstruccion |>
  as_tibble() |>
  ungroup() |>
  summarise(
    error_maximo = max(abs(error), na.rm = TRUE),
    observaciones_validas = sum(!is.na(error))
  )

# RESULTADO:
# 2.27e-13 aproximadamente cero.
# Observaciones válidas: 72

#------------------------------------------------------------
#DESCOMPOSICIÓN AUTOMÁTICA

divorcios_desc <- divorcios_nacional |>
  model(
    classical_decomposition(
      Observaciones,
      type = "additive"
    )
  ) |>
  components()


# Gráfica de la descomposición automática
divorcios_desc |>
  autoplot() +
  labs(
    title = "Descomposición clásica aditiva de los divorcios en Ecuador"
  )


#--------------------------------------------------------------
#COMPARACIÓN

# Componentes obtenidas manualmente
manual <- divorcios_resto |>
  as_tibble() |>
  select(
    Mes,
    tendencia_manual = `2x12-MA`,
    estacional_manual = estacional,
    restante_manual = resto
  )


# Componentes obtenidas por FPP3
automatico <- divorcios_desc |>
  as_tibble() |>
  select(
    Mes,
    tendencia_fpp3 = trend,
    estacional_fpp3 = seasonal,
    restante_fpp3 = random
  )


# Comparación de las componentes por mes
comparacion <- manual |>
  inner_join(automatico, by = "Mes") |>
  mutate(
    error_tendencia =
      tendencia_manual - tendencia_fpp3,
    
    error_estacional =
      estacional_manual - estacional_fpp3,
    
    error_restante =
      restante_manual - restante_fpp3
  )

comparacion


# Errores máximos absolutos
comparacion |>
  summarise(
    error_tendencia = max(
      abs(error_tendencia),
      na.rm = TRUE
    ),
    
    error_estacional = max(
      abs(error_estacional),
      na.rm = TRUE
    ),
    
    error_restante = max(
      abs(error_restante),
      na.rm = TRUE
    )
  )

# RESULTADOS:
# Todos los errores son aproximadamente cero.
#
# La descomposición manual corregida coincide
# numéricamente con classical_decomposition()

#------------------------------------------------------------
# El período estacional m = 12 es adecuado para estudiar un posible patrón anual
#La tendencia-ciclo calculada mediante una media móvil centrada 2x12-MA coincide
# con la calculada por classical_decomposition()
# La corrección fue normalizar los 12 factores estacionales
# para garantizar que su suma sea cero
# Después, las tres componentes manuales coinciden numéricamente
