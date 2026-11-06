# DESCOMPOSICIÓN MULTIPLICATIVA CLÁSICA
# DIVORCIOS EN ECUADOR

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
divorcios_nacional <- divorcios_ts %>%
  index_by(Mes) %>%
  summarise(Observaciones = sum(Observaciones))

#frecuencia mensual
interval(divorcios_nacional)

#meses faltantes
has_gaps(divorcios_nacional)
count_gaps(divorcios_nacional)

#fechas y número de observaciones
divorcios_nacional |>
  as_tibble() |>
  summarise(
    inicio = min(Mes),
    fin = max(Mes),
    observaciones = n(),
    valores_NA = sum(is.na(Observaciones)),
    valores_cero = sum(Observaciones == 0, na.rm = TRUE),
    valores_negativos = sum(Observaciones < 0, na.rm = TRUE)
  )


#----------------------------------------------------
#ESTIMAR LA TENDENCIA-CICLO
#Como la serie es mensual, utilizamos m = 12.
#Al ser un período par, calculamos una 2x12-MA

divorcios_mult <- divorcios_nacional |>
  mutate(
    `12-MA` = slider::slide_dbl(
      Observaciones,
      mean,
      .before = 6,
      .after = 5,
      .complete = TRUE
    ),
    
    `2x12-MA` = slider::slide_dbl(
      `12-MA`,
      mean,
      .before = 0,
      .after = 1,
      .complete = TRUE
    )
  )

divorcios_mult


# Gráfica de la serie original y la tendencia
divorcios_mult |>
  autoplot(Observaciones, colour = "gray") +
  geom_line(
    aes(y = `2x12-MA`),
    colour = "#D55E00"
  ) +
  labs(
    title = "Tendencia-ciclo de los divorcios en Ecuador",
    x = "Mes",
    y = "Número de divorcios"
  )


#----------------------------------------------------
#ELIMINAR LA TENDENCIA
# Y_t / T_t = S_t * R_t

divorcios_mult <- divorcios_mult |>
  mutate(
    sin_tendencia = Observaciones / `2x12-MA`
  )

divorcios_mult


# Gráfica de la serie sin tendencia
divorcios_mult |>
  autoplot(sin_tendencia) +
  labs(
    title = "Serie sin tendencia - Divorcios en Ecuador",
    x = "Mes",
    y = "Observaciones / Tendencia"
  )


#--------------------------------------------------------------
#COMPONENTE ESTACIONAL
# Identificar el mes de cada observación
divorcios_mult <- divorcios_mult |>
  mutate(
    estacion = month(Mes)
  )

# Calcular el promedio estacional para cada mes
divorcios_mult <- divorcios_mult |>
  group_by(estacion) |>
  mutate(
    media_estacional = mean(
      sin_tendencia,
      na.rm = TRUE
    )
  ) |>
  ungroup()

divorcios_mult


# Obtener los 12 promedios estacionales
medias <- divorcios_mult |>
  distinct(estacion, media_estacional) |>
  arrange(estacion)

medias


# Calcular el promedio de los 12 valores
ajuste <- mean(medias$media_estacional)

ajuste


# Ajustar los índices estacionales para que sumen 12
#
# S_j = media_estacional_j / ajuste

divorcios_mult <- divorcios_mult |>
  mutate(
    estacional = media_estacional / ajuste
  )


# Revisar los 12 índices estacionales
divorcios_mult |>
  distinct(estacion, estacional) |>
  arrange(estacion)


# Comprobar que los índices estacionales sumen 12
divorcios_mult |>
  distinct(estacion, estacional) |>
  summarise(
    suma = sum(estacional),
    promedio = mean(estacional)
  )

# RESULTADOS:
# suma  12
# promedio 1

# Gráfica de la componente estacional
divorcios_mult |>
  autoplot(estacional) +
  labs(
    title = "Componente estacional - Divorcios en Ecuador",
    x = "Mes",
    y = "Índice estacional"
  )


#-----------------------------------------------------
#COMPONENTE RESTANTE
# R_t = Y_t / (T_t * S_t)

divorcios_mult <- divorcios_mult |>
  mutate(
    restante = Observaciones / (`2x12-MA` * estacional)
  )

divorcios_mult


# Gráfica de la componente restante
divorcios_mult |>
  autoplot(restante) +
  labs(
    title = "Componente restante - Divorcios en Ecuador",
    x = "Mes",
    y = "Componente restante"
  )

# -----------------------------------------------------
# CLASSICAL_DECOMPOSITION
# Descomposición multiplicativa automática

descomposicion <- divorcios_nacional |>
  model(
    classical_decomposition(
      Observaciones,
      type = "multiplicative"
    )
  ) |>
  components()

descomposicion


# Gráfica de la descomposición automática
descomposicion |>
  autoplot() +
  labs(
    x = "Mes",
    title = "Descomposición multiplicativa clásica de los divorcios en Ecuador"
  )

# Unir los resultados manuales con los automáticos

comparacion <- divorcios_mult |>
  left_join(
    descomposicion |>
      as_tibble() |>
      select(Mes, trend, seasonal, random),
    by = "Mes"
  )


# Calcular las diferencias entre las tres componentes
comparacion <- comparacion |>
  mutate(
    diferencia_tendencia = `2x12-MA` - trend,
    diferencia_estacional = estacional - seasonal,
    diferencia_restante = restante - random
  )

comparacion


# Calcular los errores máximos absolutos
comparacion |>
  as_tibble() |>
  summarise(
    error_tendencia = max(
      abs(diferencia_tendencia),
      na.rm = TRUE
    ),
    
    error_estacional = max(
      abs(diferencia_estacional),
      na.rm = TRUE
    ),
    
    error_restante = max(
      abs(diferencia_restante),
      na.rm = TRUE
    )
  )

#ambos procedimientos coinciden, los tres errores son próximos a cero

