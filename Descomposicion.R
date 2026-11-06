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
