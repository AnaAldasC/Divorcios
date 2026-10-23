library(fpp3)
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

#guardar como .csv
write.csv(
  divorcios_ts %>%
    mutate(Mes = as.Date(Mes)),
  file = here("Data", "data.csv"),
  row.names = FALSE
)
