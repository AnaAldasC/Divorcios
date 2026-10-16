library(fpp3)
library(here)

datos_2018 <- read.csv(
  here("EDV 2018.csv"),
  sep = ";"
)

divorcios_2018 <- datos_2018 %>%
  mutate(
    fecha_insc = as.Date(fecha_insc, format = "%m/%d/%Y"),
    Mes = yearmonth(fecha_insc)
  ) %>%
  filter(year(Mes) == 2018) %>%
  count(Mes, prov_insc, cant_insc, parr_insc,
        name = "Observaciones") %>%
  as_tibble(
    index = Mes,
    key = c(prov_insc, cant_insc, parr_insc)
  )

divorcios_2018
