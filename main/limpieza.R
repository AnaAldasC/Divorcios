library(fpp3)
library(here)

datos_2018 <- read.csv(
  here("EDV 2018.csv"),
  sep = ";"
)

divorcios_2018 <- datos_2018 %>%
  mutate(
    fecha_div = as.Date(fecha_div, format = "%m/%d/%Y"),
    Mes = yearmonth(fecha_div)
  ) %>%
  filter(year(Mes) == 2018) %>%
  count(Mes, name = "Observaciones")  %>%
  as_tibble(index = Mes)

divorcios_2018
