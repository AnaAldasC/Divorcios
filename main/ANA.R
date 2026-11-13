#ANA VERÓNICA ALDÁS CÁRDENAS

library(fpp3)
library(here)

global_economy

PIB_USA <- global_economy |>
  filter(Country == "United States")
PIB_USA |>
  autoplot(GDP) +
  labs(x = "Año", y = "% del PIB", title = "PIB: USA")

#--------------------------------------------------------------------
#transormacion BOX-COX
lambda <- PIB_USA |>
  features(GDP, features = guerrero) |>
  pull(lambda_guerrero)
lambda

PIB_USA_BC <- PIB_USA |>
  mutate(box_cox(GDP, lambda))

PIB_USA_BC |>
  autoplot(box_cox(GDP, lambda)) +
  labs(
    x = "Trimestre",
    y = "",
    title = "PIB: USA"
  )
#se aplica la transformacion ya que se observa que el el tamaño de la 
#variación estacional es casi el mismo en toda la serie
#--------------------------------------------------------------------
#STL
#Se observa tendencia creciente
#No tengo estacionalidad por loq ue puedo eliminar de mi descomposicion STL
#Dejo mi ventana en 23 para tomar más datos y comparar


PIB_USA_BC |>
  model(
    STL(
      GDP ~ trend(window = 23),
      robust = FALSE
    )
  ) |>
  components() |>
  autoplot() +
  labs(x = "Mes")

PIB_USA_BC |>
  model(
    STL(
      GDP ~ trend(window = 7),
      robust = FALSE
    )
  ) |>
  components() |>
  autoplot() +
  labs(x = "Mes")
