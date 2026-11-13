library(seasonal)

classical_decomposition(Observaciones, type = "additive")

#MODELO DESCOMPOSICION CLASICA---------------------------------------------------
cd_divorcios <- divorcios_ecuador |>
  model(cd = classical_decomposition(Observaciones)) |>
  components()
autoplot(cd_divorcios) +
  labs(
    x = "Mes",
    title = "Descomposición cd de los divorcios registrados en Ecuador del 2018-2025"
  )
#Varianza constante
#depende d elos periodos
# λ=0 da una descomposición multiplicativa
#λ=1 da una descomposición aditiva


#MODELO X11------------------------------------------------------------------------
x11_divorcios <- divorcios_ecuador |>
  model(x11 = X_13ARIMA_SEATS(Observaciones ~ x11())) |>
  components()
autoplot(x11_divorcios) +
  labs(
    x = "Mes",
    title = "Descomposición X-11 de los divorcios registrados en Ecuador del 2018-2025"
  )
#Suaviza menos la tendencia

x11_divorcios |>
  ggplot(aes(x = Mes)) +
  geom_line(aes(y = Observaciones, colour = "Datos")) +
  geom_line(aes(y = season_adjust, colour = "Ajustados estacionalmente")) +
  geom_line(aes(y = trend, colour = "Tendencia")) +
  labs(
    x = "Mes",
    colour = "Serie",
    y = "Personas (miles)",
    title = "Empleo total en el comercio minorista de EE.UU."
  ) +
  scale_colour_manual(
    values = c("gray", "#0072B2", "#D55E00"),
    breaks = c("Datos", "Ajustados estacionalmente", "Tendencia")
  )

#MODELO STL-------------------------------------------------------------------
divorcios_ecuador |>
  model(
    STL(
      Observaciones ~ trend(window = 7) +
        season(window = "periodic"),
      robust = TRUE
    )
  ) |>
  components() |>
  autoplot() +
  labs(x = "Mes")

divorcios_ecuador |>
  model(
    STL(
      Observaciones ~ trend(window = 21) +
        season(window = "periodic"),
      robust = TRUE
    )
  ) |>
  components() |>
  autoplot() +
  labs(x = "Mes")


divorcios_ecuador |>
  model(
    STL(
      Observaciones ~ trend(window = 21) +
        season(window = "periodic"),
      robust = FALSE
    )
  ) |>
  components() |>
  autoplot() +
  labs(x = "Mes")

divorcios_ecuador |>
  model(
    STL(
      Observaciones ~ trend(window = 21) +
        season(window = NULL),
      robust = FALSE
    )
  ) |>
  components() |>
  autoplot() +
  labs(x = "Mes")

