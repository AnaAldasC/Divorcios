#Divorcios en el Ecuador 2018-2025
#------------------------------------------------------------------
#Base de datos de divorcios registrados en Ecuador durante 2018-2025, 
#organizada como una serie temporal mensual según la fecha 
#de inscripción del divorcio (fecha_insc). 
#Incluye el número de divorcios por mes y su distribución geográfica 
#por provincia, cantón y parroquia de inscripción.
#------------------------------------------------------------------
library(fpp3)
library(here)

#Totales Nacionales
#Agrupar los divorcios mensuales a nivel nacional
divorcios_ecuador <- divorcios_ts %>%
  as_tibble() %>%
  group_by(Mes) %>%
  summarise(Observaciones = sum(Observaciones)) %>%
  as_tsibble(index = Mes)

#-----------------------------------------------------------
# Gráficos de la Serie Temporal
#----------------------------------------------------------
#Evolución mensual de los divorcios ocurridos en Ecuador durante 2018,
#donde el eje horizontal muestra los meses de enero a diciembre y 
#el eje vertical indica el número de divorcios registrados en cada mes.
autoplot(divorcios_ecuador, Observaciones) +
  labs(
    title = "Divorcios en Ecuador",
    x = "Mes",
    y = "Número de divorcios"
  )

#-----------------------------------------------------------------------
#Gráfico de la provincia:Pichincha - cantón: Quito - parroquia: Quitumbe
#-----------------------------------------------------------------------
#Evolución mensual de los divorcios inscritos en la parroquia Quitumbe
#durante 2018-2025, según la fecha de inscripción del divorcio.
#El eje horizontal representa los meses del año y el eje vertical 
#muestra el número de divorcios correspondientes a cada mes.
divorcios_ts %>%
  filter(prov_insc == "Pichincha",
         cant_insc == "Quito",
         parr_insc == "Quitumbe") %>%
  as_tibble() %>%
  group_by(Mes) %>%
  summarise(Observaciones = sum(Observaciones)) %>%
  as_tsibble(index = Mes) %>%
  autoplot(Observaciones) +
  labs(
    title = "Divorcios en la parroquia Quitumbe en 2018-2025",
    x = "Mes",
    y = "Número de divorcios"
  )

#-------------------------------------------------------------------
#Gráfico Estacional
#-------------------------------------------------------------------
#Representa la evolución mensual de los divorcios inscritos en Ecuador 
#durante 2018-2025, comparando los diferentes años. 
#El eje horizontal muestra los meses de enero a diciembre y 
#el eje vertical indica el número de divorcios inscritos en cada mes.
divorcios_ecuador %>%
  gg_season(Observaciones, labels = "both") +
  labs(x = "Mes",
       y = "Número de divorcios",
       title = "Divorcios en Ecuador")

#------------------------------------------------------------------
#Gráfico de subseries 
#------------------------------------------------------------------
#Presenta las subseries mensuales de divorcios inscritos en Ecuador 
#durante 2018-2025, separando las observaciones por cada mes del año. 
#El eje horizontal representa los años dentro de cada mes y el eje 
#vertical muestra el número de divorcios inscritos. 
divorcios_ecuador %>%
  gg_subseries(Observaciones) +
  labs(
    x = "Mes",
    y = "Número de divorcios",
    title = "Divorcios en Ecuador"
  )

#------------------------------------------------------------------
#ACF
#------------------------------------------------------------------
divorcios_ecuador %>%
  ACF(Observaciones) %>%
  autoplot() + labs(title="ACF de divorcios en Ecuador")





