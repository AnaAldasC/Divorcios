#Divorcios en el Ecuador en 2018
#------------------------------------------------------------------
#Base de datos de divorcios registrados en Ecuador durante 2018, 
#organizada como una serie temporal mensual según la fecha 
#de inscripción del divorcio (fecha_insc). 
#Incluye el número de divorcios por mes y su distribución geográfica 
#por provincia, cantón y parroquia de inscripción.
#------------------------------------------------------------------
library(fpp3)
library(here)

#Totales Nacionales
#Agrupar los divorcios mensuales a nivel nacional
divorcios_ecuador <- divorcios_2018 %>%
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
    title = "Divorcios en Ecuador - 2018",
    x = "Mes",
    y = "Número de divorcios"
  )

#-----------------------------------------------------------------------
#Gráfico de la provincia:Pichincha - cantón: Quito - parroquia: Quitumbe
#-----------------------------------------------------------------------
#Evolución mensual de los divorcios inscritos en la parroquia Quitumbe
#durante 2018, según la fecha de inscripción del divorcio.
#El eje horizontal representa los meses del año y el eje vertical 
#muestra el número de divorcios correspondientes a cada mes.

divorcios_2018 %>%
  filter(prov_insc == "Pichincha",
         cant_insc == "Quito",
         parr_insc == "Quitumbe") %>%
  as_tibble() %>%
  group_by(Mes) %>%
  summarise(Observaciones = sum(Observaciones)) %>%
  as_tsibble(index = Mes) %>%
  autoplot(Observaciones) +
  labs(
    title = "Divorcios en la parroquia Quitumbe - 2018",
    x = "Mes",
    y = "Número de divorcios"
  )

