library(glue)
library(fpp3)
library(feasts)
library(urca)
library(fracdiff)
library(GGally)

tourism_holiday <- tourism |>
  filter(Purpose == "Holiday") |>
  summarise(Trips = sum(Trips) / 1e3)
tourism_holiday |>
  autoplot(Trips)
#Periodo: Trimestral
#Tendencia: se observa que decrece hasta aproximadamente el 2005 y tiende a crecer
#a partir de ese año
#Estacionalidad: Si se encuentra un patron definidio en un intervalo de tiempo


tourism_features <- tourism |>filter(Purpose == "Holiday") %>%
  features(Trips, feature_set(pkgs = "feasts"))
tourism_features

tourism_features |>
  select_at(vars(contains("season"), Purpose)) |>
  mutate(
    seasonal_peak_year = seasonal_peak_year +
      4 * (seasonal_peak_year == 0),
    seasonal_trough_year = seasonal_trough_year +
      4 * (seasonal_trough_year == 0),
    seasonal_peak_year = glue("Q{seasonal_peak_year}"),
    seasonal_trough_year = glue("Q{seasonal_trough_year}"),
  ) |>
  GGally::ggpairs(
    mapping = aes(colour = Purpose),
    lower = list(combo = function(data, mapping, ...) {
      GGally::ggally_facethist(data, mapping, ...) +
        scale_y_continuous(breaks = c(0, 10))
    })
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

