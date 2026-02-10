library(aimat)
library(dplyr)
library(ggplot2)
library(plotly)
load(here::here("undervisningsforlob/huspriser/data/Homedata.Rda")) # homedata
home_small <- homedata |>
  filter(Postnr=="9000", EjdType == "Villa") |> 
  select(pris = "Pris_Salg", areal = "Areal_Bolig", alder = "Alder", grund = "Areal_Grund", toiletter = "Ejd_AntalToiletter")

home_small |> ggplot(aes(x = areal, y = pris)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ toiletter)

home_small |> ggplot(aes(x = alder, y = pris)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ toiletter)

home_small |> ggplot(aes(x = alder, y = areal)) +
  geom_point(aes(color = cut(home_small$pris, breaks = 1e6*c(0, 2, 4, 10)))) +
  geom_smooth(method = "lm")

home_small |> 
  plot_ly(x = ~alder, y = ~areal, z = ~pris, color = ~toiletter, type = "scatter3d", mode = "markers")

fit_lm <- lm(pris ~ areal + alder + grund + toiletter, data = home_small)
summary(fit_lm)
