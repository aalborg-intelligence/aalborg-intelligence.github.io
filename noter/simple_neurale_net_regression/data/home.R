library(aimat)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library("writexl")
load(here::here("undervisningsforlob/huspriser/data/Homedata.Rda")) # homedata

unique(homedata$EjdType)

summary(homedata)

# , str_detect(Bynavn, "A.*borg")

home_small <- homedata |>
  filter(Postnr %in% c("9000", "9200"), EjdType == "Villa") |> 
  select(pris = "Pris_Salg", areal = "Areal_Bolig", alder = "Alder", 
         grund = "Areal_Grund", toiletter = "Ejd_AntalToiletter")

home_small$pris <- home_small$pris/1000000

writexl::write_xlsx(
home_small,
path = here::here("noter", "simple_neurale_net_regression", "data", "home_Aalborg.xlsx")
)

home_small_lejlighed <- homedata |>
  filter(Postnr %in% c("9000"), EjdType == "Ejerlejlighed") |> 
  select(pris = "Pris_Salg", areal = "Areal_Bolig", alder = "Alder", 
         altan = "Ejd_Altan", ejerudgift = "Beloeb_EjerUdgift", 
         antalrum = "Ejd_AntalRum", toiletter = "Ejd_AntalToiletter",
         dist_raadhus = "Dist_raadhus", add = "Adresse_Fuld", salgsaar = "Salgsaar") |> 
  mutate(pris_mio=pris/1000000, aar = salgsaar-2010)

summary(home_small_lejlighed)


writexl::write_xlsx(
  home_small_lejlighed,
  path = here::here("noter", "simple_neurale_net_regression", "data", "home_Aalborg9000_lejlighed.xlsx")
)


home_small_lejlighed |> 
  filter(pris_mio>5) |> 
  ggplot(aes(x = grund, y = pris)) +
  geom_point() +
  geom_smooth(method = "lm") 
# + facet_wrap(~ toiletter)

home_small |> ggplot(aes(x = alder, y = pris)) +
  geom_point(aes(color = factor(toiletter))) +
  geom_smooth(method = "lm") +
  facet_wrap(~ toiletter)

home_small |> ggplot(aes(x = alder, y = areal)) +
  geom_point(aes(color = cut(home_small$pris, breaks = 1e6*c(0, 2, 4, 10)))) +
  geom_smooth(method = "lm")

home_small |> 
  plot_ly(x = ~alder, y = ~areal, z = ~pris, color = ~toiletter, type = "scatter3d", mode = "markers")

home_small |> 
  plot_ly(x = ~alder, y = ~areal, z = ~pris, type = "scatter3d", mode = "markers")

fit_lm <- lm(pris ~ areal + alder + grund + toiletter, data = home_small)
summary(fit_lm)

fit <- nn_fun(pris_mio ~ areal + alder, data = na.omit(home_small_lejlighed), 
              n_hidden = c(0, 5), iter = 500, type = "regression", 
              lossfun = "squared", eta = .0001, weights=0, activation = "ReLu")
pred <- predict(fit, newdata = na.omit(home_small_lejlighed), type = "response")
plot(home_small_lejlighed$pris_mio, pred)
