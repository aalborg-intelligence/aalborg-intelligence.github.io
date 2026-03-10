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
  filter(Postnr %in% c("9000"), EjdType == "Ejerlejlighed", !is.na(Alder)) |> 
  mutate(pris_mio=Pris_Salg/1000000, aar = Salgsaar-2010) |>
  select(pris_mio, areal = "Areal_Bolig", alder = "Alder")  
  
summary(home_small_lejlighed)


writexl::write_xlsx(
  home_small_lejlighed,
  path = here::here("noter", "simple_neurale_net_regression", "data", "home_Aalborg9000_lejlighed.xlsx")
)

## Forskellige plots

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

##

## Modeller

fit_lm <- lm(pris_mio ~ areal + alder, data = home_small_lejlighed)
summary(fit_lm)

pred <- predict(fit_lm, home_small_lejlighed, type = "response")
plot(home_small_lejlighed$pris_mio,pred)
abline(0,1)
#fit_lm2 <- lm(pris ~ areal + alder, data = home_small_lejlighed)
#summary(fit_lm2)


fit <- nn_fun(pris_mio ~ areal + alder, data = na.omit(home_small_lejlighed), 
              n_hidden = c(0, 5), iter = 500, type = "regression", 
              lossfun = "squared", eta = .0001, weights=0, activation = "ReLu")
pred <- predict(fit, newdata = na.omit(home_small_lejlighed), type = "response")
plot(home_small_lejlighed$pris_mio, pred)



## Bruges i noten

dat <- readxl::read_excel(here::here("noter/simple_neurale_net_regression/data/home_Aalborg9000_lejlighed.xlsx"))
fit_lm <- lm(pris_mio ~ areal + alder, data = dat)
#fit <- nn_fun(price ~ area + bedrooms + bathrooms + basement, data = dat, n_hidden = c(0, 0), iter = #500, type = "regression", lossfun = "squared", eta = .0001, weights=0)
#w <- as.vector(fit$params$W1)
#b <- as.vector(fit$params$b1)
#if(!is.null(fit$scale_val)){
#  w <- w / fit$scale_val
#  b <- b - sum(w * fit$center_val)
#}
pred <- predict(fit_lm, dat, type = "response")
length(pred)
summary(pred)
plot(dat$pris_mio,pred, xlab="Lejlighedspris i millioner", ylab="Prædikteret pris", col="#020873")
abline(0,1)
summary(fit_lm)

