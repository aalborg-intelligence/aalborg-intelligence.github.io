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
# 
# home_small_lejlighed |> 
#   filter(pris_mio>5) |> 
#   ggplot(aes(x = grund, y = pris)) +
#   geom_point() +
#   geom_smooth(method = "lm") 
# + facet_wrap(~ toiletter)

# home_small |> ggplot(aes(x = alder, y = pris)) +
#   geom_point(aes(color = factor(toiletter))) +
#   geom_smooth(method = "lm") +
#   facet_wrap(~ toiletter)
# 
# home_small |> ggplot(aes(x = alder, y = areal)) +
#   geom_point(aes(color = cut(home_small$pris, breaks = 1e6*c(0, 2, 4, 10)))) +
#   geom_smooth(method = "lm")
# 
# home_small |> 
#   plot_ly(x = ~alder, y = ~areal, z = ~pris, color = ~toiletter, type = "scatter3d", mode = "markers")
# 
# home_small |> 
#   plot_ly(x = ~alder, y = ~areal, z = ~pris, type = "scatter3d", mode = "markers")
# 
##

## Modeller

fit_lm <- lm(pris_mio ~ areal + alder, data = home_small_lejlighed)
summary(fit_lm)

pred_lm <- predict(fit_lm, home_small_lejlighed, type = "response")
summary(pred_lm)
plot(home_small_lejlighed$pris_mio,pred_lm)
abline(0,1)
#fit_lm2 <- lm(pris ~ areal + alder, data = home_small_lejlighed)
#summary(fit_lm2)


fit <- nn_fun(pris_mio ~ areal + alder, data = home_small_lejlighed, 
              n_hidden = c(10, 0), iter = 20000, type = "regression", 
              lossfun = "squared", eta = .00001, activation = "ReLu")

pred <- predict(fit, newdata = home_small_lejlighed, type = "response")
plot(home_small_lejlighed$pris_mio, pred)

summary(pred)

plot(1:10, main ="Titel her")

sqrt(mean((home_small_lejlighed$pris_mio-pred)^2))

fit$params



orange <- read.delim(
  here::here("noter/simple_neurale_net_regression/data/home_Aalborg9000_lejlighed_orange_pred.tab"),
  skip = 3,
  header = FALSE)

sqrt(mean((orange[,1]-orange[,3])^2))





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




### Surface
areal_grid <- seq(30,370,length.out=100)
alder_grid <- seq(0, 200, length.out = 100)

newdata <- expand.grid(areal = areal_grid, alder = alder_grid)
newdata$pris_mio <- 0
newdata$pris_mio <- predict(fit, newdata, type = "response")[,1]

library(plotly)
hovertemplate <- paste0(
  'alder: %{y:.2f}<br>',
  'areal: %{x:.2f}<br>',
  'pris: %{z:.2f}<br>',
  '<extra></extra>')
par(mar=c(4,4,0,1))
z <- matrix(newdata$pris_mio, 100, 100)
tabs_data2 <- list(alder=alder_grid,areal=areal_grid,pris_mio = z)
# contour(llik$a, llik$b, llik$vals, xlab = "a", ylab = "b", levels = c(-1028, -1030, -1034, -1038, -1046, -1052, -1084), labels = "")
ax_x <- list(title = "areal")
ax_y <- list(title = "alder")
ax_z <- list(title = "Pris i millioner")
plot_ly(showscale = FALSE) |>
  #add_surface(x = tabs_data$areal, y = tabs_data$alder, z = tabs_data$pris_mio, hovertemplate = hovertemplate) |> 
  add_surface(x = tabs_data2$areal, y = tabs_data2$alder, z = tabs_data2$pris_mio, hovertemplate = hovertemplate)


|>
  add_markers(x = tabs_data$areal, y = tabs_data$alder, z = tabs_data$pris_mio,
              marker = list(size = 5, color = "red"), name = NULL, hovertemplate = hovertemplate) |>
  layout(scene = list(xaxis = ax_x, yaxis = ax_y, zaxis = ax_z), showlegend = FALSE)
