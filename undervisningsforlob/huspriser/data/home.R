library(aimat)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
load(here::here("undervisningsforlob/huspriser/data/Homedata.Rda")) # homedata

unique(homedata$EjdType)

# , str_detect(Bynavn, "A.*borg")

home_small <- homedata |>
  filter(Postnr %in% c("9000", "9200"), EjdType == "Villa") |> 
  select(pris = "Pris_Salg", areal = "Areal_Bolig", alder = "Alder", 
         grund = "Areal_Grund", toiletter = "Ejd_AntalToiletter")

library("writexl")
#dataTable <- data.frame(x,y+2.3)
#names(dataTable) <- c("x", "y")
write_xlsx(home_small,"home_Aalborg.xlsx")

home_small |> 
#  filter(areal>200) |> 
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
