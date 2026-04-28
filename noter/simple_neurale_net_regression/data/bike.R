library(aimat)
dat <- read.csv(here::here("noter/simple_neurale_net_regression/data/day.csv"))
summary(dat)

fit <- nn_fun(price ~ area + bedrooms + bathrooms + basement, data = dat, n_hidden = c(0, 0), iter = 500, type = "regression", lossfun = "squared", eta = .0001, weights=0)
w <- as.vector(fit$params$W1)
b <- as.vector(fit$params$b1)
if(!is.null(fit$scale_val)){
  w <- w / fit$scale_val
  b <- b - sum(w * fit$center_val)
}
print(c(bias = b, w))

pred <- predict(fit, dat, type = "response")

sqrt(mean((pred-dat$price)^2))

plot(dat$price,pred)
abline(0,1)

fit_lm <- lm(price ~ area + bedrooms + bathrooms + basement, data = dat)

pred_lm <- predict(fit_lm, dat, type = "response")

sqrt(mean((pred_lm-dat$price)^2))

summary(fit_lm)

## Malenes
summary(dat$bathrooms)
pairs(dat[,c("price", "bedrooms", "area", "bathrooms","basement")])

fitNN <- nn_fun(price ~ area + bedrooms + bathrooms + basement, data = dat, 
                n_hidden = c(40, 0), iter = 10000, type = "regression", 
                lossfun = "squared", eta = .00001, activation = "Sigmoid")

predNN <- predict(fitNN, dat, type = "response")

sum((dat$price-mean(dat$price))^2)/2
mean((dat$price-mean(dat$price))^2)

mean((dat$price)^2)


sqrt(mean((predNN-dat$price)^2))


plot(dat$price,predNN)
abline(0,1)
