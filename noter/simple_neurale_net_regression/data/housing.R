library(aimat)
dat <- readxl::read_excel(here::here("noter/simple_neurale_net_regression/data/Housing.xlsx"))
fit <- nn_fun(price ~ area + bedrooms + bathrooms, data = dat, n_hidden = c(0, 0), iter = 500, type = "regression", lossfun = "squared", eta = .0001)
w <- as.vector(fit$params$W1)
b <- as.vector(fit$params$b1)
if(!is.null(fit$scale_val)){
  w <- w / fit$scale_val
  b <- b - sum(w * fit$center_val)
}
print(c(bias = b, w))

pred <- predict(fit, dat, type = "response")

fit_lm <- lm(price ~ area + bedrooms + bathrooms, data = dat)
summary(fit_lm)
