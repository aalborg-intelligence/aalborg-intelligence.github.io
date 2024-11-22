## Data
datadir <- here::here("materialer", "tabsfunktioner", "data")
dat_navn <- file.path(datadir, "data.xlsx")
dat <- readxl::read_xlsx(dat_navn)
names(dat) <- c("x", "t")

## Cross entropy
fit <- glm(t~x, family = binomial(), data = dat)
w1_mle <- coef(fit)[2]
w0_mle <- coef(fit)[1]
w1_grid <- round(seq(0.5,1.5,by=.005)*w1_mle, 4)
w0_grid <- round(rev(seq(0.5,1.5,by=.005))*w0_mle, 2)
crossentropy <- function(w0, w1, x, t){
  o <- 1/(1+exp(-(w0+w1*x)))
  - sum(t * log(o) + (1-t)*log(1-o))
}
v_cross <- Vectorize(crossentropy, c("w0", "w1"))
cross_vals <- outer(w0_grid, w1_grid, v_cross, x = dat$x, t = dat$t)

## Squared error
squared <- function(w0, w1, x, t){
  o <- 1/(1+exp(-(w0+w1*x)))
  sum((t-o)^2)
}
v_squared <- Vectorize(squared, c("w0", "w1"))
squared_vals <- outer(w0_grid, w1_grid, v_squared, x = dat$x, t = dat$t)


tabs_list <- list(w1 = w1_grid, w0 = w0_grid, cross_vals = cross_vals, squared_vals = squared_vals)
saveRDS(tabs_list, file.path(datadir, "tabsfunktioner.rds"))


