## Data
datadir <- here::here("noter", "logistisk", "data")
dat_navn <- file.path(datadir, "blodtryk.xlsx")
dat <- readxl::read_xlsx(dat_navn)
names(dat) <- c("x", "y")

## Log-likelihood
fit <- glm(y~x, family = binomial(), data = dat)
a_mle <- coef(fit)[2]
b_mle <- coef(fit)[1]
a_grid <- round(seq(0.5,1.5,by=.005)*a_mle, 4)
b_grid <- round(rev(seq(0.5,1.5,by=.005))*b_mle, 2)
llik <- function(a, b, x, y){
  log_p <- -log(1 + exp(-a*x - b))
  log_1_minus_p <- (-a*x-b) - log(1 + exp(-a*x - b))
  sum(y * log_p + (1-y)*log_1_minus_p)
}
v_llik <- Vectorize(llik, c("a", "b"))
llik_vals <- outer(a_grid, b_grid, v_llik, x = dat$x, y = dat$y)
llik_list <- list(a = a_grid, b = b_grid, vals = llik_vals)
saveRDS(llik_list, file.path(datadir, "llik.rds"))
