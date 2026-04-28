dat <- readxl::read_excel(here::here("noter/simple_neurale_net_regression/data/home_Aalborg9000_lejlighed.xlsx"))
set.seed(42)
fit_nn10 <- nn_fun(pris_mio ~ areal + alder, data = dat, 
              n_hidden = c(10, 0), iter = 20000, type = "regression", 
              lossfun = "squared", eta = .00001, activation = "ReLu")
saveRDS(fit_nn10, file = here::here("noter/simple_neurale_net_regression/data/home_Aalborg9000_lejlighed_fit_nn.rds"))
