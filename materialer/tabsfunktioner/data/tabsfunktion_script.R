## Data
datadir <- here::here("materialer", "tabsfunktioner", "data")
dat_navn <- file.path(datadir, "data.xlsx")
dat <- readxl::read_xlsx(dat_navn)
names(dat) <- c("x", "t")

# fit <- glm(t~x, family = binomial(), data = dat)
# w1_mle <- coef(fit)[2]
# w0_mle <- coef(fit)[1]
# w1_grid <- round(seq(-4,12.5,by=.05)*w1_mle, 4)
# w0_grid <- round(rev(seq(-5,4.5,by=.05))*w0_mle, 2)

w1_grid <- seq(-1,3,length.out=400)
w0_grid <- seq(-50, 30, length.out = 400)


## Cross entropy
crossentropy <- function(w0, w1, x, t){
  o <- 1/(1+exp(-(w0+w1*x)))
  - sum(ifelse(t==1, log(o), log(1-o)))
}
v_cross <- Vectorize(crossentropy, c("w0", "w1"))
cross_vals <- outer(w0_grid, w1_grid, v_cross, x = dat$x, t = dat$t)
cross_vals[is.infinite(cross_vals)] <- NA

index_cross <- which.min(cross_vals)
i_cross <- row(cross_vals)[mm]
j_cross <- col(cross_vals)[mm]
w0_cross <- w0_grid[i]
w1_cross <- w1_grid[j]
cross_opt <- crossentropy(w0_cross, w1_cross, x=dat$x, t=dat$t)

## Squared error
squared <- function(w0, w1, x, t){
  o <- 1/(1+exp(-(w0+w1*x)))
  sum((t-o)^2)/2
}
v_squared <- Vectorize(squared, c("w0", "w1"))
squared_vals <- outer(w0_grid, w1_grid, v_squared, x = dat$x, t = dat$t)

mm <- which.min(squared_vals)
i <- row(squared_vals)[mm]
j <- col(squared_vals)[mm]
w0_squared <- w0_grid[i]
w1_squared <- w1_grid[j]
squared_opt <- squared(w0_squared, w1_squared, x=dat$x, t=dat$t)

tabs_list <- list(w1 = w1_grid, w0 = w0_grid, 
                  cross_vals = cross_vals, cross_w0 = w0_cross, cross_w1 = w1_cross, cross_opt = cross_opt,
                  squared_vals = squared_vals, squared_w0 = w0_squared, squared_w1 = w1_squared, squared_opt = squared_opt)
saveRDS(tabs_list, file.path(datadir, "tabsfunktioner.rds"))

# library(plotly)
# par(mar=c(4,4,0,1))
# tabs_data <- tabs_list
# # contour(llik$a, llik$b, llik$vals, xlab = "a", ylab = "b", levels = c(-1028, -1030, -1034, -1038, -1046, -1052, -1084), labels = "")
# ax_x <- list(title = "w1")
# ax_y <- list(title = "w0")
# ax_z <- list(title = "Squared error")
# plot_ly(showscale = FALSE) |>
#   add_surface(x = tabs_data$w1, y = tabs_data$w0, z = ~tabs_data$cross_vals) |>
#   add_markers(x = w1_opt, y = w0_opt, z = squared_opt,
#               marker = list(size = 5, color = "red"), name = NULL) |>
#   layout(scene = list(xaxis = ax_x, yaxis = ax_y, zaxis = ax_z), showlegend = FALSE)
# 
