set.seed(1)
x<-round(runif(2000,80,300))
u<-runif(2000,0,1)
b=-4.07#-4.24470
a=0.023#0.02368
p = function(x,a,b){exp(a*x + b)/(1+exp(a*x+ b))}
px <- p(x,a,b)
y <- as.numeric(u<px)
# writexl::write_xlsx(
# data.frame(blodtryk = x, syg = y),
# path = here::here("noter", "logistisk", "data", "blodtryk.xlsx")
# )
fit <- glm(y~x, family = binomial())
a_mle <- coef(fit)[2]
b_mle <- coef(fit)[1]
a_mle_pretty <- unname(signif(a_mle, 2))
b_mle_pretty <- unname(signif(b_mle, 2))
OR_mle <- unname(signif(exp(a_mle),4))
par(mar=c(4,4,0.5,1))
plot(x[1:200],y[1:200],pch=1,xlab="Blodtryk",ylab="y")
