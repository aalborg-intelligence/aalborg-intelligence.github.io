library(mosaic)
x_breaks <- c(75, 100, 125, 150, 175, 200, 225, 250, 275, 300)
cut <- cut(x, x_breaks)
tab <- tally(y ~ cut, format = "percent")[2,]
odds <- tab/(100-tab)
x_midt <- x_breaks[-1]-12.5
par(mar=c(4,4,0.5,1))
plot(x_midt, tab/100, ylim=c(0,1), pch=16, xlab="Blodtryk", ylab="p", xlim=c(70,320), xaxt="n")
abline(-0.341213 + 12.5*0.004409, 0.004409 )
axis(1, at=x_breaks)