sales <- c(5.0,6.0,6.5,7.0,7.5,8.0,10.0,10.8,12.0,13.0,15.5,15.0,16.0,17.0,
           18.0,18.0,18.5,21.0,20.0,22.0,23.0)
advert <- c(1.0,1.8,1.6,1.7,2.0,2.0,2.3,2.8,3.5,3.3,4.8,5.0,7.0,8.1,8.0,10.0,
            8.0,12.7,12.0,15.0,14.4)
marketing <- data.frame(sales,advert)
head(marketing)

plot(marketing$advert,marketing$sales)

mark_mod <- lm(sales ~ advert, data = marketing)
summary(mark_mod)

mark_mod_poly2 <- lm(sales ~ advert + I(advert^2), data = marketing)
summary(mark_mod_poly2)

X <- cbind( rep(1,21), marketing$advert, marketing$advert^2 )
t(X) %*% X
solve(t(X) %*% X) %*% t(X) %*% marketing$sales

mark_mod_poly3 <- lm(sales ~ advert + I(advert^2) + I(advert^3), data = marketing)
summary(mark_mod_poly3)


plot(marketing$advert,marketing$sales, 
     xlab = "Advert Spending (in $100,000)", ylab = "Sales (in $100,000)")
abline(mark_mod, lty = 2, col = "green", lwd = 2)
xplot <- seq(0, 16, by = 0.01)
lines(xplot, predict(mark_mod_poly2, newdata = data.frame(advert = xplot)),
      col="blue", lwd = 2)
lines(xplot, predict(mark_mod_poly3, newdata = data.frame(advert = xplot)),
      col="red", lty = 3, lwd = 3)

library(ggplot2)
ggplot(data = marketing, aes(x = advert, y = sales)) +
  stat_smooth(method = "lm", se=FALSE, color="green", formula = y ~ x) +
  stat_smooth(method = "lm", se=FALSE, color="blue", size = 1, formula = y ~ x + I(x^2)) +
  stat_smooth(method = "lm", se=FALSE, color="red", formula = y ~ x + I(x^2)+ I(x^3)) +
  geom_point(colour = "black", size = 3)



set.seed(1234)
x <- seq(0,10)
y <- 3 + x + 4*x^2 + rnorm(11,0,20)
plot(x,y,ylim=c(-100,400))
fit <- lm(y ~ x + I(x^2))
summary(fit)
fit_perf <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10))
summary(fit_perf)
xplot <- seq(0, 10, by = 0.1)
lines(xplot, predict(fit, newdata = data.frame(x = xplot)),
      col="blue", lwd = 1, lty = 1)
lines(xplot, predict(fit_perf, newdata = data.frame(x = xplot)),
      col="red", lwd = 1, lty = 1)






mpg <- c(4.8,5.7,8.6,7.3,9.8,11.2,13.7,12.4,18.2,16.8,19.9,19.0,22.4,23.5,21.3,
         22.0,20.5,19.7,18.6,19.3,14.4,13.7,12.1,13.0,10.1,9.4,8.4,7.6)
speed <- c(10,10,15,15,20,20,25,25,30,30,35,35,40,40,45,45,50,50,55,55,60,
           60,65,65,70,70,75,75)
econ <- data.frame(speed, mpg)
plot(econ$speed, econ$mpg)

fit1 <- lm(mpg ~ speed, data = econ)
abline(fit1)



plot_fit_res <- function(model){
  plot(fitted(model),resid(model), xlab = "Fitted", ylab = "Residuals")
  abline(h = 0)
}


plot_fit_res(fit1)

fit2 <- lm(mpg ~ speed + I(speed^2), data = econ)


plot(econ$speed, econ$mpg)
xplot <- seq(10, 75, by = 0.1)
lines(xplot, predict(fit2, newdata = data.frame(speed = xplot)),
      col="blue", lwd = 1, lty = 1)

plot_fit_res(fit2)



plot_econ_curve <- function(model){
  plot(econ$speed, econ$mpg, xlab = "Speed", ylab = "Miles per Gallon")
  xplot <- seq(10, 75, by = 0.1)
  lines(xplot, predict(model, newdata = data.frame(speed = xplot)),
        col="blue", lwd = 1, lty = 1)
}



summary(fit2)
plot_econ_curve(fit2)
plot_fit_res(fit2)

fit3 <- lm(mpg ~ speed + I(speed^2) + I(speed^3), data = econ)
summary(fit3)
plot_econ_curve(fit3)
plot_fit_res(fit3)

fit4 <- lm(mpg ~ speed + I(speed^2) + I(speed^3) + I(speed^4), data = econ)
summary(fit4)
plot_econ_curve(fit4)
plot_fit_res(fit4)

fit5 <- lm(mpg ~ speed + I(speed^2) + I(speed^3) + I(speed^4) + I(speed^5), data = econ)
summary(fit5)
plot_econ_curve(fit5)
plot_fit_res(fit5)

fit6 <- lm(mpg ~ speed + I(speed^2) + I(speed^3) + I(speed^4) + I(speed^5) + I(speed^6), data = econ)
summary(fit6)
plot_econ_curve(fit6)
plot_fit_res(fit6)

anova(fit4,fit6)

fit8 <- lm(mpg ~ speed + I(speed^2) + I(speed^3) + I(speed^4) + I(speed^5) + I(speed^6) + I(speed^7) + I(speed^8), data = econ)
summary(fit8)
plot_econ_curve(fit8)
plot_fit_res(fit8)

anova(fit6,fit8)

predict(fit6, data.frame(speed = 55), interval = "confidence")
predict(fit6, data.frame(speed = 55), interval = "prediction")

fit_ortho <- lm(mpg ~ poly(speed,8), data = econ)
summary(fit_ortho)
summary(fit8)
poly(econ$speed,8)
round(cor(poly(econ$speed, 8)),2)

x <- econ$speed
cbind(x,x^2,x^3,x^4,x^5,x^6,x^7,x^8)
round(cor(cbind(x,x^2,x^3,x^4,x^5,x^6,x^7,x^8)),2)








x <- c(280,284,292,295,298,304,308,315)
y <- c(770,800,840,810,735,640,590,560)
x_cent <- x - mean(x)

fit <- lm(y ~ x + I(x^2))
summary(fit)

fit <- lm(y ~ x_cent + I(x_cent^2))
summary(fit)

x^2
cor(x,x^2)
cor(x_cent,x_cent^2)

