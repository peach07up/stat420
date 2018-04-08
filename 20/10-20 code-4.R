

x1 <- rnorm(100,80,10)
x2 <- rnorm(100,70,5)
x3 <- 2*x1 + 4*x2 + 3

y <- 3 + x1 + x2 + rnorm(100)


fit <- lm(y ~ x1 + x2 + x3)
summary(fit)

x0 <- rep(1,100)
X <- cbind(x0,x1,x2,x3)

solve(t(X) %*% X)





library(faraway)
data(seatpos)

pairs(seatpos)
round(cor(seatpos),2)

fit <- lm(hipcenter ~ ., data = seatpos)
summary(fit)



fitHtShoes <- lm(HtShoes ~ . -hipcenter, data = seatpos)
summary(fitHtShoes)

vif(fit)


fitNoise <- lm(hipcenter+10*rnorm(38) ~ ., data = seatpos)
fit
fitNoise






fit2 <- lm(hipcenter ~ Age + Weight + Ht, data = seatpos)
summary(fit2)
vif(fit2)




fit3 <- lm(HtShoes ~ Age + Weight + Ht, data = seatpos)

plot(fit3$residuals, fit2$residuals)
abline(h=0,lty=2)
abline(v=0,lty=2)
abline(lm(fit2$residuals ~ fit3$residuals))

cor(fit3$residuals, fit2$residuals)















tricep <- c(19.5,24.7,30.7,29.8,19.1,25.6,31.4,27.9,22.1,25.5,
            31.1,30.4,18.7,19.7,14.6,29.5,27.7,30.2,22.7,25.2)
thigh <- c(43.1,49.8,51.9,54.3,42.2,53.9,58.5,52.1,49.9,53.5,
           56.6,56.7,46.5,44.2,42.7,54.4,55.3,58.6,48.2,51.0)
midarm <- c(29.1,28.2,37.0,31.1,30.9,23.7,27.6,30.6,23.2,24.8,
            30.0,28.3,23.0,28.6,21.3,30.1,25.7,24.6,27.1,27.5)
bodyfat <- c(11.9,22.8,18.7,20.1,12.9,21.7,27.1,25.4,21.3,19.3,
             25.4,27.2,11.7,17.8,12.8,23.9,22.6,25.4,14.8,21.1)
bodyfat <- data.frame(bodyfat, tricep, thigh, midarm)



fit <- lm(bodyfat ~., data = bodyfat)
summary(fit)
vif(fit)

fit4 <- lm(bodyfat ~ tricep, data = bodyfat)
summary(fit4)
vif(fit4)

fit1 <- lm(bodyfat ~ thigh + midarm, data = bodyfat)
fit2 <- lm(tricep ~ thigh + midarm, data = bodyfat)

cor(fit2$residuals,fit1$residuals)


plot(fit2$residuals,fit1$residuals)
abline(h=0,lty=2)
abline(v=0,lty=2)
fit3 <- lm(fit1$residuals ~ fit2$residuals)
abline(fit3)


summary(bodyfat$tricep)
summary(bodyfat$thigh)
plot(bodyfat$tricep,bodyfat$thigh)
