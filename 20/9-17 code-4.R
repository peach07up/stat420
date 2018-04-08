x <- c(2,6,8,8,12,16,20,20,22,26)
y <- c(58,105,88,118,117,137,157,169,149,202)

fit <- lm(y ~ x)

fit
summary(fit)

anova(fit)


confint(fit, level=0.90)

new <- data.frame(x=10)
predict.lm(fit,new,interval=c("confidence"),level=0.95)

new <- data.frame(x=38)
predict.lm(fit,new,interval=c("confidence"),level=0.95)
predict.lm(fit,new,interval=c("prediction"),level=0.95)

new <- data.frame(x=c(10,20,30,40))
predict.lm(fit,new,interval=c("confidence"),level=0.95)
predict.lm(fit,new,interval=c("prediction"),level=0.95)
