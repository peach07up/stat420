## STAT 420
## Homework 3
## Fall 2015

## Exercise 1,2
x <- c(16, 12, 25, 19, 21, 15, 18)
y <- c(30, 52,  7, 32,  9, 56, 38)
TVfit <- lm(y~x)

## a,b
summary(TVfit)
## c
confint(TVfit, level=.90)
## d
sig = summary(TVfit)$sigma;    sig
t = (-4- -2)/(sig/sqrt(108));  t
p.value = pt(t, 5);            p.value
fit.d=lm(y~x+offset(-2*x)) 
summary(fit.d)
## e
se = summary(TVfit)$coef[1,2]; se   ## SE of beta0hat from table
t = (104-100)/(se);            t
p.value = 1 - pt(t, 5);        p.value
## f
predict(TVfit, data.frame(x=20), interval=c("conf"), level=.90)
## g
predict(TVfit, data.frame(x=20), interval=c("pred"), level=.90)
## h
t = (104-4*14 - 40)/(9.402*sqrt(1/7+(18-14)**2/108));  t
p.value = 1 - pt(t, 5);                                p.value

SE = predict(TVfit,data.frame(x=14),interval=c("conf"),level=.90,se.fit=T)$se.fit; SE
t = (104-4*14 - 40)/SE;     t
p.value = 1 - pt(t, 5);     p.value


## Exercise 3
## a
car.fit <- lm(dist ~ speed, data=cars)
car.fit
## b
plot(cars$speed, cars$dist, xlab="Speed", ylab="Stopping Distance")
abline(car.fit)
## c
confint(car.fit, level=.90)
## d
summary(car.fit)
