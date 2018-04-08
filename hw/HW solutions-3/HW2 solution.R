## STAT 420
## Homework 2
## Fall 2015

## Exercise 1
x <- c(16, 12, 25, 19, 21, 15, 18)
y <- c(30, 52,  7, 32,  9, 56, 38)
mean(x); mean(y)
sum(x); sum(y)
x-mean(x); y-mean(y)
(x-mean(x))**2
(x-mean(x))*(y-mean(y))
(y-mean(y))**2
sum((x-mean(x))**2)
sum((x-mean(x))*(y-mean(y)))
sum((y-mean(y))**2)

x**2; x*y; y**2
sum(x**2); sum(x*y); sum(y**2)
sum(x**2)-(sum(x)**2)/7
sum(x*y)-sum(x)*sum(y)/7
sum(y**2)-(sum(y)**2)/7

## b
fit<-lm(y~x)
fit$fit
fit$res

## Exercise 2
## a
TVfit <- lm(y~x)
TVfit

## b
plot(x,y,main="Scatterplot of Time Use",
         xlab="Physical Activity (x)",
         ylab="TV Viewing (y)")
abline(fit$coefficients)

## c
TVfit$fitted

## d
TVfit$residuals
sum(TVfit$residuals)

## e
summary(TVfit)$sigma

## f
summary(TVfit)$r.squared

## g
predict(TVfit, data.frame(x=24))



## Exercise 4

x <- c(3.6,4.2,5.4,3,4.8,6)
y <- c(28,24,32,13.6,36,44)
x**2; x*y; y**2
sum(x**2); sum(x*y); sum(y**2)
venue <- lm(y ~ 0+x)
venue$coef

venue$fit
venue$res
sum(venue$res)

plot(x, y, xlim=c(0,6), ylim=c(0,45),
     main="Scatterplot of Concert Venue Revenue",
     xlab="Number of Patrons, in thousands (x)",
     ylab="Revenue, in thousands of dollars (y)")
abline(a=0,b=venue$coeff)
