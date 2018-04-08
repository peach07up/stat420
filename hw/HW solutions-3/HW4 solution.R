## STAT 420
## Homework 4
## Fall 2015

options(digits=4)
## Exercise 1
x1 <- c(2, 3, 1, 1, 2, 4, 1, 1, 3, 2)
x2 <- c(1, 0, 0, 1, 1, 1, 0, 1, 0, 0)
y  <- c(3.1, 2.6, 2.2, 2.9, 5.1, 4.5, 0.7, 2.4, 3.6, 2.9)

X <- cbind(rep(1,10),x1,x2)
t(X)%*%X
solve(t(X)%*%X)
t(X)%*%y
solve(t(X)%*%X) %*% t(X)%*%y
sum((y-mean(y))**2)
sum((lm(y~x1+x2)$resid)**2)

x0 = c(1,3,1)
solve(t(X)%*%X)%*%x0
t(x0)%*%solve(t(X)%*%X)%*%x0

x0 = c(1,4,0)
solve(t(X)%*%X)%*%x0
t(x0)%*%solve(t(X)%*%X)%*%x0


## Exercise 2
## a, b, c
fit <- lm(y~x1+x2)
summary(fit)
anova(lm(y~1),fit)

## d
se.beta1hat = summary(fit)$coef[2,2];   se.beta1hat
t = (.7-1)/se.beta1hat;                 t
p.value = pt(t, 7);                     p.value

## e
confint(fit, level=.95)[1,]

## f
confint(fit, level=.90)[3,]

## g
predict(fit, data.frame(x1=3, x2=1), interval=c("conf"), level=.95)

## h
predict(fit, data.frame(x1=4, x2=0), interval=c("pred"), level=.90)

## i
summary(fit)$r.squared

## Exercise 3

# http://lib.stat.cmu.edu/DASL/Datafiles/carmpgdat.html

mpg <- read.table("C:\\Users\\dunger\\Desktop\\Box Sync\\- STAT 420 -\\homework\\carmpgdat.txt", header=T, sep="\t")
attach(mpg)
set.seed(4)
ind <- sample(82,10)

y  <- MPG[ind]
x1 <- HP[ind]
x2 <- SP[ind]
x3 <- WT[ind]
y; x1; x2; x3

X <- cbind(rep(1,10), x1, x2, x3)
C = solve(t(X)%*%X)
x = c(1, 100, 100, 20)
t(x)%*%C%*%x
u=c(83.43,.307,-0.848,-0.595)
t(x)%*%u

## a
fit3 <- lm(y~x1+x2+x3)
summary(fit3)

predict(fit3, data.frame(x1=100,x2=100,x3=20),
        interval=c("pred"), level=.95)
C = round(solve(t(X)%*%X),3); C
x = c(1, 100, 100, 20)
Cx = round(C%*%x,3); Cx
t(x)%*%Cx


detach(mpg)










X <- cbind(rep(1,10),x1,x2,x3)
t(X)%*%X
solve(t(X)%*%X)
t(X)%*%y
solve(t(X)%*%X) %*% t(X)%*%y
sum((y-mean(y))**2)
sum((lm(y~x1+x2)$resid)**2)
summary(lm(y~x1+x2+x3))


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
