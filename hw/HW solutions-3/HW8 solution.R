## STAT 420
## Homework 8
## Fall 2015

options(digits=4)
setwd("C:/Users/dunger/Desktop/Box Sync/- STAT 420 -/homework")

## Exercise 1
productivity <- read.table("productivity.txt", header=T)
head(productivity)
attach(productivity)

## a
plot(X, Y, main="Part a")
fit.a <- lm(Y ~ X)
abline(fit.a)
summary(fit.a)

## b
fit.b <- lm(Y ~ X + I(X^2))
summary(fit.b)
anova(fit.a, fit.b)

## c
plot(X, Y, main="Part c")
abline(fit.a)
xplot <- seq(15,65,by=0.1)
lines(xplot, predict(fit.b, newdata = data.frame(X = xplot)),
      col="red", lty = 2, lwd=2)

## d
x_cent <- X - mean(X)
fit.d <- lm(Y ~ x_cent)
fit2.d <- lm(Y ~ x_cent + I(x_cent^2))
plot(x_cent, Y, main="Part d")
abline(fit.d)
xplot <- seq(-25,25,by=0.1)
lines(xplot, predict(fit2.d, newdata = data.frame(x_cent = xplot)),
      col="red", lty = 2, lwd=2)
summary(fit2.d)

## e
fit3 <- lm(Y ~ x_cent + I(x_cent^2) + I(x_cent^3))
summary(fit3)
anova(fit2.d, fit3)

## f
plot(x_cent, Y, main="Part f")
abline(fit.d)
xplot <- seq(-25,25,by=0.1)
lines(xplot, predict(fit2.d, newdata = data.frame(x_cent = xplot)),
      col="red", lty = 2, lwd=2)
lines(xplot, predict(fit3, newdata = data.frame(x_cent = xplot)),
      col="blue", lty = 3, lwd=3)

detach(productivity)

## Exercise 2
admissions <- read.table("admissions.txt", header=T)
head(admissions)
attach(admissions)

## a
plot(Score, GPA, main="Part a", col=Major+1, pch=Major+1)

## b
fit.b <- lm(GPA ~ Score + Major)
summary(fit.b)

## d
fit.d <- lm(GPA ~ Score + Major + Score:Major)
summary(fit.d)

## e
anova(fit.b, fit.d)

detach(admissions)


## Exercise 3
library(faraway) 
data(prostate)

## a
fit.a1 <- lm(lpsa ~ ., prostate)
summary(fit.a1)
fit.a2 <- lm(lpsa ~ . - gleason, prostate)
summary(fit.a2)
fit.a3 <- lm(lpsa ~ . - gleason - lcp, prostate)
summary(fit.a3)
fit.a4 <- lm(lpsa ~ . - gleason - lcp - pgg45, prostate)
summary(fit.a4)
fit.a5 <- lm(lpsa ~ . - gleason - lcp - pgg45 - age, prostate)
summary(fit.a5)
fit.a6 <- lm(lpsa ~ . - gleason - lcp - pgg45 - age - lbph, prostate)
summary(fit.a6)

## b
fit.b <- step(fit.a1, direction="backward")

## c
summary(fit.a1)$r.squared
summary(fit.a6)$r.squared
summary(fit.b)$r.squared
