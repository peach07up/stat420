## STAT 420
## Homework 7
## Fall 2015

options(digits=4)
setwd("C:/Users/dunger/Desktop/Box Sync/- STAT 420 -/homework")

## Exercise 1
plasma <- read.table("plasma.txt")
names(plasma) <- c("X", "Y")
attach(plasma)

## a
plot(plasma, main="Part a")
fit.a <- lm(Y ~ X)
abline(fit.a)
summary(fit.a)

## b
plot(fit.a$fitted, fit.a$resid, main="Part b")
abline(h=0)

## c
plot(X, log(Y), main="Part c")
fit.c <- lm(log(Y) ~ X)
abline(fit.c)
summary(fit.c)

## d
plot(fit.c$fitted, fit.c$resid, main="Part d")
abline(h=0)

## e
library(MASS)
boxcox(fit.a, plotit = TRUE)
title("Part e")
boxcox(fit.a, plotit = TRUE, lambda = seq(-1.5,0.5,by = 0.1))
title("Part e (zoomed in)")

## f
plot(X, Y^(-0.5), main="Part f")
fit.f <- lm(Y^(-0.5) ~ X)
abline(fit.f)
summary(fit.f)

## g
plot(fit.f$fitted, fit.f$resid, main="Part g")
abline(h=0)

## h
hist(fit.f$resid, breaks=8)
qqnorm(fit.f$resid)
qqline(fit.f$resid)
shapiro.test(fit.f$resid)


## Exercise 2
library(faraway)
data(longley)

## a
round(cor(longley),2)

## b
plot(longley)

## c
fit <- lm(Employed ~ ., data = longley)
vif(fit)

## d
Popn.fit <- lm(Population ~ . - Employed, data = longley)
summary(Popn.fit)$r.squared

## e
Empl.fit <- lm(Employed ~ . - Population, data = longley)
cor(resid(Popn.fit), resid(Empl.fit))

## f
summary(fit)
fit2 <- lm(Employed ~ Year + Armed.Forces + Unemployed, data = longley)
summary(fit2)
vif(fit2)

## g
anova(fit2, fit)
