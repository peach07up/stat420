## STAT 420
## Homework 5
## Fall 2015

options(digits=4)
setwd("C:/Users/dunger/Desktop/Box Sync/- STAT 420 -/homework")


## Exercise 2
library(faraway)    # This accesses the data from the faraway package.
data(prostate)
prostate[1:5,]      # This prints the first 5 obs.
attach(prostate)    # This attaches an alias to each column so you don???t need to say prostate$lpsa, for example.

## a
fit = lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45)
summary(fit)

## b
confint(fit, "age", level=0.95)
confint(fit, "age", level=0.90)

## c
newc = data.frame(lcavol=1.44692, lweight=3.62301,
                  age=65.00000, lbph=0.30010, svi=0.00000, lcp=-0.79851,
                  gleason=7.00000, pgg45=15.00000)
predict.lm(fit,newc,interval=c("prediction"),level=0.95)

## d
newd = data.frame(lcavol=1.44692, lweight=3.62301,
                  age=20.00000, lbph=0.30010, svi=0.00000, lcp=-0.79851,
                  gleason=7.00000, pgg45=15.00000)
predict.lm(fit,newd,interval=c("prediction"),level=0.95)


## e
plot(fit$fitted.values,fit$residuals)
abline(h=0,lty=2)
library(lmtest)
bptest(fit)

## f
hist(fit$residuals)
qqnorm(fit$residuals)
shapiro.test(fit$residuals)

## g
summary(fit)
fit2 = lm(lpsa~lcavol+lweight+svi)
summary(fit2)
anova(fit2,fit)

