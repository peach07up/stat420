## STAT 420
## Homework 6
## Fall 2015

options(digits=4)
setwd("C:/Users/dunger/Desktop/Box Sync/- STAT 420 -/homework")

## Exercise 1
library(faraway)
data(prostate)
attach(prostate)

fit = lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45)

## a
lev <- hatvalues(fit)
lev_mean <- mean(lev)
lev[lev > 2 * lev_mean]

## b
sresid <- rstudent(fit)
n <- length(sresid)
p <- length(fit$coefficients)
df <- n - p - 1
alpha <- 0.05

# without bonferroni
crit <- qt(1 - alpha/2,df)
sresid[abs(sresid) > crit]

# with bonferroni
crit <- qt(1 - (alpha/2)/n,df)
sresid[abs(sresid) > crit]

## c
cook <- cooks.distance(fit)
cook[cook > 4/n]
par(mfrow=c(2,3))
plot(fit, 1:6)

## d
fit_d <- lm(lpsa~lcavol+lweight+age+lbph+svi+lcp+gleason+pgg45, subset = cook < 4/n)
coef(fit)
coef(fit_d)
coef(fit_d) - coef(fit)
plot(fit_d, 1:6)


## Exercise 2
library(faraway)
library(lmtest)
data(tvdoctor)
?tvdoctor
life_model <- lm(life ~ . , data=tvdoctor)
  

## a
plot(life_model$fitted, life_model$resid, main="Part a")
abline(h=0)
bptest(life_model)

## b
hist(life_model$resid, breaks=8)
qqnorm(life_model$resid)
qqline(life_model$resid)
shapiro.test(life_model$resid)

## c
lev <- hatvalues(life_model)
lev_mean <- mean(lev)
lev[lev > 2 * lev_mean]

## d
sresid <- rstudent(life_model)
n <- length(sresid)
p <- length(life_model$coefficients)
df <- n - p - 1
alpha <- 0.05

# without bonferroni
crit <- qt(1 - alpha/2,df)
sresid[abs(sresid) > crit]

# with bonferroni
crit <- qt(1 - (alpha/2)/n,df)
sresid[abs(sresid) > crit]

## e
cook <- cooks.distance(life_model)
cook[cook > 4/n]
par(mfrow=c(2,3))
plot(life_model, 1:6)

## f
life_f <- lm(life ~ ., data = tvdoctor, subset = cook < 4/n)
coef(life_model)
coef(life_f)
coef(life_f) - coef(life_model)
plot(life_f, 1:6)

