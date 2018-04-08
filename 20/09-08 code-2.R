## Simple Linear Regression
## Data
x = c(2,6,8,8,12,16,20,20,22,26)
y = c(58,105,88,118,117,137,157,169,149,202)
N = length(x)

## Estimate the regression line using pre-defined R functions
fit = lm(y ~ x)
fit
summary(fit)
names(fit)
names(summary(fit))

plot(x,y)
abline(fit$coefficients)

fit$fitted.values
fit$residuals
summary(fit)$sigma

sum(fit$residuals^2)
sum((y-fit$fitted.values)^2)

s2 = sum(fit$residuals^2)/(N-2); s2
s = sqrt(s2); s

## SXX, SXY, SYY
SXX = sum((x-mean(x))^2); SXX
SXY = sum((x-mean(x))*(y-mean(y))); SXY
SYY = sum((y-mean(y))^2); SYY
beta1hat = SXY/SXX; beta1hat
beta0hat = mean(y)-beta1hat*mean(x); beta0hat

## Matrix approach
Xmat = cbind(rep(1,N), x); Xmat
XX = t(Xmat) %*% Xmat; XX
XXinv = solve(XX); XXinv
XY = t(Xmat) %*% y; XY
betahat = XXinv %*% XY; betahat

## Predicting Y values
predict(fit,data.frame(x=10))
predict(fit,data.frame(x=38))

## Estimate the regression line using pre-defined R functions
fit1 = glm(y ~ x)
fit1
summary(fit1)
names(fit1)

## True relationship 
beta0 = 10
beta1 = 5
truevar = 10

## x's fixed
N = 20
x = seq(25, 30, length=N)
trueline = beta0 + beta1*x

## Y data
yobs = trueline + rnorm(N, 0, sqrt(truevar))

plot(x, yobs)
lines(x, trueline, col=2)

regout = lm(yobs~x)
estline = regout$coeff[1] + regout$coeff[2]*x
lines(x, estline, col=1)


##### Sampling - plot of many estimates

plot(x, trueline, type="l", ylim=c(125, 170), col=2)
S = 100
for(s in 1:S){
  yobs = trueline + rnorm(N, 0, sqrt(truevar))
  regout = lm(yobs~x)
  estline = regout$coeff[1] + regout$coeff[2]*x
  lines(x, estline, col=1)
}
lines(x, trueline, type="l",col=2)


##### Sampling distribution of the estimators

simsize=1000
beta0est = c(1:simsize)
beta1est = c(1:simsize)
varest = c(1:simsize)

for (s in 1:simsize){
  yobs = trueline + rnorm(N, 0, sqrt(truevar))
  regout = lm(yobs~x)
  beta0est[s] = regout$coeff[1]
  beta1est[s] = regout$coeff[2]
  varest[s] = sum((regout$resid)^2)/(N-2)
}

## Histogram of beta0est
hist(beta0est, nclass=10)

## Histogram of beta1est
hist(beta1est, nclass=10)

## Histogram of varest
hist(varest, nclass=10)
