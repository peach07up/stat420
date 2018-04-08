# working with leverages
x <- c(2,6,8,8,12,16,20,20,22,26)
y <- c(58,105,88,118,117,137,157,169,149,202)
X <- cbind(rep(1,10), x)

H <- X %*% solve(t(X)%*%X) %*% t(X)
diag(H)    
sum(diag(H))

fit <- lm(y ~ x)
hatvalues(fit)

x1 <- c( 0,11,11, 7, 4,10, 5, 8)
x2 <- c( 1, 5, 4, 3, 1, 4, 4, 2)
y  <- c(11,15,13,14, 0,19,16, 8)

plot(x1,x2)

X <- cbind( rep(1,8), x1, x2 )
H <- X %*% solve(t(X)%*%X) %*% t(X)
diag(H)    
sum(diag(H))

2*mean(diag(H))
min(diag(H))
max(diag(H))
      
fit <- lm(y ~ x1 + x2)
hatvalues(fit)

lm(y ~ x1 + x2)


y[1] <- 20          ### point 1 has large leverage
lm(y ~ x1 + x2)

y[1] <- 11
y[4] <- 30          ### point 4 has small leverage
lm(y ~ x1 + x2)

mean(x1)
mean(x2)









# plots with appear on page 87 of the book
set.seed(123)
testdata <- data.frame(x=1:10,y=1:10+rnorm(10))
lmod <- lm(y ~ x, testdata)
plot(y ~ x, testdata)

# plot 1
p1 <- c(5.5,12)
lmod1 <- lm(y ~ x, rbind(testdata, p1))
plot(y ~ x, rbind(testdata, p1))
points(5.5,12,pch=4,cex=2)
abline(lmod)
abline(lmod1, lty=2)

# plot 2
p2 <- c(15,15.1)
lmod2 <- lm(y ~ x, rbind(testdata, p2))
plot(y ~ x, rbind(testdata, p2))
points(15,15.1,pch=4,cex=2)
abline(lmod)
abline(lmod2,lty=2)

# plot 3
p3 <- c(15,5.1)
lmod3 <- lm(y ~ x, rbind(testdata, p3))
plot(y ~ x, rbind(testdata, p3))
points(15,5.1,pch=4,cex=2)
abline(lmod)
abline(lmod3,lty=2)










# load the savings data
library(faraway)
data(savings)
?savings

# fit a model with every predictor
mymodel <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = savings)
summary(mymodel)


# calculate leverages, find the ones we should look at
lev <- hatvalues(mymodel)
lev
lev_mean <- mean(lev)
sum(lev > 2 * lev_mean)
lev[lev > 2 * lev_mean]
max(lev)

# calculate the studentized residuals
sresid <- rstudent(mymodel)
sresid
hist(sresid)

# find the outliers
n <- length(sresid)
p <- length(mymodel$coefficients)
df <- n - p - 1
alpha <- 0.05

# without bonferroni
crit <- qt(1 - 0.05/2,df)
sum(abs(sresid) > crit)
sresid[abs(sresid) > crit]
max(abs(sresid))

# with bonferroni
crit <- qt(1 - (0.05/2)/n,df)
sum(abs(sresid) > crit)
sresid[abs(sresid) > crit]
max(abs(sresid))

# compare with/withou bonferroni, and fdr
pvals <- pt(-abs(sresid),df)*2
padjb <- p.adjust(pvals, method = "bonferroni")
padjf <- p.adjust(pvals, method = "fdr")
cbind(pvals,padjb,padjf)


# use a package to find the outliers
library(car)
outlierTest(mymodel)



# calculate cook's distances, find the ones we should look at
cook <- cooks.distance(mymodel)
cook[cook > 4/length(savings$ddpi)]


# fit a model without the observation that has the largest cook's distance
modified <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = savings, subset = cook < max(cook))
summary(modified)

# compare to the original model
coef(mymodel)
coef(modified)
(coef(mymodel) - coef(modified)) / coef(mymodel)




#view all diagnostic plots
plot(mymodel)






# a quick quantile regression example
#install.packages("quantreg")
library(quantreg)
data(engel)
plot(engel$income, engel$foodexp, cex=.7, xlab="Household Income", ylab="Food Expenditure")
lsmod <- lm(foodexp ~ income, data = engel)
qrmod <- rq(foodexp ~ income, data = engel)
lsmod
qrmod
abline(lsmod, col="blue")
abline(qrmod, col="red")




