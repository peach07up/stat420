case1201 <- read.csv("case1201.csv")

pairs(SAT ~ TAKERS + INCOME + YEARS + PUBLIC + EXPEND + RANK, case1201)
pairs(SAT ~ log(TAKERS) + INCOME + YEARS + PUBLIC + EXPEND + RANK, case1201)
case1201 <- subset(case1201, STATE != "Alaska")


library(broom)
fit <- lm(SAT ~ log(TAKERS) + INCOME + YEARS + PUBLIC + EXPEND + RANK, case1201)
#summary(fit)
tidy(fit)
glance(fit)


fit1 <- update(fit, . ~ . - PUBLIC)
tidy(fit1)

fit2 <- update(fit1, . ~ . - INCOME)
tidy(fit2)

anova(fit2,fit)


# backward AIC
fit_back_aic <- step(fit, direction = "backward")
tidy(fit_back_aic)
glance(fit_back_aic)

# forward AIC
fit_start <- lm(SAT ~ 1, case1201)
fit_forw_aic <- step(fit_start, 
                     SAT ~ log(TAKERS) + INCOME + YEARS + PUBLIC + EXPEND + RANK,
                     direction = "forward")
tidy(fit_forw_aic)
glance(fit_forw_aic)

# stepwise AIC
fit_both_aic <- step(fit, direction = "both")
tidy(fit_both_aic)
glance(fit_both_aic)


n <- length(resid(fit))

# backwards BIC
fit_back_bic <- step(fit, direction = "backward", k = log(n))
tidy(fit_back_bic)
glance(fit_back_bic)

# forwards BIC
fit_forw_bic <- step(fit_start, 
                     SAT ~ log(TAKERS) + INCOME + YEARS + PUBLIC + EXPEND + RANK, 
                     direction = "forward", k = log(n))
tidy(fit_forw_bic)
glance(fit_forw_bic)

# stepwise BIC
fit_both_bic <- step(fit, direction = "both", k = log(n))
tidy(fit_both_bic)
glance(fit_both_bic)



library(leaps)
all_fits <- regsubsets(SAT ~ 
                         log(TAKERS) + INCOME + YEARS + PUBLIC + EXPEND + RANK,
                       data = case1201)

all_fits_sum <- summary(all_fits)
all_fits_sum$which


# AIC
p <- length(coef(fit))
n <- length(resid(fit))
AIC <- n*log(all_fits_sum$rss/n) + 2*(2:p)
plot(AIC ~ I(2:p), ylab = "AIC", xlab = "p")
which.min(AIC)

# BIC
BIC <- n*log(all_fits_sum$rss/n) + log(n)*(2:p)
plot(BIC ~ I(2:p), ylab = "BIC", xlab = "p")
which.min(BIC)

# R2adj
R_adj <- all_fits_sum$adjr2
plot(R_adj ~ I(2:p), ylab = "Adjusted R-Squared", xlab = "p")
which.max(R_adj)
R_adj










tricep <- c(19.5,24.7,30.7,29.8,19.1,25.6,31.4,27.9,22.1,25.5,
            31.1,30.4,18.7,19.7,14.6,29.5,27.7,30.2,22.7,25.2)
thigh <- c(43.1,49.8,51.9,54.3,42.2,53.9,58.5,52.1,49.9,53.5,
           56.6,56.7,46.5,44.2,42.7,54.4,55.3,58.6,48.2,51.0)
midarm <- c(29.1,28.2,37.0,31.1,30.9,23.7,27.6,30.6,23.2,24.8,
            30.0,28.3,23.0,28.6,21.3,30.1,25.7,24.6,27.1,27.5)
bodyfat <- c(11.9,22.8,18.7,20.1,12.9,21.7,27.1,25.4,21.3,19.3,
             25.4,27.2,11.7,17.8,12.8,23.9,22.6,25.4,14.8,21.1)
bodyfat <- data.frame(bodyfat, tricep, thigh, midarm)

rbind(
  summary( lm(bodyfat~tricep, data = bodyfat) )$adj.r.squared,
  summary( lm(bodyfat~thigh, data = bodyfat) )$adj.r.squared,
  summary( lm(bodyfat~midarm, data = bodyfat) )$adj.r.squared,
  summary( lm(bodyfat~tricep+thigh, data = bodyfat) )$adj.r.squared,
  summary( lm(bodyfat~tricep+midarm, data = bodyfat) )$adj.r.squared,
  summary( lm(bodyfat~thigh+midarm, data = bodyfat) )$adj.r.squared,
  summary( lm(bodyfat~tricep+thigh+midarm, data = bodyfat) )$adj.r.squared
)

fit <- lm(bodyfat ~ tricep + thigh + midarm, data = bodyfat)
all_fits <- regsubsets(bodyfat ~ tricep + thigh + midarm, data = bodyfat)
all_fits_sum <- summary(all_fits)
all_fits_sum$which
all_fits_sum$adjr2

fit_back <- step(fit, dir="backward")
summary(fit_back)
fit_forw <- step(lm(bodyfat ~ 1, data = bodyfat),
                 bodyfat~tricep+thigh+midarm, dir="forward")
summary(fit_forw)

rbind(
  extractAIC( lm(bodyfat~tricep, data = bodyfat) ),
  extractAIC( lm(bodyfat~thigh, data = bodyfat) ),
  extractAIC( lm(bodyfat~midarm, data = bodyfat) ),
  extractAIC( lm(bodyfat~tricep+thigh, data = bodyfat) ),
  extractAIC( lm(bodyfat~tricep+midarm, data = bodyfat) ),
  extractAIC( lm(bodyfat~thigh+midarm, data = bodyfat) ),
  extractAIC( lm(bodyfat~tricep+thigh+midarm, data = bodyfat) )
)

n <- length(resid(fit))
p <- length(coef(fit))
AIC <- n*log(all_fits_sum$rss/n) + 2*(2:p)
AIC



fit1 <- lm(bodyfat~tricep, data = bodyfat)
fit2 <- lm(bodyfat~thigh, data = bodyfat)
fit3 <- lm(bodyfat~midarm, data = bodyfat)
fit4 <- lm(bodyfat~tricep+thigh, data = bodyfat)
fit5 <- lm(bodyfat~tricep+midarm, data = bodyfat)
fit6 <- lm(bodyfat~thigh+midarm, data = bodyfat)
fit7 <- lm(bodyfat~tricep+thigh+midarm, data = bodyfat)


rbind(
  sum( (resid(fit1) / (1 - hatvalues(fit1)))^2 ),
  sum( (resid(fit2) / (1 - hatvalues(fit2)))^2 ),
  sum( (resid(fit3) / (1 - hatvalues(fit3)))^2 ),
  sum( (resid(fit4) / (1 - hatvalues(fit4)))^2 ),
  sum( (resid(fit5) / (1 - hatvalues(fit5)))^2 ),
  sum( (resid(fit6) / (1 - hatvalues(fit6)))^2 ),
  sum( (resid(fit7) / (1 - hatvalues(fit7)))^2 )
)
