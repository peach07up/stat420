salary <- c(26075,79370,65726,41983,62308,41154,53610,33697,22444,32562,43076,
            56000,58667,22210,20521,49727,33233,43628,16105,65644,63022,47780,
            38853,66537,67447,64785,61581,70678,51301,39346,24833,65929,41721,
            82641,99139,52624,50594,53272,65343,46216,54288,20844,32586,71235,
            36530,52745,67282,80931,32303,38371)
years <- c(7,28,23,18,19,15,24,13, 2, 8,20,21,18, 7, 2,18,11,21, 4,24,20,20,15,
           25,25,28,26,27,20,18, 1,26,20,26,28,23,17,25,26,19,16, 3,12,23,20,
           19,27,25,12,11)
initech <- data.frame(years, salary)


initech_fit <- lm(salary ~ years, data = initech)
plot(initech$years, initech$salary)
abline(initech_fit)
plot(fitted(initech_fit), resid(initech_fit))
abline(h = 0, lty = 2)
#plot(initech_fit)
summary(initech_fit)

initech_fit_log <- lm(log(salary) ~ years, data = initech)
plot(initech$years, log(initech$salary))
abline(initech_fit_log)
plot(initech$years, initech$salary)
curve(exp(initech_fit_log$coef[1] + initech_fit_log$coef[2] * x), 0, 30, add = T)
plot(fitted(initech_fit_log), resid(initech_fit_log))
abline(h = 0, lty = 2)
#plot(initech_fit)
summary(initech_fit_log)


library(MASS)
library(faraway)

data(savings)
savings_model <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = savings)

boxcox(savings_model, plotit = TRUE)
boxcox(savings_model, plotit = TRUE, lambda = seq(0.5,1.5,by = 0.1))
#plot(savings_model)
plot(fitted(savings_model), resid(savings_model))
abline(h = 0, lty = 2)
summary(savings_model)


data(gala)
gala_model <- lm(Species ~ Area + Elevation + Nearest + Scruz +  Adjacent, data = gala)
plot(fitted(gala_model), resid(gala_model))
abline(h = 0, lty = 2)
summary(gala_model)

boxcox(gala_model, lambda=seq(-0.25,0.75,by=0.05),plotit=T)
gala_model_cox <- lm(((Species^0.3)-1)/0.3 ~ Area + Elevation + Nearest + Scruz +  Adjacent, data = gala)
plot(fitted(gala_model_cox), resid(gala_model_cox))
abline(h = 0, lty = 2)
summary(gala_model_cox)


boxcox(initech_fit)
