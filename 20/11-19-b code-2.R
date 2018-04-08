Wheat <- c(5.2,4.5,6.0,6.1,6.7,5.7)
Barley <- c(6.5,8.0,6.1,7.5,5.9,5.6)
Maize <- c(5.8,4.7,6.4,4.9,6.0,5.2)
Oats <- c(8.3,6.1,7.8,7.0,5.6,7.2)

Grain <- c(rep("Wheat",6), rep("Barley",6), rep("Maize",6), rep("Oats",6))
Thiamin <- c(Wheat, Barley, Maize, Oats)

Cereal <- data.frame(Grain, Thiamin)
is.factor(Cereal$Grain)

fit <- glm(Thiamin ~ Grain, data = Cereal)
summary(fit)
summary(aov(fit))

par(mfrow=c(2,2))
plot(fit)

dev.off()

par(mfrow=c(1,2))
boxplot(resid(fit) ~ Grain, data = Cereal)
hist(resid(fit))

dev.off()

library(ggplot2)
p <- ggplot(Cereal, aes(Grain, Thiamin))
p + geom_boxplot(aes(fill = Grain))

library(broom)
head(augment(fit))


m <- ggplot(augment(fit), aes(x=.resid))
m + geom_histogram(colour = "darkgreen", fill = "white", binwidth = 0.5)

shapiro.test(resid(fit))


wheat <- c(rep(1,6),rep(0,18))
barley <- c(rep(0,6),rep(1,6),rep(0,12))
maize <- c(rep(0,12),rep(1,6),rep(0,6))
oats <- c(rep(0,18),rep(1,6))

thiamin <- Thiamin

cereal <- data.frame(wheat, barley, maize, oats, thiamin)
cereal

fit2 <- lm(thiamin ~ maize + oats + wheat, data = cereal)
summary(fit2)


anova(lm(thiamin ~ 1, data = cereal), fit2)

fit3 <- lm(thiamin ~ maize + oats + wheat + barley, data = cereal)
summary(fit3)


fit4 <- lm(thiamin ~ maize + oats + wheat + barley + 0, data = cereal)
summary(fit4)


grain <- c(rep(1,6),rep(2,6),rep(3,6),rep(4,6))
cereal2 <- data.frame(grain,thiamin); cereal2

lm(thiamin ~ grain, data = cereal2)
lm(thiamin ~ factor(grain), data = cereal2)

is.factor(grain)

