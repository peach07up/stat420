Wheat <- c(5.2,4.5,6.0,6.1,6.7,5.7)
Barley <- c(6.5,8.0,6.1,7.5,5.9,5.6)
Maize <- c(5.8,4.7,6.4,4.9,6.0,5.2)
Oats <- c(8.3,6.1,7.8,7.0,5.6,7.2)	

Grain <- c(rep("Wheat",6),rep("Barley",6),rep("Maize",6),rep("Oats",6))
Thiamin <- c(Wheat,Barley,Maize,Oats)


results <- glm(Thiamin ~ factor(Grain))
summary(results)
summary(aov(results))

par(mfrow=c(2,2))
plot(results)
boxplot(results$residuals ~ Grain)
hist(results$residuals)
 
shapiro.test(results$residuals)

 
######################

maize <- c(rep(0,12),rep(1,6),rep(0,6))
oats <- c(rep(0,18),rep(1,6))
wheat <- c(rep(1,6),rep(0,18))

maize
oats
wheat


results2 <- lm(Thiamin ~ maize + oats + wheat)
summary(results2)

anova(lm(Thiamin ~ 1), results2)

######################


barley <- c(rep(0,6),rep(1,6),rep(0,12))
barley
results3 <- lm(Thiamin ~ maize + oats + wheat + barley)
summary(results3)


######################


Grain2 <- c(rep(1,6),rep(2,6),rep(3,6),rep(4,6))
results4 <- glm(Thiamin ~ factor(Grain2))
summary(aov(results4))
