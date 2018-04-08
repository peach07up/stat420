### SKIN EXAMPLE ###

# skin.csv contains the average annual mortality due to malignant melanoma for
# white males during 1950 to 1959 per 10 mil, for each state and the District of 
# Columbia (Alaska, Hawaii, New Hampshire, and South Carolina are excluded ),
# the latitude at the centroid of the state, and whether the state borders an ocean.  
# (Fisher and Van Belle (1993). Biostatistics: A methodology for the health sciences.)


plot(skin$latitude, skin$mortality, type = "n")
text(skin$latitude, skin$mortality, skin$state, col = skin$ocean+1)

fit <- lm(mortality ~ latitude + ocean, data = skin)
summary(fit)

abline(fit$coeff[1],fit$coeff[2],col=1,lty=1)
abline(fit$coeff[1]+fit$coeff[3],fit$coeff[2],col=2,lty=2)

fit2 <- lm(mortality ~ latitude + ocean + latitude:ocean, data = skin)
#fit2 <- lm(mortality ~ latitude*ocean, data = skin)
summary(fit2)

anova(fit,fit2)




### COOKIES EXAMPLE ###


# A company wishes to study the effects of three different types of promotion on
# sales of its cookies.  The three promotions were:
#   
# Treatment 1: Sampling of product by customers in store and regular shelf space
# Treatment 2: Special display shelves at ends of aisle in addition to regular shelf space
# Treatment 3: Additional shelf space in regular location
# 
# Fifteen stores were selected as the experimental units. Each store was randomly
# assigned one of the promotion types, with five stores assigned to each type of 
# promotion. Other relevant conditions under the control of the company, such as 
# price and advertising, were kept the same for all stores in the experiment.  
# Data on the number of cases of the product sold during the promotional period, 
# denoted by Y, are presented in the table below, as are also data on the sales 
# of the product in the preceding period, denoted by X. Sales of the preceding 
# period are to be used as the covariate variable.





plot(cookies$previous, cookies$promo, 
     col = cookies$treat1 + 2*cookies$treat2 + 3*cookies$treat3,
     pch = cookies$treat1 + 2*cookies$treat2 + 3*cookies$treat3)


fit <- lm(promo ~ previous + treat2 + treat3, data = cookies)
summary(fit)

fit0 <- lm(promo ~ previous, data = cookies)
anova(fit0,fit)

fit2 <- lm(promo ~ previous + treat1 + treat2 + treat3 + 0, data = cookies)
summary(fit2)
anova(fit0,fit2)

plot(cookies$previous, cookies$promo, 
     col = cookies$treat1 + 2*cookies$treat2 + 3*cookies$treat3,
     pch = cookies$treat1 + 2*cookies$treat2 + 3*cookies$treat3)

abline(fit$coeff[1],fit$coeff[2],col=1,lty=1)
abline(fit$coeff[1]+fit$coeff[3],fit$coeff[2],col=2,lty=2)
abline(fit$coeff[1]+fit$coeff[4],fit$coeff[2],col=3,lty=3)

fit_bad <- lm(promo ~ previous + treat1 + treat2 + treat3, data = cookies)
summary(fit_bad)

fit3 <- lm(promo ~ previous + treat2 + treat3 + previous:treat2 
           + previous:treat3, data = cookies)
summary(fit3)
anova(fit,fit3)

fit4 <- lm(promo ~ treat1 + treat2 + treat3 + previous:treat1 
           + previous:treat2 + previous:treat3 + 0, data = cookies)
summary(fit4)
anova(fit2,fit4)


fit_factor <- lm(promo ~ previous + treatment, data = cookies)
summary(fit_factor)
