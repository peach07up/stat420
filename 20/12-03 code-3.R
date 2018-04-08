install.packages("ISLR")
install.packages("ggplot2")
install.packages("pROC")
install.packages("boot")
install.packages("broom")
library(ISLR)
library(ggplot2)
library(pROC)
library(boot)
library(broom)

# using simple linear regression
with(credit, plot(spend,premium))
fit_lm <- lm(premium ~ spend, data = credit)
abline(fit_lm)
summary(fit_lm)
plot(fit_lm)

# fitting logistic regression
fit_lr <- glm(premium ~ spend, data = credit, family = binomial)
#fit <- glm(premium ~ spend, data = credit, family = binomial("logit"))
summary(fit_lr)
glance(fit_lr)

# plot both linear and logistic regression using ggplot2
ggplot(credit, aes(x=spend, y=premium)) + geom_point() + 
  stat_smooth(method="glm", family="binomial", se=FALSE) + 
  stat_smooth(method="lm", family="binomial", se=FALSE, color = "red")

# fitting a larger logistic regression
fit <- glm(premium ~ spend + addcard, data = credit, family = binomial)
summary(fit)
glance(fit)

# three different ways to get the predicted probabilities for each observation
fit$fitted.values
fitted(fit)
predict(fit, type = "response")

# making predictions about specific new observations
predict(fit, data.frame(spend = 36, addcard = 1), type = "response")
predict(fit, data.frame(spend = 36, addcard = 0), type = "response")
predict(fit, data.frame(spend = 50, addcard = 1), type = "response")

# checking goodness of fit of the model
with(fit, pchisq(deviance, df.residual, lower.tail = FALSE))

# comparing models using deviance
with(fit, null.deviance - deviance)
with(fit, df.null - df.residual)
with(fit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

# use a predicted probability of 0.5 as a cutoff for predicting whether or not
# someone will get a premium card. compare these results to the actual results
# using a confustion matrix
predicted <- predict(fit, type = "response") > 0.5
actual <- credit$premium
table(predicted, actual)

# relabeling TRUE/FALSE to 1/0
predicted <- (predict(fit, type = "response") > 0.5) * 1
table(predicted, actual)

# proportion of correct / incorrect predictions
mean(actual == predicted)
mean(actual != predicted)

# plotting the ROC curve
credit$prob <- predict(fit, type = "response")
g <- roc(premium ~ prob, data = credit)
plot(g)    



set.seed(5432)
# split the data into training and testing (validation) data
train_index <- sample(nrow(credit),trunc(nrow(credit)/2))
train_data <- credit[train_index,]
test_data <- credit[-train_index,]

# train a model using only the training data 
train_mod <- glm(premium ~ spend + addcard, data = train_data, family = binomial)


# confusion matrix for the training data
train_pred <- (predict(train_mod, train_data, type = "response") > 0.5) * 1
table(train_pred, train_data$premium)
mean(train_data$premium == train_pred)
mean(train_data$premium != train_pred)

# confusion matrix for the testing data, predictions are made using the model
# fit using the training data on the observations from the test data
test_pred <- (predict(train_mod, test_data, type = "response") > 0.5) * 1
table(test_pred, test_data$premium)
mean(test_data$premium == test_pred)
mean(test_data$premium != test_pred)

# cross-validated test error
cv.glm(credit, fit, K = 2)$delta[1]
