## 실습 1.8

# read the Advertisign data
adv.dat <- read.csv("Advertising.csv")
head(adv.dat)

# fitting the linear regression
lm.fit <- lm(Sales ~ Radio, data = adv.dat)
summary(lm.fit)

# scatter plot, fitted line, 95% confidence interval, 95% prediction interval
attach(adv.dat)
pred <- predict(lm.fit, level = 0.95, interval = "prediction")
conf <- predict(lm.fit, level = 0.95, interval = "confidence")
o <- order(Radio, decreasing = F) 

plot(Radio, Sales)
abline(lm.fit, col = "red", lwd = 2)
lines(Radio[o], pred[,2][o], col = "blue", lwd = 2, lty = "dashed")
lines(Radio[o], pred[,3][o], col = "blue", lwd = 2, lty = "dashed")
lines(Radio[o], conf[,2][o], col = "blue", lwd = 2, lty = "dashed")
lines(Radio[o], conf[,3][o], col = "blue", lwd = 2, lty = "dashed")
detach(adv.dat)
# -------------------------------------------------------------------------
## 실습 5

# read the Boston data
library(MASS) # for Boston data
library(effects) # for effect function
library(dplyr) 
Boston.dat <- Boston %>% 
              select(crim, chas, rm, age, tax, black, lstat, medv) %>% 
              mutate(chas = as.factor(chas))
head(Boston.dat)
# fitting the linear regression
# (1) model without any interaction terms
lm.fit <- lm(medv ~ ., data = Boston.dat)
summary(lm.fit)
# (2)-1 model with interaction term : continuous * categorical
lm.fit <- lm(medv ~ chas*lstat, data = Boston.dat)
summary(lm.fit)

a <- effect(term = "chas*lstat", mod = lm.fit)
plot(a, multiline = T)
# (2)-2 model with interaction term : continuous * continuous
lm.fit <- lm(medv ~ tax*lstat, data = Boston.dat)
summary(lm.fit)


