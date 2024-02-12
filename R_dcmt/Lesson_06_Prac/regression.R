## -------------------------------------------------------------------------------------------------------------------------------------------------
## section 0 ##

# packages needed for this class
name_pkg <- c(
  "Hmisc", "psych", # For describe functions
  "ggplot2", # For ggplot function
  "GGally", #ggpairs, ggduo function
  "MASS", # For Boston data set
  "ISLR", # For Carseats data set
  "effects", # For effect function
  "dplyr", # For select function
  "olsrr", # For variable selection
  "knitr"
  )
bool_nopkg <- !name_pkg %in% rownames(installed.packages())
if (any(bool_nopkg)) {
  install.packages(name_pkg[bool_nopkg], repos = "http://cran.us.r-project.org")
}
# load multiple packages
invisible(lapply(name_pkg, library, character.only = T))
## -------------------------------------------------------------------------------------------------------------------------------------------------
## section 1 ##

adv = read.csv("Advertising.csv", header=T, sep=",")
adv = adv[,-1]
names(adv) = tolower(names(adv)) 
str(adv)
head(adv)
## -------------------------------------------------------------------------------------------------------------------------------------------------
## section 1.1 ##

# summary(adv) 
# Hmisc::describe(adv)
# psych::describe(adv)
# hist(adv)
# pairs(adv) 
# cor(adv)
# ggpairs(adv)
#ggplot(adv, aes(x=sales)) + geom_histogram(bins=10)

attach(adv)
#par("mar")
par(mar=c(1,1,1,1))
par(mfrow=c(1,3))
plot(tv,sales)
plot(radio,sales)
plot(newspaper,sales)
par(mfrow=c(1,1))
detach(adv)
## -------------------------------------------------------------------------------------------------------------------------------------------------
## section 1.2 ##

lm.fit = lm(sales ~ tv, data=adv)
str(lm.fit)
summary(lm.fit)
coef(lm.fit)

lm.fit2 = lm(sales ~ radio, data=adv)
summary(lm.fit2)
## -------------------------------------------------------------------------------------------------------------------------------------------------
## section 1.3 ##

attach(adv)
par(mar=c(3,3,3,3))
plot(tv,sales)
abline(lm.fit,col="red")
detach(adv)
## -------------------------------------------------------------------------------------------------------------------------------------------------
## section 1.4 ##

confint(lm.fit, level=0.90)

attach(adv)
c.pred=predict(lm.fit, level = 0.95, interval="confidence")
p.pred=predict(lm.fit, level = 0.95, interval="prediction")
plot(tv,sales)
abline(lm.fit,col="red", lwd = 2)
o = order(tv, decreasing = F)
lines(tv[o],p.pred[,2][o], lty = "dashed", col="blue", type = "l", lwd = 2)
lines(tv[o],p.pred[,3][o], lty = "dashed", col="blue", lwd = 2)
lines(tv[o],c.pred[,2][o], col="blue", lwd = 2)
lines(tv[o],c.pred[,3][o], col="blue", lwd = 2)
detach(adv)
## -------------------------------------------------------------------------------------------------------------------------------------------------
## section 1.5 ##

predict(lm.fit, data.frame(tv=c(147)), level=0.90, interval="prediction")
predict(lm.fit, data.frame(tv=c(147)), level=0.90, interval="confidence")
predict(lm.fit, data.frame(tv=c(230.1, 44.5, 17.2)), level=0.95, interval="confidence")
predict(lm.fit, data.frame(tv=c(200, 50, 50)), level=0.90, interval="prediction")
predict(lm.fit, data.frame(tv=c(230.1, 44.5, 17.2)), level=0.95, interval="none")
## -------------------------------------------------------------------------------------------------------------------------------------------------
## section 1.6 ##

x = runif(100)*300
y = 7.0 + 0.04*x + rnorm(100, 0, 3.259)

sim.fit = lm(y~x)

plot(x, y, xlim=c(0,300), ylim=c(0,30))
abline(c(7.0, 0.04), col="red")
abline(sim.fit,col="blue")

coef(sim.fit)

plot(1, xlim=c(0,300), ylim=c(0,30), type="n")
betas = numeric()
for(i in 1:100) {
  x = runif(100)*300
  y = 7.0 + 0.04*x + rnorm(100, 0, 3.259)
  sim.fit = lm(y~x)
  abline(sim.fit,col=i)
  betas = rbind(betas, coef(sim.fit))
}
betas = data.frame(betas)
summary(betas)
sapply(betas, sd)
hist(betas)  ###
## -------------------------------------------------------------------------------------------------------------------------------------------------
## section 1.7 ##

attach(adv)
plot(tv, sales);abline(lm.fit, lwd=3, col="red")
detach(adv)
## -------------------------------------------------------------------------------------------------------------------------------------------------
## section 1.9 ##

lm.fit = lm(sales ~ tv + radio + newspaper, data=adv)
summary(lm.fit)
## -------------------------------------------------------------------------------------------------------------------------------------------------
## section 2 ##

data(Boston)
boston = Boston %>% 
         select(crim, chas, rm, age, tax, black, lstat, medv) %>% 
         mutate(chas = as.factor(chas))
head(boston)

lm.fit = lm(medv ~ rm + tax + lstat, data = boston)

lm.fit_1 = lm(medv ~ rm + tax, data = boston)
resid_1 = lm.fit_1$residuals

lm.fit_2 = lm(lstat ~ rm + tax, data = boston)
resid_2 = lm.fit_2$residuals

data_resid = data.frame(res1 = resid_1, res2 = resid_2)
lm.fit_3 = lm(resid_1 ~ 0 + resid_2, data = data_resid)

coeff_lstat_lm.fit = as.vector(lm.fit$coefficients[4])
coeff_lstat_lm.fit_3 = as.vector(lm.fit_3$coefficients)
all.equal(coeff_lstat_lm.fit, coeff_lstat_lm.fit_3)
## -------------------------------------------------------------------------------------------------------------------------------------------------
## section 3 ##
## section 3.1 ##

credit = read.csv("Credit.csv", header=T, sep=",")
credit = credit[,-1]
names(credit) = tolower(names(credit))
head(credit)
## -------------------------------------------------------------------------------------------------------------------------------------------------
## section 3.2 ##

attach(credit)
library(Hmisc)
summary(credit)
Hmisc::describe(credit)
psych::describe(credit)  
sapply(credit[,-(7:10)], sd)
hist(credit)
#pairs(credit)  # 자료형이 수치형인 변수에 대해서만 실행가능
cor(credit[,-(7:10)])
ggpairs(credit)
#그림 확대하여 살펴보기 
ggduo(credit, columnsX = 7:8, columnsY = 1:3) 
ggduo(credit, columnsX = 7:8, columnsY = 11:11) 
#연속형과 이산형의 연관관계 
detach(credit)
## -------------------------------------------------------------------------------------------------------------------------------------------------
## section 3.3 ##

attach(credit)
lm.fit1 = lm(balance ~ gender, data = credit)
summary(lm.fit1)
lm.fit2 = lm(balance ~ ethnicity, data = credit)
summary(lm.fit2)

fethnicity=as.factor(ethnicity)
str(fethnicity)
detach(credit)
## -------------------------------------------------------------------------------------------------------------------------------------------------
## section 3.4 ##

lm.fit3= lm(balance ~., data = credit)
summary(lm.fit3)
## -------------------------------------------------------------------------------------------------------------------------------------------------
## section 4 ##
## section 4.1 ##

# lm.fit = lm(sales ~ tv*radio, data=adv)
lm.fit = lm(sales ~ tv+radio+tv:radio, data=adv)
summary(lm.fit)
## -------------------------------------------------------------------------------------------------------------------------------------------------
## section 4.2 ##
data(Carseats)
head(Carseats)
## -------------------------------------------------------------------------------------------------------------------------------------------------
## section 4.3 ##
Carseats1= Carseats %>% select(Sales, Urban, US)
head(Carseats1)

fit1 = lm(Sales ~ US * Urban, data = Carseats1)
summary(fit1)

coeff <- fit1$coefficients
x = c(0,1)

Urban_No = matrix(0,2,1)
Urban_No[1,1] <- t(coeff)%*%c(1,0,0,0) # beta_0
Urban_No[2,1] <- t(coeff)%*%c(1,1,0,0) # beta_0 + beta_1

Urban_Yes <- matrix(0,2,1)
Urban_Yes[1,1] <- t(coeff)%*%c(1,0,1,0) # beta_0 + beta_2
Urban_Yes[2,1] <- t(coeff)%*%c(1,1,1,1) # beta_0 + beta_1 + beta_2 + beta_3

plot(x, Urban_No, type = "b", col = "red", lwd = 2, 
     xlab = "US", ylab = "Sales", main = "US*Urban effect plot", xaxt = "n")
axis(side = 1, at = c(0,1), labels = c("No", "Yes"))
lines(x, Urban_Yes, type = "b", col = "blue", lwd = 2)
legend("topleft", legend = c("Urban : No", "Urban : Yes"), col = c("red", "blue"), lwd = 2)

a = effect(term = "US*Urban", mod  = fit1)
plot(a, multiline =  TRUE)
## -------------------------------------------------------------------------------------------------------------------------------------------------
## section 4.4 ##
Carseats2 = Carseats %>% select(Sales, Price, Urban)
head(Carseats2)

fit1 = lm(Sales ~ Price * Urban, data = Carseats2)
summary(fit1)

coeff = fit1$coefficients
x = seq(min(Carseats2$Price), max(Carseats2$Price), length = 10) # grid points
# beta_0 + beta_1 * Price
Urban_No = Vectorize(function(x) t(coeff) %*% c(1, x, 0, 0))
# (beta_0 + beta_2) + (beta_1 + beta_3) * Price
Urban_Yes = Vectorize(function(x) t(coeff) %*% c(1, x, 1, x)) 

plot(x, Urban_No(x), type = "l", col = "red", lwd = 2, 
     xlab = "Price", ylab = "Sales", main = "Price*Urban effect plot")
lines(x, Urban_Yes(x), type = "l", col = "blue", lwd = 2)
legend("topright", legend = c("Urban : No", "Urban : Yes"), col = c("red", "blue"), lwd = 2)

a = effect(term = "Price*Urban", mod = fit1)
plot(a, multiline = TRUE)
## -------------------------------------------------------------------------------------------------------------------------------------------------
## section 5 ##

data(Boston)
boston = Boston %>% 
         select(crim, chas, rm, age, tax, black, lstat, medv) %>% 
         mutate(chas = as.factor(chas))
head(Boston)
## -------------------------------------------------------------------------------------------------------------------------------------------------
## section 6 ##

hitters.dat = read.csv("Hitters.csv") %>% na.omit() %>% select(AtBat:CWalks, PutOuts:Salary)
head(hitters.dat)

lm.fit = lm(Salary ~., data = hitters.dat)
summary(lm.fit)
# based on p-value
ols_step_forward_p(lm.fit, penter = 0.05, progress = T, details = F)
ols_step_backward_p(lm.fit, prem = 0.05, progress = T, details = F)
# based on AIC
ols_step_forward_aic(lm.fit, progress = T, details = F)
ols_step_backward_aic(lm.fit, progress = T, details = F)


