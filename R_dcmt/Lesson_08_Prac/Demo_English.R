library(MASS)
library(caret)  
library(dplyr)  
library(ggplot2)
library(GGally)  
library(pROC)
 
# Load required packages and supporting scripts ---------------------------
source("decisionplot.R")


# Titanic data expanded ---------------------------------------------------
# https://www.kaggle.com/c/titanic
# Predict whether Titanic passengers' survival 
titan <-read.csv("train.csv")
View(titan)
library(dplyr)  
titan <- titan %>% 
  dplyr::select(Sex, Pclass, Survived) %>% 
  mutate(Survived = as.factor(ifelse(Survived == 1,"Yes","No")))

report.tab <- xtabs( ~  Sex + Pclass + Survived, data = titan)
mosaicplot(report.tab)

library(ggplot2)
titan  %>% ggplot(aes(x = Sex)) + geom_bar(aes(fill = Survived), position = "fill") + facet_wrap(~Pclass)
titan  %>% ggplot(aes(x = Sex)) + geom_bar(aes(fill = Survived), position = "fill")  
titan  %>% ggplot(aes(x = Pclass)) + geom_bar(aes(fill = Survived), position = "fill") 


titan %>% group_by(Sex, Pclass) %>% 
  summarise(P_survive = mean ( Survived == "Yes"), P_demise= mean ( Survived == "No")) 


# Graduate admissions data  --------------------------------------------------------------
# https://www.kaggle.com/mohansacharya/graduate-admissions

rawdata <- read.csv("Admission_Predict.csv")
data <- rawdata %>% mutate(Class = ifelse(Chance.of.Admit > 0.7, "Admit", "No")) %>% 
  dplyr::select(TOEFL.Score,CGPA,Class)
data$Class <- as.factor(data$Class)

library(GGally) # for pairwise scatterplot  
ggpairs(data, mapping = aes(color = Class))

data %>% ggplot(aes(x = TOEFL.Score, y= CGPA, color = Class)) + geom_point()



# Section 2 ---------------------------------------------------------------
 

# knn ---------------------------------------------------------------------
library(caret) 
# Make sure Class variable is a factor
knn_out <- knn3(Class ~ ., data = data, k = 5) 
decisionplot(model = knn_out, data = data, class = "Class", main = "kNN")
knn_predictions <- predict(knn_out, data, type = "class")

# After normalization 
data2 <- data %>% mutate(TOEFL.Score = c(scale(TOEFL.Score)), 
                         CGPA = c(scale(CGPA)))
out <- knn3(Class ~ ., data = data2, k = 5) 
decisionplot(model = out, data = data2, class = "Class", main = "kNN")




# LDA ---------------------------------------------------------------------

lda_out <- lda(Class ~ ., data = data)
decisionplot(model = lda_out, data = data, class = "Class", main = "LDA")
lda_predictions <- predict(lda_out,data)
 
# QDA 
qda_out <- qda(Class ~ ., data = data)
decisionplot(model = qda_out, data = data, class = "Class", main = "QDA")
qda_predictions <- predict(qda_out,data)

# Logistic regression -----------------------------------------------------
logi_out <- glm(Class ~ ., "binomial", data = data) 
summary(logi_out)
decisionplot2(model = logi_out, data = data, class = "Class", main = "Logistic Regression") 


# Section 3 ---------------------------------------------------------------
cm <- confusionMatrix(lda_predictions$class,data$Class)
cm

library(pROC)
roc.out <- roc(response = data$Class, predictor = lda_predictions$posterior[,1])
plot.roc(roc.out, print.auc = TRUE)


## Switch positive and negative class 
head(data$Class) # First level of the factor is treated positive 

# To make "No" class as positive: 
data.s <- data
data.s$Class <- relevel(data.s$Class, "No")

lda_out2 <- lda(Class ~ ., data = data.s)
lda_predictions2 <- predict(lda_out2,data.s)
cm2 <- confusionMatrix(lda_predictions2$class,data.s$Class)
cm2
 
roc.out <- roc(response = data.s$Class, predictor = lda_predictions2$posterior[,1])
plot.roc(roc.out, print.auc = TRUE)




# For Practice ------------------------------------------------------------

# install the following packages, if not already installed 
# 
# library(skimr) # for skim 
# library(dplyr) # for data manipulation
# library(corrplot) # for correlation plot 
# library(GGally) # for pairwise scatterplot  
# library(broom) # for cleaning up t.test output
# library(moments) # to compute skewness and kurtosis 
# library(caret)  # for Classification (And REgression) Training 



 


