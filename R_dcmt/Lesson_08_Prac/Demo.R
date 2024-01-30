install.packages(c("caret","dplyr","GGally","pROC"))
library(MASS)
library(caret)  
library(dplyr)  
library(ggplot2)
library(GGally)  
library(pROC)

# 필요한 패키지와 스크립트 다운 받기 --------------------------------------
source("decisionplot.R")


# 타이타닉 자료 -----------------------------------------------------------
# https://www.kaggle.com/c/titanic
# 타이타닉 승객들의 생존여부 예측
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


# 대학원 입학 자료 -------------------------------------------------------
# https://www.kaggle.com/mohansacharya/graduate-admissions

rawdata <- read.csv("Admission_Predict.csv")
data <- rawdata %>% mutate(Class = ifelse(Chance.of.Admit > 0.7, "Admit", "No")) %>% 
  dplyr::select(TOEFL.Score,CGPA,Class)
data$Class <- as.factor(data$Class)

library(GGally) # 쌍별 산점도를 그리기 위해  
ggpairs(data, mapping = aes(color = Class))

data %>% ggplot(aes(x = TOEFL.Score, y= CGPA, color = Class)) + geom_point()



# Section 2 ---------------------------------------------------------------
 

# knn ---------------------------------------------------------------------
library(caret) 
# Class 변수가 범주형 변수인지 확인할 것
knn_out <- knn3(Class ~ ., data = data, k = 5) 
decisionplot(model = knn_out, data = data, class = "Class", main = "kNN")
knn_predictions <- predict(knn_out, data, type = "class")

# 정규화를 한 이후에 
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

# 로지스틱 회귀분석 -------------------------------------------------------
logi_out <- glm(Class ~ ., "binomial", data = data) 
summary(logi_out)
decisionplot2(model = logi_out, data = data, class = "Class", main = "Logistic Regression") 


# Section 3 ---------------------------------------------------------------
cm <- confusionMatrix(lda_predictions$class,data$Class)
cm

library(pROC)
roc.out <- roc(response = data$Class, predictor = lda_predictions$posterior[,1])
plot.roc(roc.out, print.auc = TRUE)


## positive 범주와 negative 범주를 바꿈
head(data$Class) # 변수의 첫 번째 범주를 positivskime 범주로 취급

# "No" 범주를 positive으로 바꿈: 
data.s <- data
data.s$Class <- relevel(data.s$Class, "No")

lda_out2 <- lda(Class ~ ., data = data.s)
lda_predictions2 <- predict(lda_out2,data.s)
cm2 <- confusionMatrix(lda_predictions2$class,data.s$Class)
cm2
 
roc.out <- roc(response = data.s$Class, predictor = lda_predictions2$posterior[,1])
plot.roc(roc.out, print.auc = TRUE)




# 연습을 들어가기 전에 -----------------------------------------------------

# 다음의 패키지들 중 설치되지 않은 것들을 설치하자
# 
# library(skimr) # 데이터 요약
# library(dplyr) # 데이터 전처리
# library(corrplot) # 상관계수 행렬
# library(GGally) # 쌍별 산점도  
# library(broom) # t.test의 결과를 깔끔하게 
# library(moments) # 왜도와 첨도 계산 
# library(caret)  # 복잡한 회귀와 분류 문제에 대한 모형 훈련 (classification and regression training)





 


