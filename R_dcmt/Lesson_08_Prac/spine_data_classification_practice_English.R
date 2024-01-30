# Predicting lower back pain


# load libraries ----------------------------------------------------------
library(MASS)
library(car) 

library(skimr) # for skim 
library(dplyr) # for data manipulation
library(corrplot) # for correlation plot 
library(GGally) # for pairwise scatterplot  
library(broom) # for cleaning up t.test output
library(moments) # to compute skewness and kurtosis 
library(caret)  # for Classification (And REgression) Training 

# Load data ---------------------------------------------------------------


# Data obtained from 
# https://www.kaggle.com/sammy123/lower-back-pain-symptoms-dataset
data <- read.csv("Dataset_spine.csv")


# Exploratory Data Analysis -----------------------------------------------

# It is ideal to inspect each variable when the number of variables is not so large
# Take a look at the data 
summary(data)

# An alternative to summary() is skim() in skimr package
library(skimr) 
skim(data)

# Varible "X" is strange... inspect further, then you will find it consisting of a codebook 
# We can use the codebook to make the variable names meaningful.

# Next few lines of code renames the variable names, then remove the "X" variable. 
library(dplyr)  
data <- data %>% 
  dplyr::rename('pelvic_incidence' = Col1, 
                'pelvic_tilt' = Col2, 
                'lumbar_lordosis_angle' = Col3,
                'sacral_slope' = Col4, 
                'pelvic_radius' = Col5, 
                'degree_spondylolisthesis' = Col6, 
                'pelvic_slope' = Col7, 
                'direct_tilt' = Col8, 
                'thoracic_slope' = Col9, 
                'cervical_tilt' = Col10, 
                'sacrum_angle' = Col11, 
                'scoliosis_slope' = Col12) %>%
  dplyr::select(-X)

head(data)

# We will use the first 12 variables to predict Class_att 

# Since there are only 12 predictors (not so large), it makes sense to inspect their association structure, 
library(corrplot)  
corrplot(cor(data[,1:12])) 
# First six variables seem to be correlated heavily. In fact, the variable "scaral_slope" is perfectly correlated with a combination of the first two variables. 
# sacral_slope =  pelvic_incidence - pelvic_tilt,
# confirmed by the error-zero regression result:
summary(lm(sacral_slope ~ pelvic_incidence + pelvic_tilt, data = data))

# for some analysis, it makes sense to remove "sacral_slope"
# data <- data %>% dplyr::select(-sacral_slope)
# however we will postpone this, until later. 

# With not so large number of predictors, we can look at the scatterplot.
library(GGally) 
ggpairs(data, mapping = aes(color = Class_att))


# Variable transformation  ------------------------------------------------
# Sometimes it makes sense to create another variable from existing ones to boost the performance of statistical learning
# In data science community, this is sometimes called "feature engineering". 
# In particular, we have seen that the variable "degree_spondylolisthesis" (at the sixth column) more or less discriminates lower back pain.
# Let us take a closer look. 
# (ggpairs is not supposed to be used for one variable. We here use it to compare two univariate distributions.)

summary(data[,6]) 
ggpairs(data, mapping = aes(color = Class_att), columns = 6) 

# Data are piled near 0; there's an outlier. Try transforming. 
# The variable x is replaced by log10(x - 2min(x))
min_d_s <- min(data$degree_spondylolisthesis) * 2
tdata <- data %>% mutate( degree_spondylolisthesis = 
                  log10( degree_spondylolisthesis - min_d_s ) )
ggpairs(tdata, mapping = aes(color = Class_att), columns = 6) 

# In the above, log10(x - 2min(x)) is one of many reasonable transformations. Try more! 
# For some methods including knn, the range of data impacts the quality.
# One simple solution of disregarding the range is to scale it to have zero mean and one standard deviation

tdata.num.id <- unlist(lapply(tdata, is.numeric))
tdata[,tdata.num.id] <- tdata[,tdata.num.id] %>% scale()

# These two transformations can be made for a new entry in the database, which is important for the task of classification,
# since your goal is to classify *new* observations into either "Abnormal" or "Normal". 

# Take a look at it again
ggpairs(tdata, mapping = aes(color = Class_att))

# There are some variables with outliers such as "scaral_slope". You may try more variable transformations. 
# By this time, you probably have noticed that variables 7-12 do not appear to have any discriminating power. 
# Let us make a note of it, and move on. 


# EDA and variable transformation for many, many variables ----------------
# When there are high tens or hundreds (or even thousands) of variables, the above exploratory procedure does not make sense. 
# It still makes sense to "look" at the data variable by variable, but not by the graphical form. 
# One way is to inspect (a) t-statistics and (b) skewness/kurtosis of each variable, among others. 
# The next code does it. 
library(broom)  
library(moments)  
t_stats <- lapply(dplyr::select_if(tdata, is.numeric), function (x) broom::tidy(t.test(x ~ tdata$Class_att) ) ) 
t_stats <- bind_rows(t_stats, .id = "variable")
skew <- sapply(dplyr::select_if(tdata, is.numeric), moments::skewness)
kurt <- sapply(dplyr::select_if(tdata, is.numeric), moments::kurtosis)
t_stats %>% 
  dplyr::select(variable, statistic, p.value) %>% 
  bind_cols(list(skewness = skew,kurtosis = kurt ))

# Split train and testing data --------------------------------------------
N <- nrow(tdata)

# It is always a good idea to set the random seed.
set.seed(1)
# We will take 75% of the data for training. The remaining 25% will be set aside for testing.
train.id <- sample(1:N, floor(N*.75), replace = F)
trdata <- tdata[train.id,]
test <- tdata[-train.id,] 


# Train k-NN classifier ---------------------------------------------------

# To train k-NN classifier, we will use caret::knn3 
# As a starter, let us use k = 5
library(caret)   
out <- knn3(Class_att ~ ., data = trdata, k = 5)
out
# Inspect the in-sample classification performance
# Below, class_pred is the predicted class, or simply classified labels.
# prob_pred is the result of voting of the 5 neighborsfor Abnormal and Normal (in proportions)
(class_pred <- predict(out, trdata, type = "class"))
(prob_pred <- predict(out, trdata, type = "prob"))

# confusionMatrix produces the confusion matrix, and various statistics obtained from it.
confusionMatrix(class_pred,trdata$Class_att)
# Focus on 
#  1) Accuracy 
#  2) Sensitivity
#  3) Specificity
#  4) If the class sizes are highly unbalanced, then balanced accuracy
# For this classification, Accuracy, Sensitivity and  Specificity are similar

# More importantly, inspect the out of sample classification performance 
confusionMatrix(predict(out, test, type = "class"),
                test$Class_att)
# See that out-of-sample accuracy is slightly decreased from the in-sample accuracy (this is common)
# We have set k = 5 for no obvious reason. We should try other choices. 
# Assume we are only accessible to the train data. We should then use cross-validation

# The package caret is well-suited for this purpose (for light users). 
# We will use the train function to repeatedly fit knn for various values of K (3 - 71), for various folds of data (10 folds by default). 

set.seed(2)
train_knn <- train(Class_att ~ ., method = "knn", 
                   data = trdata,
                   trControl = trainControl(method = "cv"),
                   tuneGrid = data.frame(k = seq(3, 71, 2)))
ggplot(train_knn, highlight = TRUE)
# Using 19 neighbors provides the best accuracy. Choose K = 19.
train_knn$bestTune

# Evaluate the performance of the tuned models 
confusionMatrix(predict(train_knn, trdata), 
                trdata$Class_att)
confusionMatrix(predict(train_knn, test),
                test$Class_att)

# We can of course re-create knn object with the chosen k (= 19). The result is just the same
out19 <- knn3(Class_att ~ ., data = trdata, k = 19)
confusionMatrix(predict(out19, test, type = "class"),
                test$Class_att) 

# Train linear discriminant analysis -------------------------------------- 

# Use MASS::lda() function 
out <- lda(Class_att ~  ., data = trdata) 
# Let us ignore the warning for now 

out

# Perform in-sample and out-of-sample predictions 
# Below, each output is a list consisting of 1) classification results,
# 2) Posterior probability (assuming normal distributions)
# 3) Projected scores onto the FLD direction vector
pred.train <- predict(out, trdata)
pred.test <- predict(out, test)

confusionMatrix(pred.train$class, trdata$Class_att)
confusionMatrix(pred.test$class, test$Class_att)
# The performance is not bad indeed. 

# ROC (Receiver-Operating Curve) and AUC (Area Under the Curve) shows the tradeoff between sensitiviy and specificity
library(pROC)
roc.out <- roc(response = test$Class_att, predictor = pred.test$posterior[,1])
plot.roc(roc.out,  
         print.auc = TRUE,
         main = "ROC and AUC: Binary LDA - Test data")

# We can manually choose to use a selection of variables. 
# For example, as we suspected that variables in columns #7-#12 do not contribute much, let us use variables in columns 1-6.
out16 <- lda(Class_att ~  ., data = trdata[,c(1:6,13)])
pred.test <- predict(out16, test)
confusionMatrix(pred.test$class, test$Class_att)


# Furthermore, the variable "sacral_slope" was collinear with two pelvic_* variables. Remove it. (No difference)
out17 <- lda(Class_att ~  ., data = trdata[,c(1:3,5,6,13)])
pred.test <- predict(out17, test)
confusionMatrix(pred.test$class, test$Class_att)
  
 


# Train quadratic discriminant analysis -----------------------------------
 

# Use MASS::qda() function 
out <- qda(Class_att ~  ., data = trdata) 
# The collinearty causes an error. We should explicitly deal with it. 
trdata.q <- trdata %>% select(-sacral_slope)
out <- qda(Class_att ~  ., data = trdata.q)  

# Prediction and confusion matrix
pred.test <- predict(out, test) 
confusionMatrix(pred.test$class, test$Class_att) 

# ROC (Receiver-Operating Curve) and AUC (Area Under the Curve)  
roc.out <- roc(response = test$Class_att, predictor = pred.test$posterior[,1])
plot.roc(roc.out,  
         print.auc = TRUE,
         main = "ROC and AUC: Binary QDA - Test data")



# caret implementation of lda & qda and model comparison ---------------------------------------

# We will use the dataset with 'sacral_slope' removed.
trdata.q <- trdata %>% select(-sacral_slope)


# Now suppose we want to compare the performances of lda and qda, without access to the test data. 
# It is then required to cross-validate your (lda or qda) fit.
train_lda <- train(Class_att ~ ., method = "lda", 
                   data = trdata.q,
                   trControl = trainControl(method = "cv"))

train_qda <- train(Class_att ~ ., method = "qda", 
                   data = trdata.q,
                   trControl = trainControl(method = "cv"))

# We can now summarize results from all cross-validations (resamples) by caret::resamples function.
# 
models <- resamples(list(LDA = train_lda,
                         QDA = train_qda,
                         kNN = train_knn))   # including the knn results
summary(models)
dotplot(models, metric = "Accuracy")

# Train logistic regression -----------------------------------------------

# We will again use the data with no collinearity. Otherwise, the algorithm to compute logistic regression
# fit will not converge. 
trdata.q <- trdata %>% select(-sacral_slope)
glm.out <- glm(Class_att ~ ., family = "binomial", data = trdata.q)
summary(glm.out)
# GLM is indeed like a regression, and it comes with significance of each predictor. 
# You can try stepwise variable selection, using AIC value. 
glm.step.out <- stepAIC(glm.out,direction = "backward")

# As a classification method, we can predict, extract the confusion matrix and draw the ROC. 
# Let us switch to using the caret package
 
train_glm <- train(Class_att ~ ., method = "glm", 
                   data = trdata.q,
                   trControl = trainControl(method = "cv"))
summary(train_glm$finalModel)
train_glmstep <- train(Class_att ~ ., method = "glmStepAIC", 
                   data = trdata.q,
                   trControl = trainControl(method = "cv"))
summary(train_glmstep$finalModel)

# By this time, you probably have noticed that simply indicating the method argument is enough to try different methods. 
# See https://topepo.github.io/caret/available-models.html for available models in caret
# The reason why we use cv is to compare the performance of logistic regression with those of lda, qda, and knn. 

# Prediction (of test data) 
pred.test <- predict(train_glm, test)
pred.test_prob <- predict(train_glm, test, type = "prob")
# Confusion Matrix
confusionMatrix(pred.test, test$Class_att)
# ROC (Receiver-Operating Curve) and AUC (Area Under the Curve)  
roc.out <- roc(response = test$Class_att, predictor = pred.test_prob[,1])
plot.roc(roc.out,  
         print.auc = TRUE,
         main = "ROC and AUC: Binary logistic regression - Test data")




# Grand comparison --------------------------------------------------------


# We can now summarize results from all cross-validations (resamples) by caret::resamples function.
# 
allmodels <- list(LDA = train_lda,
                  QDA = train_qda,
                  kNN = train_knn,
                  Logistic = train_glm,
                  Logistic_AIC = train_glmstep)
mods <- resamples(allmodels)   # including the knn results
summary(mods)
dotplot(mods, metric = "Accuracy")

# In many real-life situations, we do not have testing data. Testing data exist only in classroom and competitions. 
# Since we have set aside the testing, check the accuracy of the methods in testing data. 

aa<- as.data.frame(
       lapply(allmodels, 
            function (x) 
              {a <- confusionMatrix(predict(x, test), test$Class_att); c(a$overall,a$byClass)}) )
# Interpretation is yours.




 