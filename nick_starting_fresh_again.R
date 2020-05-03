##########
# Preamble
##########

library(tidyverse)
library(GGally)
library(caret)
library(modelr)
library(glmnet)
library(randomForest)
library(gbm)
library(MASS)
library(gam)

set.seed(362)

######
# Data
######

data <- read_csv("Data/final_train.csv", col_types = "dfffdddddddddddddf") %>%
  mutate(log_limit_bal = log(limit_bal),
         log_age = log(age),
         log_bill_amt1 = log(bill_amt1),
         log_bill_amt2 = log(bill_amt2),
         log_bill_amt3 = log(bill_amt3),
         log_bill_amt4 = log(bill_amt4),
         log_bill_amt5 = log(bill_amt5),
         log_bill_amt6 = log(bill_amt6),
         log_pay_amt1 = ifelse(pay_amt1 != 0, log(pay_amt1), log(.01)),
         log_pay_amt2 = ifelse(pay_amt2 != 0, log(pay_amt2), log(.01)),
         log_pay_amt3 = ifelse(pay_amt3 != 0, log(pay_amt3), log(.01)),
         log_pay_amt4 = ifelse(pay_amt4 != 0, log(pay_amt4), log(.01)),
         log_pay_amt5 = ifelse(pay_amt5 != 0, log(pay_amt5), log(.01)),
         log_pay_amt6 = ifelse(pay_amt6 != 0, log(pay_amt6), log(.01))) %>%
  # filter(!is.na(log_bill_amt1) &
  #          !is.na(log_bill_amt2) &
  #          !is.na(log_bill_amt3) &
  #          !is.na(log_bill_amt4) &
  #          !is.na(log_bill_amt5) &
  #          !is.na(log_bill_amt6) &
  #          !is.infinite(log_bill_amt1) &
  #          !is.infinite(log_bill_amt2) &
  #          !is.infinite(log_bill_amt3) &
  #          !is.infinite(log_bill_amt4) &
  #          !is.infinite(log_bill_amt5) &
  #          !is.infinite(log_bill_amt6)) %>%
  mutate(perc_paid1 = log_pay_amt1/log_bill_amt1,
         perc_paid2 = log_pay_amt2/log_bill_amt2,
         perc_paid3 = log_pay_amt3/log_bill_amt3,
         perc_paid4 = log_pay_amt4/log_bill_amt4,
         perc_paid5 = log_pay_amt5/log_bill_amt5,
         perc_paid6 = log_pay_amt6/log_bill_amt6) %>%
  mutate(mean_bill = (bill_amt1+bill_amt2+bill_amt3+bill_amt4+bill_amt5+bill_amt6)/6,
         mean_pay = (pay_amt1+pay_amt2+pay_amt3+pay_amt4+pay_amt5+pay_amt6)/6) %>%
  mutate(log_mean_bill = log(mean_bill),
         log_mean_pay = log(mean_pay)) %>%
  mutate(refunded = ifelse(bill_amt1 < 0 |
                             bill_amt2 < 0 |
                             bill_amt3 < 0 |
                             bill_amt4 < 0 |
                             bill_amt5 < 0 |
                             bill_amt6 < 0, 1, 0)) %>%
  mutate(under_27 = as.factor(ifelse(age < 27, 1, 0)),
         bw_27_40 = as.factor(ifelse(age >= 27 & age < 40, 1, 0)),
         bw_40_55 = as.factor(ifelse(age >= 40 & age < 55, 1, 0)),
         above_55 = as.factor(ifelse(age >= 55, 1, 0))) %>%
  mutate(high_pay = as.factor(ifelse(log(mean_pay) >= 8, 1, 0)),
         low_pay = as.factor(ifelse(log(mean_pay) < 8, 1, 0))) %>%
  mutate(low_limit = as.factor(ifelse(limit_bal < 1.25e5, 1, 0)),
         high_limit = as.factor(ifelse(limit_bal >= 1.25e5, 1, 0))) %>%
  mutate(no_bills = as.factor(ifelse(bill_amt1+bill_amt2+bill_amt3+bill_amt4+bill_amt5+bill_amt6 == 0, 1, 0)),
         no_pay = as.factor(ifelse(pay_amt1+pay_amt2+pay_amt3+pay_amt4+pay_amt5+pay_amt6 == 0, 1, 0)))


compete <- read_csv("Data/final_compete.csv", col_types = "ddfffddddddddddddd")

train <- data %>%
  sample_frac(.7)
test <- data %>%
  setdiff(train)
trainControl <- trainControl(method = "cv", number = 10)

###########
# Visualize
###########

ggplot(train) +
  geom_density(mapping = aes(x = pay_amt6/bill_amt6, color = default)) +#, position = "fill")
  xlim(-5, 5)

ggpairs(c(2,3,4,18), data = train)

##########
# Modeling
##########

# RF
train_rf <- train %>%
  dplyr::select(-log_bill_amt1, -log_bill_amt2, -log_bill_amt3, -log_bill_amt4, -log_bill_amt5, -log_bill_amt6, 
         -log_pay_amt1, -log_pay_amt2, -log_pay_amt3, -log_pay_amt4, -log_pay_amt5, -log_pay_amt6, 
         -perc_paid1, -perc_paid2, -perc_paid3, -perc_paid4, -perc_paid5, -perc_paid6, -log_mean_bill, -log_mean_pay)
model_rf <- randomForest(default ~ . , data = train_rf, mtry = 6, ntree = 50, importance = TRUE)
pred_rf <- predict(model_rf, test)
confusionMatrix(pred_rf, test$default)

model_rf_cv <- train(default ~ . , data = train_rf, method = "rf")

# GLM w/ StepWise Selecction
train_lda <- train_rf %>%
  dplyr::select(-log_age, -limit_bal, -refunded, -age, -log_limit_bal,
                -bill_amt1,-bill_amt2,-bill_amt3,-bill_amt4,-bill_amt5,-bill_amt6,
                -pay_amt1,-pay_amt2,-pay_amt3,-pay_amt4,-pay_amt5,-pay_amt6)
model_lda_cv <- train(default ~ . , data = train_lda, method = "lda")
confusionMatrix(predict(model_lda_cv, test), test$default)

model_glm <- glm(default ~., data = train_lda, family = "binomial")
odds <- exp(predict(model_glm, test, type = "response"))
prob <- odds/(1+odds)
pred_glm <- ifelse(predict(model_glm, test, type = "response") >= .5, 1, 0)
confusionMatrix(as.factor(pred_glm), test$default)

#####
# PCA
#####

pca_data <- data %>%
  dplyr::select(bill_amt1, bill_amt2, bill_amt3, bill_amt4, bill_amt5, bill_amt6,
         pay_amt1, pay_amt2, pay_amt3, pay_amt4, pay_amt5, pay_amt6)
pca <- prcomp(pca_data, center = TRUE, scale = TRUE)

ggplot(data) +
  geom_point(mapping = aes(x = pca$x[,1], pca$x[,2], color = default)) +
  xlim(-5,20) +
  ylim(-5, 25)

data_pca <- read_csv("Data/final_train.csv", col_types = "dfffdddddddddddddf") %>%
  cbind(pca$x[,1], pca$x[,2]) %>%
  dplyr::select(-bill_amt1, -bill_amt2, -bill_amt3, -bill_amt4, -bill_amt5, -bill_amt6,
                -pay_amt1, -pay_amt2, -pay_amt3, -pay_amt4, -pay_amt5, -pay_amt6)
names(data_pca)[7] <- "comp1"
names(data_pca)[8] <- "comp2"
train_pca <- data_pca %>%
  sample_frac(.7)
test_pca <- data_pca %>%
  setdiff(train_pca)
trainControl_pca <- trainControl(method = "cv", number = 10)

model_pca_glm <- glm(default ~., data =train_pca, family = "binomial")
pred_pca_glm <- ifelse(predict(model_pca_glm, test_pca, type = "response") > .6, 1, 0)
confusionMatrix(as.factor(pred_pca_glm), test_pca$default)

model_pca_rf <- randomForest(default ~., data =train_pca, mtry = 3, ntree = 50, importance = TRUE)
pred_pca_rf <- predict(model_pca_rf, test_pca)
confusionMatrix(pred_pca_rf, test_pca$default)

# .7748

model_pca_lda <- train(default ~., data = train_pca, method = "lda")
pred_pca_lda <- predict(model_pca_lda, test_pca)
confusionMatrix(pred_pca_lda, test_pca$default)

# .7921

log_loss(test_pca, pred_pca_lda)
############################

log_loss <- function(test, pred) {
  test_pca <- test_pca %>% 
    mutate(default_num = as.numeric(as.character(default))) %>%
    cbind(pred_pca_lda)
  test %>%
    summarise(log_loss = -mean(default_num*log(as.numeric(as.character(pred_pca_lda))) + (default_num)*log(1-as.numeric(as.character(pred_pca_lda)))))
}
