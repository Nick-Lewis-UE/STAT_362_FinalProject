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
  # mutate(log_limit_bal = log(limit_bal),
  #        log_age = log(age),
  #        log_bill_amt1 = log(bill_amt1),
  #        log_bill_amt2 = log(bill_amt2),
  #        log_bill_amt3 = log(bill_amt3),
  #        log_bill_amt4 = log(bill_amt4),
  #        log_bill_amt5 = log(bill_amt5),
  #        log_bill_amt6 = log(bill_amt6),
  #        log_pay_amt1 = ifelse(pay_amt1 != 0, log(pay_amt1), log(.01)),
  #        log_pay_amt2 = ifelse(pay_amt2 != 0, log(pay_amt2), log(.01)),
  #        log_pay_amt3 = ifelse(pay_amt3 != 0, log(pay_amt3), log(.01)),
  #        log_pay_amt4 = ifelse(pay_amt4 != 0, log(pay_amt4), log(.01)),
  #        log_pay_amt5 = ifelse(pay_amt5 != 0, log(pay_amt5), log(.01)),
  #        log_pay_amt6 = ifelse(pay_amt6 != 0, log(pay_amt6), log(.01))) %>%
  # dplyr::select(-bill_amt1, -bill_amt2, -bill_amt3, -bill_amt4, -bill_amt5, -bill_amt6, 
  #        -pay_amt1, -pay_amt2, -pay_amt3, -pay_amt4, -pay_amt5, -pay_amt6, -limit_bal, -age) %>%
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
  # mutate(perc_paid1 = log_pay_amt1/log_bill_amt1,
  #        perc_paid2 = log_pay_amt2/log_bill_amt2,
  #        perc_paid3 = log_pay_amt3/log_bill_amt3,
  #        perc_paid4 = log_pay_amt4/log_bill_amt4,
  #        perc_paid5 = log_pay_amt5/log_bill_amt5,
  #        perc_paid6 = log_pay_amt6/log_bill_amt6) %>%
  mutate(mean_bill = (bill_amt1+bill_amt2+bill_amt3+bill_amt4+bill_amt5+bill_amt6)/6,
         mean_pay = (pay_amt1+pay_amt2+pay_amt3+pay_amt4+pay_amt5+pay_amt6)/6)#%>%
  # mutate(refunded = ifelse(bill_amt1 < 0 | 
  #                            bill_amt2 < 0 |
  #                            bill_amt3 < 0 |
  #                            bill_amt4 < 0 |
  #                            bill_amt5 < 0 |
  #                            bill_amt6 < 0, 1, 0))
compete <- read_csv("Data/final_compete.csv", col_types = "ddfffddddddddddddd")

train <- data %>%
  sample_frac(.7)
test <- data %>%
  setdiff(train)
train <- train %>%
  filter(!is.infinite(perc_paid3))

###############
# Visualization
###############

ggpairs(27:32, data = train)
ggplot(data) +
  geom_density(mappin = aes(x = log(limit_bal), color = default))

########
# Models
########

model_lda <- lda(default ~., train)
pred_lda <- predict(model_lda, test)
confusionMatrix(pred_lda$class, test$default)

kfold_trcontrol <- trainControl(method = "cv", number = 10)
model_lda_kfold <- train(default ~ ., data = train, trControl = kfold_trcontrol, method = "lda")
pred_lda_kfold <- predict(model_lda_kfold, test)
confusionMatrix(pred_lda_kfold, test$default)

model_qda_kfold <- train(default ~ ., data = train, trControl = kfold_trcontrol, method = "qda")
pred_qda_kfold <- predict(model_qda_kfold, test)
confusionMatrix(pred_qda_kfold, test$default)

model_glm_kfold <- train(default ~., data = train, trControl = kfold_trcontrol, method = "glm")
pred_glm_kfold <- predict(model_glm_kfold, test)
confusionMatrix(pred_glm_kfold, test$default)

model_glm_spline_cv <- gam(default ~ s(mean_bill), data = train, family = "binomial")
model_glm_spline_cv <- gam(default ~ mean_bill, data = train, family = "binomial")
plot(model_glm_spline_cv)

model_glm_spline_cv <- gam(default ~ s(mean_bill) + s(age) + log(limit_bal) + sex + education + marriage,
                           data = train, family = "binomial")
pred_glm_spline_cv <- predict(model_glm_spline_cv, test, type = "response")
pred_glm_spline_cv <- ifelse(pred_glm_spline_cv >= .6, 1, 0)
confusionMatrix(as.factor(pred_glm_spline_cv), test$default)

trainControl <- trainControl(method = "cv", number = 10)
model_glm_spline_cv <- train(default ~ . -mean_bill -mean_pay,
                             data = train, trControl = trainControl, method = "gamSpline")
pred_glm_spline_cv <- predict(model_glm_spline_cv, test, type = "prob")
pred_glm_spline_cv <- ifelse(pred_glm_spline_cv[1] >= .35, 1, 0)
confusionMatrix(as.factor(pred_glm_spline_cv), test$default)
