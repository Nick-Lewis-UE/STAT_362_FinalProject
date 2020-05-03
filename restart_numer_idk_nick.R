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

log_loss <- function(t, error) {
  error <- ifelse(error == 0, exp(-10), error)
  error <- ifelse(error == 1, .99999999999, error)
  t <- t %>% 
    mutate(default_num = as.numeric(as.character(default)))
  t %>%
    summarise(log_loss = -mean(default_num*log(error) + (1-default_num)*log(1-error)))
}

######
# Data
######

data <- read_csv("Data/final_train.csv", col_types = "dfffdddddddddddddf") %>%
  mutate(log_limit_bal = log(limit_bal),
         log_age = log(age),
         log_bill_amt1 = ifelse(bill_amt1 != 0, log(bill_amt1), log(.01)),
         log_bill_amt2 = ifelse(bill_amt2 != 0, log(bill_amt2), log(.01)),
         log_bill_amt3 = ifelse(bill_amt3 != 0, log(bill_amt3), log(.01)),
         log_bill_amt4 = ifelse(bill_amt4 != 0, log(bill_amt4), log(.01)),
         log_bill_amt5 = ifelse(bill_amt5 != 0, log(bill_amt5), log(.01)),
         log_bill_amt6 = ifelse(bill_amt6 != 0, log(bill_amt6), log(.01)),
         log_pay_amt1 = ifelse(pay_amt1 != 0, log(pay_amt1), log(.01)),
         log_pay_amt2 = ifelse(pay_amt2 != 0, log(pay_amt2), log(.01)),
         log_pay_amt3 = ifelse(pay_amt3 != 0, log(pay_amt3), log(.01)),
         log_pay_amt4 = ifelse(pay_amt4 != 0, log(pay_amt4), log(.01)),
         log_pay_amt5 = ifelse(pay_amt5 != 0, log(pay_amt5), log(.01)),
         log_pay_amt6 = ifelse(pay_amt6 != 0, log(pay_amt6), log(.01))) %>%
filter(!is.nan(log_bill_amt1) &
         !is.nan(log_bill_amt2) &
         !is.nan(log_bill_amt3) &
         !is.nan(log_bill_amt4) &
         !is.nan(log_bill_amt5) &
         !is.nan(log_bill_amt6) &
         !is.infinite(log_bill_amt1) &
         !is.infinite(log_bill_amt2) &
         !is.infinite(log_bill_amt3) &
         !is.infinite(log_bill_amt4) &
         !is.infinite(log_bill_amt5) &
         !is.infinite(log_bill_amt6)) %>%
mutate(perc_paid1 = log_pay_amt1/log_bill_amt1,
       perc_paid2 = log_pay_amt2/log_bill_amt2,
       perc_paid3 = log_pay_amt3/log_bill_amt3,
       perc_paid4 = log_pay_amt4/log_bill_amt4,
       perc_paid5 = log_pay_amt5/log_bill_amt5,
       perc_paid6 = log_pay_amt6/log_bill_amt6) %>%
mutate(mean_bill = (bill_amt1+bill_amt2+bill_amt3+bill_amt4+bill_amt5+bill_amt6)/6,
       mean_pay = (pay_amt1+pay_amt2+pay_amt3+pay_amt4+pay_amt5+pay_amt6)/6) %>%
mutate(log_mean_bill = ifelse(mean_bill != 0, log(mean_bill), log(.001)),
       log_mean_pay = ifelse(mean_pay != 0, log(mean_pay), log(.01))) %>%
  # mutate(refunded = ifelse(bill_amt1 < 0 |
  #                            bill_amt2 < 0 |
  #                            bill_amt3 < 0 |
  #                            bill_amt4 < 0 |
  #                            bill_amt5 < 0 |
  #                            bill_amt6 < 0, 1, 0)) %>%
  mutate(under_27 = as.factor(ifelse(age < 27, 1, 0)),
         bw_27_40 = as.factor(ifelse(age >= 27 & age < 40, 1, 0)),
         bw_40_55 = as.factor(ifelse(age >= 40 & age < 55, 1, 0)),
         above_55 = as.factor(ifelse(age >= 55, 1, 0))) %>%
  dplyr::select(-age) %>%
  mutate(high_pay = as.factor(ifelse(log(mean_pay) >= 8, 1, 0)),
         low_pay = as.factor(ifelse(log(mean_pay) < 8, 1, 0))) %>%
  mutate(low_limit = as.factor(ifelse(limit_bal < 1.25e5, 1, 0)),
         high_limit = as.factor(ifelse(limit_bal >= 1.25e5, 1, 0))) %>%
    dplyr::select(-mean_bill, -mean_pay) %>%
  mutate(no_bills = as.factor(ifelse(bill_amt1+bill_amt2+bill_amt3+bill_amt4+bill_amt5+bill_amt6 == 0, 1, 0)),
         no_pay = as.factor(ifelse(pay_amt1+pay_amt2+pay_amt3+pay_amt4+pay_amt5+pay_amt6 == 0, 1, 0))) %>%
    dplyr::select(-bill_amt1, -bill_amt2, -bill_amt3, -bill_amt4, -bill_amt5, -bill_amt6,
                  -pay_amt1, -pay_amt2, -pay_amt3, -pay_amt4, -pay_amt5, -pay_amt6)


compete <- read_csv("Data/final_compete.csv", col_types = "ddfffddddddddddddd") %>%
  mutate(log_limit_bal = log(limit_bal),
         log_age = log(age),
         log_bill_amt1 = ifelse(bill_amt1 != 0, log(bill_amt1), log(.01)),
         log_bill_amt2 = ifelse(bill_amt2 != 0, log(bill_amt2), log(.01)),
         log_bill_amt3 = ifelse(bill_amt3 != 0, log(bill_amt3), log(.01)),
         log_bill_amt4 = ifelse(bill_amt4 != 0, log(bill_amt4), log(.01)),
         log_bill_amt5 = ifelse(bill_amt5 != 0, log(bill_amt5), log(.01)),
         log_bill_amt6 = ifelse(bill_amt6 != 0, log(bill_amt6), log(.01)),
         log_pay_amt1 = ifelse(pay_amt1 != 0, log(pay_amt1), log(.01)),
         log_pay_amt2 = ifelse(pay_amt2 != 0, log(pay_amt2), log(.01)),
         log_pay_amt3 = ifelse(pay_amt3 != 0, log(pay_amt3), log(.01)),
         log_pay_amt4 = ifelse(pay_amt4 != 0, log(pay_amt4), log(.01)),
         log_pay_amt5 = ifelse(pay_amt5 != 0, log(pay_amt5), log(.01)),
         log_pay_amt6 = ifelse(pay_amt6 != 0, log(pay_amt6), log(.01))) %>%
  filter(!is.nan(log_bill_amt1) &
           !is.nan(log_bill_amt2) &
           !is.nan(log_bill_amt3) &
           !is.nan(log_bill_amt4) &
           !is.nan(log_bill_amt5) &
           !is.nan(log_bill_amt6) &
           !is.infinite(log_bill_amt1) &
           !is.infinite(log_bill_amt2) &
           !is.infinite(log_bill_amt3) &
           !is.infinite(log_bill_amt4) &
           !is.infinite(log_bill_amt5) &
           !is.infinite(log_bill_amt6)) %>%
  mutate(perc_paid1 = log_pay_amt1/log_bill_amt1,
         perc_paid2 = log_pay_amt2/log_bill_amt2,
         perc_paid3 = log_pay_amt3/log_bill_amt3,
         perc_paid4 = log_pay_amt4/log_bill_amt4,
         perc_paid5 = log_pay_amt5/log_bill_amt5,
         perc_paid6 = log_pay_amt6/log_bill_amt6) %>%
  mutate(mean_bill = (bill_amt1+bill_amt2+bill_amt3+bill_amt4+bill_amt5+bill_amt6)/6,
         mean_pay = (pay_amt1+pay_amt2+pay_amt3+pay_amt4+pay_amt5+pay_amt6)/6) %>%
  mutate(log_mean_bill = ifelse(mean_bill != 0, log(mean_bill), log(.001)),
         log_mean_pay = ifelse(mean_pay != 0, log(mean_pay), log(.01))) %>%
  # mutate(refunded = ifelse(bill_amt1 < 0 |
  #                            bill_amt2 < 0 |
  #                            bill_amt3 < 0 |
  #                            bill_amt4 < 0 |
  #                            bill_amt5 < 0 |
  #                            bill_amt6 < 0, 1, 0)) %>%
  mutate(under_27 = as.factor(ifelse(age < 27, 1, 0)),
         bw_27_40 = as.factor(ifelse(age >= 27 & age < 40, 1, 0)),
         bw_40_55 = as.factor(ifelse(age >= 40 & age < 55, 1, 0)),
         above_55 = as.factor(ifelse(age >= 55, 1, 0))) %>%
  # dplyr::select(-age) %>%
  mutate(high_pay = as.factor(ifelse(log(mean_pay) >= 8, 1, 0)),
         low_pay = as.factor(ifelse(log(mean_pay) < 8, 1, 0))) %>%
  mutate(low_limit = as.factor(ifelse(limit_bal < 1.25e5, 1, 0)),
         high_limit = as.factor(ifelse(limit_bal >= 1.25e5, 1, 0))) %>%
  dplyr::select(-mean_bill, -mean_pay) %>%
  mutate(no_bills = as.factor(ifelse(bill_amt1+bill_amt2+bill_amt3+bill_amt4+bill_amt5+bill_amt6 == 0, 1, 0)),
         no_pay = as.factor(ifelse(pay_amt1+pay_amt2+pay_amt3+pay_amt4+pay_amt5+pay_amt6 == 0, 1, 0))) %>%
  dplyr::select(-bill_amt1, -bill_amt2, -bill_amt3, -bill_amt4, -bill_amt5, -bill_amt6,
                -pay_amt1, -pay_amt2, -pay_amt3, -pay_amt4, -pay_amt5, -pay_amt6)

train <- data %>%
  sample_frac(.7)
test <- data %>%
  setdiff(train)
train <- train %>%
  filter(!is.infinite(perc_paid3))
trainControl <- trainControl(method = "cv", number = 10)

# RF

mod_rf_4 <- randomForest(default ~ ., data = train, mtry = 5, ntree = 50, importance = TRUE)
pred_rf_4 <- predict(mod_rf_4, test, type = "prob")
confusionMatrix(pred_rf_4, test$default)
log_loss(test, pred_rf_4[,1])

pred_rf_vec = vector(length = 17)
for (i in 1:17) {
  mod <- randomForest(default ~ ., data = train, mtry = i, ntree = 50, importance = TRUE)
  pred_rf_vec[i] <- log_loss(test, predict(mod, test, type = "prob")[,1])#mean(predict(mod, test) == test$default)
}

train_control_kfold <- trainControl(method = "cv", number = 10) # WARNING: WILL TAKE LONG
model_kfold_rf <- train(default ~ ., data = train, trControl = train_control_kfold, method = "rf")
pred_rf_cv <- predict(model_kfold_rf, test, type = "prob")
log_loss(test, pred_rf_cv[,1])

# Bagging

model_bag <- randomForest(default ~ ., data = train, mtry = 17, ntree = 25, importance = TRUE)
pred_bag <- predict(model_bag, test, type = "prob")
confusionMatrix(pred_bag, test$default)
log_loss(test, pred_bag)

# Boosting

model_boost_1 <- gbm(default ~ ., data = train, distribution = "bernoulli", shrinkage = .01, 
                     n.trees = 2000, interaction.depth = 4)
pred_boost_1 <- predict(model_boost_1, newdata = test, n.trees = 2000)
pred_boost_1 <- ifelse(pred_boost_1 >= .5, 1, 0)
confusionMatrix(as.factor(pred_boost_1), test$default)
log_loss(test, pred_boost_1)

compete_pred <- predict(model_kfold_rf, compete, type = "prob")[,1]
compete_pred <- data.frame(ID = 1:length(compete_pred),
                           probs = compete_pred)
write_csv(compete_pred, "compete_preds_1.csv")

