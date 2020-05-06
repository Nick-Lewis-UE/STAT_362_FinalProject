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

log_loss <- function(t, pred) {
  pred <- ifelse(pred == 0, exp(-10), pred)
  pred <- ifelse(pred == 1, .99999999999, pred)
  t <- t %>% 
    mutate(default_num = as.numeric(as.character(default)))
  t %>%
    summarise(log_loss = -mean(default_num*log(pred) + (1-default_num)*log(1-pred)))
}

#################################
# Loading and Processing the Data
#################################

# Process Train Data for Random Forest
data <- read_csv("Data/final_train.csv", col_types = "dfffdddddddddddddf") %>%
  mutate(log_limit_bal = log(limit_bal),
         log_age = log(age),
         log_bill_amt1 = ifelse(bill_amt1 > 0, log(bill_amt1), 
                                ifelse(bill_amt1 == 0, log(.01), -log(abs(bill_amt1)))),
         log_bill_amt2 = ifelse(bill_amt2 > 0, log(bill_amt2), 
                                ifelse(bill_amt2 == 0, log(.01), -log(abs(bill_amt2)))),
         log_bill_amt3 = ifelse(bill_amt3 > 0, log(bill_amt3), 
                                ifelse(bill_amt3 == 0, log(.01), -log(abs(bill_amt3)))),
         log_bill_amt4 = ifelse(bill_amt4 > 0, log(bill_amt4), 
                                ifelse(bill_amt4 == 0, log(.01), -log(abs(bill_amt4)))),
         log_bill_amt5 = ifelse(bill_amt5 > 0, log(bill_amt5), 
                                ifelse(bill_amt5 == 0, log(.01), -log(abs(bill_amt5)))),
         log_bill_amt6 = ifelse(bill_amt6 > 0, log(bill_amt6), 
                                ifelse(bill_amt6 == 0, log(.01), -log(abs(bill_amt6)))),
         log_pay_amt1 = ifelse(pay_amt1 != 0, log(pay_amt1), log(.01)),
         log_pay_amt2 = ifelse(pay_amt2 != 0, log(pay_amt2), log(.01)),
         log_pay_amt3 = ifelse(pay_amt3 != 0, log(pay_amt3), log(.01)),
         log_pay_amt4 = ifelse(pay_amt4 != 0, log(pay_amt4), log(.01)),
         log_pay_amt5 = ifelse(pay_amt5 != 0, log(pay_amt5), log(.01)),
         log_pay_amt6 = ifelse(pay_amt6 != 0, log(pay_amt6), log(.01))) %>%
  mutate(perc_paid1 = ifelse(log_bill_amt1 != 0, log_pay_amt1/log_bill_amt1, log_pay_amt1/.01),
         perc_paid2 = ifelse(log_bill_amt2 != 0, log_pay_amt2/log_bill_amt2, log_pay_amt1/.01),
         perc_paid3 = ifelse(log_bill_amt3 != 0, log_pay_amt3/log_bill_amt3, log_pay_amt1/.01),
         perc_paid4 = ifelse(log_bill_amt4 != 0, log_pay_amt4/log_bill_amt4, log_pay_amt1/.01),
         perc_paid5 = ifelse(log_bill_amt5 != 0, log_pay_amt5/log_bill_amt5, log_pay_amt1/.01),
         perc_paid6 = ifelse(log_bill_amt6 != 0, log_pay_amt6/log_bill_amt6, log_pay_amt1/.01)) %>%
  mutate(mean_bill = (bill_amt1+bill_amt2+bill_amt3+bill_amt4+bill_amt5+bill_amt6)/6,
         mean_pay = (pay_amt1+pay_amt2+pay_amt3+pay_amt4+pay_amt5+pay_amt6)/6) %>%
  mutate(log_mean_bill = ifelse(mean_bill > 0, log(mean_bill), 
                                ifelse(mean_bill == 0, log(.01), -log(abs(mean_bill)))),
         log_mean_pay = ifelse(mean_pay != 0, log(mean_pay), log(.01))) %>%
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

# Process Compete Data for Random Forest
compete <- read_csv("Data/final_compete.csv", col_types = "ddfffddddddddddddd") %>%
  mutate(log_limit_bal = log(limit_bal),
         log_age = log(age),
         log_bill_amt1 = ifelse(bill_amt1 > 0, log(bill_amt1), 
                                ifelse(bill_amt1 == 0, log(.01), -log(abs(bill_amt1)))),
         log_bill_amt2 = ifelse(bill_amt2 > 0, log(bill_amt2), 
                                ifelse(bill_amt2 == 0, log(.01), -log(abs(bill_amt2)))),
         log_bill_amt3 = ifelse(bill_amt3 > 0, log(bill_amt3), 
                                ifelse(bill_amt3 == 0, log(.01), -log(abs(bill_amt3)))),
         log_bill_amt4 = ifelse(bill_amt4 > 0, log(bill_amt4), 
                                ifelse(bill_amt4 == 0, log(.01), -log(abs(bill_amt4)))),
         log_bill_amt5 = ifelse(bill_amt5 > 0, log(bill_amt5), 
                                ifelse(bill_amt5 == 0, log(.01), -log(abs(bill_amt5)))),
         log_bill_amt6 = ifelse(bill_amt6 > 0, log(bill_amt6), 
                                ifelse(bill_amt6 == 0, log(.01), -log(abs(bill_amt6)))),
         log_pay_amt1 = ifelse(pay_amt1 != 0, log(pay_amt1), log(.01)),
         log_pay_amt2 = ifelse(pay_amt2 != 0, log(pay_amt2), log(.01)),
         log_pay_amt3 = ifelse(pay_amt3 != 0, log(pay_amt3), log(.01)),
         log_pay_amt4 = ifelse(pay_amt4 != 0, log(pay_amt4), log(.01)),
         log_pay_amt5 = ifelse(pay_amt5 != 0, log(pay_amt5), log(.01)),
         log_pay_amt6 = ifelse(pay_amt6 != 0, log(pay_amt6), log(.01))) %>%
  mutate(perc_paid1 = ifelse(log_bill_amt1 != 0, log_pay_amt1/log_bill_amt1, log_pay_amt1/.01),
         perc_paid2 = ifelse(log_bill_amt2 != 0, log_pay_amt2/log_bill_amt2, log_pay_amt1/.01),
         perc_paid3 = ifelse(log_bill_amt3 != 0, log_pay_amt3/log_bill_amt3, log_pay_amt1/.01),
         perc_paid4 = ifelse(log_bill_amt4 != 0, log_pay_amt4/log_bill_amt4, log_pay_amt1/.01),
         perc_paid5 = ifelse(log_bill_amt5 != 0, log_pay_amt5/log_bill_amt5, log_pay_amt1/.01),
         perc_paid6 = ifelse(log_bill_amt6 != 0, log_pay_amt6/log_bill_amt6, log_pay_amt1/.01)) %>%
  mutate(mean_bill = (bill_amt1+bill_amt2+bill_amt3+bill_amt4+bill_amt5+bill_amt6)/6,
         mean_pay = (pay_amt1+pay_amt2+pay_amt3+pay_amt4+pay_amt5+pay_amt6)/6) %>%
  mutate(log_mean_bill = ifelse(mean_bill > 0, log(mean_bill), 
                                ifelse(mean_bill == 0, log(.01), -log(abs(mean_bill)))),
         log_mean_pay = ifelse(mean_pay != 0, log(mean_pay), log(.01))) %>%
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

# Divide up for CV for RF
train <- data %>%
  sample_frac(.7)
test <- data %>%
  setdiff(train)
train <- train %>%
  filter(!is.infinite(perc_paid3))
test <- test %>%
  filter(!is.infinite(perc_paid3))
trainControl <- trainControl(method = "cv", number = 10)

# Create Data for PCA
pca_data <- read_csv("Data/final_train.csv", col_types = "dfffdddddddddddddf") %>%
  dplyr::select(bill_amt1, bill_amt2, bill_amt3, bill_amt4, bill_amt5, bill_amt6,
                pay_amt1, pay_amt2, pay_amt3, pay_amt4, pay_amt5, pay_amt6)
pca <- prcomp(pca_data, center = TRUE, scale = TRUE) # prcomp() performs PCA

data_pca <- read_csv("Data/final_train.csv", col_types = "dfffdddddddddddddf") %>%
  cbind(pca$x[,1], pca$x[,2]) %>% # add the PCA features
  dplyr::select(-bill_amt1, -bill_amt2, -bill_amt3, -bill_amt4, -bill_amt5, -bill_amt6,
                -pay_amt1, -pay_amt2, -pay_amt3, -pay_amt4, -pay_amt5, -pay_amt6) # remove old features

names(data_pca)[7] <- "comp1" # rename pca column 1
names(data_pca)[8] <- "comp2" # "               " 2
train_pca <- data_pca %>%
  sample_frac(.7)
test_pca <- data_pca %>%
  setdiff(train_pca)

#####################
# Building the Models
#####################

# Train RF using the Caret package - WARNING: TAKES A LONG TIME
model_kfold_rf <- train(default ~ ., data = train, trControl = train_control_kfold, method = "rf")

# Extract final model and note the build
model_kfold_rf$finalModel # mtry = 2, n.trees = 500

# Reproduce this model for the sake of time
model_final_cv_rf <- randomForest(default ~., data = train, mtry = 2, n.trees = 500) # model 1

# Remove the low importance variables
new_train <- train %>% 
  dplyr::select(-no_pay, -no_bills, -high_limit, -low_limit, -low_pay, 
                -high_pay, -above_55, -bw_40_55, -bw_27_40, -under_27)
model_final_cv_rf <- randomForest(default ~., data = new_train, mtry = 2, n.trees = 500) # model 3

# Train LDA using the Caret package and PCA data
model_pca_lda <- train(default ~., data = train_pca, method = "lda")


# LDA - SAM
#All-subsets variable selection
model_subset <- regsubsets(default ~ ., nbest = 3, data = train)
plot(model_subset)
#Optimal variables: log_limit_bal, log_bill_amt2, log_bill_amt3, log_pay_amt1, log_pay_amt2)
#Using optimal variables from all-subsets selection
model_lda <- lda(default ~ log_limit_bal + log_bill_amt2 + log_bill_amt3 + log_pay_amt1 + log_pay_amt2 , data = train)


###############################
# Testing the Model and Results
###############################

# make predictions from CV random forest
pred_rf_cv <- predict(model_kfold_rf, test, type = "prob")
log_loss(test, pred_rf_cv[,1]) # ~0.455

# make predictions from RF final model extraction
pred_final_rf_cv <- predict(model_final_cv_rf, test, type = "prob")
log_loss(test, pred_final_rf_cv[,1]) # ~0.455

# Make predictions from the CV PCA LDA
pred_pca_lda <- predict(model_pca_lda, test_pca, type = "prob")
log_loss(test_pca, pred_pca_lda[,1]) # ~0.477

# Make predictions from LDA - SAM
prob_lda <- predict(model_lda, newdata = test)
prob_lda_prob <- prob_lda$posterior[1:7343]
log_loss(test, prob_lda_prob)

############
# Deployment
############

# Deploy for Random Forest
compete_pred <- predict(model_final_cv_rf, compete, type = "prob")[,1]
compete_pred <- data.frame(ID = 1:length(compete_pred),
                           probs = compete_pred)
write_csv(compete_pred, "compete_preds_1.csv")

# Deploy for LDA - SAM
compete_pred_lda <- predict(model_lda, compete, type = "response")
prob_lda_compete <- compete_pred_lda$posterior[1:4326]
compete_pred_lda_final <- data.frame(ID = 1:4326,
                           probs = prob_lda_compete)
write_csv(compete_pred_lda_final, "compete_preds_2_lda.csv")

