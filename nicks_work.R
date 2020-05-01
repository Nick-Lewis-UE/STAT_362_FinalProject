# I'm just playing around a little, investigating and such

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

set.seed(362)

######
# Data
######

data <- read_csv("Data/final_train.csv", col_types = "dfffdddddddddddddf")
compete <- read_csv("Data/final_compete.csv", col_types = "ddfffddddddddddddd")

train <- data %>%
  sample_frac(.7)
test <- data %>%
  setdiff(train)

###################
# EDA (plot matrix)
###################

plot_matrix <- ggpairs(train)

ggplot(train) +
  geom_density(mapping = aes(log(limit_bal)))
plot_matrix[1,1]
plot_matrix[2,2]
plot_matrix[3,3]
plot_matrix[4,4]
plot_matrix[5,5]
ggplot(train) +
  geom_density(mapping = aes(log(age)))
plot_matrix[6,6]
ggplot(train) +
  geom_density(mapping = aes(log(bill_amt2)))
plot_matrix[7,7]
ggplot(train) +
  geom_density(mapping = aes(log(bill_amt2)))
plot_matrix[8,8]
ggplot(train) +
  geom_density(mapping = aes(log(bill_amt3)))
plot_matrix[9,9]
ggplot(train) +
  geom_density(mapping = aes(log(bill_amt4)))
plot_matrix[10,10]
ggplot(train) +
  geom_density(mapping = aes(log(bill_amt5)))
plot_matrix[11,11]
ggplot(train) +
  geom_density(mapping = aes(log(bill_amt6)))
plot_matrix[12,12]
ggplot(train) +
  geom_density(mapping = aes(log(pay_amt1)))
plot_matrix[13,13]
ggplot(train) +
  geom_density(mapping = aes(log(pay_amt2)))
plot_matrix[14,14]
ggplot(train) +
  geom_density(mapping = aes(log(pay_amt3)))
plot_matrix[15,15]
ggplot(train) +
  geom_density(mapping = aes(log(pay_amt4)))
plot_matrix[16,16]
ggplot(train) +
  geom_density(mapping = aes(log(pay_amt5)))
plot_matrix[17,17]
ggplot(train) +
  geom_density(mapping = aes(log(pay_amt6)))
plot_matrix[18,18]

for (i in 1:18) {
  for (j in 1:18) {
    print(plot_matrix[i,j])
  }
}

###########
# LASSO/GLM
###########

x_train <- model.matrix(default ~ ., train)[,-1]
y_train <- train$default
x_test <- model.matrix(default ~ ., test)[,-1]
y_test <- test$default
lasso <- cv.glmnet(x_train, y_train, alpha = 1, family = "binomial")
best_lambda <- lasso$lambda.min

l_range <- 10^seq(10, -2, length = 100)
model_lasso <- glmnet(x_train, y_train, alpha = 1, family = "binomial",
                lambda = l_range)
pred_lasso <- predict(model_lasso, s = best_lambda, newx = x_test, type = "response")
pred_lasso <- ifelse(pred_lasso >= .5, 1, 0)
lasso_coef <- predict(model_lasso, type = 'coefficients', s = best_lambda)

# This model predicts all 1s, and i don't know why
sum(pred_lasso != 1) # 0


###############
# Random Forest
###############

model_rf <- randomForest(default ~ ., data = train, mtry = 4, ntree = 25, importance = TRUE)
pred_rf <- predict(model_rf, test)
confusionMatrix(pred_rf, test$default)
plot(model_rf)

# Overall Accuracy of .7985

model_rf_2 <- randomForest(default ~ ., data = train, mtry = 4, ntree = 250, importance = TRUE)

model_rf_3 <- randomForest(default ~ ., data = train, mtry = 4, ntree = 50, importance = TRUE)
pred_rf_3 <- predict(model_rf_3, test)
confusionMatrix(pred_rf_3, test$default)

# Overall Accuracy of .8007
pred_rf_vec = vector(length = 17)
for (i in 1:17) {
  mod <- randomForest(default ~ ., data = train, mtry = i, ntree = 50, importance = TRUE)
  pred_rf_vec[i] <- mean(predict(mod, test) == test$default)
}

# Best Accuracy 0.8043597 - not significantly different




#########
# Bagging
#########

model_bag <- randomForest(default ~ ., data = train, mtry = 17, ntree = 25, importance = TRUE)
pred_bag <- predict(model_bag, test)
confusionMatrix(pred_bag, test$default)

# Overall Accuracy of .7978

model_bag_2 <- randomForest(default ~ ., data = train, mtry = 17, ntree = 250, importance = TRUE)
model_bag_3 <- randomForest(default ~ ., data = train, mtry = 17, ntree = 50, importance = TRUE)
pred_bag_3 <- predict(model_bag_3, test)
confusionMatrix(pred_bag_3, test$default)

# Overall Accuracy 0.8029 

##########
# Boosting
##########

train_2 <- train %>%
  mutate(default = as.character(default))
test_2 <- test %>%
  mutate(default = as.character(default))

model_boost_1 <- gbm(default ~ ., data = train_2, distribution = "bernoulli", shrinkage = .01, 
                   n.trees = 2000, interaction.depth = 4)
pred_boost_1 <- predict(model_boost, newdata = test_2, n.trees = 2000, type = "response")
pred_boost_1 <- ifelse(pred_boost >= .5, 1, 0)
confusionMatrix(as.factor(pred_boost_1), test$default)

# Overall Accuracy of .8074

model_boost_2 <- gbm(default ~ ., data = train_2, distribution = "bernoulli", shrinkage = .01, 
                     n.trees = 3000, interaction.depth = 4)
pred_boost_2 <- predict(model_boost, newdata = test_2, n.trees = 2000, type = "response")
pred_boost_2 <- ifelse(pred_boost >= .5, 1, 0)
confusionMatrix(as.factor(pred_boost_2), test$default)

# Same Accuracy

##########
# Caret CV
##########

train_partition <- createDataPartition(y = data$default, p=.7, list = FALSE)
train <- data[train_partition,]
test <- data[-train_partition,]

train_control_kfold <- trainControl(method = "cv", number = 10) # WARNING: WILL TAKE LONG

model_kfold_rf <- train(default ~ ., data = train, trControl = train_control_kfold, method = "rf")
mean(predict(model_kfold_rf, test) == test$default)

# Conclusion - not much different from models above

#################
# Transformations
#################

data_t <- data %>%
  mutate(limit_bal = log(limit_bal),
         age = log(age),
         # bill_amt1 = log(bill_amt1),
         # bill_amt2 = log(bill_amt2),
         # bill_amt3 = log(bill_amt3),
         # bill_amt4 = log(bill_amt4),
         # bill_amt5 = log(bill_amt5),
         # bill_amt6 = log(bill_amt6),
         pay_amt1 = ifelse(pay_amt1 != 0, log(pay_amt1), log(.01)),
         pay_amt2 = ifelse(pay_amt2 != 0, log(pay_amt2), log(.01)),
         pay_amt3 = ifelse(pay_amt3 != 0, log(pay_amt3), log(.01)),
         pay_amt4 = ifelse(pay_amt4 != 0, log(pay_amt4), log(.01)),
         pay_amt5 = ifelse(pay_amt5 != 0, log(pay_amt5), log(.01)),
         pay_amt6 = ifelse(pay_amt6 != 0, log(pay_amt6), log(.01)))

train_partition_t <- createDataPartition(y = data_t$default, p=.7, list = FALSE)
train_t <- data[train_partition_t,]
test_t <- data[-train_partition_t,]

##################################
# Random Forest w/ Transformations
##################################

model_rf_t_1 <- randomForest(default ~ ., data = train_t, mtry = 4, ntree = 250, importance = TRUE)
model_rf_t_2 <- randomForest(default ~ ., data = train_t, mtry = 4, ntree = 30, importance = TRUE)
for (i in 1:17) {
  mod <- randomForest(default ~ ., data = train_t, mtry = i, ntree = 30, importance = TRUE)
  pred_rf_vec[i] <- mean(predict(mod, test_t) == test_t$default)
}

########################
# LDA w/ Transformations
########################

model_lda_t <- lda(default ~ ., data = train_t)
pred_lda_t <- predict(model_lda_t, test_t)
confusionMatrix(pred_lda_t$class, test_t$default)

model_qda_t <- qda(default ~ ., data = train_t)
pred_qda_t <- predict(model_qda_t, test_t)
pred_lda_t$posterior
confusionMatrix(pred_qda_t$class, test_t$default)

################################
# GLM w/ LASSO & Transformations
################################

x_train_t <- model.matrix(default ~ ., train_t)[,-1]
y_train_t <- train_t$default
x_test_t <- model.matrix(default ~ ., test_t)[,-1]
y_test_t <- test_t$default
lasso_t <- cv.glmnet(x_train_t, y_train_t, alpha = 1, family = "binomial")
best_lambda_t <- lasso_t$lambda.min

l_range <- 10^seq(10, -2, length = 100)
model_lasso_t <- glmnet(x_train_t, y_train_t, alpha = 1, family = "binomial",
                      lambda = l_range)
pred_lasso_t <- predict(model_lasso_t, s = best_lambda_t, newx = x_test_t, type = "response")
pred_lasso_t <- ifelse(pred_lasso_t >= .5, 1, 0)
lasso_coef <- predict(model_lasso, type = 'coefficients', s = best_lambda)

# This model predicts all 1s, and i don't know why
sum(pred_lasso_t != 1) # 0

############
# downSample
############

down_train <- downSample(train, train$default)[-19]

##################
# RF w/ downSample
##################

model_down_rf_1 <- randomForest(default ~ ., data = down_train, mtry = 4, ntree = 250, importance = TRUE)

model_down_rf_2 <- randomForest(default ~ ., data = down_train, mtry = 4, ntree = 75, importance = TRUE)
pred_down_rf_2 <- predict(model_down_rf_2, test)
confusionMatrix(pred_down_rf_2, test$default)

pred_rf_vec = vector(length = 17)
for (i in 1:17) {
  mod <- randomForest(default ~ ., data = down_train, mtry = i, ntree = 75, importance = TRUE)
  pred_rf_vec[i] <- mean(predict(mod, test) == test$default)
}

######################
# Create New Variables
######################

data_n <- data %>%
  mutate(perc_paid1 = pay_amt1/bill_amt1,
         perc_paid2 = pay_amt2/bill_amt2,
         perc_paid3 = pay_amt3/bill_amt3,
         perc_paid4 = pay_amt4/bill_amt4,
         perc_paid5 = pay_amt5/bill_amt5,
         perc_paid6 = pay_amt6/bill_amt6) %>%
  filter(!is.na(perc_paid1) &
           !is.na(perc_paid2) &
           !is.na(perc_paid3) &
           !is.na(perc_paid4) &
           !is.na(perc_paid5) &
           !is.na(perc_paid6) &
           !is.infinite(perc_paid1) &
           !is.infinite(perc_paid2) &
           !is.infinite(perc_paid3) &
           !is.infinite(perc_paid4) &
           !is.infinite(perc_paid5) &
           !is.infinite(perc_paid6))

ggplot(data_n) +
  geom_boxplot(mapping = aes(y = default, x = perc_paid6)) +
  xlim(0,2.5)

train_partition_n <- createDataPartition(y = data_n$default, p=.7, list = FALSE)
train_n <- data_n[train_partition_n,]
test_n <- data_n[-train_partition_n,]

model_rf <- randomForest(default ~ ., data = train_n, mtry = 4, ntree = 250, importance = TRUE)
model_rf_2 <- randomForest(default ~ ., data = train_n, mtry = 4, ntree = 25, importance = TRUE)
rf_n_pred <- predict(model_rf_2, test_n)
confusionMatrix(rf_n_pred, test_n$default)

ggplot(data_n) +
  geom_density(mapping = aes(x = perc_paid6)) +
  xlim(-10,10)
