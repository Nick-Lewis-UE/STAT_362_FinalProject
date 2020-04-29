# I'm just playing around a little, investigating and such
library(tidyverse)
library(GGally)
library(caret)
library(modelr)
library(glmnet)

set.seed(362)

data <- read_csv("Data/final_train.csv", col_types = "dfffdddddddddddddf")
compete <- read_csv("Data/final_compete.csv", col_types = "ddfffddddddddddddd")

train <- data %>%
  sample_frac(.7)
test <- data %>%
  setdiff(train)

plot_matrix <- ggpairs(train)

plot_matrix[1,1]
plot_matrix[2,2]
plot_matrix[3,3]
plot_matrix[4,4]
plot_matrix[5,5]
plot_matrix[6,6]
plot_matrix[7,7]
plot_matrix[8,8]
plot_matrix[9,9]
plot_matrix[10,10]
plot_matrix[11,11]
plot_matrix[12,12]
plot_matrix[13,13]
plot_matrix[14,14]
plot_matrix[15,15]
plot_matrix[16,16]
plot_matrix[17,17]
plot_matrix[18,18]

x_train <- model.matrix(default ~ ., train)[,-1]
y_train <- train$default
x_test <- model.matrix(default ~ ., test)[,-1]
y_test <- test$default
lasso <- cv.glmnet(x_train, y_train, alpha = 0, family = "binomial")
best_lambda <- lasso$lambda.min

model_lasso <- glmnet(x_train, y_train, alpha = 0, family = "binomial",
                lambda = best_lambda)
pred_lasso <- predict(model_lasso, newx = x_test, type = "response")
lasso_coef <- predict(model_lasso, type = 'coefficients', s = best_lambda)

# Model accuracy
observed.classes <- test.data$diabetes
mean(predicted.classes == observed.classes)
