library(tidyverse)

train <- read_csv("Data/final_train.csv")
mod_trivial <- train %>% 
  group_by(default) %>% 
  tally() %>% 
  mutate(proportion = n/sum(n)) %>% 
  .[2,c(1,3)]

pred_trivial <- train %>%
  pull()
