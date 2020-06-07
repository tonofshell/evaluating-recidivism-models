### MODELING SCRIPT ###

# Adapted from model_fitting.R from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VWDGHT

#### Setup ####

library(tidyverse)
library(cowpoke)
require(caret)
require(ggplot2)
require(pROC)
library(tictoc)
library(gbm)
library(here)
library(doParallel)
cl <- makePSOCKcluster(8)
registerDoParallel(cl)
set.seed(60615)

inline_if = function(test, yes, no) {
  if (test) {
    return(yes)
  }
  no
}

#### Load data ####
all_data = readRDS(here("cleaned_data.rds"))
discrim_vars = read_csv(here("names_to_keep.csv")) %>% filter(discrim) %>% .$value %>% .[. %in% names(all_data)]
outcome_vars = read_csv(here("names_to_keep.csv")) %>% filter(outcome) %>% .$value %>% .[. %in% names(all_data)]

fta_all_data = all_data %>% select(-YEARSEQ, -outcome_vars[outcome_vars != "FTA1"]) %>% filter(FTA1 != "Not applicable") %>% mutate(FTA_OUT = FTA1 == "Yes, FTA" ) %>% select(-FTA1)
fta_fair_data = fta_all_data %>% select(-all_of(discrim_vars))

rm(all_data)

#### Create Model Function ####

# data_set = fta_all_data
# outcome_name = "FTA_OUT"

train_gbm = function(data_set, outcome_name) {
  test_indices = createDataPartition(data_set[[outcome_name]], p = 0.25, list = FALSE)
  out = list()
  out$testing_set = data_set[test_indices, ]
  out$training_set = data_set[-test_indices, ]
  options(na.action = 'na.pass')
  train_mat = model.matrix(~., out$training_set) %>% .[, -1]
  colnames(train_mat) = make.names(colnames(train_mat))
  test_mat = model.matrix(~., out$testing_set) %>% .[, -1]
  colnames(test_mat) = make.names(colnames(test_mat))
  outcome_index = data_set[[outcome_name]] %>% is.logical() %>% inline_if(colnames(train_mat) == paste0(outcome_name, "TRUE"), colnames(train_mat) == outcome_name) %>% which()
  
  ctrl = trainControl(method = 'cv', number = 5, classProbs = T, returnData = F)
  trgrid = expand.grid(n.trees = c(1:25 *100), interaction.depth = c(2,3,4), shrinkage = c(.025, .05, .1),  n.minobsinnode = 20)
  tic()
  out$train_fit = train(x = train_mat[, -outcome_index],
                    y = ifelse(train_mat[, outcome_index], "Yes", "No"),
                    method = 'gbm',
                    trControl = ctrl,
                    tuneGrid = trgrid)
  toc()
  out$train_preds = predict(out$train_fit, newdata = train_mat, type = "prob") %>% select(Yes) %>% rename("train_prob" = Yes)
  out$test_preds = predict(out$train_fit, newdata = test_mat, type = "prob") %>% select(Yes) %>% rename("test_prob" = Yes)
  out
}

#### Fit and Save Models ####

sampled_fair = fta_fair_data %>% sample_n(5000) %>% train_gbm("FTA_OUT")
saveRDS(sampled_fair, here("sampled_fair_model.rds"))
rm(sampled_fair)

sampled_all = fta_all_data %>% sample_n(5000) %>% train_gbm("FTA_OUT")
saveRDS(sampled_all, here("sampled_all_model.rds"))
rm(sampled_all)

full_fair = fta_fair_data %>% train_gbm("FTA_OUT")
saveRDS(full_fair, here("full_fair_model.rds"))
rm(full_fair)

full_all = fta_all_data %>% train_gbm("FTA_OUT")
saveRDS(full_all, here("full_all_model.rds"))
rm(full_all)

# test = fta_all_data %>% sample_n(250) %>% train_gbm("FTA_OUT")
# saveRDS(test, here("test.rds"))
# roc(as.numeric(test$testing_set$FTA_OUT), as.numeric(test$test_preds$test_prob > 0.5))
# varImp(test$train_fit)

stopCluster(cl)