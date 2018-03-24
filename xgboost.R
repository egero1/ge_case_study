library(mlr)
library(data.table)
library(dplyr)
library(xgboost)
library(caret)




all_data <- use_data

# create data table data frames
setDT(all_data)
setDT(trainData) 
setDT(testData)

# using one hot encoding
labels_all <- all_data$Label2
labels <- trainData$Label2
ts_label <- testData$Label2
new_all <- model.matrix(~.+0, data = all_data[,-c("Label2"), with = F]) 
new_tr <- model.matrix(~.+0, data = trainData[,-c("Label2"), with = F]) 
new_ts <- model.matrix(~.+0, data = testData[,-c("Label2"), with = F])

# convert factor to numeric
labels_all <- as.numeric(labels_all) - 1
labels <- as.numeric(labels) - 1
ts_label <- as.numeric(ts_label) - 1

# preparing matrix 
adata <- xgb.DMatrix(data = new_all, label = labels_all) 
dtrain <- xgb.DMatrix(data = new_tr, label = labels) 
dtest <- xgb.DMatrix(data = new_ts, label = ts_label)

###############################################################################################
## XGradient Boost Models - Hospital Type classification
###############################################################################################

params <- list(booster = "gbtree"
               ,objective = "multi:softmax"
               ,num_class = 23
               ,eta = 0.3
               ,gamma = 0
               ,max_depth = 6
               ,min_child_weight = 1
               ,subsample = 1
               ,colsample_bytree = 1
)

xgbcv <- xgb.cv(params = params
                ,data = dtrain
                ,nrounds = 200
                ,nfold = 2606
                ,showsd = TRUE
                ,stratified = TRUE
                ,print_every_n = 10
                ,early_stopping_rounds = 20
                ,maximize = FALSE
                ,eval_metric = "mlogloss"
)

xgbcv$best_iteration

# first default - model training
xgb_ht <- xgb.train (params = params
                     ,data = dtrain
                     ,nrounds = 54
                     ,watchlist = list(val = dtest, train = dtrain)
                     ,print_every_n = 10
                     ,early_stopping_rounds = 10
                     ,maximize = F 
                     ,eval_metric = "mlogloss"
)

# model prediction
xgbpred_ht <- predict(xgb_ht, dtest)
#xgbpred <- ifelse (xgbpred > 0.5, 1, 0)

# confusion matrix
xgbcm_ht <- caret::confusionMatrix(xgbpred_ht, ts_label, mode = 'everything')
#Accuracy - 99.17% Kappa - 0.9852

# save models and results
saveRDS(xgb_ht, "xgb_ht.rds")
saveRDS(xgbpred_ht, "xgbpred_ht.rds")
xgb_htt <- readRDS("xgb_ht.rds")

library(ggplot)
# view variable importance plot
mat <- xgb.importance (feature_names = colnames(new_tr), model = xgb_ht)
xgb.plot.importance (importance_matrix = mat[1:20], main = "XGBoost Model Variable Importance - Label") 

ggplot(mat[1:10], aes(x = reorder(Feature, Gain), Gain)) +
        geom_bar(stat = "identity", aes(fill = Feature)) + 
        coord_flip() + 
        guides(fill=FALSE) + 
        ggtitle("XGBoost Model Variable Importance - Label") +
        xlab("Variables") +
        ylab("")

## predict on full data set for use in mobile app
# get results for full data set
# model prediction
xgb_pred_ht_all <- predict(xgb_ht, adata)

# confusion matrix
xgb_cm_ht_all <- caret::confusionMatrix(xgb_pred_ht_all, labels_all, mode = 'everything')
#Accuracy - 98 Kappa - 0.9566


# Convert predictions to hospital
xgb_pred_ht_all <- as.factor(xgb_pred_ht_all)
xgb_pred_ht_all <- factor(xgb_pred_ht_all, 
                          levels = c(0,1), 
                          labels = levels(use_data$Label2))

p <- xgb.plot.multi.trees(model = xgb_ht, 
                          features_keep = 3)
print(p)