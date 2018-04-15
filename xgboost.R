#library(mlr)
library(openxlsx)
library(data.table)
library(dplyr)
library(xgboost)
library(caret)
library(ggplot2)


###############################################################################
# Load data and prepare dataset
###############################################################################

# Load data
ru <- read.xlsx("Data/CollatedPneumoconiosisData-GE Internal.xlsx", 1, colNames = TRUE)
rm <- read.xlsx("Data/CollatedPneumoconiosisData-GE Internal.xlsx", 2, colNames = TRUE)
rl <- read.xlsx("Data/CollatedPneumoconiosisData-GE Internal.xlsx", 3, colNames = TRUE)
lu <- read.xlsx("Data/CollatedPneumoconiosisData-GE Internal.xlsx", 4, colNames = TRUE)
lm <- read.xlsx("Data/CollatedPneumoconiosisData-GE Internal.xlsx", 5, colNames = TRUE)
ll <- read.xlsx("Data/CollatedPneumoconiosisData-GE Internal.xlsx", 6, colNames = TRUE)

ru$Position <- "Right Upper"
rm$Position <- "Right Middle"
rl$Position <- "Right Lower"
lu$Position <- "Left Upper"
lm$Position <- "Left Middle"
ll$Position <- "Left Lower"

# Combine all sections into one dataframe
full <- rbind(ru, rm, rl, lu, lm, ll)

# Prepare data set
# Set levels of the factor - makes it easier to read
use_data <- full %>%
        mutate(Label = factor(recode(Label, '0' = 'Normal', '1' = 'Abnormal'))) %>%
        mutate(Position = factor(Position)) %>% 
        dplyr::select(-PatientNumMasked)

# Create training and test datasets
trainIndex <- createDataPartition(use_data$Label, p = 0.7, list = FALSE)
trainData <- use_data[trainIndex,]
testData <- use_data[-trainIndex,]

all_data <- use_data

# create data table data frames
setDT(all_data)
setDT(trainData) 
setDT(testData)

# using one hot encoding
labels_all <- all_data$Label
labels <- trainData$Label
ts_label <- testData$Label
new_all <- model.matrix(~.+0, data = all_data[,-c("Label"), with = F]) 
new_tr <- model.matrix(~.+0, data = trainData[,-c("Label"), with = F]) 
new_ts <- model.matrix(~.+0, data = testData[,-c("Label"), with = F])

# convert factor to numeric
labels_all <- as.numeric(labels_all) - 1
labels <- as.numeric(labels) - 1
ts_label <- as.numeric(ts_label) - 1

# preparing matrix 
adata <- xgb.DMatrix(data = new_all, label = labels_all) 
dtrain <- xgb.DMatrix(data = new_tr, label = labels) 
dtest <- xgb.DMatrix(data = new_ts, label = ts_label)

###############################################################################################
## XGradient Boost Models 
###############################################################################################

params <- list(booster = "gbtree"
               ,objective = "multi:softmax"
               ,num_class = 2
               ,eta = 0.3
               ,gamma = 0
               ,max_depth = 6
               ,min_child_weight = 1
               ,subsample = 1
               ,colsample_bytree = 1)

xgbcv <- xgb.cv(params = params
                ,data = dtrain
                ,nrounds = 200
                ,nfold = 10
                ,showsd = TRUE
                ,stratified = TRUE
                ,print_every_n = 10
                ,early_stopping_rounds = 20
                ,maximize = FALSE
                ,eval_metric = "mlogloss")

xgbcv$best_iteration

# first default - model training
xgb_ht <- xgb.train (params = params
                     ,data = dtrain
                     ,nrounds = 41
                     ,watchlist = list(val = dtest, train = dtrain)
                     ,print_every_n = 10
                     ,early_stopping_rounds = 10
                     ,maximize = F 
                     ,eval_metric = "mlogloss")

# model prediction
xgbpred_ht <- predict(xgb_ht, dtest)
#xgbpred <- ifelse (xgbpred > 0.5, 1, 0)

# confusion matrix
xgbcm_ht <- caret::confusionMatrix(xgbpred_ht, ts_label, mode = 'everything')
#Accuracy - 95.13% Kappa - 0.8942

# save models and results
saveRDS(xgb_ht, "xgb_ht.rds")
saveRDS(xgbpred_ht, "xgbpred_ht.rds")
xgb_htt <- readRDS("xgb_ht.rds")

# view variable importance plot
mat <- xgb.importance (feature_names = colnames(new_tr), model = xgb_ht)

ggplot(mat[1:10], aes(x = reorder(Feature, Gain), Gain)) +
        geom_bar(stat = "identity", aes(fill = Feature)) + 
        coord_flip() + 
        guides(fill=FALSE) + 
        ggtitle("XGBoost Model Variable Importance - Label") +
        xlab("Variables") +
        ylab("")

## predict on full data set 
# get results for full data set
xgb_pred_ht_all <- predict(xgb_ht, adata)

# confusion matrix
xgb_cm_ht_all <- caret::confusionMatrix(xgb_pred_ht_all, labels_all, mode = 'everything')

p <- xgb.plot.multi.trees(model = xgb_ht, 
                          features_keep = 3)
print(p)
