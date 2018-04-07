## Eric Gero
## eric.gero@ge.com

setwd("~/Documents/R_Projects/GE_Case_Study")

# Define functions

# Needed to load rJava
# dyn.load(paste0(system2('/usr/libexec/java_home', stdout = TRUE), '/lib/server/libjvm.dylib'))

# Load libraries
library(openxlsx)
library(dplyr)
library(caret)
library(randomForest)
library(ggplot2)
library(corrplot)
library(fBasics)
library(rpart)
library(rpart.plot)
library(pROC)
library(klaR)
library(gridExtra)
library(parallel)
library(doParallel)

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

###############################################################################
# Exploratory Data Analysis
###############################################################################

# Count of individual patients
length(unique(full$PatientNumMasked))

# Count of observations by segment
observations <- use_data %>% 
        group_by(Position) %>%
        summarize(Count = n())
observations$Position <- as.character(observations$Position)
observations <- rbind(observations, c('Total', sum(observations$Count)))

# Summary 
stats <- basicStats(use_data[-c(40,41)])[c("Mean", "Median", "Stdev", "Minimum", "Maximum", "NAs"),]
t(round(stats, 2))

# Verify that all labels for patients are the same
checklabel <- full %>%
        group_by(PatientNumMasked) %>%
        mutate(minLabel = min(Label)) %>%
        mutate(maxLabel = max(Label)) %>%
        mutate(count = length(PatientNumMasked)) %>%
        dplyr::select(PatientNumMasked, minLabel, maxLabel, count) 

# If the min and max are equal, they will be dropped.  If list is empty
# then the label is consistent across data sets for all patients
checklabel <- checklabel %>%
        dplyr::filter(minLabel != maxLabel)

checklabel <- ifelse(dim(checklabel)[1] == 0, 'no', dim(checklabel)[1])

# Correlation between predictors
corr <- cor(use_data[-c(40,41)])
corrplot(corr, type = "upper", tl.cex = 0.5, tl.col = "blue", tl.srt = 45)

# Review and remove highly correlated predictors
correlationMatrix <- cor(use_data[-c(40,41)])
highCorrelation <- findCorrelation(correlationMatrix, cutoff = 0.50)
rows <- length(colnames(use_data[highCorrelation]))
highly_correlated <- data.frame("Correlated Variables 1" = colnames(use_data[highCorrelation])[1:(rows / 2)], 
                                "Correlated Variables 2" = colnames(use_data[highCorrelation])[(rows/2 + 1):rows])

use_data_lc <- use_data[,-highCorrelation]
low_corr <- use_data_lc %>%
        dplyr::select(-Position, -Label)
low_corr<- cor(low_corr)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(low_corr, method = "color", col = col(200),
         type = "upper", order = "hclust", number.cex = .7,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "blue", tl.srt = 45, tl.cex = 0.5, # Text label color and rotation
         # Combine with significance
         sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag = FALSE)

# Scatter plot between predictors; scatter plot for response is pointless since it is categorical
pairs(Label ~ ., data = use_data[c(1,41)])

# Check for outliers - Boxplots
plots <- list(0)
for(var in 3:4) {
        p <- ggplot(data = use_data, aes(x = '', y = use_data[,var])) + 
                geom_boxplot(color='red', fill='orange', alpha = 0.2) +
                labs(title = paste('Outliers - ', colnames(use_data[var]))) +
                ylab('Boxplot') +
                xlab('') + 
        coord_cartesian(ylim = c(min(use_data[,var]), max(use_data[,var])))
        plots[[var-2]] <- p
}

grid.arrange(plots[[1]], plots[[2]], ncol = 2)

###############################################################################
# Exploratory Model and Important Variables
###############################################################################

tree <- rpart(Label ~., data = use_data)
tree.pred <- predict(tree, use_data, type = 'class')
tree.cm <- confusionMatrix(tree.pred, use_data$Label, positive = 'Abnormal')

tree.combined <- data.frame(c(tree.cm$overall, tree.cm$byClass))

#scaled <- scale(use_data[-c(40,41)])
#for(var in 1:length(use_data[-c(40, 41)])) {
        
#        boxplot(scaled[var], main = colnames(scaled[var]))
#}
#scaled <- scale(use_data[-c(40,41)])
#outlierKD(use_data, Hist_0_0_0_Mean)

rpart.plot(tree)

tree.pred <- predict(tree, use_data)
tree.ROC <- roc(use_data$Label, tree.pred[,1])
plot(tree.ROC, col = 'blue', main = paste('Area under the curve (AUC):', round(auc(tree.ROC),2)))
auc(tree.ROC)

# Review the dataset
summary(use_data)

# Review the evaluation metrics
tree.cm$overall

###############################################################################
# Feature selection using Recursive Feature Elimination or RFE
# https://www.analyticsvidhya.com/blog/2016/12/practical-guide-to-implement-machine-learning-with-caret-package-in-r-with-practice-problem/
###############################################################################

# Enable parallel processing and reserve resources
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

set.seed(1234)
control <- rfeControl(functions = rfFuncs
                      ,method = "repeatedcv"
                      ,number = 5
                      ,verbose = FALSE
                      ,allowParallel = TRUE)

pref_variables <- rfe(use_data[-40], use_data[,40], rfeControl = control)

# Print the results
pref_variables

# List the variables
predictors(pref_variables)

# Disable parallel processing and release resources
stopCluster(cluster)
registerDoSEQ()

plot(pref_variables, type = c("g", "o"))

# Create a data set using the preferred variables
model_data <- use_data %>%
        dplyr::select(predictors(pref_variables), Label)

predictors_md <- as.data.frame(colnames(model_data[-17]))
predictors_lc <- as.data.frame(colnames(use_data_lc[-12]))
colnames(predictors_md) <- "Backwards Selection"
colnames(predictors_lc) <- "Low Correlation"
common_predictors <- as.data.frame(intersect(predictors_md$`Backwards Selection`, predictors_lc$`Low Correlation`))
colnames(common_predictors) <- "Common Predictors"

###############################################################################
# Modeling
###############################################################################

# Create a data frame to hold the results
model_results <- data.frame(Model = character()
                          ,Data = character()
                          ,ROC = numeric()
                          ,Accuracy = numeric()
                          ,Kappa = numeric()
                          ,F1 = numeric()
                          ,Sensitivity = numeric()
                          ,Specificity = numeric())

###############################################################################
# GLM - LOOCV
# http://www.rebeccabarter.com/blog/2017-11-17-caret_tutorial/
# https://www.analyticsvidhya.com/blog/2016/12/practical-guide-to-implement-machine-learning-with-caret-package-in-r-with-practice-problem/
# https://www.analyticsvidhya.com/blog/2016/12/practical-guide-to-implement-machine-learning-with-caret-package-in-r-with-practice-problem/
###############################################################################

# Enable parallel processing and reserve resources
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

# Set up training conditions - must use LOOCV
fitControl <- trainControl(method = "LOOCV"
                           ,classProbs = TRUE 
                           ,summaryFunction = twoClassSummary
                           ,savePredictions = 'final')

# Full data set
set.seed(1234)
glm.model.ud <- caret::train(Label ~.
                          ,data = use_data
                          ,family = 'binomial'
                          ,method = 'glm'
                          ,trControl = fitControl
                          ,metric = "ROC")

# Get the confusion matrix
glm.cm.ud <- caret::confusionMatrix(glm.model.ud$pred$pred, glm.model.ud$pred$obs, mode = "everything")

# Get the performance metrics from the model and save for comparison
performance <- getTrainPerf(glm.model.ud)
model_results <- rbind(model_results
                        ,data.frame(Model = 'GLM'
                        ,Data = 'Full'
                        ,ROC = performance[,1]
                        ,Accuracy = glm.cm.ud$overall[1]
                        ,Kappa = glm.cm.ud$overall[2]
                        ,F1 = glm.cm.ud$byClass[7]
                        ,Sensitivity = performance[,2]
                        ,Specificity = performance[,3]))

# RFE selection data set
set.seed(1234)
glm.model.md <- caret::train(Label ~.
                          ,data = model_data
                          ,family = 'binomial'
                          ,method = 'glm'
                          ,trControl = fitControl
                          ,metric = "ROC")

# Get the confusion matrix
glm.cm.md <- caret::confusionMatrix(glm.model.md$pred$pred, glm.model.md$pred$obs, mode = "everything")

# Get the performance metrics from the model and save for comparison
performance <- getTrainPerf(glm.model.md)
model_results <- rbind(model_results
                        ,data.frame(Model = 'GLM'
                        ,Data = 'RFE Selection'
                        ,ROC = performance[,1]
                        ,Accuracy = glm.cm.md$overall[1]
                        ,Kappa = glm.cm.md$overall[2]
                        ,F1 = glm.cm.md$byClass[7]
                        ,Sensitivity = performance[,2]
                        ,Specificity = performance[,3]))

# Low correlation data set
set.seed(1234)
glm.model.lc <- caret::train(Label ~.
                          ,data = use_data_lc
                          ,family = 'binomial'
                          ,method = 'glm'
                          ,trControl = fitControl
                          ,metric = "ROC")

# Get the confusion matrix
glm.cm.lc <- caret::confusionMatrix(glm.model.lc$pred$pred, glm.model.lc$pred$obs, mode = "everything")

# Get the performance metrics from the model and save for comparison
performance <- getTrainPerf(glm.model.lc)
model_results <- rbind(model_results
                        ,data.frame(Model = 'GLM'
                        ,Data = 'Low Correlation'
                        ,ROC = performance[,1]
                        ,Accuracy = glm.cm.lc$overall[1]
                        ,Kappa = glm.cm.lc$overall[2]
                        ,F1 = glm.cm.lc$byClass[7]
                        ,Sensitivity = performance[,2]
                        ,Specificity = performance[,3]))

rownames(model_results) <- NULL

# Disable parallel processing and release resources
stopCluster(cluster)
registerDoSEQ()

glm.ROC <- roc(glm.model.ud$pred$obs, glm.model.ud$pred$Normal)
plot(glm.ROC, col = 'blue', main = paste('GLM - Area under the curve (AUC):', round(auc(glm.ROC),2)))

# Print model coefficients
coefficients <- as.data.frame(glm.model.ud$finalModel$coefficients)
coefficients

# Save models in case we want to review them later
saveRDS(model_results, "final_results.rds")
saveRDS(glm.model.ud, "Models/glm_use_data.rds")
saveRDS(glm.model.md, "Models/glm_model_data.rds")
saveRDS(glm.model.lc, "Models/glm_use_data_lc.rds")

###############################################################################
# SVM - LOOCV Caret
# https://stats.stackexchange.com/questions/136274/leave-one-subject-out-cv-method
###############################################################################

# Enable parallel processing and reserve resources
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)


# Set up training conditions - must use LOOCV
fitControl <- trainControl(method = "LOOCV"
                           ,classProbs = TRUE 
                           ,summaryFunction = twoClassSummary
                           ,savePredictions = 'final')

# Full data set
set.seed(1234)
svm.model.ud <- caret::train(Label ~ .
                          ,data = use_data 
                          ,method = "svmRadial"
                          ,trControl = fitControl
                          ,metric = "Accuracy")

# Get the confusion matrix
svm.cm.ud <- caret::confusionMatrix(svm.model.ud$pred$pred, svm.model.ud$pred$obs, mode = "everything")

# Get the performance metrics from the model and save for comparison
performance <- getTrainPerf(svm.model.ud)
model_results <- rbind(model_results
                        ,data.frame(Model = 'SVM'
                        ,Data = 'Full'
                        ,ROC = performance[,1]
                        ,Accuracy = svm.cm.ud$overall[1]
                        ,Kappa = svm.cm.ud$overall[2]
                        ,F1 = svm.cm.ud$byClass[7]
                        ,Sensitivity = performance[,2]
                        ,Specificity = performance[,3]))

# RFE selection data set
set.seed(1234)
svm.model.md <- caret::train(Label ~ .
                          ,data = model_data
                          ,method = "svmRadial"
                          ,trControl = fitControl
                          ,metric = "Accuracy")

# Get the confusion matrix
svm.cm.md <- caret::confusionMatrix(svm.model.md$pred$pred, svm.model.md$pred$obs, mode = "everything")

# Get the performance metrics from the model and save for comparison
performance <- getTrainPerf(svm.model.md)
model_results <- rbind(model_results
                        ,data.frame(Model = 'SVM'
                        ,Data = 'RFE Selection'
                        ,ROC = performance[,1]
                        ,Accuracy = svm.cm.md$overall[1]
                        ,Kappa = svm.cm.md$overall[2]
                        ,F1 = svm.cm.md$byClass[7]
                        ,Sensitivity = performance[,2]
                        ,Specificity = performance[,3]))

# Low correlation data set
set.seed(1234)
svm.model.lc <- caret::train(Label ~ .
                           ,data = use_data_lc
                           ,method = "svmRadial"
                           ,trControl = fitControl
                           ,metric = "Accuracy")

# Get the confusion matrix
svm.cm.lc <- caret::confusionMatrix(svm.model.lc$pred$pred, svm.model.lc$pred$obs, mode = "everything")

# Get the performance metrics from the model and save for comparison
performance <- getTrainPerf(svm.model.lc)
model_results <- rbind(model_results
                        ,data.frame(Model = 'SVM'
                        ,Data = 'Low Correlation'
                        ,ROC = performance[,1]
                        ,Accuracy = svm.cm.lc$overall[1]
                        ,Kappa = svm.cm.lc$overall[2]
                        ,F1 = svm.cm.lc$byClass[7]
                        ,Sensitivity = performance[,2]
                        ,Specificity = performance[,3]))

rownames(model_results) <- NULL

# Disable parallel processing and release resources
stopCluster(cluster)
registerDoSEQ()

svm.ROC <- roc(svm.model.ud$pred$obs, svm.model.ud$pred$Normal)
plot(svm.ROC, col = 'blue', main = paste('SVM - Area under the curve (AUC):', round(auc(svm.ROC),2)))

# Save models in case we want to review them later
saveRDS(model_results, "final_results.rds")
saveRDS(svm.model.ud, "Models/svm_use_data.rds")
saveRDS(svm.model.md, "Models/svm_model_data.rds")
saveRDS(svm.model.lc, "Models/svm_use_data_lc.rds")

###############################################################################
# kNN - LOOCV Caret
###############################################################################

# Enable parallel processing and reserve resources
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

# Set up training conditions - must use LOOCV
fitControl <- trainControl(method = "LOOCV"
                           ,classProbs = TRUE 
                           ,summaryFunction = twoClassSummary
                           ,savePredictions = 'final')

# Full data set
set.seed(1234)
knn.model.ud <- caret::train(Label ~ .
                          ,data = use_data
                          ,method = "knn"
                          ,trControl = fitControl
                          ,metric = "ROC" 
                          ,preProc = c("center", "scale")
                          ,tuneLength = 20)

# Get the confusion matrix
knn.cm.ud <- caret::confusionMatrix(knn.model.ud$pred$pred, knn.model.ud$pred$obs, mode = "everything")

# Get the performance metrics from the model and save for comparison
performance <- getTrainPerf(knn.model.ud)
model_results <- rbind(model_results
                        ,data.frame(Model = 'kNN'
                        ,Data = 'Full'
                        ,ROC = performance[,1]
                        ,Accuracy = knn.cm.ud$overall[1]
                        ,Kappa = knn.cm.ud$overall[2]
                        ,F1 = knn.cm.ud$byClass[7]
                        ,Sensitivity = performance[,2]
                        ,Specificity = performance[,3]))

# RFE selection data set
set.seed(1234)
knn.model.md <- caret::train(Label ~ .
                             ,data = model_data
                             ,method = "knn"
                             ,trControl = fitControl
                             ,metric = "ROC" 
                             ,preProc = c("center", "scale")
                             ,tuneLength = 20)

# Get the confusion matrix
knn.cm.md <- caret::confusionMatrix(knn.model.md$pred$pred, knn.model.md$pred$obs, mode = "everything")

# Get the performance metrics from the model and save for comparison
performance <- getTrainPerf(knn.model.md)
model_results <- rbind(model_results
                        ,data.frame(Model = 'kNN'
                        ,Data = 'RFE Selection'
                        ,ROC = performance[,1]
                        ,Accuracy = knn.cm.md$overall[1]
                        ,Kappa = knn.cm.md$overall[2]
                        ,F1 = knn.cm.md$byClass[7]
                        ,Sensitivity = performance[,2]
                        ,Specificity = performance[,3]))

# Low correlation data set
set.seed(1234)
knn.model.lc <- caret::train(Label ~ .
                             ,data = use_data_lc
                             ,method = "knn"
                             ,trControl = fitControl
                             ,metric = "ROC" 
                             ,preProc = c("center", "scale")
                             ,tuneLength = 20)

# Get the confusion matrix
knn.cm.lc <- caret::confusionMatrix(knn.model.lc$pred$pred, knn.model.lc$pred$obs, mode = "everything")

# Get the performance metrics from the model and save for comparison
performance <- getTrainPerf(knn.model.lc)
model_results <- rbind(model_results
                        ,data.frame(Model = 'kNN'
                        ,Data = 'Low Correlation'
                        ,ROC = performance[,1]
                        ,Accuracy = knn.cm.lc$overall[1]
                        ,Kappa = knn.cm.lc$overall[2]
                        ,F1 = knn.cm.lc$byClass[7]
                        ,Sensitivity = performance[,2]
                        ,Specificity = performance[,3]))

rownames(model_results) <- NULL

# Disable parallel processing and release resources
stopCluster(cluster)
registerDoSEQ()

knn.ROC <- roc(knn.model.md$pred$obs, knn.model.md$pred$Normal)
plot(knn.ROC, col = 'blue', main = paste('SVM - Area under the curve (AUC):', round(auc(knn.ROC),2)))

# Save models in case we want to review them later
saveRDS(model_results, "final_results.rds")
saveRDS(knn.model.ud, "Models/knn_use_data.rds")
saveRDS(knn.model.md, "Models/knn_model_data.rds")
saveRDS(knn.model.lc, "Models/knn_use_data_lc.rds")

###############################################################################
# Naive Bayes- LOOCV Caret
###############################################################################

train_data <- use_data

# Set up training conditions - must use LOOCV
fitControl <- trainControl(method = "LOOCV"
                           ,classProbs = TRUE 
                           ,summaryFunction = twoClassSummary
                           ,savePredictions = 'final')

# Third model is a k-Nearest Neighbor
set.seed(1234)
nb.model <- caret::train(Label2 ~ .
                         ,data = train_data 
                         ,method = "nb"
                         ,trControl = fitControl
                         ,metric = "ROC")

# Get the confusion matrix
nb.cm <- caret::confusionMatrix(nb.model$pred$pred, nb.model$pred$obs, mode = "everything")

# Get the performance metrics from the model and save for comparison
performance <- getTrainPerf(nb.model)
nb_results <- data.frame("Model" = "Naive Bayes"
                         ,"Data" = "model_data"
                         ,"ROC" = performance[,1]
                         ,"Accuracy" = nb.cm$overall[1]
                         ,"Kappa" = nb.cm$overall[2]
                         ,"Sensitivity" = performance[,2]
                         ,"Specificity" = performance[,3])

nb.ROC <- roc(nb.model$pred$obs, nb.model$pred$Normal)
plot(nb.ROC, col = "blue")
auc(nb.ROC)

# Add model results to dataframe for comparison
final_results <- rbind(final_results, nb_results)

# Save models in case we want to review them later
saveRDS(final_results, "final_results.rds")
saveRDS(nb.model, "Models/nb_use_data.rds")
saveRDS(nb.model, "Models/nb_use_data_lc.rds")
saveRDS(nb.model, "Models/nb_model_data.rds")

###############################################################################
# Random Forest - LOOCV  91.55 Acc 0.8111 Kappa
# https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
# https://www.analyticsvidhya.com/blog/2016/12/practical-guide-to-implement-machine-learning-with-caret-package-in-r-with-practice-problem/
###############################################################################


cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

train_data <- use_data_lc

# Set up training conditions - must use LOOCV
fitControl <- trainControl(method = "LOOCV"
                           ,classProbs = TRUE 
                           ,summaryFunction = twoClassSummary
                           ,savePredictions = 'final'
                           ,allowParallel = TRUE)

# Third model is a k-Nearest Neighbor
set.seed(1234)
rf.model <- caret::train(Label2 ~.
            ,data = train_data
            ,method = 'rf'
            ,trControl = fitControl
            ,metric = "ROC")

stopCluster(cluster)
registerDoSEQ()

rf.cm <- caret::confusionMatrix(rf.model$pred$pred, rf.model$pred$obs)

# Get the performance metrics from the model and save for comparison
performance <- getTrainPerf(rf.model)
rf_results <- data.frame("Model" = "Random Forest"
                         ,"Data" = "use_data_lc"
                         ,"ROC" = performance[,1]
                         ,"Accuracy" = rf.cm$overall[1]
                         ,"Kappa" = rf.cm$overall[2]
                         ,"Sensitivity" = performance[,2]
                         ,"Specificity" = performance[,3])

rf.ROC <- roc(rf.model$pred$obs, rf.model$pred$Normal)
plot(rf.ROC, col = "blue")
auc(rf.ROC)

# Add model results to dataframe for comparison
final_results <- rbind(final_results, rf_results)

# Save models in case we want to review them later
saveRDS(final_results, "final_results.rds")
saveRDS(rf.model, "Models/rf_use_data.rds")
saveRDS(rf.model, "Models/rf_use_data_lc.rds")
saveRDS(rf.model, "Models/rf_model_data.rds")

###############################################################################
# Nueral Network - LOOCV  
# https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
# https://www.analyticsvidhya.com/blog/2016/12/practical-guide-to-implement-machine-learning-with-caret-package-in-r-with-practice-problem/
###############################################################################

library(parallel)
library(doParallel)
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

train_data <- model_data

# Set up training conditions - must use LOOCV
fitControl <- trainControl(method = "LOOCV"
                           ,classProbs = TRUE 
                           ,summaryFunction = twoClassSummary
                           ,savePredictions = 'final'
                           ,allowParallel = TRUE)

# Third model is a k-Nearest Neighbor
set.seed(1234)
nnet.model <- caret::train(Label2 ~.
                         ,data = train_data
                         ,method = 'nnet'
                         ,trControl = fitControl
                         ,metric = "ROC")

stopCluster(cluster)
registerDoSEQ()

nnet.cm <- caret::confusionMatrix(nnet.model$pred$pred, nnet.model$pred$obs)

# Get the performance metrics from the model and save for comparison
performance <- getTrainPerf(rf.model)
nnet_results <- data.frame("Model" = "Random Forest"
                         ,"Data" = "model_data"
                         ,"ROC" = performance[,1]
                         ,"Accuracy" = nnet.cm$overall[1]
                         ,"Kappa" = nnet.cm$overall[2]
                         ,"Sensitivity" = performance[,2]
                         ,"Specificity" = performance[,3])

nnet.ROC <- roc(nnet.model$pred$obs, nnet.model$pred$Normal)
plot(nnet.ROC, col = "blue")
auc(nnet.ROC)

# Add model results to dataframe for comparison
final_results <- rbind(final_results, nnet_results)

# Save models in case we want to review them later
saveRDS(final_results, "final_results.rds")
saveRDS(nnet.model, "Models/nnet_use_data.rds")
saveRDS(nnet.model, "Models/nnet_use_data_lc.rds")
saveRDS(nnet.model, "Models/nnet_model_data.rds")

##################################################################################################################


#############junk





saveRDS(rf, "random_forest.rds")
varImp(rf)
#varImpPlot(rf$finalModel)

imp <- as.data.frame(rf$finalModel$importance)

ggplot(imp, aes(x = reorder(rownames(imp), MeanDecreaseGini), MeanDecreaseGini)) +
        geom_bar(stat = "identity", aes(fill = rownames(imp))) + 
        coord_flip() + 
        guides(fill=FALSE) + 
        ggtitle("Random Forest Variable Importance - Hospital Type") +
        xlab("Variables") +
        ylab("Mean Decrease Gini")


rf_pred_test <- predict(rf, testData)

confusionMatrix(rf_pred_test, testData$Label2)



###############################################################################
# GLM - LOOCV  need to implement LOOCV
# https://gerardnico.com/lang/r/cross_validation
# https://rpubs.com/maulikpatel/226237
# http://www.rebeccabarter.com/blog/2017-11-17-caret_tutorial/
# https://www.analyticsvidhya.com/blog/2016/12/practical-guide-to-implement-machine-learning-with-caret-package-in-r-with-practice-problem/
###############################################################################

library(glm)
library(boot)

glm.fit <- glm(Label2 ~., data = use_data, family = binomial)
cv <- cv.glm(use_data, glm.fit)
#cv.glm(trainData, glm.fit)$delta
glm.fit.probs <- predict(glm.fit, testData, type = "response")
glm.pred <- rep("Abnormal", 781)
glm.pred[glm.fit.probs > .5] = "Normal"

###############################################################################
# SVM - LOOCV Caret
# https://stats.stackexchange.com/questions/136274/leave-one-subject-out-cv-method
###############################################################################

fitControl <- trainControl(method = "LOOCV",
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary)

svm.model2 <- caret::train(Label2 ~ .
                           ,data = model_data 
                           ,method = "svmRadial"
                           ,trControl = fitControl
                           ,metric = "ROC" 
                           ,preProc = c("center", "scale"))

# Save models to prevent the need to rerun
saveRDS(svm.model, "loocv_svm.rds")
saveRDS(svm.model2, "loocv_svm2.rds")

#do I need to repredict?
set.seed(1234)
fitControl <- trainControl(classProbs = TRUE, 
                           summaryFunction = twoClassSummary)

test.results <- matrix(NA, nrow = dim(use_data)[1], ncol = 2)
for(row in 1:dim(model_data[1])) {
        svm.final <- caret::train(Label2 ~ .
                                  ,data = model_data[-row,]
                                  ,method = "svmRadial"
                                  ,metric = "ROC" 
                                  ,preProc = c("center", "scale")
                                  ,trControl = fitControl
                                  ,tuneGrid = data.frame(C = 1, sigma = 0.01985963))
}
test.pred <- rep("Abnormal", dim(model_data)[1])
test.pred[test.results[,2] > .5] = "Normal"
finalResults <- as.data.frame(cbind(test.results, full$PatientNumMasked, test.pred, full$Label2))

confusionMatrix(finalResults$glm.pred, finalResults$V5)
caret::confusionMatrix(as.factor(pred), model_data$Label2, mode = "everything", positive = 'Normal')

svm.cm <- caret::confusionMatrix(svm.model$pred$pred[which(svm.model$pred$C == 1)], model_data$Label2, mode = "everything", positive = 'Normal')
svm.ROC <- roc(model_data$Label2, svm.model$pred$Normal[which(svm.model$pred$C == 1)])
plot(svm.ROC, col = "blue")
auc(svm.ROC)

svm.cm2 <- caret::confusionMatrix(svm.model2$pred$pred[which(svm.model2$pred$C == 1)], model_data$Label2, mode = "everything", positive = 'Normal')
svm.ROC2 <- roc(model_data$Label2, svm.model2$pred$Normal[which(svm.model2$pred$C == 1)])
plot(svm.ROC2, col = "blue")
auc(svm.ROC2)

svm.model2$finalModel
plot(svm.model2)

#####################################################################################
glm.results <- matrix(NA, nrow = dim(use_data)[1], ncol = 2)
for(row in 1:dim(use_data[1])) {
        glmFit <- glm(Label2 ~ ., data = use_data[-row,], family = binomial)
        glmFit.response <- predict(glmFit, use_data[row,], type = "response")
        glm.results[row,] <- c(row, glmFit.response)
}
glm.pred <- rep("Abnormal", dim(use_data)[1])
glm.pred[results[,2] > .5] = "Normal"
finalResults <- as.data.frame(cbind(glm.results, full$PatientNumMasked, glm.pred, full$Label2))

confusionMatrix(finalResults$glm.pred, finalResults$V5)


# https://www.r-bloggers.com/random-forests-in-r/
rf.results <- matrix(NA, nrow = dim(use_data)[1], ncol = 2)
for(row in 1:dim(use_data[1])) {
        rfFit <- randomForest(Label2 ~ ., data = use_data[-row,])
        rfFit.response <- predict(rfFit, use_data[row,], type = "response")
        rf.results[row,] <- c(row, rfFit.response)
}

plot(rfFit)

rf.pred <- rep("Abnormal", dim(use_data)[1])
rf.pred[rf.results[,2] == 2] = "Normal"
rf.finalResults <- as.data.frame(cbind(rf.results, full$PatientNumMasked, rf.pred, full$Label2))

confusionMatrix(rf.finalResults$rf.pred, rf.finalResults$V5)

# https://www.r-bloggers.com/random-forests-in-r/
rf.results2 <- matrix(NA, nrow = 20, ncol = 2)
for(row in 1:20) {
        rfFit <- randomForest(Label2 ~ ., data = use_data[-row,])
        rfFit.response <- predict(rfFit, use_data[row,], type = "raw")
        rf.results2[row,] <- c(row, rfFit.response)
}


# set up data
# create loop
# remove 1 row (x1, y1)
# fit model
# predict based on model
# loop until done

# noticed in the data that not all quadrants have values for each patient

# Verify that all labels for patients are the same
checkLabel <- full %>%
        group_by(PatientNumMasked) %>%
        mutate(minLabel = min(Label)) %>%
        mutate(maxLabel = max(Label)) %>%
        mutate(count = length(PatientNumMasked)) %>%
        select(PatientNumMasked, minLabel, maxLabel, count) 

# If the min and max are equal, they will be dropped.  If list is empty
# then the label is consistent across data sets for all patients
checkLabel %>%
        filter(minLabel != maxLabel)







lung_segment <- c('Right Upper', 'Right Middle', 'Right Lower', 'Left Upper', 'Left Middle', 'Left Lower', 'Total')
observations <- c(397, 470, 446, 392, 467, 434, 2606)
data_table <- data.frame(lung_segment, observations)

length(unique(full$PatientNumMasked))
library(e1071)
svm <- svm(Label2 ~., use_data)
confusionMatrix(predict(svm, use_data), use_data$Label2)

svm.results <- matrix(NA, nrow = dim(use_data)[1], ncol = 2)
for(row in 1:dim(use_data[1])) {
        svmFit <- randomForest(Label2 ~ ., data = use_data[-row,])
        svmFit.response <- predict(svmFit, use_data[row,], type = "response")
        svm.results[row,] <- c(row, svmFit.response)
}

svm.pred <- rep("Abnormal", dim(use_data)[1])
svm.pred[svm.results[,2] == 2] = "Normal"
svm.finalResults <- as.data.frame(cbind(svm.results, full$PatientNumMasked, svm.pred, full$Label2))

confusionMatrix(svm.finalResults$svm.pred, svm.finalResults$V5)




