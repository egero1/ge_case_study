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

decisionplot <- function(model, data, class = NULL, predict_type = "class",
                         resolution = 100, showgrid = TRUE, ...) {
        
        if(!is.null(class)) cl <- data[,class] else cl <- 1
        data <- data[,1:2]
        k <- length(unique(cl))
        
        plot(data, col = as.integer(cl)+1L, pch = as.integer(cl)+1L, ...)
        legend("top", legend = unique(cl), horiz = TRUE, 
               col = as.integer(unique(cl))+1L, pch = as.integer(unique(cl))+1L)
        
        # make grid
        r <- sapply(data, range, na.rm = TRUE)
        xs <- seq(r[1,1], r[2,1], length.out = resolution)
        ys <- seq(r[1,2], r[2,2], length.out = resolution)
        g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
        colnames(g) <- colnames(r)
        g <- as.data.frame(g)
        
        ### guess how to get class labels from predict
        ### (unfortunately not very consistent between models)
        p <- predict(model, g, type = predict_type)
        if(is.list(p)) p <- p$class
        p <- as.factor(p)
        
        if(showgrid) points(g, col = as.integer(p)+1L, pch = ".")
        
        z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
        contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
                lwd = 1, levels = (1:(k-1))+.5)
        
        invisible(z)
}

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
                          ,Accuracy = numeric()
                          ,Sensitivity = numeric()
                          ,Specificity = numeric()
                          ,Precision = numeric()
                          ,Kappa = numeric()
                          ,F1 = numeric()
                          ,ROC = numeric())

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
                        ,Accuracy = glm.cm.ud$overall[1]
                        ,Sensitivity = performance[,2]
                        ,Specificity = performance[,3]
                        ,Precision = glm.cm.ud$byClass[5]
                        ,Kappa = glm.cm.ud$overall[2]
                        ,F1 = glm.cm.ud$byClass[7]
                        ,ROC = performance[,1]))

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
                        ,Accuracy = glm.cm.md$overall[1]
                        ,Sensitivity = performance[,2]
                        ,Specificity = performance[,3]
                        ,Precision = glm.cm.md$byClass[5]
                        ,Kappa = glm.cm.md$overall[2]
                        ,F1 = glm.cm.md$byClass[7]
                        ,ROC = performance[,1]))

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
                        ,Accuracy = glm.cm.lc$overall[1]
                        ,Sensitivity = performance[,2]
                        ,Specificity = performance[,3]
                        ,Precision = glm.cm.lc$byClass[5]
                        ,Kappa = glm.cm.lc$overall[2]
                        ,F1 = glm.cm.lc$byClass[7]
                        ,ROC = performance[,1]))

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
saveRDS(model_results, "model_results.rds")
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
                        ,Accuracy = svm.cm.ud$overall[1]
                        ,Sensitivity = performance[,2]
                        ,Specificity = performance[,3]
                        ,Precision = svm.cm.ud$byClass[5]
                        ,Kappa = svm.cm.ud$overall[2]
                        ,F1 = svm.cm.ud$byClass[7]
                        ,ROC = performance[,1]))

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
                        ,Accuracy = svm.cm.md$overall[1]
                        ,Sensitivity = performance[,2]
                        ,Specificity = performance[,3]
                        ,Presicion = svm.cm.md$byClass[5]
                        ,Kappa = svm.cm.md$overall[2]
                        ,F1 = svm.cm.md$byClass[7]
                        ,ROC = performance[,1]))

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
                        ,Accuracy = svm.cm.lc$overall[1]
                        ,Sensitivity = performance[,2]
                        ,Specificity = performance[,3]
                        ,Presicion = svm.cm.lc$byClass[5]
                        ,Kappa = svm.cm.lc$overall[2]
                        ,F1 = svm.cm.lc$byClass[7]
                        ,ROC = performance[,1]))

rownames(model_results) <- NULL

# Disable parallel processing and release resources
stopCluster(cluster)
registerDoSEQ()

svm.ROC <- roc(svm.model.ud$pred$obs, svm.model.ud$pred$Normal)
plot(svm.ROC, col = 'blue', main = paste('SVM - Area under the curve (AUC):', round(auc(svm.ROC),2)))

# Save models in case we want to review them later
saveRDS(model_results, "model_results.rds")
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
                        ,Accuracy = knn.cm.ud$overall[1]
                        ,Sensitivity = performance[,2]
                        ,Specificity = performance[,3]
                        ,Presicion = knn.cm.ud$byClass[5]
                        ,Kappa = knn.cm.ud$overall[2]
                        ,F1 = knn.cm.ud$byClass[7]
                        ,ROC = performance[,1]))

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
                        ,Accuracy = knn.cm.md$overall[1]
                        ,Sensitivity = performance[,2]
                        ,Specificity = performance[,3]
                        ,Presicion = knn.cm.md$byClass[5]
                        ,Kappa = knn.cm.md$overall[2]
                        ,F1 = knn.cm.md$byClass[7]
                        ,ROC = performance[,1]))

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
                        ,Accuracy = knn.cm.lc$overall[1]
                        ,Sensitivity = performance[,2]
                        ,Specificity = performance[,3]
                        ,Presicion = knn.cm.lc$byClass[5]
                        ,Kappa = knn.cm.lc$overall[2]
                        ,F1 = knn.cm.lc$byClass[7]
                        ,ROC = performance[,1]))

rownames(model_results) <- NULL

# Disable parallel processing and release resources
stopCluster(cluster)
registerDoSEQ()

knn.ROC <- roc(knn.model.md$pred$obs, knn.model.md$pred$Normal)
plot(knn.ROC, col = 'blue', main = paste('kNN - Area under the curve (AUC):', round(auc(knn.ROC),2)))

plot(knn.model.md, main = paste('k = ', knn.model.md$bestTune))

# Save models in case we want to review them later
saveRDS(model_results, "model_results.rds")
saveRDS(knn.model.ud, "Models/knn_use_data.rds")
saveRDS(knn.model.md, "Models/knn_model_data.rds")
saveRDS(knn.model.lc, "Models/knn_use_data_lc.rds")

###############################################################################
# Naive Bayes- LOOCV Caret
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
nb.model.ud <- caret::train(Label ~ .
                         ,data = use_data 
                         ,method = "nb"
                         ,trControl = fitControl
                         ,metric = "ROC")

# Get the confusion matrix
nb.cm.ud <- caret::confusionMatrix(nb.model.ud$pred$pred, nb.model.ud$pred$obs, mode = "everything")

# Get the performance metrics from the model and save for comparison
performance <- getTrainPerf(nb.model.ud)
model_results <- rbind(model_results
                        ,data.frame(Model = 'Naive Bayes'
                        ,Data = 'Full'
                        ,Accuracy = nb.cm.ud$overall[1]
                        ,Sensitivity = performance[,2]
                        ,Specificity = performance[,3]
                        ,Presicion = nb.cm.ud$byClass[5]
                        ,Kappa = nb.cm.ud$overall[2]
                        ,F1 = nb.cm.ud$byClass[7]
                        ,ROC = performance[,1]))

# RFE selection data set
set.seed(1234)
nb.model.md <- caret::train(Label ~ .
                            ,data = model_data 
                            ,method = "nb"
                            ,trControl = fitControl
                            ,metric = "ROC")

# Get the confusion matrix
nb.cm.md <- caret::confusionMatrix(nb.model.md$pred$pred, nb.model.md$pred$obs, mode = "everything")

# Get the performance metrics from the model and save for comparison
performance <- getTrainPerf(nb.model.md)
model_results <- rbind(model_results
                        ,data.frame(Model = 'Naive Bayes'
                        ,Data = 'RFE Selection'
                        ,Accuracy = nb.cm.md$overall[1]
                        ,Sensitivity = performance[,2]
                        ,Specificity = performance[,3]
                        ,Presicion = nb.cm.md$byClass[5]
                        ,Kappa = nb.cm.md$overall[2]
                        ,F1 = nb.cm.md$byClass[7]
                        ,ROC = performance[,1]))

# Low correlation data set
set.seed(1234)
nb.model.lc <- caret::train(Label ~ .
                            ,data = use_data_lc
                            ,method = "nb"
                            ,trControl = fitControl
                            ,metric = "ROC")

# Get the confusion matrix
nb.cm.lc <- caret::confusionMatrix(nb.model.lc$pred$pred, nb.model.lc$pred$obs, mode = "everything")

# Get the performance metrics from the model and save for comparison
performance <- getTrainPerf(nb.model.lc)
model_results <- rbind(model_results
                        ,data.frame(Model = 'Naive Bayes'
                        ,Data = 'Low Correlation'
                        ,Accuracy = nb.cm.lc$overall[1]
                        ,Sensitivity = performance[,2]
                        ,Specificity = performance[,3]
                        ,Presicion = nb.cm.lc$byClass[5]
                        ,Kappa = nb.cm.lc$overall[2]
                        ,F1 = nb.cm.lc$byClass[7]
                        ,ROC = performance[,1]))

rownames(model_results) <- NULL

# Disable parallel processing and release resources
stopCluster(cluster)
registerDoSEQ()

nb.ROC <- roc(nb.model.md$pred$obs, nb.model.md$pred$Normal)
plot(nb.ROC, col = 'blue', main = paste('NB - Area under the curve (AUC):', round(auc(nb.ROC),2)))

# Save models in case we want to review them later
saveRDS(model_results, "model_results.rds")
saveRDS(nb.model.ud, "Models/nb_use_data.rds")
saveRDS(nb.model.md, "Models/nb_model_data.rds")
saveRDS(nb.model.lc, "Models/nb_use_data_lc.rds")

###############################################################################
# Random Forest - LOOCV  91.55 Acc 0.8111 Kappa
# https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
# https://www.analyticsvidhya.com/blog/2016/12/practical-guide-to-implement-machine-learning-with-caret-package-in-r-with-practice-problem/
###############################################################################

# Enable parallel processing and reserve resources
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

# Set up training conditions - must use LOOCV
fitControl <- trainControl(method = "LOOCV"
                           ,classProbs = TRUE 
                           ,summaryFunction = twoClassSummary
                           ,savePredictions = 'final'
                           ,allowParallel = TRUE)

# Full dat set
set.seed(1234)
rf.model.ud <- caret::train(Label ~.
                            ,data = use_data
                            ,method = 'rf'
                            ,trControl = fitControl
                            ,metric = "ROC")

rf.cm.ud <- caret::confusionMatrix(rf.model.ud$pred$pred, rf.model.ud$pred$obs)

# Get the performance metrics from the model and save for comparison
performance <- getTrainPerf(rf.model.ud)
model_results <- rbind(model_results
                        ,data.frame(Model = 'Random Forest'
                        ,Data = 'Full'
                        ,Accuracy = rf.cm.ud$overall[1]
                        ,Sensitivity = performance[,2]
                        ,Specificity = performance[,3]
                        ,Presicion = rf.cm.ud$byClass[5]
                        ,Kappa = rf.cm.ud$overall[2]
                        ,F1 = rf.cm.ud$byClass[7]
                        ,ROC = performance[,1]))

# RFE selection data set
set.seed(1234)
rf.model.md <- caret::train(Label ~.
                            ,data = model_data
                            ,method = 'rf'
                            ,trControl = fitControl
                            ,metric = "ROC")

rf.cm.md <- caret::confusionMatrix(rf.model.md$pred$pred, rf.model.md$pred$obs)

# Get the performance metrics from the model and save for comparison
performance <- getTrainPerf(rf.model.md)
model_results <- rbind(model_results
                        ,data.frame(Model = 'Random Forest'
                        ,Data = 'RFE Selection'
                        ,Accuracy = rf.cm.md$overall[1]
                        ,Sensitivity = performance[,2]
                        ,Specificity = performance[,3]
                        ,Presicion = rf.cm.md$byClass[5]
                        ,Kappa = rf.cm.md$overall[2]
                        ,F1 = rf.cm.md$byClass[7]
                        ,ROC = performance[,1]))

# Low correlation data set
set.seed(1234)
rf.model.lc <- caret::train(Label ~.
                            ,data = use_data_lc
                            ,method = 'rf'
                            ,trControl = fitControl
                            ,metric = "ROC")

rf.cm.lc <- caret::confusionMatrix(rf.model.lc$pred$pred, rf.model.lc$pred$obs)

# Get the performance metrics from the model and save for comparison
performance <- getTrainPerf(rf.model.lc)
model_results <- rbind(model_results
                        ,data.frame(Model = 'Random Forest'
                        ,Data = 'Low Correlation'
                        ,Accuracy = rf.cm.lc$overall[1]
                        ,Sensitivity = performance[,2]
                        ,Specificity = performance[,3]
                        ,Presicion = rf.cm.lc$byClass[5]
                        ,Kappa = rf.cm.lc$overall[2]
                        ,F1 = rf.cm.lc$byClass[7]
                        ,ROC = performance[,1]))

rownames(model_results) <- NULL

# Disable parallel processing and release resources
stopCluster(cluster)
registerDoSEQ()

rf.ROC <- roc(rf.model.md$pred$obs, rf.model.md$pred$Normal)
plot(rf.ROC, col = 'blue', main = paste('RF - Area under the curve (AUC):', round(auc(rf.ROC),2)))

# Variable importance plot
imp <- as.data.frame(rf.model.md$finalModel$importance)
ggplot(imp, aes(x = reorder(rownames(imp), MeanDecreaseGini), MeanDecreaseGini)) +
        geom_bar(stat = "identity", aes(fill = rownames(imp))) + 
        coord_flip() + 
        guides(fill=FALSE) + 
        ggtitle("Random Forest Variable Importance - Hospital Type") +
        xlab("Variables") +
        ylab("Mean Decrease Gini")

# Save models in case we want to review them later
saveRDS(model_results, "model_results.rds")
saveRDS(rf.model.ud, "Models/rf_use_data.rds")
saveRDS(rf.model.md, "Models/rf_model_data.rds")
saveRDS(rf.model.lc, "Models/rf_use_data_lc.rds")


##################################Figure out what to use here

###############################################################################
# XGBoost - LOOCV  
# https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
# https://www.analyticsvidhya.com/blog/2016/12/practical-guide-to-implement-machine-learning-with-caret-package-in-r-with-practice-problem/
###############################################################################

# Enable parallel processing and reserve resources
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

# Set up training conditions - must use LOOCV
fitControl <- trainControl(method = "LOOCV"
                           ,classProbs = TRUE 
                           ,summaryFunction = twoClassSummary
                           ,savePredictions = 'final'
                           ,allowParallel = TRUE
                           ,verboseIter = TRUE)

# set up the cross-validated hyper-parameter search
xgb_grid = expand.grid(nrounds = 2
                       ,max_depth = c(5, 10, 15)
                       ,eta = c(0.01, 0.001, 0.0001)
                       ,gamma = c(1, 2, 3)
                       ,colsample_bytree = c(0.4, 0.7, 1.0)
                       ,min_child_weight = c(0.5, 1, 1.5)
                       ,subsample = 1)

# Full data set
set.seed(1234)
xgb.model.ud <- caret::train(Label ~.
                              ,data = use_data
                              ,method = 'xgbTree'
                              ,trControl = fitControl
                              ,tuneGrid = xgb_grid
                              ,metric = "ROC")

xgb.cm.ud <- caret::confusionMatrix(xgb.model.ud$pred$pred, xgb.model.ud$pred$obs)

# Get the performance metrics from the model and save for comparison
performance <- getTrainPerf(nnet.model.ud)
model_results <- rbind(model_results
                       ,data.frame(Model = 'Neural Network'
                                   ,Data = 'Full'
                                   ,Accuracy = nnet.cm.ud$overall[1]
                                   ,Kappa = nnet.cm.ud$overall[2]
                                   ,F1 = nnet.cm.ud$byClass[7]
                                   ,ROC = performance[,1]
                                   ,Sensitivity = performance[,2]
                                   ,Specificity = performance[,3]))

rownames(model_results) <- NULL
###########################################################################
# Set up training conditions - must use LOOCV
fitControl <- trainControl(method = "repeatedcv"
                           ,number = 10
                           ,repeats = 5
                           ,classProbs = TRUE 
                           ,summaryFunction = twoClassSummary
                           ,savePredictions = 'final'
                           ,allowParallel = TRUE
                           ,verboseIter = TRUE)

# set up the cross-validated hyper-parameter search
xgb_grid = expand.grid(nrounds = 2
                       ,max_depth = c(5, 10, 15)
                       ,eta = c(0.01, 0.001, 0.0001)
                       ,gamma = c(1, 2, 3)
                       ,colsample_bytree = c(0.4, 0.7, 1.0)
                       ,min_child_weight = c(0.5, 1, 1.5)
                       ,subsample = 1)

# Full data set
set.seed(1234)
xgb.model.ud.cv <- caret::train(Label ~.
                             ,data = trainData
                             ,method = 'xgbTree'
                             ,trControl = fitControl
                             ,tuneGrid = xgb_grid
                             ,metric = "ROC")

xgb.cm.ud.cv <- caret::confusionMatrix(xgb.model.ud.cv$pred$pred, xgb.model.ud.cv$pred$obs)

# Get the performance metrics from the model and save for comparison
performance <- getTrainPerf(nnet.model.cv)
model_results <- rbind(model_results
                       ,data.frame(Model = 'Neural Network'
                                   ,Data = 'Full'
                                   ,Accuracy = nnet.cm.cv$overall[1]
                                   ,Kappa = nnet.cm.cv$overall[2]
                                   ,F1 = nnet.cm.cv$byClass[7]
                                   ,ROC = performance[,1]
                                   ,Sensitivity = performance[,2]
                                   ,Specificity = performance[,3]))

rownames(model_results) <- NULL

# Disable parallel processing and release resources
stopCluster(cluster)
registerDoSEQ()

nnet.ROC <- roc(nnet.model$pred$obs, nnet.model$pred$Normal)
plot(nnet.ROC, col = "blue")
auc(nnet.ROC)

# Save models in case we want to review them later
saveRDS(model_results, "model_results.rds")
saveRDS(nnet.model, "Models/nnet_use_data.rds")
saveRDS(nnet.model, "Models/nnet_use_data_lc.rds")
saveRDS(nnet.model, "Models/nnet_model_data.rds")


# play with this
vars <- model_data[,c('Hist_2_150_2_Entropy', 'Hist_2_30_2_Entropy', 'Label')]
fitControl = trainControl(classProbs = TRUE)

set.seed(123)
impVars <- train(Label ~ .,
                 data = vars,
                 method = "svmRadial",
                 importance = TRUE,
                 trControl = fitControl)

decisionplot(model = impVars, data = vars, class = "Label", predict_type = "raw")


###############################################################################
# Nueral Network - LOOCV  
# https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
# https://www.analyticsvidhya.com/blog/2016/12/practical-guide-to-implement-machine-learning-with-caret-package-in-r-with-practice-problem/
###############################################################################

# Enable parallel processing and reserve resources
cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cluster)

# Set up training conditions - must use LOOCV
fitControl <- trainControl(method = "LOOCV"
                           ,classProbs = TRUE 
                           ,summaryFunction = twoClassSummary
                           ,savePredictions = 'final'
                           ,allowParallel = TRUE)

# Full data set
set.seed(1234)
nnet.model.ud <- caret::train(Label ~.
                              ,data = use_data
                              ,method = 'nnet'
                              ,trControl = fitControl
                              ,metric = "ROC")

nnet.cm.ud <- caret::confusionMatrix(nnet.model.ud$pred$pred, nnet.model.ud$pred$obs)

# Get the performance metrics from the model and save for comparison
performance <- getTrainPerf(nnet.model.ud)
model_results <- rbind(model_results
                       ,data.frame(Model = 'Neural Network'
                                   ,Data = 'Full'
                                   ,Accuracy = nnet.cm.ud$overall[1]
                                   ,Sensitivity = performance[,2]
                                   ,Specificity = performance[,3]
                                   ,Presicion = nnet.cm.ud$byClass[5]
                                   ,Kappa = nnet.cm.ud$overall[2]
                                   ,F1 = nnet.cm.ud$byClass[7]
                                   ,ROC = performance[,1]))

rownames(model_results) <- NULL
###########################################################################
# Set up training conditions - must use LOOCV
fitControl <- trainControl(method = "repeatedcv"
                           ,number = 10
                           ,repeats = 5
                           ,classProbs = TRUE 
                           ,summaryFunction = twoClassSummary
                           ,savePredictions = 'final'
                           ,allowParallel = TRUE
                           ,verboseIter = TRUE)

nnetGrid <-  expand.grid(size = seq(from = 1, to = 10, by = 1),
                         decay = seq(from = 0.1, to = 0.5, by = 0.1))

# Full data set
set.seed(1234)
nnet.model.cv <- caret::train(Label ~.
                              ,data = model_data
                              ,method = 'nnet'
                              ,trControl = fitControl
                              ,tuneGrid = nnetGrid
                              ,metric = "ROC")

nnet.cm.cv <- caret::confusionMatrix(nnet.model.cv$pred$pred, nnet.model.cv$pred$obs)

# Get the performance metrics from the model and save for comparison
performance <- getTrainPerf(nnet.model.cv)
model_results <- rbind(model_results
                       ,data.frame(Model = 'Neural Network'
                                   ,Data = 'Full'
                                   ,Accuracy = nnet.cm.cv$overall[1]
                                   ,Kappa = nnet.cm.cv$overall[2]
                                   ,F1 = nnet.cm.cv$byClass[7]
                                   ,ROC = performance[,1]
                                   ,Sensitivity = performance[,2]
                                   ,Specificity = performance[,3]))

rownames(model_results) <- NULL

# Disable parallel processing and release resources
stopCluster(cluster)
registerDoSEQ()

nnet.ROC <- roc(nnet.model$pred$obs, nnet.model$pred$Normal)
plot(nnet.ROC, col = "blue")
auc(nnet.ROC)

# Save models in case we want to review them later
saveRDS(model_results, "model_results.rds")
saveRDS(nnet.model, "Models/nnet_use_data.rds")
saveRDS(nnet.model, "Models/nnet_use_data_lc.rds")
saveRDS(nnet.model, "Models/nnet_model_data.rds")
