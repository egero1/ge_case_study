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
library(reshape2)
library(corrplot)
library(fBasics)
library(rpart)
library(rpart.plot)

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

# Set levels of the factor - makes it easier to read
full$Label2 <- ifelse(full$Label == 0, 'Normal', 'Abnormal')

# Prepare data set
use_data <- full %>%
        mutate(Label2 = factor(Label2)) %>%
        mutate(Position = factor(Position)) %>% 
        select(-PatientNumMasked, -Label)

# Create training and test datasets
trainIndex <- createDataPartition(use_data$Label2, p = 0.7, list = FALSE)
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
        select(-Position, -Label2)
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
pairs(Label2 ~ ., data = use_data[c(1,41)])

# Check for outliers
for(var in 1:length(use_data[-c(40, 41)])) {
        #variable <- use_data[var]
        #colnames(variable) <- colnames(use_data[var])
        p <- ggplot(data = use_data, aes(x = "", y = use_data[var])) + 
                geom_boxplot(color="red", fill="orange", alpha=0.2) +
                labs(title = paste("Outliers - ", colnames(use_data[var])))
        coord_cartesian(ylim = c(min(use_data[var]), max(use_data[var])))
        print(p)
        #boxplot(use_data[var], main = colnames(use_data[var]))
}

###############################################################################
# Exploratory Model and Important Variables
###############################################################################

naive <- rpart(Label2 ~., data = use_data)
rpart.plot(naive)
naive.pred <- predict(navie, use_, type = 'class')

cm <- confusionMatrix(naive.pred, use_data$Label2, positive = 'Normal')

scaled <- scale(use_data[-c(40,41)])
for(var in 1:length(use_data[-c(40, 41)])) {
        
        boxplot(scaled[var], main = colnames(scaled[var]))
}
scaled <- scale(use_data[-c(40,41)])
outlierKD(use_data, Hist_0_0_0_Mean)

# Review the dataset
summary(use_data)

###############################################################################
# Feature selection using Recursive Feature Elimination or RFE
# https://www.analyticsvidhya.com/blog/2016/12/practical-guide-to-implement-machine-learning-with-caret-package-in-r-with-practice-problem/
###############################################################################

control <- rfeControl(functions = rfFuncs
                      ,method = "repeatedcv"
                      ,number = 3
                      ,verbose = FALSE)

pref_variables <- rfe(use_data[-41], use_data[,41], rfeControl = control)

# Print the results
pref_variables

# List the variables
predictors(pref_variables)

plot(pref_variables, type = c("g", "o"))

# Create a data set using the preferred variables
model_data <- use_data %>%
        select(predictors(pref_variables), Label2)

###############################################################################
# GLM - LOOCV
# http://www.rebeccabarter.com/blog/2017-11-17-caret_tutorial/
# https://www.analyticsvidhya.com/blog/2016/12/practical-guide-to-implement-machine-learning-with-caret-package-in-r-with-practice-problem/
# https://www.analyticsvidhya.com/blog/2016/12/practical-guide-to-implement-machine-learning-with-caret-package-in-r-with-practice-problem/
###############################################################################

# Set up training conditions - must use LOOCV
fitControl <- trainControl(method = "loocv"
                           ,summaryFunction = twoClassSummary
                           ,classProbs = TRUE)

#fitControl <- trainControl(method = "repeatedcv"
#                           ,number = 1
#                           ,repeats = 5
#                           ,returnResamp="none"
#                           ,classProbs = TRUE
#                           ,savePredictions = 'final')

glm.model <- train(Label2 ~.
             ,data = model_data
             ,family = "binomial"
             ,method = 'glm'
             ,trControl = fitControl
             ,metric = "ROC")

glm_pred_test <- predict(glm.model, model_data)

confusionMatrix(glm_pred_test, model_data$Label2, mode = 'everything', positive = 'Normal')

plot roc

###############################################################################
# Random Forest - LOOCV  91.55 Acc 0.8111 Kappa
# https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
# https://www.analyticsvidhya.com/blog/2016/12/practical-guide-to-implement-machine-learning-with-caret-package-in-r-with-practice-problem/
###############################################################################


rf <- train(Label2 ~.
            ,data = trainData
            ,method = 'rf'
            ,trControl = fitControl
            ,metric = "ROC"
            ,preProc = c("center", "scale"))

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
# GLM - LOOCV
# http://www.rebeccabarter.com/blog/2017-11-17-caret_tutorial/
# https://www.analyticsvidhya.com/blog/2016/12/practical-guide-to-implement-machine-learning-with-caret-package-in-r-with-practice-problem/
# https://www.analyticsvidhya.com/blog/2016/12/practical-guide-to-implement-machine-learning-with-caret-package-in-r-with-practice-problem/
###############################################################################

# Set up training conditions - must use LOOCV
fitControl <- trainControl(method = "repeatedcv"
                           ,number = 1
                           ,repeats = 5
                           ,returnResamp="none"
                           ,classProbs = TRUE
                           ,savePredictions = 'final')

glm <- train(Label2 ~.
             ,data = trainData
             ,family = "binomial"
             ,method = 'glm'
             ,trControl = fitControl
             ,metric = "ROC")

glm_pred_test <- predict(glm, testData)

confusionMatrix(glm_pred_test, testData$Label, mode = 'everything', positive = '1')

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



#############junk
male = data.frame(c(127,44,28,83,0,6,78,6,5,213,73,20,214,28,11)) # data from page 66
ggplot(data = male, aes(x = "male", y = male)) + 
        geom_boxplot() +
        coord_cartesian(ylim = c(0, 150))


lapply(use_data, seq(1:39), tab_summary)




        
rbind(observations, 'Total' = sum(observations$Count))



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

###############################################################################
# SVM - LOOCV Caret
# https://stats.stackexchange.com/questions/136274/leave-one-subject-out-cv-method
###############################################################################

fitControl <- trainControl(method = "LOOCV",
                           classProbs = TRUE, 
                           summaryFunction = twoClassSummary)

svm.model <- caret::train(Label2 ~ .
                   ,data = use_data 
                   ,method = "svmRadial"
                   ,trControl = fitControl
                   ,metric = "ROC" 
                   ,preProc = c("center", "scale"))


saveRDS(svm.model, "loocv_svm.rds")


