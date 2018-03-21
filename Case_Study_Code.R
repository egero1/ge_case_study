## Eric Gero
## eric.gero@ge.com

setwd("~/Documents/R_Projects/GE_Case_Study")

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

# https://www.r-bloggers.com/identify-describe-plot-and-remove-the-outliers-from-the-dataset/
outlierKD <- function(dt, var) {
        var_name <- eval(substitute(var),eval(dt))
        na1 <- sum(is.na(var_name))
        m1 <- mean(var_name, na.rm = T)
        par(mfrow=c(2, 2), oma=c(0,0,3,0))
        boxplot(var_name, main="With outliers")
        hist(var_name, main="With outliers", xlab=NA, ylab=NA)
        outlier <- boxplot.stats(var_name)$out
        mo <- mean(outlier)
        var_name <- ifelse(var_name %in% outlier, NA, var_name)
        boxplot(var_name, main="Without outliers")
        hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
        title("Outlier Check", outer=TRUE)
        na2 <- sum(is.na(var_name))
        cat("Outliers identified:", na2 - na1, "n")
        cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
        cat("Mean of the outliers:", round(mo, 2), "n")
        m2 <- mean(var_name, na.rm = T)
        cat("Mean without removing outliers:", round(m1, 2), "n")
        cat("Mean if we remove outliers:", round(m2, 2), "n")
        response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
        if(response == "y" | response == "yes"){
                dt[as.character(substitute(var))] <- invisible(var_name)
                assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
                cat("Outliers successfully removed", "n")
                return(invisible(dt))
        } else{
                cat("Nothing changed", "n")
                return(invisible(var_name))
        }
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

# Check scatter plots of continuous variables
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        # correlation coefficient
        r <- cor(x, y)
        txt <- format(c(r, 0.123456789), digits = digits)[1]
        txt <- paste("r= ", txt, sep = "")
        text(0.5, 0.6, txt)
        
        # p-value calculation
        p <- cor.test(x, y)$p.value
        txt2 <- format(c(p, 0.123456789), digits = digits)[1]
        txt2 <- paste("p= ", txt2, sep = "")
        if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
        text(0.5, 0.4, txt2)
}

graphics.off()
par(mar = c(1,1,1,1))
pairs(use_data[-c(10:41)], upper.panel = panel.cor)

# Summary 
stats <- basicStats(use_data[-c(40,41)])[c("Mean", "Median", "Stdev", "Minimum", "Maximum", "NAs"),]
t(round(stats, 2))
options(qwraps2_markup = "markdown")
col < -seq(1, ncol(use_data), by = 1)
mysummary <- mapply(tab_summary, use_data[,col])
summary_table(dplyr::group_by(use_data, Position), mysummary)

# Scatter plot between predictors; scatter plot for response is pointless since it is categorical

library(lattice)
splom(use_data, groups = Position)
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

# Correlation plot
corr <- cor(use_data[-c(40,41)])
corrplot(corr, type = "upper", tl.cex = 0.5, tl.col = "blue", tl.srt = 45)

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


