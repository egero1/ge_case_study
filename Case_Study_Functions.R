# https://www.r-bloggers.com/identify-describe-plot-and-remove-the-outliers-from-the-dataset/

dyn.load('/Library/Java/JavaVirtualMachines/jdk-9.0.1.jdk/Contents/Home/lib/server/libjvm.dylib')
library(rJava)
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

# Check scatter plots of continuous variables
panel.cor <- function(x, y, digits = 2, cex.cor, ...) {
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

American Lung Association. (2018). Pneumoconiosis. Retrieved March 17, 2018, from http://www.lung.org/lung-health-and-diseases/lung-disease-lookup/pneumoconiosis/
        
        http://www.miningmonthly.com/coal/safety-and-health/black-lung-case-detected-in-nsw/
        
        https://pdfs.semanticscholar.org/5a1e/9346f3a1cb3a67db4f55d5c628092a87f4ff.pdf

#where d is the difference scale and θ is the orientation at which the difference is computed. f_x and f_y represent the first order difference while f_xx,f_yy,f_xy represent the second order difference. We use the first and second order difference filter bank with given orientations θ∈{0,30,35,60,90,120,135,150,180} and given scale d∈{1,2}. We can calculate 6 intensity-based features (mean, variance, skewness, kurtosis, energy, entropy) for each filtered image, along with the same features for the raw image without filtering, amounting to a total of 222 features. A subset of 34 features from this set has been provided in the attached data sheet. These features are labeled with the prefix Hist_d_θ. 

#Co-occurrence matrix based: We also extract a set of 5 features based on the gray level co-occurrence matrix computed for the ROI, namely energy, entropy, local homogeneity, correlation and inertia. The co-occurrence matrix allows us to capture the level of similarity and dissimilarity among adjacent pixels in an ROI. Thus, an ROI with an opacity will contain adjacent pixels with similarly high intensities, whereas a normal ROI will not contain such adjacent pixels. Computing these features for various orientations δ={0,45,90,135} captures this information for various types of adjacency. A subset of 5 of out of 25 such features has been provided in the attached data sheet. These features are labeled with the prefix CoMatrix_Degδ.