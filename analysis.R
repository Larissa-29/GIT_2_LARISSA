#Poweranalyse
install.packages(pwr)
library(pwr)

pwr.r.test(n=800, r=0.1, sig.level = 0.05, power = NULL, alternative = c("two.sided"))
pwr.r.test(n=800, r=0.3, sig.level = 0.05, power = NULL, alternative = c("two.sided"))
pwr.r.test(n=800, r=0.5, sig.level = 0.05, power = NULL, alternative = c("two.sided"))

pwr.r.test(n=800, r=0.1, sig.level = 0.005, power = NULL, alternative = c("two.sided"))
pwr.r.test(n=800, r=0.3, sig.level = 0.005, power = NULL, alternative = c("two.sided"))
pwr.r.test(n=800, r=0.5, sig.level = 0.005, power = NULL, alternative = c("two.sided"))

pwr.r.test(n=NULL, r=0.1, sig.level = 0.005, power = 0.9, alternative = c("two.sided"))
pwr.r.test(n=NULL, r=0.3, sig.level = 0.005, power = 0.9, alternative = c("two.sided"))
pwr.r.test(n=NULL, r=0.5, sig.level = 0.005, power = 0.9, alternative = c("two.sided"))

pwr.r.test(n=250, r=0.1, sig.level = 0.05, power = NULL, alternative = c("two.sided"))
pwr.r.test(n=250, r=0.3, sig.level = 0.05, power = NULL, alternative = c("two.sided"))
pwr.r.test(n=250, r=0.5, sig.level = 0.05, power = NULL, alternative = c("two.sided"))

### Datenanalyse
setwd("/Users/larissa-ravessoud/Documents/GitHub/GIT_2_LARISSA")

# Load the dataset from a tab-delimited text file
mydata <- read.table("mydata.txt", header=T, sep="\t")

# View the dataset and check its dimensions and summary statistics
View(mydata)
dim(mydata)
summary(mydata)
head(mydata)

# Convert "Sex" and "Filter" columns to factors (categorical variables)
mydata$Sex <- as.factor(mydata$Sex)
mydata$Filter <- as.factor(mydata$Filter)
summary(mydata)

# Filter the dataset to include only rows where "Filter" equals 0
mydata <- subset(mydata, Filter == 0)
nrow(mydata)

t.test(mydata$Testosteron ~ mydata$Sex)

# Create a boxplot to visualize the distribution of testosterone levels by sex
plot(mydata$Testosteron ~ mydata$Sex)

summary(mydata$Testosteron[mydata$Sex==0])
summary(mydata$Testosteron[mydata$Sex==1])

# Rename the "Sex" factor levels to "female" and "male" for better readability
mydata$Sex_ch <- factor(mydata$Sex, levels=c(0,1), labels=c("female", "male"))
summary(mydata)

summary(lm(Extraversion ~ Sex_ch, data=mydata))

# Calculate the correlation between two variables: EM_SD and EM_LD
cor(mydata$EM_SD, mydata$EM_LD, use="pairwise")
# Perform a paired t-test to compare EM_SD and EM_LD
t.test(mydata$EM_SD, mydata$EM_LD)

summary(mydata$EM_SD)
summary(mydata$EM_LD)

# Create a new variable "EM" as the average of EM_SD and EM_LD
mydata$EM <- (mydata$EM_SD + mydata$EM_LD)/2

# Plot the density distributions of EM_SD, EM_LD, and their average EM
plot(density(mydata$EM_SD), main="Compare EM SD and LD", frame.plot=F)
lines(density(mydata$EM_LD), col="red")
lines(density(mydata$EM), col="green")

# Calculate the correlation between fMRI measurements for amygdala and hippocampus
cor(mydata$fMRI_amy_neg_neu, mydata$fMRI_hipp_neg_neu)
plot(mydata$fMRI_amy_neg_neu, mydata$fMRI_hipp_neg_neu, pch=19)
abline(lm(mydata$fMRI_hipp_neg_neu ~ mydata$fMRI_amy_neg_neu))

# Perform a multiple linear regression to examine the relationship between EM, Sex, Extraversion, and fMRI hippocampal activity
# The model investigates how EM is influenced by Sex, Extraversion, and fMRI_hipp_neg_neu
summary(lm(EM ~ Sex_ch + Extraversion + fMRI_hipp_neg_neu, data=mydata))

######
repdata <- read.table("repdata.txt", header=T, sep="\t")
repdata <- subset(repdata, Filter==0)

#Ich würde Darstellungen und Diagramme in ein Separates skript setzen 



#Excercice 7


### Datenanalyse für die Frage: Wie beeinflussen Testosteronspiegel und Extraversion die Gedächtnisleistung?

# Set working directory
setwd("/Users/larissa-ravessoud/Documents/GitHub/GIT_2_LARISSA")

# Load the dataset
mydata <- read.table("mydata.txt", header=T, sep="\t")

# View the dataset and check its dimensions
View(mydata)
dim(mydata)
summary(mydata)
head(mydata)

# Convert "Sex" and "Filter" columns to factors (categorical variables)
mydata$Sex <- as.factor(mydata$Sex)
mydata$Filter <- as.factor(mydata$Filter)

# Filter the dataset to include only rows where "Filter" equals 0
mydata <- subset(mydata, Filter == 0)

# Check the number of rows after filtering
nrow(mydata)

# Renaming Sex for clarity ("female" and "male" labels)
mydata$Sex_ch <- factor(mydata$Sex, levels=c(0, 1), labels=c("female", "male"))

# Visualizing the distribution of testosterone by sex
t.test(mydata$Testosteron ~ mydata$Sex)  # T-test for testosterone by sex
plot(mydata$Testosteron ~ mydata$Sex)  # Boxplot

# Summary statistics for testosterone levels by sex
summary(mydata$Testosteron[mydata$Sex == 0]) 
summary(mydata$Testosteron[mydata$Sex == 1])

# If they are not numeric, convert them to numeric
mydata$Testosteron <- as.numeric(mydata$Testosteron)
mydata$EM <- (mydata$EM_SD + mydata$EM_LD) / 2
mydata$EM <- as.numeric(mydata$EM)

# Handle missing values and compute the correlation between Testosteron and EM
cor_testosterone_memory <- cor(mydata$Testosteron, mydata$EM, use="complete.obs")
cat("Correlation between Testosterone and Memory Performance: ", cor_testosterone_memory, "\n")

# Checking the relationship between extraversion and memory performance
cor(mydata$Extraversion, mydata$EM, use="pairwise")  # Correlation test between extraversion and memory

# Performing a linear regression to examine how testosterone and extraversion influence memory performance
lm_model <- lm(EM ~ Testosteron + Extraversion, data=mydata)
summary(lm_model)  # Summary of the regression model

# Visualizing the effect of testosterone on memory performance
plot(mydata$Testosteron, mydata$EM, main="Testosterone vs Memory Performance",
     xlab="Testosterone Levels", ylab="Memory Performance")
abline(lm(mydata$EM ~ mydata$Testosteron), col="blue")  # Regression line

# Visualizing the effect of extraversion on memory performance
plot(mydata$Extraversion, mydata$EM, main="Extraversion vs Memory Performance",
     xlab="Extraversion", ylab="Memory Performance")
abline(lm(mydata$EM ~ mydata$Extraversion), col="red")  # Regression line

# Investigating interaction effects: Does the combination of testosterone and extraversion affect memory performance?
interaction_model <- lm(EM ~ Testosteron * Extraversion, data=mydata)
summary(interaction_model)  # Summary of the interaction model

# Checking the correlation between testosterone, extraversion, and memory performance
cor_testosterone_extraversion <- cor(mydata$Testosteron, mydata$Extraversion, use="pairwise")
cat("Correlation between testosterone and extraversion:", cor_testosterone_extraversion, "\n")

# Check model assumptions: Residuals vs Fitted values for the linear model
plot(lm_model$residuals ~ lm_model$fitted.values, main="Residuals vs Fitted values",
     xlab="Fitted values", ylab="Residuals")
abline(h=0, col="red")  # Horizontal line at 0

# Optional: If you want to assess the impact of sex as a covariate
lm_sex_model <- lm(EM ~ Testosteron + Extraversion + Sex_ch, data=mydata)
summary(lm_sex_model)  # Check if sex also affects memory performance

# If the dataset includes fMRI or other brain activity data, you could also add that in your models
# For example, adding fMRI hippocampal activity as a covariate
lm_fmri_model <- lm(EM ~ Testosteron + Extraversion + fMRI_hipp_neg_neu, data=mydata)
summary(lm_fmri_model)

### Sensitivity Analysis

# Check for outliers in the 'Testosteron' and 'EM' variables
boxplot(mydata$Testosteron, main="Testosterone - Boxplot for Outliers")
boxplot(mydata$EM, main="Memory Performance (EM) - Boxplot for Outliers")

# Remove outliers (Testosterone and EM) based on a 1.5 * IQR rule
Q1_testosterone <- quantile(mydata$Testosteron, 0.25)
Q3_testosterone <- quantile(mydata$Testosteron, 0.75)
IQR_testosterone <- IQR(mydata$Testosteron)
lower_bound_testosterone <- Q1_testosterone - 1.5 * IQR_testosterone
upper_bound_testosterone <- Q3_testosterone + 1.5 * IQR_testosterone
mydata_no_outliers_testosterone <- subset(mydata, Testosteron >= lower_bound_testosterone & Testosteron <= upper_bound_testosterone)

Q1_em <- quantile(mydata$EM, 0.25)
Q3_em <- quantile(mydata$EM, 0.75)
IQR_em <- IQR(mydata$EM)
lower_bound_em <- Q1_em - 1.5 * IQR_em
upper_bound_em <- Q3_em + 1.5 * IQR_em
mydata_no_outliers_em <- subset(mydata_no_outliers_testosterone, EM >= lower_bound_em & EM <= upper_bound_em)

# Perform the analysis after removing outliers
lm_sensitivity_outliers <- lm(EM ~ Testosteron + Extraversion, data=mydata_no_outliers_em)
summary(lm_sensitivity_outliers)

# Sensitivity to missing data handling (complete observations vs. mean imputation)
mydata_imputed <- mydata
mydata_imputed$Testosteron[is.na(mydata_imputed$Testosteron)] <- mean(mydata_imputed$Testosteron, na.rm=TRUE)
mydata_imputed$EM[is.na(mydata_imputed$EM)] <- mean(mydata_imputed$EM, na.rm=TRUE)

# Perform the analysis after imputation
lm_sensitivity_imputation <- lm(EM ~ Testosteron + Extraversion, data=mydata_imputed)
summary(lm_sensitivity_imputation)

# Compare results from original, outlier-free, and imputed data models
cat("Sensitivity Analysis Summary: \n")
cat("Model with original data:\n")
summary(lm(EM ~ Testosteron + Extraversion, data=mydata))
cat("Model after removing outliers:\n")
summary(lm_sensitivity_outliers)
cat("Model after imputation:\n")
summary(lm_sensitivity_imputation)

### Replication Analysis

# Load the replication dataset (assuming a separate file "repdata.txt" is provided)
repdata <- read.table("repdata.txt", header=T, sep="\t")

# Check the structure of the replication data
str(repdata)
summary(repdata)

# Ensure the same preprocessing as the original dataset (e.g., filtering, conversion of categorical variables)
repdata$Sex <- as.factor(repdata$Sex)
repdata$Filter <- as.factor(repdata$Filter)
repdata <- subset(repdata, Filter == 0)

# Create the EM variable if necessary (assuming EM is an average of EM_SD and EM_LD)
repdata$EM <- (repdata$EM_SD + repdata$EM_LD) / 2

# Confirm that 'Testosteron' and 'Extraversion' are numeric
repdata$Testosteron <- as.numeric(repdata$Testosteron)
repdata$Extraversion <- as.numeric(repdata$Extraversion)
repdata$EM <- as.numeric(repdata$EM)

# Perform the same linear regression as in the original analysis
lm_replication <- lm(EM ~ Testosteron + Extraversion, data=repdata)
summary(lm_replication)

# Compare the results of the replication analysis to the original dataset's results
cat("Replication Analysis Results: \n")
cat("Original study model:\n")
summary(lm(EM ~ Testosteron + Extraversion, data=mydata))
cat("Replication study model:\n")
summary(lm_replication)

# If you want to compare the models' coefficients statistically (e.g., using interaction terms)
lm_replication_interaction <- lm(EM ~ Testosteron * Extraversion, data=repdata)
summary(lm_replication_interaction)


