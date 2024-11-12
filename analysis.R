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

# Reading in raw data and basic quality control
library(readr)

# Load raw data
raw_data <- read_csv("path/to/your/raw_data.csv")

# Basic quality control: remove rows with NA and filter based on a condition
raw_data <- raw_data %>%
  drop_na() %>%
  filter(some_metric > threshold_value)  # Replace 'some_metric' and 'threshold_value' as needed

print("Data loaded and basic quality control applied.")


# 2. Reliability/Validation/Aggregation steps
# Calculate correlations between independent variables
correlation_matrix = raw_data.corr()
# Assuming correlation_matrix is a data frame or matrix in R
reliable_vars <- names(correlation_matrix)[apply(correlation_matrix, 2, max) > 0.5]

# Aggregating variables if r > 0.5
aggregated_data = raw_data[reliable_vars].mean(axis=1)
raw_data['aggregated_var'] = aggregated_data

print("Data reliability check and aggregation completed.")

# 3. Main analysis
# Example: Linear regression analysis
model <- lm(dependent_var ~ independent_var1 + independent_var2, data = raw_data)
summary(model)

X = raw_data[['independent_var1', 'independent_var2']]  # Replace with your actual independent variables
y = raw_data['dependent_var']  # Replace with your actual dependent variable

model = LinearRegression()
model.fit(X, y)

print("Main analysis completed. Model coefficients:", model.coef_)

# Install and load the boot package
install.packages("boot")
library(boot)

# Define a function for resampling and fitting the model
boot_fn <- function(data, indices) {
  d <- data[indices, ]  # Resample data
  fit <- lm(dependent_var ~ independent_var1 + independent_var2, data = d)
  return(coef(fit))
}

# Perform bootstrapping with 1000 replications
boot_result <- boot(data = raw_data, statistic = boot_fn, R = 1000)
print(boot_result)

bootstrap_samples = 1000
coefficients <- c()  # Initializes an empty vector
  
coefficients_mean = np.mean(coefficients, axis=0)
print("Sensitivity analysis completed. Mean coefficients from bootstrapping:", coefficients_mean)

# 5. Replication
# Replication Script for Linear Regression Example
# Author: Your Name
# Date: 2024-11-12
# This script replicates the analysis from the paper XYZ, including steps for data loading, cleaning, model fitting, and results saving.

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(lmtest)

# Set working directory
setwd("path_to_your_directory")

# Load data
data <- read.csv("data_file.csv")

# Data cleaning (e.g., handle missing data)
data$column_name <- as.factor(data$column_name)
data <- na.omit(data)

# Fit a linear regression model
model <- lm(dependent_variable ~ independent_variable1 + independent_variable2, data = data)

# Model summary
summary(model)

# Diagnostic test
bptest(model)

# Save model coefficients to CSV
write.csv(coef(model), "model_coefficients.csv")

# Create a plot of residuals
ggplot(data, aes(x = fitted(model), y = residuals(model))) +
  geom_point() +
  theme_minimal() +
  labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals")

# Save the plot
ggsave("residuals_plot.png")

# Save model for future use
save(model, file = "model.RData")

