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

