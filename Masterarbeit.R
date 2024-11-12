# Mixed Linear Model in R using lme4 package

# Load necessary library
library(Matrix)
library(lme4)


# Example dataset: Create or load your data
# Assuming 'data' is your dataframe, 'stress_level' is the dependent variable (response),
# 'time' and 'group' are the fixed effects, and 'subject_id' is the grouping variable
# that will be used as a random effect.

# Fit a mixed linear model
# Replace 'data', 'stress_level', 'time', 'group', and 'subject_id' with your actual variables
model <- lmer(stress_level ~ time * group + (1 | subject_id), data = data)

# Print the summary of the model
summary(model)

# Check model diagnostics
plot(model)
qqnorm(resid(model))
qqline(resid(model))

# Optional: Perform an ANOVA test
anova(model)

# Optional: Extract random effects
ranef(model)

# Note: Ensure that your data meets the assumptions for mixed models,
# including normality of residuals and homoscedasticity.
