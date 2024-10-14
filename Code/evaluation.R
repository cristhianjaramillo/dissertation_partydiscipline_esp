#### EVALUATION OF MODEL ####

library(betareg)
library(MASS)
library(xtable)
library(officer)
library(rvg)
library(kableExtra)
library(flextable)
library(car)
library(sandwich)
library(gridExtra)
library(caret)
library(readxl)
library(dplyr)

# Base

base <- read_excel("../Final data/Final_dataset.xlsx", 
                   sheet = "FINAL_edited")

base <- base[,c(6,7,8,11,12,13,14,15,17,18)] #These are the columns needed for the regression

base <- as.data.frame(base)
names(base)

# Editing some of the variables

base$gender <-  factor(base$gender, levels = c(0,1), labels = c("Female", "Male"))
base$role <-  factor(base$role, levels = c(0,1), labels = c("No", "Yes"))
base$re_elected <-  factor(base$re_elected, levels = c(0,1), labels = c("No", "Yes"))
base$rule <-  factor(base$rule, levels = c(0,1), labels = c("No", "Yes"))
base$party_affiliation <-  factor(base$party_affiliation, levels = c(0,1), labels = c("No", "Yes"))

# Standarization

base_b <- base %>%
  mutate(pol_experience_std = as.numeric(scale(pol_experience)),
         number_parties_std = as.numeric(scale(number_parties)),
         age_std = as.numeric(scale(age)),
         prop_votes_std = as.numeric(scale(prop_votes)))

# Handling exact 1 values by subtracting a small constant

base_b$party_discipline_adjusted <- ifelse(base_b$party_discipline == 1, 1 - 0.001, base_b$party_discipline)

summary(base_b$party_discipline_adjusted)

# Models

beta_model1 <- betareg(party_discipline_adjusted ~ 
                         party_affiliation, 
                       data = base_b)

beta_model2 <- betareg(party_discipline_adjusted ~ 
                         party_affiliation +
                         gender, 
                       data = base_b)

beta_model3 <- betareg(party_discipline_adjusted ~ 
                         party_affiliation +
                         gender + 
                         re_elected, 
                       data = base_b)

beta_model4 <- betareg(party_discipline_adjusted ~ 
                         party_affiliation +
                         gender +
                         re_elected +
                         role, 
                       data = base_b)

beta_model5 <- betareg(party_discipline_adjusted ~ 
                         party_affiliation +
                         gender + 
                         re_elected +
                         role +
                         pol_experience_std, 
                       data = base_b)

beta_model6 <- betareg(party_discipline_adjusted ~ 
                         party_affiliation +
                         gender + 
                         re_elected +
                         role +
                         pol_experience_std +
                         age_std, 
                       data = base_b)

beta_model7 <- betareg(party_discipline_adjusted ~ 
                         party_affiliation +
                         gender + 
                         re_elected +
                         role +
                         pol_experience_std +
                         age_std + 
                         prop_votes_std, 
                       data = base_b)

beta_model8 <- betareg(party_discipline_adjusted ~ 
                         party_affiliation +
                         gender + 
                         re_elected +
                         role +
                         pol_experience_std +
                         age_std + 
                         prop_votes_std +
                         rule, 
                       data = base_b)

#### Calculate AIC and BIC for each model ####

aic_values <- AIC(beta_model8, beta_model1, beta_model2, beta_model3, beta_model4,
                  beta_model5, beta_model6, beta_model7)

bic_values <- BIC(beta_model8, beta_model1, beta_model2, beta_model3, beta_model4,
                  beta_model5, beta_model6, beta_model7)

# Processing the data to make a table

aic_values$model <- rownames(aic_values) 
rownames(aic_values) <- NULL

aic_values <- aic_values[,c(3,1,2)]

aic_values <- aic_values %>% mutate(model = dplyr::recode(model,
                            `beta_model8` = "Model 8 (full model)",
                            `beta_model1` = "Model 1",
                            `beta_model2` = "Model 2",
                            `beta_model3` = "Model 3",
                            `beta_model4` = "Model 4",
                            `beta_model5` = "Model 5",
                            `beta_model6` = "Model 6",
                            `beta_model7` = "Model 7"))

bic_values$model <- rownames(bic_values) 
rownames(bic_values) <- NULL

bic_values <- bic_values[,c(3,1,2)]

bic_values <- bic_values %>% mutate(model = dplyr::recode(model,
                             `beta_model8` = "Model 8 (full model)",
                             `beta_model1` = "Model 1",
                             `beta_model2` = "Model 2",
                             `beta_model3` = "Model 3",
                             `beta_model4` = "Model 4",
                             `beta_model5` = "Model 5",
                             `beta_model6` = "Model 6",
                             `beta_model7` = "Model 7"))


aic_values$BIC <- bic_values$BIC

aic_values$df <- NULL

colnames(aic_values)[1] ="Model"

final_table <- aic_values

# This is the final table with AIC and BIC values

final_table

# Standard error of PHI

model_summary1 <- summary(beta_model1)
model_summary2 <- summary(beta_model2)
model_summary3 <- summary(beta_model3)
model_summary4 <- summary(beta_model4)
model_summary5 <- summary(beta_model5)
model_summary6 <- summary(beta_model6)
model_summary7 <- summary(beta_model7)
model_summary8 <- summary(beta_model8)

# Extract the standard errors for the mean submodel
mean_se <- model_summary$coefficients$mean[, "Std. Error"]

# Extract the standard errors for the precision submodel (phi)
precision_se <- model_summary$coefficients$precision[, "Std. Error"]



#### Further evaluation for model 8 ####

# Calculating residuals
residuals <- residuals(beta_model8)

par(mfrow = c(1, 3))  # 1 rows, 3 columns

# Residuals vs. Fitted plot
plot1 <- plot(predict(beta_model8, type = "response"), residuals,
     xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs. Fitted")

# Normal Q-Q plot of residuals

plot2 <- qqnorm(residuals)
qqline(residuals)

# Calculate the standardized residuals
bread <- bread(beta_model8)
std_residuals <- residuals(beta_model8) / sqrt(diag(bread))

# Scale-Location plot
plot3 <- plot(predict(beta_model8, type = "response"), sqrt(abs(std_residuals)),
     xlab = "Fitted Values", ylab = "Square Root of Standardized Residuals",
     main = "Scale-Location Plot")

# Save the combined plot as a JPG file with dimensions in inches
jpg_file <- "../Figures/evaluation.jpg"
dev.print(jpeg, file = jpg_file, width = 248.7, height = 144.6, units = "mm", res = 300)
dev.off() 

# Creating indices for cross-validation
set.seed(123)  # For reproducibility
folds <- createFolds(base_b$party_discipline_adjusted, k = 10)  # 10-fold cross-validation

# Looping for cross-validation

# An empty vector to store cross-validated predictions
cv_predictions <- rep(NA, nrow(base_b))

for (i in seq_along(folds)) {
  # Training and testing datasets
  train_indices <- unlist(folds[-i])
  test_indices <- folds[[i]]
  
  train_data <- base_b[train_indices, ]
  test_data <- base_b[test_indices, ]
  
  # Fit beta regression model on the training data
  model <- betareg(party_discipline_adjusted ~ party_affiliation + gender + re_elected +
                     role + pol_experience_std + age_std + prop_votes_std + rule,
                   data = train_data)
  
  # Predict on the testing data
  cv_predictions[test_indices] <- predict(model, newdata = test_data, type = "response")
}

# Calculate cross-validated MSE
cv_mse <- mean((cv_predictions - base_b$party_discipline_adjusted)^2)

cv_mse # Result: 0.03152845