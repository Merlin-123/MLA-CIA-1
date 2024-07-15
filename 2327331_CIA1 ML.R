# Load necessary libraries
library(psych)
library(ggplot2)
library(DataExplorer)
library(car)
library(lmtest)
library(Metrics)
library(MASS)


#MULTIPLE LINEAR REGRESSION
# Understand data structure
str(hr.data.1)
is.na(hr.data.1)

dev.off()

# Adjust the margins
par(mar = c(1, 1, 1, 1))

# Missingness analysis
summary(hr.data.1)
plot_missing(hr.data.1)

# Exploratory Data Analysis (EDA)
pairs.panels(hr.data.1)
plot_histogram(hr.data.1)
plot_density(hr.data.1)
plot_correlation(hr.data.1)

# Split the data into training and testing sets
set.seed(1234)
mixed <- hr.data.1[order(runif(nrow(hr.data.1))),]

# For a 70-30 split
training <- mixed[1:round(0.7 * 850),]
testing <- mixed[(round(0.7 * 850) + 1):850,]



# Build a full model with relevant independent variables
fullmodel <- lm(Monthly.Income ~ ., data = training)
summary(fullmodel)


# Stepwise model selection
step_model <- stepAIC(fullmodel, direction = "both")
summary(step_model)

# Predict and evaluate on test data
fullmodel_pred <- predict(fullmodel, newdata = testing)
step_model_pred <- predict(step_model, newdata = testing)

# Calculate performance metrics
fullmodel_r2 <- summary(fullmodel)$r.squared
fullmodel_test_r2 <- cor(testing$Monthly.Income, fullmodel_pred)^2

step_model_r2 <- summary(step_model)$r.squared
step_model_test_r2 <- cor(testing$Monthly.Income, step_model_pred)^2

# Compare R-squared values
cat("Full Model - Train R2:", fullmodel_r2, "Test R2:", fullmodel_test_r2, "\n")
cat("Stepwise Model - Train R2:", step_model_r2, "Test R2:", step_model_test_r2, "\n")

# Check for multicollinearity
vif(fullmodel)

# Check for autocorrelation
dwtest(fullmodel)

# Model diagnostics
par(mfrow = c(2, 2))
plot(fullmodel)

# Save the model
saveRDS(step_model, "hr_model.rds")

# Load the model (for future use)
loaded_model <- readRDS("hr_model.rds")


# Predict on new data
new_data <- testing[1:5, ]
predictions <- predict(loaded_model, new_data)
print(predictions)










