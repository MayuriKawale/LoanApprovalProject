######################################################################
#
# Final Project - first round
#
# Course     : ISE-5103-001 Intelligent Data Analytics
# Instructor : Charles Nicholson
# Primary GTA: Sindhuja Rao
# Author     : Mayuri Kawale/Gen Feng/Manisha Fnu/Seunghyun Oh
# Homework   : Final Project
# Date       : 12/12/2024
# 
# 1, All code can run normally or run in batches
#
######################################################################


######################
# Preloading Package
######################

library(caret)
library(tidyr)
library(base)
library(datasets)
library(graphics)
library(grDevices)
library(methods)
library(stats)
library(utils)
library(tidyverse)
library(dplyr) 
library(knitr)      
library(kableExtra) 
library(parallel)
library(doParallel)
library(randomForest)
library(MASS)        
library(gbm)         
library(xgboost)     
library(ranger)      
library(glmnet)      
library(mice)
library(car)
library(pROC)

######################
# load datafile to dataset
######################

getwd()

train_data <- read.csv("credit_risk_dataset.csv")
train_data_new <- train_data

######################
# function define
######################

func_UTF8 <- 
  function(x) {
    # Converts character data to UTF-8 encoding
    if(is.character(x)) {
      iconv(x, from = "UTF-8", to = "UTF-8", sub = "byte")
    } 
    else { x }
  }

Q1 <- function(x, na.rm=TRUE){
  # Calculates the first quartile
  quantile(x, na.rm=na.rm)[2]
}

Q3 <- function(x, na.rm=TRUE){
  # Calculates the third quartile
  quantile(x, na.rm=na.rm)[4]
}

numSummary <- 
  function(x) {
    # Provides a numerical summary of the data including length, distinct values, missing values, mean, quantiles, and standard deviation
    return(c(length(x), 
             n_distinct(x),
             sum(is.na(x) | x == "" | trimws(x) == ""), 
             mean(x, na.rm=TRUE), 
             min(x, na.rm=TRUE), 
             Q1(x, na.rm=TRUE), 
             median(x, na.rm=TRUE),
             Q3(x, na.rm=TRUE), 
             max(x, na.rm=TRUE), 
             sd(x, na.rm = TRUE)))
  }

catSummary <- 
  function(x) {
    # Provides a categorical summary including modes and their frequencies
    return(c(length(x), 
             n_distinct(x), 
             sum(is.na(x) | x == "" | trimws(x) == ""),
             getmodes(x,type=1),  
             getmodesCnt(x,type=1),
             getmodes(x,type=2),  
             getmodesCnt(x,type=2),
             getmodes(x,type=-1), 
             getmodesCnt(x,type=-1)))
  }

getmodes <- 
  function(v,type=1) {
    # Determines the mode(s) based on the specified type
    tbl <- table(v)
    m1 <- which.max(tbl)
    if (type == 1) {
      return(names(m1))  # Most frequent value
    } else if (type == 2) {
      return(names(which.max(tbl[-m1])))  # Second most frequent value
    } else if (type == -1) {
      return(names(which.min(tbl)))  # Least frequent value
    } else {
      stop("Invalid type selected")
    }
  }

getmodesCnt <- 
  function(v,type=1) {
    # Counts the frequency of mode(s) based on the specified type
    tbl <- table(v)
    m1 <- which.max(tbl)
    if (type == 1) {
      return(max(tbl))  # Frequency of the most frequent value
    } else if (type == 2) {
      return(max(tbl[-m1]))  # Frequency of the second most frequent value
    } else if (type == -1) {
      return(min(tbl))  # Frequency of the least frequent value
    } else {
      stop("Invalid type selected")
    }
  }

get_mode <- function(v) {
  # Returns the most frequent value in the vector
  uniq_vals <- unique(v)
  uniq_vals[which.max(tabulate(match(v, uniq_vals)))]
}

custom_summary <- function(data, lev = NULL, model = NULL) {
  # Custom evaluation function to calculate ROC, sensitivity, specificity, accuracy, and kappa
  sens_value <- sensitivity(data$pred, data$obs, positive = lev[1])
  spec_value <- specificity(data$pred, data$obs, negative = lev[2])
  roc_value <- pROC::roc(data$obs, data[, lev[1]])$auc
  accuracy_value <- mean(data$pred == data$obs)
  kappa_value <- confusionMatrix(data$pred, data$obs)$overall["Kappa"]
  c(
    ROC = roc_value,
    Sens = sens_value,
    Spec = spec_value,
    Accuracy = accuracy_value,
    Kappa = kappa_value
  )
}

######################
# data divided
######################

# Apply UTF-8 encoding conversion to all columns in the dataset
train_data_new[] <- lapply(train_data_new, func_UTF8)

# Select numeric columns from the dataset for numerical analysis
numData_train <- train_data_new %>%
  dplyr::select(where(is.numeric))

# Convert character columns to factors for categorical analysis
catData_train <- train_data_new %>%
  dplyr::transmute(across(where(is.character), as.factor))

######################
# Generate a data quality report for the numeric variables
######################

# Generate numerical summary for all numeric columns in the dataset
numericSummary_train <- numData_train %>% dplyr::reframe(across(everything(), numSummary))

# Add column headers for the summary statistics
numericSummary_train <- 
  cbind(stat=c("n", "unique", "missing", "mean",
               "min", "Q1", "median", "Q3", "max", "sd"), 
        numericSummary_train)

# Reshape the summary into a tidy format and calculate percentages for missing and unique values
numericSummary_train <- numericSummary_train %>%
  pivot_longer(!stat, names_to = "variable", values_to = "value") %>% # Long format
  pivot_wider(names_from = stat, values_from = value) %>%            # Wide format
  dplyr::mutate(missing_pct = 100 * missing / n,                     # Percentage of missing values
                unique_pct = 100 * unique / n) %>%                   # Percentage of unique values
  dplyr::select(variable, n, missing, missing_pct, unique, unique_pct, everything()) # Rearrange columns

# Set display options for numeric precision and disable scientific notation
options(digits = 3)
options(scipen = 99)

# Display the summary table with styling
numericSummary_train %>% kable() %>% kable_styling(font_size = 12)

# Save the styled summary table as an HTML file
save_kable(
  numericSummary_train %>% kable() %>% kable_styling(font_size = 12),
  file = "numericSummary_train.html"
)

######################
# Generate a data quality report for the categorical variables
######################

# Generate categorical summary for all categorical columns in the dataset
categoricalSummary_train <- catData_train %>% dplyr::reframe(across(everything(), catSummary))

# Add column headers for the summary statistics
categoricalSummary_train <- cbind(
  stat = c("n", "unique", "missing", "1st mode", "1st mode freq", "2nd mode", 
           "2nd mode freq", "least common", "least common freq"), 
  categoricalSummary_train
)

# Reshape the summary into a tidy format and calculate additional statistics
categoricalSummary_train <- categoricalSummary_train %>%
  pivot_longer(!stat, names_to = "variable", values_to = "value") %>%  # Long format
  pivot_wider(names_from = stat, values_from = value) %>%             # Wide format
  dplyr::mutate(
    across(c(n, missing, unique, `1st mode freq`, `2nd mode freq`, `least common freq`), as.numeric), # Convert to numeric
    missing_pct = 100 * missing / n,                               # Percentage of missing values
    unique_pct = 100 * unique / n,                                 # Percentage of unique values
    freqRatio = `1st mode freq` / `2nd mode freq`                  # Ratio of the top two modes' frequencies
  ) %>% 
  dplyr::select(variable, n, missing, missing_pct, unique, unique_pct, freqRatio, everything()) # Rearrange columns

# Set display options for numeric precision and disable scientific notation
options(digits = 3)
options(scipen = 99)

# Display the summary table with styling
categoricalSummary_train %>% kable() %>% kable_styling(font_size = 12)

# Save the styled summary table as an HTML file
save_kable(
  categoricalSummary_train %>% kable() %>% kable_styling(font_size = 12),
  file = "categoricalSummary_train.html"
)

######################
# data preparing 1
######################


# Filter out records where person_age exceeds a reasonable limit (e.g., 120 years)
train_data_new <- train_data_new[train_data_new$person_age <= 120, ]

# Filter out records where person_emp_length is greater than person_age
# Retain rows where person_emp_length is NA or person_emp_length is less than or equal to person_age
train_data_new <- train_data_new[
  (is.na(train_data_new$person_emp_length)) | 
  (train_data_new$person_emp_length <= train_data_new$person_age), 
]

######################
# data preparing 2
######################

# Perform imputation for the "person_emp_length" column using the MICE package
# Select relevant columns for imputation
imputed_data <- mice(
  train_data_new[, c("person_emp_length", "person_age", "person_income", "cb_person_cred_hist_length")], 
  method = "pmm", # Use Predictive Mean Matching (PMM) method
  m = 1,          # Number of multiple imputations
  maxit = 5,      # Maximum number of iterations
  seed = 123      # Seed for reproducibility
)

# Retrieve the completed dataset after imputation
completed_data <- complete(imputed_data)

# Update the original dataset with imputed values for "person_emp_length"
train_data_new$person_emp_length <- completed_data$person_emp_length

# Perform imputation for the "loan_int_rate" column
imputed_data <- mice(
  train_data_new[, c("loan_int_rate", "loan_amnt", "person_income", 
                     "loan_grade", "cb_person_cred_hist_length", 
                     "cb_person_default_on_file")], 
  method = "pmm", # Use Predictive Mean Matching (PMM) method
  m = 1,          # Number of multiple imputations
  maxit = 5,      # Maximum number of iterations
  seed = 123      # Seed for reproducibility
)

# Check the missing values in "loan_int_rate" column
summary(is.na(train_data_new$loan_int_rate))

# Retrieve the completed dataset after imputation
completed_data <- complete(imputed_data)

# Update the original dataset with imputed values for "loan_int_rate"
train_data_new$loan_int_rate <- completed_data$loan_int_rate

######################
# data preparing 3
######################

# Perform one-hot encoding for "person_home_ownership" and retain relevant categories
train_data_new$person_home_ownership <- 
  ifelse(train_data_new$person_home_ownership %in% c("OWN", "OTHER"), "OTHER", train_data_new$person_home_ownership)

# Create dummy variables for "person_home_ownership"
encoded_data <- model.matrix(~ person_home_ownership - 1, data = train_data_new)

# Combine the encoded variables back into the original dataset
train_data_new <- cbind(train_data_new, encoded_data)

# Perform one-hot encoding for "loan_intent"
encoded_data <- model.matrix(~ loan_intent - 1, data = train_data_new)

# Combine the encoded variables back into the original dataset
train_data_new <- cbind(train_data_new, encoded_data)

# Simplify and factorize "loan_grade"
train_data_new$loan_grade <- ifelse(train_data_new$loan_grade %in% c("F", "G"), "Other", train_data_new$loan_grade)
train_data_new$loan_grade <- factor(train_data_new$loan_grade, levels = c("A", "B", "C", "D", "E", "Other"), ordered = TRUE)

# Convert loan grade to numeric
train_data_new$loan_grade_numeric <- as.numeric(train_data_new$loan_grade)

# Encode "cb_person_default_on_file" as a binary variable
train_data_new$cb_person_default_on_file <- factor(train_data_new$cb_person_default_on_file, levels = c("N", "Y"))
train_data_new$cb_person_default_numeric <- as.numeric(train_data_new$cb_person_default_on_file) - 1

# Drop original categorical columns
train_data_new <- train_data_new %>% dplyr::select(-person_home_ownership, -loan_intent, -loan_grade, -cb_person_default_on_file)

# Apply log transformation to numerical features
train_data_new$log_age <- log(train_data_new$person_age + 1)
train_data_new$log_income <- log(train_data_new$person_income + 1)
train_data_new$log_emp_length <- log(train_data_new$person_emp_length + 1)
train_data_new$log_loan_amnt <- log(train_data_new$loan_amnt + 1)
train_data_new$log_int_rate <- log(train_data_new$loan_int_rate + 1)

# Create a new feature representing the percentage of income allocated to the loan
train_data_new$loan_percent_income <- train_data_new$loan_amnt / train_data_new$person_income

# Log-transform the credit history length
train_data_new$log_cred_hist_length <- log(train_data_new$cb_person_cred_hist_length + 1)

# Drop original numerical columns after creating transformed versions
train_data_new <- train_data_new %>% dplyr::select(-person_age, -person_income, -person_emp_length, -loan_amnt, -loan_int_rate, -cb_person_cred_hist_length)

######################
# data preparing 4
######################


# Preserve the loan_status variable before processing
var_loan_status <- train_data_new$loan_status

# Apply preprocessing to center and scale numeric features
preprocess_params <- preProcess(train_data_new, method = c("center", "scale"))
train_data_new <- predict(preprocess_params, train_data_new)

# Reattach the loan_status variable after preprocessing
train_data_new$loan_status <- var_loan_status

# Ensure loan_status is a factor variable for classification
train_data_new$loan_status <- as.factor(train_data_new$loan_status)

# Identify near-zero variance features
nzv <- nearZeroVar(train_data_new, saveMetrics = TRUE)

# Remove near-zero variance features from the dataset
train_data_new <- train_data_new[, !nzv$nzv]

# Display the structure of the updated dataset
str(train_data_new)

# Save the processed loan_status variable for later use
v_loan_status <- train_data_new$loan_status

######################
# GLM modeling 1 
# All Variables
######################

# Optional: Add interaction terms for feature engineering
# train_data_new$interaction1 <- train_data_new$loan_percent_income * train_data_new$log_income
# train_data_new$interaction2 <- train_data_new$log_int_rate * train_data_new$loan_grade_numeric
# train_data_new$interaction3 <- train_data_new$person_home_ownershipRENT * train_data_new$loan_percent_income


# Define training control parameters
train_control <- trainControl(
  method = "cv",               # Use cross-validation
  number = 10,                 # Perform 10-fold cross-validation
  summaryFunction = custom_summary,  # Use custom evaluation function
  classProbs = TRUE,           # Output class probabilities
  savePredictions = "final"    # Save final predictions
)

# Convert loan_status to a factor with appropriate labels
train_data_new$loan_status <- factor(train_data_new$loan_status, levels = c(0, 1), labels = c("No", "Yes"))

# Train a Generalized Linear Model (GLM) using caret
glm_cv <- train(
  x = train_data_new[, -1],    # Exclude the first column (assume it's an identifier)
  y = train_data_new$loan_status,  # Target variable
  method = "glm",              # GLM method
  family = binomial(link = "logit"),  # Logistic regression
  trControl = train_control,   # Training control parameters
  metric = "ROC"               # Use ROC as the performance metric
)

# Display cross-validation results
glm_cv$results
print(glm_cv)

# Summarize the trained model
summary(glm_cv)

######################
# GLM modeling 2
# Selected Variables + Interaction
######################

# Define formula with interactions for the GLM model
formula_with_interactions <- as.formula(
  "loan_status ~ 
   loan_percent_income + 
   log_income + 
   log_int_rate + 
   loan_grade_numeric + 
   person_home_ownershipRENT + 
   loan_percent_income * log_income + 
   log_int_rate * loan_grade_numeric + 
   person_home_ownershipRENT * loan_percent_income"
)

# Convert loan_status to a factor with proper labels
train_data_new$loan_status <- factor(train_data_new$loan_status, levels = c(0, 1), labels = c("No", "Yes"))

# Define training control parameters
train_control <- trainControl(
  method = "cv",               # Use cross-validation
  number = 10,                 # Perform 10-fold cross-validation
  summaryFunction = custom_summary,  # Use custom evaluation function
  classProbs = TRUE,           # Output class probabilities
  savePredictions = "final"    # Save final predictions
)

# Train the GLM model using the formula with interaction terms
glm_cv <- train(
  formula_with_interactions,   # Formula with interaction terms
  data = train_data_new,       # Dataset
  method = "glm",              # GLM method
  family = binomial(link = "logit"),  # Logistic regression
  trControl = train_control,   # Training control parameters
  metric = "ROC"               # Use ROC as the performance metric
)

# Display cross-validation results
glm_cv$results
print(glm_cv)

# Summarize the trained model
summary(glm_cv)

# Evaluate the model with a confusion matrix
# confusionMatrix(predict(glm_cv, train_data_new), train_data_new$loan_status)

# Plot the ROC curve
# roc_curve <- roc(train_data_new$loan_status, predict(glm_cv, train_data_new, type = "prob")[, "Yes"])
# plot(roc_curve)

######################
# GLM modeling 3
# All Variables + Interaction
######################

# Create interaction terms for the model
train_data_new$interaction1 <- train_data_new$loan_percent_income * train_data_new$log_income
train_data_new$interaction2 <- train_data_new$log_int_rate * train_data_new$loan_grade_numeric
train_data_new$interaction3 <- train_data_new$person_home_ownershipRENT * train_data_new$loan_percent_income

# Set up training control for cross-validation
train_control <- trainControl(
  method = "cv",               # Perform cross-validation
  number = 10,                 # Use 10-fold cross-validation
  summaryFunction = custom_summary,  # Custom evaluation function
  classProbs = TRUE,           # Output class probabilities
  savePredictions = "final"    # Save final predictions
)

# Convert loan_status to a factor with appropriate labels
train_data_new$loan_status <- factor(train_data_new$loan_status, levels = c(0, 1), labels = c("No", "Yes"))

# Train the GLM model using caret
glm_cv <- train(
  x = train_data_new[, -1],    # Exclude the first column (assume it's an identifier)
  y = train_data_new$loan_status,  # Target variable
  method = "glm",              # Use GLM method
  family = binomial(link = "logit"), # Logistic regression
  trControl = train_control,   # Cross-validation control
  metric = "ROC"               # Use ROC as the evaluation metric
)

# Display cross-validation results
glm_cv$results
print(glm_cv)

# Summarize the GLM model
summary(glm_cv)

######################
# LASSO modeling 1
# All Variables
######################

# Define training control parameters for cross-validation
train_control <- trainControl(
  method = "cv",               # Use cross-validation
  number = 10,                 # Perform 10-fold cross-validation
  summaryFunction = custom_summary,  # Use custom evaluation function
  classProbs = TRUE,           # Output class probabilities
  savePredictions = "final"    # Save final predictions
)

# Set up parallel processing to speed up computations
cl <- makeCluster(detectCores() - 1)  # Leave one core for the operating system
registerDoParallel(cl)

# Convert loan_status to factor with appropriate labels
train_data_new$loan_status <- factor(train_data_new$loan_status,
                                     levels = c("0", "1"),
                                     labels = c("No", "Yes"))

# Train LASSO model using caret with glmnet
lasso_model <- train(
  x = train_data_new[, -1],          # Predictors (exclude identifier column)
  y = train_data_new$loan_status,   # Target variable
  method = "glmnet",                # LASSO regression method
  trControl = train_control,        # Training control parameters
  tuneGrid = expand.grid(           # Grid of alpha and lambda values for tuning
    alpha = 1,                      # LASSO regularization
    lambda = seq(0.001, 0.1, by = 0.01)  # Lambda values to try
  ),
  metric = "ROC"                    # Use ROC as the performance metric
)

# Stop parallel processing
stopCluster(cl)
registerDoSEQ()

# Display cross-validation results
lasso_model$results
print(lasso_model)

# Summarize the LASSO model
summary(lasso_model)

# Find the lambda value with the highest ROC
best_lambda_roc <- lasso_model$results %>%
  filter(ROC == max(ROC)) %>%
  pull(lambda)

# Find the lambda value with the highest Accuracy
best_lambda_accuracy <- lasso_model$results %>%
  filter(Accuracy == max(Accuracy)) %>%
  pull(lambda)

# Output the best lambda values
cat("Best Lambda (ROC):", best_lambda_roc, "\n")
cat("Best Lambda (Accuracy):", best_lambda_accuracy, "\n")

# Get detailed results for the best lambda (based on ROC)
best_lambda_results <- lasso_model$results %>%
  filter(lambda == best_lambda_roc)

print(best_lambda_results)

######################
# LASSO modeling 2
# Selected Variables + Interaction
######################


# Define formula with interactions for LASSO model
formula_with_interactions <- as.formula(
  "loan_status ~ 
   loan_percent_income + 
   log_income + 
   log_int_rate + 
   loan_grade_numeric + 
   person_home_ownershipRENT + 
   loan_percent_income * log_income + 
   log_int_rate * loan_grade_numeric + 
   person_home_ownershipRENT * loan_percent_income"
)

# Convert loan_status to factor with appropriate labels
train_data_new$loan_status <- factor(train_data_new$loan_status, levels = c(0, 1), labels = c("No", "Yes"))

# Define training control parameters
train_control <- trainControl(
  method = "cv",               # Use cross-validation
  number = 10,                 # Perform 10-fold cross-validation
  summaryFunction = custom_summary,  # Use custom evaluation function
  classProbs = TRUE,           # Output class probabilities
  savePredictions = "final"    # Save final predictions
)

# Set up parallel processing to speed up computations
cl <- makeCluster(detectCores() - 1)  # Leave one core for the operating system
registerDoParallel(cl)

# Train LASSO model using caret with formula and interaction terms
lasso_model <- train(
  formula_with_interactions,   # Formula with interaction terms
  data = train_data_new,       # Dataset
  method = "glmnet",           # LASSO regression method
  trControl = train_control,   # Training control parameters
  tuneGrid = expand.grid(      # Grid of alpha and lambda values for tuning
    alpha = 1,                 # LASSO regularization
    lambda = seq(0.001, 0.1, by = 0.01)  # Lambda values to try
  ),
  metric = "ROC"               # Use ROC as the performance metric
)

# Stop parallel processing
stopCluster(cl)
registerDoSEQ()

# Display cross-validation results
lasso_model$results
print(lasso_model)

# Summarize the LASSO model
summary(lasso_model)

# Find the lambda value with the highest ROC
best_lambda_roc <- lasso_model$results %>%
  filter(ROC == max(ROC)) %>%
  pull(lambda)

# Find the lambda value with the highest Accuracy
best_lambda_accuracy <- lasso_model$results %>%
  filter(Accuracy == max(Accuracy)) %>%
  pull(lambda)

# Output the best lambda values
cat("Best Lambda (ROC):", best_lambda_roc, "\n")
cat("Best Lambda (Accuracy):", best_lambda_accuracy, "\n")

# Get detailed results for the best lambda (based on ROC)
best_lambda_results <- lasso_model$results %>%
  filter(lambda == best_lambda_roc)

print(best_lambda_results)


######################
# LASSO modeling 3
# All Variables + Interaction
######################




# Create interaction terms for the model
train_data_new$interaction1 <- train_data_new$loan_percent_income * train_data_new$log_income
train_data_new$interaction2 <- train_data_new$log_int_rate * train_data_new$loan_grade_numeric
train_data_new$interaction3 <- train_data_new$person_home_ownershipRENT * train_data_new$loan_percent_income

# Define training control parameters for cross-validation
train_control <- trainControl(
  method = "cv",               # Use cross-validation
  number = 10,                 # Perform 10-fold cross-validation
  summaryFunction = custom_summary,  # Use custom evaluation function
  classProbs = TRUE,           # Output class probabilities
  savePredictions = "final"    # Save final predictions
)

# Convert loan_status to factor with appropriate labels
train_data_new$loan_status <- factor(train_data_new$loan_status, levels = c(0, 1), labels = c("No", "Yes"))

# Set up parallel processing to speed up computations
cl <- makeCluster(detectCores() - 1)  # Leave one core for the operating system
registerDoParallel(cl)

# Train LASSO model using caret with all variables and interaction terms
lasso_model <- train(
  x = train_data_new[, -1],          # Exclude the first column (assume it's an identifier)
  y = train_data_new$loan_status,   # Target variable
  method = "glmnet",                # LASSO regression method
  trControl = train_control,        # Training control parameters
  tuneGrid = expand.grid(           # Grid of alpha and lambda values for tuning
    alpha = 1,                      # LASSO regularization
    lambda = seq(0.001, 0.1, by = 0.01)  # Lambda values to try
  ),
  metric = "ROC"                    # Use ROC as the performance metric
)

# Stop parallel processing
stopCluster(cl)
registerDoSEQ()

# Display cross-validation results
lasso_model$results
print(lasso_model)

# Summarize the LASSO model
summary(lasso_model)

# Find the lambda value with the highest ROC
best_lambda_roc <- lasso_model$results %>%
  filter(ROC == max(ROC)) %>%
  pull(lambda)

# Find the lambda value with the highest Accuracy
best_lambda_accuracy <- lasso_model$results %>%
  filter(Accuracy == max(Accuracy)) %>%
  pull(lambda)

# Output the best lambda values
cat("Best Lambda (ROC):", best_lambda_roc, "\n")
cat("Best Lambda (Accuracy):", best_lambda_accuracy, "\n")

# Get detailed results for the best lambda (based on ROC)
best_lambda_results <- lasso_model$results %>%
  filter(lambda == best_lambda_roc)

print(best_lambda_results)

######################
# GBM modeling 1
# All Variables
######################



# Set training control parameters for cross-validation
train_control <- trainControl(
  method = "cv",               # Perform cross-validation
  number = 10,                 # Use 10-fold cross-validation
  summaryFunction = custom_summary,  # Custom evaluation function
  classProbs = TRUE,           # Output class probabilities
  savePredictions = "final"    # Save final predictions
)

# Convert loan_status to a factor with appropriate labels
train_data_new$loan_status <- factor(train_data_new$loan_status,
                                     levels = c("0", "1"),
                                     labels = c("No", "Yes"))

# Set up parallel processing to optimize computation
cl <- makeCluster(detectCores() - 1)  # Use all cores except one
registerDoParallel(cl)

# Train Gradient Boosting Machine (GBM) model
gbm_model <- train(
  x = train_data_new[, -1],    # Exclude the first column (assume it's an identifier)
  y = train_data_new$loan_status,  # Target variable
  method = "gbm",              # GBM method
  trControl = train_control,   # Cross-validation control
  metric = "ROC"               # Use ROC as the evaluation metric
)

# Stop parallel processing
stopCluster(cl)
registerDoSEQ()

# Display cross-validation results
gbm_model$results
print(gbm_model)

# Summarize the GBM model
summary(gbm_model)

# Display the best parameters based on cross-validation
gbm_model$bestTune

# Find parameters with the highest ROC
best_params_ROC <- gbm_model$results %>%
  as_tibble() %>%           # Convert to tibble for easier manipulation
  arrange(desc(ROC)) %>%    # Sort by descending ROC
  slice(1)                  # Select the top row

print(best_params_ROC)

# Find parameters with the highest Accuracy
best_params_Accuracy <- gbm_model$results %>%
  as_tibble() %>%           # Convert to tibble
  arrange(desc(Accuracy)) %>% # Sort by descending Accuracy
  slice(1)                  # Select the top row

print(best_params_Accuracy)

# Print best parameters for highest ROC
cat("Best Parameters (ROC):", 
    best_params_ROC$n.trees, 
    best_params_ROC$interaction.depth, 
    best_params_ROC$shrinkage, 
    "\n")

# Print best parameters for highest Accuracy
cat("Best Parameters (Accuracy):", 
    best_params_Accuracy$n.trees, 
    best_params_Accuracy$interaction.depth, 
    best_params_Accuracy$shrinkage, 
    "\n")

######################
# GBM modeling 2
# Selected Variables + Interaction
######################

# Define formula with interaction terms for GBM model
formula_with_interactions <- as.formula(
  "loan_status ~ 
   loan_percent_income + 
   log_income + 
   log_int_rate + 
   loan_grade_numeric + 
   person_home_ownershipRENT + 
   loan_percent_income * log_income + 
   log_int_rate * loan_grade_numeric + 
   person_home_ownershipRENT * loan_percent_income"
)

# Convert loan_status to factor with appropriate labels
train_data_new$loan_status <- factor(train_data_new$loan_status, levels = c(0, 1), labels = c("No", "Yes"))

# Set training control parameters for cross-validation
train_control <- trainControl(
  method = "cv",               # Perform cross-validation
  number = 10,                 # Use 10-fold cross-validation
  summaryFunction = custom_summary,  # Custom evaluation function
  classProbs = TRUE,           # Output class probabilities
  savePredictions = "final"    # Save final predictions
)

# Set up parallel processing to optimize computation
cl <- makeCluster(detectCores() - 1)  # Use all cores except one for the OS
registerDoParallel(cl)

# Train GBM model using the formula with interactions
gbm_model <- train(
  formula_with_interactions,   # Formula including interaction terms
  data = train_data_new,       # Dataset
  method = "gbm",              # GBM method
  trControl = train_control,   # Cross-validation control
  metric = "ROC"               # Use ROC as the evaluation metric
)

# Stop parallel processing
stopCluster(cl)
registerDoSEQ()

# Display cross-validation results
gbm_model$results
print(gbm_model)

# Summarize the GBM model
summary(gbm_model)

# Display the best parameters from cross-validation
gbm_model$bestTune

# Find parameters with the highest ROC
best_params_ROC <- gbm_model$results %>%
  as_tibble() %>%           # Convert to tibble for easier manipulation
  arrange(desc(ROC)) %>%    # Sort by descending ROC
  slice(1)                  # Select the top row

print(best_params_ROC)

# Find parameters with the highest Accuracy
best_params_Accuracy <- gbm_model$results %>%
  as_tibble() %>%           # Convert to tibble
  arrange(desc(Accuracy)) %>% # Sort by descending Accuracy
  slice(1)                  # Select the top row

print(best_params_Accuracy)

# Print best parameters for highest ROC
cat("Best Parameters (ROC):", 
    best_params_ROC$n.trees, 
    best_params_ROC$interaction.depth, 
    best_params_ROC$shrinkage, 
    "\n")

# Print best parameters for highest Accuracy
cat("Best Parameters (Accuracy):", 
    best_params_Accuracy$n.trees, 
    best_params_Accuracy$interaction.depth, 
    best_params_Accuracy$shrinkage, 
    "\n")

######################
# GBM modeling 3
# All Variables + Interaction
######################

# Create interaction terms for the model
train_data_new$interaction1 <- train_data_new$loan_percent_income * train_data_new$log_income
train_data_new$interaction2 <- train_data_new$log_int_rate * train_data_new$loan_grade_numeric
train_data_new$interaction3 <- train_data_new$person_home_ownershipRENT * train_data_new$loan_percent_income

# Set training control parameters for cross-validation
train_control <- trainControl(
  method = "cv",               # Perform cross-validation
  number = 10,                 # Use 10-fold cross-validation
  summaryFunction = custom_summary,  # Custom evaluation function
  classProbs = TRUE,           # Output class probabilities
  savePredictions = "final"    # Save final predictions
)

# Convert loan_status to factor with appropriate labels
train_data_new$loan_status <- factor(train_data_new$loan_status, levels = c(0, 1), labels = c("No", "Yes"))

# Set up parallel processing to optimize computation
cl <- makeCluster(detectCores() - 1)  # Use all cores except one for the OS
registerDoParallel(cl)

# Train GBM model with all variables and interaction terms
gbm_model <- train(
  x = train_data_new[, -1],    # Exclude the first column (assume it's an identifier)
  y = train_data_new$loan_status,  # Target variable
  method = "gbm",              # GBM method
  trControl = train_control,   # Cross-validation control
  metric = "ROC"               # Use ROC as the evaluation metric
)

# Stop parallel processing
stopCluster(cl)
registerDoSEQ()

# Display cross-validation results
gbm_model$results
print(gbm_model)

# Summarize the GBM model
summary(gbm_model)

# Display the best parameters from cross-validation
gbm_model$bestTune

# Find parameters with the highest ROC
library(dplyr)
best_params_ROC <- gbm_model$results %>%
  as_tibble() %>%           # Convert to tibble for easier manipulation
  arrange(desc(ROC)) %>%    # Sort by descending ROC
  slice(1)                  # Select the top row

print(best_params_ROC)

# Find parameters with the highest Accuracy
best_params_Accuracy <- gbm_model$results %>%
  as_tibble() %>%           # Convert to tibble
  arrange(desc(Accuracy)) %>% # Sort by descending Accuracy
  slice(1)                  # Select the top row

print(best_params_Accuracy)

# Print best parameters for highest ROC
cat("Best Parameters (ROC):", 
    best_params_ROC$n.trees, 
    best_params_ROC$interaction.depth, 
    best_params_ROC$shrinkage, 
    "\n")

# Print best parameters for highest Accuracy
cat("Best Parameters (Accuracy):", 
    best_params_Accuracy$n.trees, 
    best_params_Accuracy$interaction.depth, 
    best_params_Accuracy$shrinkage, 
    "\n")

######################
# Random modeling 1
# All Variables
######################


# Set up training control for repeated cross-validation
train_control <- trainControl(
  method = 'repeatedcv',       # Perform repeated cross-validation
  number = 10,                 # Use 10-fold cross-validation
  repeats = 5,                 # Repeat the process 5 times
  search = 'random',           # Randomized search for hyperparameter tuning
  classProbs = TRUE,           # Output class probabilities
  verboseIter = TRUE,          # Display training progress
  summaryFunction = custom_summary,  # Custom evaluation function
  savePredictions = "final"    # Save final predictions
)

# Convert loan_status to a factor with appropriate labels
train_data_new$loan_status <- factor(train_data_new$loan_status,
                                     levels = c("0", "1"),
                                     labels = c("No", "Yes"))

# Load required libraries
library(caret)
library(ranger)
library(parallel)
library(doParallel)

# Set up parallel processing to optimize computation
cl <- makeCluster(detectCores() - 1)  # Use all cores except one for the OS
registerDoParallel(cl)

# Set seed for reproducibility
set.seed(123)

# Train the Ranger model (Random Forest implementation)
ranger_model <- train(
  x = train_data_new[, -1],    # Exclude the first column (assume it's an identifier)
  y = train_data_new$loan_status,  # Target variable
  method = 'ranger',           # Use the ranger method
  num.trees = 200,             # Number of trees in the forest
  trControl = train_control,   # Cross-validation control
  metric = "ROC"               # Use ROC as the evaluation metric
)

# Stop parallel processing
stopCluster(cl)
registerDoSEQ()

# Save the trained model to a file
saveRDS(ranger_model, file = "ranger_model_1.rds")

# To load the model, use: ranger_model <- readRDS("ranger_model_1.rds")

# Display cross-validation results
print(ranger_model)
ranger_model$results

# Display the best-tuned hyperparameters
ranger_model$bestTune

# Access and print the final model details
final_model <- ranger_model$finalModel
print(final_model)

######################
# Random modeling 2
# Selected Variables + Interaction
######################


# Define formula with interaction terms for the Ranger model
formula_with_interactions <- as.formula(
  "loan_status ~ 
   loan_percent_income + 
   log_income + 
   log_int_rate + 
   loan_grade_numeric + 
   person_home_ownershipRENT + 
   loan_percent_income * log_income + 
   log_int_rate * loan_grade_numeric + 
   person_home_ownershipRENT * loan_percent_income"
)

# Convert loan_status to a factor with appropriate labels
train_data_new$loan_status <- factor(train_data_new$loan_status, levels = c(0, 1), labels = c("No", "Yes"))

# Set up training control for repeated cross-validation
train_control <- trainControl(
  method = 'repeatedcv',       # Perform repeated cross-validation
  number = 10,                 # Use 10-fold cross-validation
  repeats = 5,                 # Repeat the process 5 times
  search = 'random',           # Randomized search for hyperparameter tuning
  classProbs = TRUE,           # Output class probabilities
  verboseIter = TRUE,          # Display training progress
  summaryFunction = custom_summary,  # Custom evaluation function
  savePredictions = "final"    # Save final predictions
)

# Set up parallel processing to optimize computation
cl <- makeCluster(detectCores() - 1)  # Use all cores except one for the OS
registerDoParallel(cl)

# Set seed for reproducibility
set.seed(123)

# Train the Ranger model with interaction terms
ranger_model <- train(
  formula_with_interactions,   # Formula including interaction terms
  data = train_data_new,       # Dataset
  method = 'ranger',           # Use the ranger method
  num.trees = 200,             # Number of trees in the forest
  trControl = train_control,   # Cross-validation control
  metric = "ROC"               # Use ROC as the evaluation metric
)

# Stop parallel processing
stopCluster(cl)
registerDoSEQ()

# Save the trained model to a file
saveRDS(ranger_model, file = "ranger_model_2.rds")

# To load the model, use: ranger_model <- readRDS("ranger_model_2.rds")

# Display cross-validation results
print(ranger_model)
ranger_model$results

# Display the best-tuned hyperparameters
ranger_model$bestTune

# Access and print the final model details
final_model <- ranger_model$finalModel
print(final_model)

######################
# Random modeling 3
# All Variables + Interaction
######################


# Create interaction terms for the model
train_data_new$interaction1 <- train_data_new$loan_percent_income * train_data_new$log_income
train_data_new$interaction2 <- train_data_new$log_int_rate * train_data_new$loan_grade_numeric
train_data_new$interaction3 <- train_data_new$person_home_ownershipRENT * train_data_new$loan_percent_income

# Set up training control for cross-validation
train_control <- trainControl(
  method = "cv",               # Perform cross-validation
  number = 10,                 # Use 10-fold cross-validation
  summaryFunction = custom_summary,  # Custom evaluation function
  classProbs = TRUE,           # Output class probabilities
  savePredictions = "final"    # Save final predictions
)

# Convert loan_status to factor with appropriate labels
train_data_new$loan_status <- factor(train_data_new$loan_status, levels = c(0, 1), labels = c("No", "Yes"))

# Set up parallel processing to optimize computation
cl <- makeCluster(detectCores() - 1)  # Use all cores except one for the OS
registerDoParallel(cl)

# Train the Ranger model (Random Forest implementation)
ranger_model <- train(
  x = train_data_new[, -1],    # Exclude the first column (assume it's an identifier)
  y = train_data_new$loan_status,  # Target variable
  method = 'ranger',           # Use the ranger method
  num.trees = 200,             # Number of trees in the forest
  trControl = train_control,   # Cross-validation control
  metric = "ROC"               # Use ROC as the evaluation metric
)

# Stop parallel processing
stopCluster(cl)
registerDoSEQ()

# Save the trained model to a file
saveRDS(ranger_model, file = "ranger_model_3.rds")

# To load the model, use: ranger_model <- readRDS("ranger_model_3.rds")

# Display cross-validation results
print(ranger_model)
ranger_model$results

# Display the best-tuned hyperparameters
ranger_model$bestTune

# Access and print the final model details
final_model <- ranger_model$finalModel
print(final_model)







######################################################################
#
# Final Project second round
#
# Course     : ISE-5103-001 Intelligent Data Analytics
# Instructor : Charles Nicholson
# Primary GTA: Sindhuja Rao
# Author     : Mayuri Kawale/Gen Feng/Manisha Fnu/Seunghyun Oh
# Homework   : Final Project
# Date       : 12/12/2024
# 
# 1, All code can run normally or run in batches
#
######################################################################

######################
# Preloading Package
######################

library(caret)
library(tidyr)
library(base)
library(datasets)
library(graphics)
library(grDevices)
library(methods)
library(stats)
library(utils)
library(tidyverse)
library(dplyr) 
library(knitr)      
library(kableExtra) 
library(parallel)
library(doParallel)
library(randomForest)
library(MASS)        
library(gbm)         
library(xgboost)     
library(ranger)      
library(glmnet)      
library(mice)
library(car)
library(pROC)


######################
# load datafile to dataset
######################

getwd()

train_data <- read.csv("credit_risk_dataset.csv")
train_data_new <- train_data

######################
# function define
######################

func_UTF8 <- 
  function(x) {
    # Converts character data to UTF-8 encoding
    if(is.character(x)) {
      iconv(x, from = "UTF-8", to = "UTF-8", sub = "byte")
    } 
    else { x }
  }

Q1 <- function(x, na.rm=TRUE){
  # Calculates the first quartile
  quantile(x, na.rm=na.rm)[2]
}

Q3 <- function(x, na.rm=TRUE){
  # Calculates the third quartile
  quantile(x, na.rm=na.rm)[4]
}

numSummary <- 
  function(x) {
    # Provides a numerical summary of the data including length, distinct values, missing values, mean, quantiles, and standard deviation
    return(c(length(x), 
             n_distinct(x),
             sum(is.na(x) | x == "" | trimws(x) == ""), 
             mean(x, na.rm=TRUE), 
             min(x, na.rm=TRUE), 
             Q1(x, na.rm=TRUE), 
             median(x, na.rm=TRUE),
             Q3(x, na.rm=TRUE), 
             max(x, na.rm=TRUE), 
             sd(x, na.rm = TRUE)))
  }

catSummary <- 
  function(x) {
    # Provides a categorical summary including modes and their frequencies
    return(c(length(x), 
             n_distinct(x), 
             sum(is.na(x) | x == "" | trimws(x) == ""),
             getmodes(x,type=1),  
             getmodesCnt(x,type=1),
             getmodes(x,type=2),  
             getmodesCnt(x,type=2),
             getmodes(x,type=-1), 
             getmodesCnt(x,type=-1)))
  }

getmodes <- 
  function(v,type=1) {
    # Determines the mode(s) based on the specified type
    tbl <- table(v)
    m1 <- which.max(tbl)
    if (type == 1) {
      return(names(m1))  # Most frequent value
    } else if (type == 2) {
      return(names(which.max(tbl[-m1])))  # Second most frequent value
    } else if (type == -1) {
      return(names(which.min(tbl)))  # Least frequent value
    } else {
      stop("Invalid type selected")
    }
  }

getmodesCnt <- 
  function(v,type=1) {
    # Counts the frequency of mode(s) based on the specified type
    tbl <- table(v)
    m1 <- which.max(tbl)
    if (type == 1) {
      return(max(tbl))  # Frequency of the most frequent value
    } else if (type == 2) {
      return(max(tbl[-m1]))  # Frequency of the second most frequent value
    } else if (type == -1) {
      return(min(tbl))  # Frequency of the least frequent value
    } else {
      stop("Invalid type selected")
    }
  }

get_mode <- function(v) {
  # Returns the most frequent value in the vector
  uniq_vals <- unique(v)
  uniq_vals[which.max(tabulate(match(v, uniq_vals)))]
}

custom_summary <- function(data, lev = NULL, model = NULL) {
  # Custom evaluation function to calculate ROC, sensitivity, specificity, accuracy, and kappa
  sens_value <- sensitivity(data$pred, data$obs, positive = lev[1])
  spec_value <- specificity(data$pred, data$obs, negative = lev[2])
  roc_value <- pROC::roc(data$obs, data[, lev[1]])$auc
  accuracy_value <- mean(data$pred == data$obs)
  kappa_value <- confusionMatrix(data$pred, data$obs)$overall["Kappa"]
  c(
    ROC = roc_value,
    Sens = sens_value,
    Spec = spec_value,
    Accuracy = accuracy_value,
    Kappa = kappa_value
  )
}

######################
# data divided
######################

# Apply UTF-8 encoding conversion to all columns in the dataset
train_data_new[] <- lapply(train_data_new, func_UTF8)

# Select numeric columns from the dataset for numerical analysis
numData_train <- train_data_new %>%
  dplyr::select(where(is.numeric))

# Convert character columns to factors for categorical analysis
catData_train <- train_data_new %>%
  dplyr::transmute(across(where(is.character), as.factor))

######################
# Generate a data quality report for the numeric variables
######################

# Generate numerical summary for all numeric columns in the dataset
numericSummary_train <- numData_train %>% dplyr::reframe(across(everything(), numSummary))

# Add column headers for the summary statistics
numericSummary_train <- 
  cbind(stat=c("n", "unique", "missing", "mean",
               "min", "Q1", "median", "Q3", "max", "sd"), 
        numericSummary_train)

# Reshape the summary into a tidy format and calculate percentages for missing and unique values
numericSummary_train <- numericSummary_train %>%
  pivot_longer(!stat, names_to = "variable", values_to = "value") %>% # Long format
  pivot_wider(names_from = stat, values_from = value) %>%            # Wide format
  dplyr::mutate(missing_pct = 100 * missing / n,                     # Percentage of missing values
                unique_pct = 100 * unique / n) %>%                   # Percentage of unique values
  dplyr::select(variable, n, missing, missing_pct, unique, unique_pct, everything()) # Rearrange columns

# Set display options for numeric precision and disable scientific notation
options(digits = 3)
options(scipen = 99)

# Display the summary table with styling
numericSummary_train %>% kable() %>% kable_styling(font_size = 12)

# Save the styled summary table as an HTML file
save_kable(
  numericSummary_train %>% kable() %>% kable_styling(font_size = 12),
  file = "numericSummary_train.html"
)

######################
# Generate a data quality report for the categorical variables
######################

# Generate categorical summary for all categorical columns in the dataset
categoricalSummary_train <- catData_train %>% dplyr::reframe(across(everything(), catSummary))

# Add column headers for the summary statistics
categoricalSummary_train <- cbind(
  stat = c("n", "unique", "missing", "1st mode", "1st mode freq", "2nd mode", 
           "2nd mode freq", "least common", "least common freq"), 
  categoricalSummary_train
)

# Reshape the summary into a tidy format and calculate additional statistics
categoricalSummary_train <- categoricalSummary_train %>%
  pivot_longer(!stat, names_to = "variable", values_to = "value") %>%  # Long format
  pivot_wider(names_from = stat, values_from = value) %>%             # Wide format
  dplyr::mutate(
    across(c(n, missing, unique, `1st mode freq`, `2nd mode freq`, `least common freq`), as.numeric), # Convert to numeric
    missing_pct = 100 * missing / n,                               # Percentage of missing values
    unique_pct = 100 * unique / n,                                 # Percentage of unique values
    freqRatio = `1st mode freq` / `2nd mode freq`                  # Ratio of the top two modes' frequencies
  ) %>% 
  dplyr::select(variable, n, missing, missing_pct, unique, unique_pct, freqRatio, everything()) # Rearrange columns

# Set display options for numeric precision and disable scientific notation
options(digits = 3)
options(scipen = 99)

# Display the summary table with styling
categoricalSummary_train %>% kable() %>% kable_styling(font_size = 12)

# Save the styled summary table as an HTML file
save_kable(
  categoricalSummary_train %>% kable() %>% kable_styling(font_size = 12),
  file = "categoricalSummary_train.html"
)

######################
# data preparing 1
######################

# Filter out records where person_age exceeds a reasonable limit (e.g., 120 years)
train_data_new <- train_data_new[train_data_new$person_age <= 120, ]

# Filter out records where person_emp_length is greater than person_age
# Retain rows where person_emp_length is NA or person_emp_length is less than or equal to person_age
train_data_new <- train_data_new[
  (is.na(train_data_new$person_emp_length)) | 
  (train_data_new$person_emp_length <= train_data_new$person_age), 
]

######################
# data preparing 2
######################

# Perform imputation for the "person_emp_length" column using the MICE package
# Select relevant columns for imputation
imputed_data <- mice(
  train_data_new[, c("person_emp_length", "person_age", "person_income", "cb_person_cred_hist_length")], 
  method = "pmm", # Use Predictive Mean Matching (PMM) method
  m = 1,          # Number of multiple imputations
  maxit = 5,      # Maximum number of iterations
  seed = 123      # Seed for reproducibility
)

# Retrieve the completed dataset after imputation
completed_data <- complete(imputed_data)

# Update the original dataset with imputed values for "person_emp_length"
train_data_new$person_emp_length <- completed_data$person_emp_length

# Perform imputation for the "loan_int_rate" column
imputed_data <- mice(
  train_data_new[, c("loan_int_rate", "loan_amnt", "person_income", 
                     "loan_grade", "cb_person_cred_hist_length", 
                     "cb_person_default_on_file")], 
  method = "pmm", # Use Predictive Mean Matching (PMM) method
  m = 1,          # Number of multiple imputations
  maxit = 5,      # Maximum number of iterations
  seed = 123      # Seed for reproducibility
)

# Check the missing values in "loan_int_rate" column
summary(is.na(train_data_new$loan_int_rate))

# Retrieve the completed dataset after imputation
completed_data <- complete(imputed_data)

# Update the original dataset with imputed values for "loan_int_rate"
train_data_new$loan_int_rate <- completed_data$loan_int_rate

######################
# data preparing 3
######################

# Perform one-hot encoding for "person_home_ownership" and retain relevant categories
train_data_new$person_home_ownership <- 
  ifelse(train_data_new$person_home_ownership %in% c("OWN", "OTHER"), "OTHER", train_data_new$person_home_ownership)

# Create dummy variables for "person_home_ownership"
encoded_data <- model.matrix(~ person_home_ownership - 1, data = train_data_new)

# Combine the encoded variables back into the original dataset
train_data_new <- cbind(train_data_new, encoded_data)

# Perform one-hot encoding for "loan_intent"
encoded_data <- model.matrix(~ loan_intent - 1, data = train_data_new)

# Combine the encoded variables back into the original dataset
train_data_new <- cbind(train_data_new, encoded_data)

# Simplify and factorize "loan_grade"
train_data_new$loan_grade <- ifelse(train_data_new$loan_grade %in% c("F", "G"), "Other", train_data_new$loan_grade)
train_data_new$loan_grade <- factor(train_data_new$loan_grade, levels = c("A", "B", "C", "D", "E", "Other"), ordered = TRUE)

# Convert loan grade to numeric
train_data_new$loan_grade_numeric <- as.numeric(train_data_new$loan_grade)

# Encode "cb_person_default_on_file" as a binary variable
train_data_new$cb_person_default_on_file <- factor(train_data_new$cb_person_default_on_file, levels = c("N", "Y"))
train_data_new$cb_person_default_numeric <- as.numeric(train_data_new$cb_person_default_on_file) - 1

# Drop original categorical columns
train_data_new <- train_data_new %>% dplyr::select(-person_home_ownership, -loan_intent, -loan_grade, -cb_person_default_on_file)

# Apply log transformation to numerical features
train_data_new$log_age <- log(train_data_new$person_age + 1)
train_data_new$log_income <- log(train_data_new$person_income + 1)
train_data_new$log_emp_length <- log(train_data_new$person_emp_length + 1)
train_data_new$log_loan_amnt <- log(train_data_new$loan_amnt + 1)
train_data_new$log_int_rate <- log(train_data_new$loan_int_rate + 1)

# Create a new feature representing the percentage of income allocated to the loan
train_data_new$loan_percent_income <- train_data_new$loan_amnt / train_data_new$person_income

# Log-transform the credit history length
train_data_new$log_cred_hist_length <- log(train_data_new$cb_person_cred_hist_length + 1)

# Drop original numerical columns after creating transformed versions
train_data_new <- train_data_new %>% dplyr::select(-person_age, -person_income, -person_emp_length, -loan_amnt, -loan_int_rate, -cb_person_cred_hist_length)

######################
# data preparing 4
######################

# Preserve the loan_status variable before processing
var_loan_status <- train_data_new$loan_status

# Apply preprocessing to center and scale numeric features
preprocess_params <- preProcess(train_data_new, method = c("center", "scale"))
train_data_new <- predict(preprocess_params, train_data_new)

# Reattach the loan_status variable after preprocessing
train_data_new$loan_status <- var_loan_status

# Ensure loan_status is a factor variable for classification
train_data_new$loan_status <- as.factor(train_data_new$loan_status)

# Identify near-zero variance features
nzv <- nearZeroVar(train_data_new, saveMetrics = TRUE)

# Remove near-zero variance features from the dataset
train_data_new <- train_data_new[, !nzv$nzv]

# Display the structure of the updated dataset
str(train_data_new)

# Save the processed loan_status variable for later use
v_loan_status <- train_data_new$loan_status


#############################################
# GBM modeling 1
# all valuables
#############################################

# Attach loan_status variable back to the dataset
train_data_new$loan_status <- v_loan_status

# Set up cross-validation parameters for training
train_control <- trainControl(
  method = "cv",                # Cross-validation
  number = 10,                  # 10-fold CV
  summaryFunction = custom_summary,  # Custom evaluation metrics
  classProbs = TRUE,            # Enable class probabilities
  savePredictions = "final"     # Save predictions for final evaluation
)

# Convert loan_status to a factor with specific labels
train_data_new$loan_status <- factor(train_data_new$loan_status,
                                     levels = c("0", "1"),
                                     labels = c("No", "Yes"))

# Set a random seed for reproducibility
set.seed(123)

# Train the GBM model and measure execution time
time_taken <- system.time({
  
  # Create a parallel cluster to utilize multiple CPU cores
  cl <- makeCluster(detectCores() - 1)  
  registerDoParallel(cl)

  # Train the GBM model using the caret package
  gbm_model_31_1 <- train(
    x = train_data_new[, -1],  # Features (excluding loan_status)
    y = train_data_new$loan_status,  # Target variable
    method = "gbm",            # Gradient Boosting Machine method
    trControl = train_control, # Training control parameters
    metric = "ROC"             # Use ROC as the evaluation metric
  )

  # Stop the parallel cluster
  stopCluster(cl)
  registerDoSEQ()

})

# Print the execution time
print(time_taken)

# Save the trained GBM model to a file
save(gbm_model_31_1, file = "gbm_model_31_1.RData")

# Display results and summary of the GBM model
gbm_model_31_1$results
print(gbm_model_31_1)
summary(gbm_model_31_1)

# Retrieve the best hyperparameters based on ROC
gbm_model_31_1$bestTune

# Find and print the best hyperparameters based on ROC
best_params_ROC <- gbm_model_31_1$results %>%
  as_tibble() %>%           # Convert to tibble for easier manipulation
  arrange(desc(ROC)) %>%    # Sort by descending ROC
  slice(1)                  # Select the top result
print(best_params_ROC)

# Find and print the best hyperparameters based on Accuracy
best_params_Accuracy <- gbm_model_31_1$results %>%
  as_tibble() %>%           # Convert to tibble for easier manipulation
  arrange(desc(Accuracy)) %>% # Sort by descending Accuracy
  slice(1)                  # Select the top result
print(best_params_Accuracy)

# Extract and display best parameters for ROC
best_params_ROC <- best_params_ROC[1, ] 
cat("Best Parameters (ROC):", best_params_ROC$n.trees, 
    best_params_ROC$interaction.depth, 
    best_params_ROC$shrinkage, "\n")

# Extract and display best parameters for Accuracy
best_params_Accuracy <- best_params_Accuracy[1, ]  
cat("Best Parameters (Accuracy):", best_params_Accuracy$n.trees, 
    best_params_Accuracy$interaction.depth, 
    best_params_Accuracy$shrinkage, "\n")

#############################################
# GBM modeling 2
# all valuables + all interaction
#############################################

# Attach loan_status variable back to the dataset
train_data_new$loan_status <- v_loan_status

# Create interaction terms between different variables
train_data_new$interaction1 <- train_data_new$loan_percent_income * train_data_new$person_home_ownershipMORTGAGE
train_data_new$interaction2 <- train_data_new$loan_percent_income * train_data_new$person_home_ownershipOTHER
train_data_new$interaction3 <- train_data_new$loan_percent_income * train_data_new$person_home_ownershipRENT
train_data_new$interaction4 <- train_data_new$loan_percent_income * train_data_new$loan_intentDEBTCONSOLIDATION
train_data_new$interaction5 <- train_data_new$loan_percent_income * train_data_new$loan_intentEDUCATION
# ... (Additional interactions are created similarly for other combinations)

# Set up cross-validation parameters for model training
train_control <- trainControl(
  method = "cv",                # Cross-validation
  number = 10,                  # 10-fold CV
  summaryFunction = custom_summary,  # Custom evaluation metrics
  classProbs = TRUE,            # Enable class probabilities
  savePredictions = "final"     # Save predictions for final evaluation
)

# Convert loan_status to a factor with specific labels
train_data_new$loan_status <- factor(train_data_new$loan_status,
                                     levels = c("0", "1"),
                                     labels = c("No", "Yes"))

# Set a random seed for reproducibility
set.seed(123)

# Train the GBM model and measure execution time
time_taken <- system.time({
  
  # Create a parallel cluster to utilize multiple CPU cores
  cl <- makeCluster(detectCores() - 1)  
  registerDoParallel(cl)

  # Train the GBM model using the caret package
  gbm_model_31_2 <- train(
    x = train_data_new[, -1],  # Features (excluding loan_status)
    y = train_data_new$loan_status,  # Target variable
    method = "gbm",            # Gradient Boosting Machine method
    trControl = train_control, # Training control parameters
    metric = "ROC"             # Use ROC as the evaluation metric
  )

  # Stop the parallel cluster
  stopCluster(cl)
  registerDoSEQ()

})

# Print the execution time
print(time_taken)

# Save the trained GBM model to a file
save(gbm_model_31_2, file = "gbm_model_31_2.RData")

# Display results and summary of the GBM model
gbm_model_31_2$results
print(gbm_model_31_2)
summary(gbm_model_31_2)

# Retrieve the best hyperparameters based on ROC
gbm_model_31_2$bestTune

# Extract the best hyperparameters based on ROC
best_params_ROC <- gbm_model_31_2$results %>%
  as_tibble() %>%           # Convert to tibble for easier manipulation
  arrange(desc(ROC)) %>%    # Sort by descending ROC
  slice(1)                  # Select the top result
print(best_params_ROC)

# Extract the best hyperparameters based on Accuracy
best_params_Accuracy <- gbm_model_31_2$results %>%
  as_tibble() %>%           # Convert to tibble for easier manipulation
  arrange(desc(Accuracy)) %>% # Sort by descending Accuracy
  slice(1)                  # Select the top result
print(best_params_Accuracy)

# Display the best parameters for ROC
best_params_ROC <- best_params_ROC[1, ]  
cat("Best Parameters (ROC):", best_params_ROC$n.trees, 
    best_params_ROC$interaction.depth, 
    best_params_ROC$shrinkage, "\n")

# Display the best parameters for Accuracy
best_params_Accuracy <- best_params_Accuracy[1, ]  
cat("Best Parameters (Accuracy):", best_params_Accuracy$n.trees, 
    best_params_Accuracy$interaction.depth, 
    best_params_Accuracy$shrinkage, "\n")

#############################################
# GBM modeling 3
# selected valuables and interaction based on modeling 2
#############################################

# Restore original loan status
train_data_new$loan_status <- v_loan_status

# Create interaction terms for selected features
train_data_new$interaction1 <- train_data_new$loan_percent_income * train_data_new$person_home_ownershipMORTGAGE
train_data_new$interaction2 <- train_data_new$loan_percent_income * train_data_new$person_home_ownershipOTHER
train_data_new$interaction3 <- train_data_new$loan_percent_income * train_data_new$person_home_ownershipRENT
# Additional interaction terms are computed between various features
# (e.g., loan percent income, home ownership, loan intent, and numeric/log-transformed variables)

# Define the formula with selected interaction terms
formula_with_interactions <- as.formula("
loan_status ~ interaction3 + loan_percent_income + loan_grade_numeric + log_income + interaction55 +
interaction68 + interaction101 + interaction93 + interaction129 + interaction44 +
interaction94 + person_home_ownershipRENT + interaction47 + interaction72 +
interaction30 + interaction51 + person_home_ownershipOTHER + interaction26 +
interaction41 + interaction40 + interaction6 + interaction104 + interaction52 +
interaction23 + interaction110 + interaction13 + interaction130 + interaction82 +
interaction80 + interaction131 + interaction118 + interaction134 + interaction1 +
interaction121 + interaction60 + interaction29 + interaction54 + interaction119 +
interaction71 + interaction128 + interaction86 + interaction115 + interaction144 +
interaction124 + interaction74 + interaction139 + interaction114 + interaction31 +
interaction135 + interaction59 + interaction97 + interaction61 + interaction35 +
interaction7 + log_int_rate + interaction136 + interaction8 + interaction15 +
interaction2 + interaction146 + interaction46 + interaction36 + interaction141 +
interaction27 + interaction58 + interaction151 + interaction147 + interaction22 +
interaction133 + interaction10 + interaction105 + interaction152 + interaction91 +
interaction70 + interaction96 + interaction142 + interaction148
")

# Configure training control for cross-validation
train_control <- trainControl(
  method = "cv",                # Perform cross-validation
  number = 10,                  # Use 10 folds
  summaryFunction = custom_summary,  # Use custom evaluation metrics
  classProbs = TRUE,            # Compute class probabilities
  savePredictions = "final"     # Save final predictions
)

# Convert loan status to factor levels
train_data_new$loan_status <- factor(train_data_new$loan_status,
                                     levels = c("0", "1"),
                                     labels = c("No", "Yes"))

# Train the Gradient Boosting Machine model with interaction terms
set.seed(123)
time_taken <- system.time({
  cl <- makeCluster(detectCores() - 1)  # Use all but one core
  registerDoParallel(cl)

  gbm_model_31_3 <- train(
    formula_with_interactions,      # Use the formula with interactions
    data = train_data_new,          # Training data
    method = "gbm",                 # Gradient Boosting Machine
    trControl = train_control,      # Training control
    metric = "ROC"                  # Use ROC as the evaluation metric
  )

  stopCluster(cl)
  registerDoSEQ()
})
print(time_taken)

# Save the trained GBM model to a file
save(gbm_model_31_3, file = "gbm_model_31_3.RData")

# Display model results and summary
gbm_model_31_3$results
print(gbm_model_31_3)
summary(gbm_model_31_3)

# Extract the best tuning parameters based on ROC
best_params_ROC <- gbm_model_31_3$results %>%
  as_tibble() %>%
  arrange(desc(ROC)) %>%
  slice(1)

print(best_params_ROC)

# Extract the best tuning parameters based on Accuracy
best_params_Accuracy <- gbm_model_31_3$results %>%
  as_tibble() %>%
  arrange(desc(Accuracy)) %>%
  slice(1)

print(best_params_Accuracy)

# Display the best parameters for ROC
best_params_ROC <- best_params_ROC[1, ]
cat("Best Parameters (ROC):", best_params_ROC$n.trees, 
    best_params_ROC$interaction.depth, 
    best_params_ROC$shrinkage, 
    "\n")

# Display the best parameters for Accuracy
best_params_Accuracy <- best_params_Accuracy[1, ]
cat("Best Parameters (Accuracy):", best_params_Accuracy$n.trees, 
    best_params_Accuracy$interaction.depth, 
    best_params_Accuracy$shrinkage, 
    "\n")

#############################################
# GBM modeling 4
# regular parameters based on modeling 3
#############################################

# Restore the original loan status variable
train_data_new$loan_status <- v_loan_status

# Generate interaction terms to capture relationships between features
train_data_new$interaction1 <- train_data_new$loan_percent_income * train_data_new$person_home_ownershipMORTGAGE
train_data_new$interaction2 <- train_data_new$loan_percent_income * train_data_new$person_home_ownershipOTHER
# ... (more interactions involving home ownership, loan intent, grade, and transformed variables)

# Define the formula for the GBM model using selected interaction terms
formula_with_interactions <- as.formula("
loan_status ~ interaction3 + loan_percent_income + loan_grade_numeric + log_income + interaction55 +
interaction68 + interaction101 + interaction93 + interaction129 + interaction72 + interaction44 + 
interaction94 + interaction41 + interaction51 + person_home_ownershipOTHER + interaction47 +
interaction30 + interaction104 + interaction40 + interaction26 + interaction52 + interaction134 +
interaction82 + interaction131 + interaction23 + interaction110 + interaction119 + interaction80 +
interaction130 + interaction6 + interaction128 + interaction1 + interaction86 + interaction105 +
interaction118 + person_home_ownershipRENT + interaction54 + interaction59 + interaction61 +
interaction71 + interaction139 + interaction121 + interaction46 + interaction97 + interaction74 +
interaction36 + interaction60 + interaction136 + interaction141 + interaction96 + interaction144 +
interaction152 + interaction142 + interaction10 + interaction70 + interaction13 + interaction115 +
interaction114 + interaction133 + interaction31 + interaction147 + log_int_rate + interaction151
")

# Set up cross-validation and evaluation metrics
train_control <- trainControl(
  method = "cv",                # Use cross-validation
  number = 10,                  # Perform 10-fold CV
  summaryFunction = custom_summary,  # Use a custom summary function
  classProbs = TRUE,            # Enable probability predictions
  savePredictions = "final"     # Save final predictions
)

# Define the hyperparameter grid for tuning GBM
tuneGrid <- expand.grid(
  n.trees = seq(100, 1000, by = 100),  # Number of boosting iterations
  interaction.depth = c(1, 3, 5, 7),  # Depth of each tree
  shrinkage = c(0.01, 0.05, 0.1, 0.2), # Learning rate
  n.minobsinnode = c(10, 20)          # Minimum number of samples in terminal nodes
)

# Convert loan status to a factor variable for classification
train_data_new$loan_status <- factor(train_data_new$loan_status,
                                     levels = c("0", "1"),
                                     labels = c("No", "Yes"))

# Train the GBM model with interaction terms and hyperparameter tuning
set.seed(123)
time_taken <- system.time({
  cl <- makeCluster(detectCores() - 1)  # Use all but one core for parallel processing
  registerDoParallel(cl)

  gbm_model_31_4 <- train(
    formula_with_interactions,  # Formula including interaction terms
    data = train_data_new,      # Training dataset
    method = "gbm",             # Gradient Boosting Machine
    trControl = train_control,  # Cross-validation settings
    metric = "ROC",             # Use ROC as the evaluation metric
    tuneGrid = tuneGrid         # Hyperparameter grid
  )

  stopCluster(cl)               # Stop parallel processing
  registerDoSEQ()
})
print(time_taken)

# Save the trained GBM model to a file
save(gbm_model_31_4, file = "gbm_model_31_4.RData")

# Load the model if needed
load("gbm_model_31_4.RData")

# Display model results and summary
gbm_model_31_4$results
print(gbm_model_31_4)
summary(gbm_model_31_4)

# Identify the best tuning parameters based on ROC
best_params_ROC <- gbm_model_31_4$results %>%
  as_tibble() %>%
  arrange(desc(ROC)) %>%
  slice(1)

print(best_params_ROC)

# Identify the best tuning parameters based on Accuracy
best_params_Accuracy <- gbm_model_31_4$results %>%
  as_tibble() %>%
  arrange(desc(Accuracy)) %>%
  slice(1)

print(best_params_Accuracy)

# Display the best parameters for ROC
best_params_ROC <- best_params_ROC[1, ]
cat("Best Parameters (ROC):", best_params_ROC$n.trees, 
    best_params_ROC$interaction.depth, 
    best_params_ROC$shrinkage, 
    "\n")

# Display the best parameters for Accuracy
best_params_Accuracy <- best_params_Accuracy[1, ]
cat("Best Parameters (Accuracy):", best_params_Accuracy$n.trees, 
    best_params_Accuracy$interaction.depth, 
    best_params_Accuracy$shrinkage, 
    "\n")

#############################################
# GBM modeling 5
# random parameters based on modeling 3
#############################################

# Restore original loan status
train_data_new$loan_status <- v_loan_status

# Generate interaction terms for feature engineering
train_data_new$interaction1 <- train_data_new$loan_percent_income * train_data_new$person_home_ownershipMORTGAGE
train_data_new$interaction2 <- train_data_new$loan_percent_income * train_data_new$person_home_ownershipOTHER
# ... (similar lines to create additional interaction terms)

# Define the formula for the model with selected interaction terms
formula_with_interactions <- as.formula("
loan_status ~ 
interaction3 + loan_percent_income + loan_grade_numeric + log_income + interaction55 + interaction68 + interaction101 +
interaction93 + interaction129 + interaction72 + interaction44 + interaction94 + interaction41 + interaction51 +
person_home_ownershipOTHER + interaction47 + interaction30 + interaction104 + interaction40 + interaction26 + interaction52 +
interaction134 + interaction82 + interaction131 + interaction23 + interaction110 + interaction119 + interaction80 +
interaction130 + interaction6 + interaction128 + interaction1 + interaction86 + interaction105 + interaction118 +
person_home_ownershipRENT + interaction54 + interaction59 + interaction61 + interaction71 + interaction139 + interaction121 +
interaction46 + interaction97 + interaction74 + interaction36 + interaction60 + interaction136 + interaction141 + interaction96 +
interaction144 + interaction152 + interaction142 + interaction10 + interaction70 + interaction13 + interaction115 +
interaction114 + interaction133 + interaction31 + interaction147 + log_int_rate + interaction151")

# Set up cross-validation and hyperparameter tuning
train_control <- trainControl(
  method = "cv",                # Perform cross-validation
  number = 10,                  # Use 10-fold CV
  search = "random",            # Random search for hyperparameter tuning
  summaryFunction = custom_summary,  # Custom metric function
  classProbs = TRUE,            # Enable probability predictions
  savePredictions = "final"     # Save final predictions
)

# Encode loan status as a factor variable for binary classification
train_data_new$loan_status <- factor(train_data_new$loan_status, levels = c("0", "1"), labels = c("No", "Yes"))

# Train the model with random hyperparameter search
set.seed(123)
time_taken <- system.time({
  cl <- makeCluster(detectCores() - 1)  # Use all available cores except one
  registerDoParallel(cl)

  gbm_model_31_5 <- train(
    formula_with_interactions,  # Formula with interaction terms
    data = train_data_new,      # Training dataset
    method = "gbm",             # Gradient Boosting Machine
    trControl = train_control,  # Cross-validation settings
    metric = "ROC",             # Use ROC as evaluation metric
    tuneLength = 20             # Randomly search 20 sets of hyperparameters
  )

  stopCluster(cl)               # Stop parallel processing
  registerDoSEQ()
})
print(time_taken)

# Save and load the trained model
save(gbm_model_31_5, file = "gbm_model_31_5.RData")
load("gbm_model_31_5.RData")

# Display model results and the best hyperparameters
gbm_model_31_5$results
print(gbm_model_31_5)
summary(gbm_model_31_5)

best_params <- gbm_model_31_5$bestTune

# Calculate feature importance and save the plot
importance <- summary(gbm_model_31_5, n.trees = gbm_model_31_5$bestTune$n.trees)
png("importance.png", width = 800, height = 600)
ggplot(importance, aes(x = reorder(var, rel.inf), y = rel.inf)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(title = "Feature Importance", x = "Features", y = "Relative Importance")
dev.off()

# Save and visualize the ROC-AUC tuning results
png("ROC_AUC.png", width = 800, height = 600)
ggplot(gbm_model_31_5$results, aes(x = n.trees, y = ROC, color = as.factor(interaction.depth))) +
  geom_line() +
  labs(title = "Hyperparameter Tuning Results", x = "Number of Trees", y = "ROC-AUC")
dev.off()

# Update the dataset for GBM training
train_data_new$loan_status <- as.numeric(train_data_new$loan_status) - 1

# Train the final GBM model with the best parameters
time_taken <- system.time({
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)

  final_model <- gbm(
    formula = formula_with_interactions,
    data = train_data_new,
    distribution = "bernoulli",  # Binary classification
    n.trees = best_params$n.trees,
    interaction.depth = best_params$interaction.depth,
    shrinkage = best_params$shrinkage,
    n.minobsinnode = best_params$n.minobsinnode,
    verbose = FALSE
  )

  stopCluster(cl)
  registerDoSEQ()
})
print(time_taken)

# Save and load the final model
save(final_model, file = "final_model.RData")
load("final_model.RData")

# Evaluate the model and visualize residuals
predicted_probs <- predict(final_model, train_data_new, n.trees = best_params$n.trees, type = "response")
predicted_classes <- ifelse(predicted_probs > 0.5, "Yes", "No")
predicted_classes <- factor(predicted_classes, levels = c("No", "Yes"))
confusion_matrix = confusionMatrix(predicted_classes, train_data_new$loan_status)
print(confusion_matrix)

residuals <- as.numeric(train_data_new$loan_status == "Yes") - predicted_probs
png("Residuals_Distribution.png", width = 800, height = 600)
ggplot(data.frame(residuals), aes(x = residuals)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Residuals Distribution", x = "Residuals", y = "Frequency")
dev.off()

# Display best parameters for both ROC and Accuracy
best_params_ROC <- gbm_model_31_5$results %>%
  as_tibble() %>%
  arrange(desc(ROC)) %>%
  slice(1)
print(best_params_ROC)

best_params_Accuracy <- gbm_model_31_5$results %>%
  as_tibble() %>%
  arrange(desc(Accuracy)) %>%
  slice(1)
print(best_params_Accuracy)

#############################################
# GBM modeling 6
# PCA + random  based on modeling 5
#############################################

# Restore original loan status variable
train_data_new$loan_status <- v_loan_status

# Generate interaction terms for feature engineering
train_data_new$interaction1 <- train_data_new$loan_percent_income * train_data_new$person_home_ownershipMORTGAGE
train_data_new$interaction2 <- train_data_new$loan_percent_income * train_data_new$person_home_ownershipOTHER
# ... (similar lines to create additional interaction terms)

# Select numeric variables for PCA
numeric_vars <- train_data_new %>% dplyr::select(where(is.numeric))

# Perform Principal Component Analysis (PCA)
pca_result <- prcomp(numeric_vars, center = TRUE, scale. = TRUE)

# Summarize PCA results
summary(pca_result)

# Calculate cumulative variance explained by components
cumulative_variance <- cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2))

# Plot cumulative variance to determine the number of components to retain
plot(cumulative_variance, type = "b", main = "Cumulative Variance Explained",
     xlab = "Number of Principal Components", ylab = "Cumulative Variance")

# Determine the number of components that explain at least 95% of variance
num_pcs <- which(cumulative_variance >= 0.95)[1]
print(paste("Number of components to retain:", num_pcs))

# Extract the selected principal components
pca_data <- as.data.frame(pca_result$x[, 1:num_pcs])

# Combine PCA results with loan status for modeling
train_data_pca <- cbind(pca_data, loan_status = train_data_new$loan_status)

# Visualize PCA results using the first two components
ggplot(pca_data, aes(x = PC1, y = PC2, color = train_data_new$loan_status)) +
  geom_point(alpha = 0.6) +
  labs(title = "PCA Result Visualization", x = "Principal Component 1", y = "Principal Component 2")

# Set up cross-validation and hyperparameter tuning
train_control <- trainControl(
  method = "cv",                # Perform cross-validation
  number = 10,                  # 10-fold CV
  search = "random",            # Random search for hyperparameter tuning
  summaryFunction = custom_summary,  # Custom evaluation metric
  classProbs = TRUE,            # Enable probability predictions
  savePredictions = "final"     # Save predictions for all CV folds
)

# Convert loan status to a factor variable for classification
train_data_pca$loan_status <- factor(train_data_pca$loan_status,
                                     levels = c("0", "1"),
                                     labels = c("No", "Yes"))

# Train a Gradient Boosting Machine (GBM) model using PCA-transformed data
set.seed(123)
time_taken <- system.time({
  cl <- makeCluster(detectCores() - 1)  # Utilize all but one CPU core
  registerDoParallel(cl)

  gbm_model_6 <- train(
    loan_status ~ .,           # Use all PCA components as predictors
    data = train_data_pca,     # PCA-transformed training data
    method = "gbm",            # Gradient Boosting Machine algorithm
    trControl = train_control, # Cross-validation settings
    metric = "ROC",            # Optimize for ROC-AUC
    tuneLength = 20            # Randomly search 20 hyperparameter combinations
  )

  stopCluster(cl)              # Stop parallel processing
  registerDoSEQ()
})
print(time_taken)

# Save the trained GBM model
save(gbm_model_31_6, file = "gbm_model_31_6.RData")

# Display GBM model results and summary
gbm_model_31_6$results
print(gbm_model_31_6)
summary(gbm_model_31_6)

#############################################
# Random modeling 1
# all valuables + 400 trees
#############################################

# Assign original loan status to the data
train_data_new$loan_status <- v_loan_status

# Set up training control for the model
train_control <- trainControl(
  method = 'repeatedcv',        # Repeated cross-validation
  number = 10,                 # 10-fold cross-validation
  repeats = 5,                 # Repeat the process 5 times
  search = 'random',           # Random search for hyperparameter tuning
  classProbs = TRUE,           # Enable probability predictions
  verboseIter = TRUE,          # Show progress during training
  summaryFunction = custom_summary,  # Custom evaluation metrics
  savePredictions = "final"    # Save final predictions
)

# Convert loan status to a factor for classification
train_data_new$loan_status <- factor(train_data_new$loan_status,
                                     levels = c("0", "1"),
                                     labels = c("No", "Yes"))

# Train a Random Forest model using the ranger package
set.seed(123)  # Set seed for reproducibility
time_taken <- system.time({
  
  cl <- makeCluster(detectCores() - 1)  # Use all but one CPU core for parallel processing
  registerDoParallel(cl)

  # Train the model
  ranger_model_41_1 <- train(
    x = train_data_new[, -1],          # Predictor variables (all columns except the first)
    y = train_data_new$loan_status,   # Target variable
    method = 'ranger',                # Random Forest algorithm
    num.trees = 400,                  # Number of trees
    trControl = train_control,        # Cross-validation settings
    metric = "ROC",                   # Optimize for ROC-AUC
    importance = "impurity"           # Use impurity-based feature importance
  )

  stopCluster(cl)  # Stop parallel processing
  registerDoSEQ()
})
print(time_taken)  # Display time taken to train the model

# Save the trained model
save(ranger_model_41_1, file = "ranger_model_41_1.RData")

# Print model details and results
print(ranger_model_41_1)
ranger_model_41_1$results  # Display model performance metrics
ranger_model_41_1$bestTune  # Display best hyperparameter combination

# Extract and print the final trained Random Forest model
final_model <- ranger_model_41_1$finalModel
print(final_model)

# Compute and display feature importance values
importance_values <- importance(ranger_model_41_1$finalModel)
print(importance_values)

#############################################
# Random modeling 2
# all valuables + + all interactions + 400 trees
#############################################

# Assign original loan status to the data
train_data_new$loan_status <- v_loan_status

# Create interaction terms between selected features
train_data_new$interaction1 <- train_data_new$loan_percent_income * train_data_new$person_home_ownershipMORTGAGE
train_data_new$interaction2 <- train_data_new$loan_percent_income * train_data_new$person_home_ownershipOTHER
train_data_new$interaction3 <- train_data_new$loan_percent_income * train_data_new$person_home_ownershipRENT
# ... (similar for all interaction terms) ...

# Define training control settings for model training
train_control <- trainControl(
  method = 'repeatedcv',        # Repeated cross-validation
  number = 10,                 # 10-fold cross-validation
  repeats = 5,                 # Repeat 5 times
  search = 'random',           # Randomized search for hyperparameters
  classProbs = TRUE,           # Enable probability predictions
  verboseIter = TRUE,          # Show progress during training
  summaryFunction = custom_summary,  # Custom evaluation metric
  savePredictions = "final"    # Save final predictions
)

# Convert loan status into a factor for classification
train_data_new$loan_status <- factor(
  train_data_new$loan_status,
  levels = c("0", "1"),
  labels = c("No", "Yes")
)

# Train a Random Forest model using the ranger method
set.seed(123)  # Set random seed for reproducibility
time_taken <- system.time({
  
  # Enable parallel processing using available CPU cores
  cl <- makeCluster(detectCores() - 1)  
  registerDoParallel(cl)

  # Train the model with interaction terms and other features
  ranger_model_41_2 <- train(
    x = train_data_new[, -1],          # Predictor variables (excluding first column)
    y = train_data_new$loan_status,   # Target variable
    method = 'ranger',                # Random Forest algorithm using ranger
    num.trees = 400,                  # Number of trees
    trControl = train_control,        # Training control settings
    metric = "ROC",                   # Optimize for ROC-AUC
    importance = 'impurity'           # Calculate impurity-based feature importance
  )

  stopCluster(cl)  # Stop parallel processing
  registerDoSEQ()
})

# Print time taken for training
print(time_taken)

# Save the trained model to a file
save(ranger_model_41_2, file = "ranger_model_41_2.RData")

# Display model results and best-tuned parameters
print(ranger_model_41_2)
ranger_model_41_2$results
ranger_model_41_2$bestTune

# Extract and print the final trained Random Forest model
final_model <- ranger_model_41_2$finalModel
print(final_model)

# Extract and display feature importance values
importance_values <- importance(ranger_model_41_2$finalModel)
print(importance_values)

#############################################
# Random modeling 3
# all valuables + + top-50 interactions + 400 trees based on modeling 2
#############################################

# Assign loan status to the dataset
train_data_new$loan_status <- v_loan_status

# Create interaction terms between selected variables
train_data_new$interaction1 <- train_data_new$loan_percent_income * train_data_new$person_home_ownershipMORTGAGE
train_data_new$interaction2 <- train_data_new$loan_percent_income * train_data_new$person_home_ownershipOTHER
train_data_new$interaction3 <- train_data_new$loan_percent_income * train_data_new$person_home_ownershipRENT
# ... (similar interactions continue for other combinations) ...

# Define a formula with main variables and selected interaction terms
formula_with_interactions <- 
  as.formula("loan_status ~ loan_percent_income + person_home_ownershipRENT + loan_intentDEBTCONSOLIDATION + 
              loan_intentMEDICAL + loan_grade_numeric + log_income + log_int_rate + interaction1 + interaction2 + 
              interaction3 + interaction4 + interaction6 + interaction10 + interaction11 + interaction13 + 
              interaction15 + interaction26 + interaction29 + interaction30 + interaction35 + interaction41 + 
              interaction44 + interaction49 + interaction51 + interaction52 + interaction55 + interaction58 + 
              interaction59 + interaction60 + interaction68 + interaction71 + interaction72 + interaction74 + 
              interaction91 + interaction93 + interaction94 + interaction101 + interaction105 + interaction107 + 
              interaction128 + interaction129 + interaction134")

# Configure cross-validation settings for training
train_control <- trainControl(
  method = 'repeatedcv',          # Repeated cross-validation
  number = 10,                   # 10-fold cross-validation
  repeats = 5,                   # Repeated 5 times
  search = 'random',             # Randomized hyperparameter search
  classProbs = TRUE,             # Enable probability predictions
  verboseIter = TRUE,            # Display progress during training
  summaryFunction = custom_summary,  # Custom evaluation metric
  savePredictions = "final"      # Save final predictions
)

# Convert loan status to factor for classification tasks
train_data_new$loan_status <- factor(
  train_data_new$loan_status,
  levels = c("0", "1"),
  labels = c("No", "Yes")
)

# Train a Random Forest model using the ranger method
set.seed(123)  # Set random seed for reproducibility
time_taken <- system.time({
  
  # Enable parallel processing
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)

  # Train the model with the defined formula
  ranger_model_41_3 <- train(
    formula_with_interactions,    # Formula including interaction terms
    data = train_data_new,        # Training dataset
    method = 'ranger',            # Random Forest algorithm
    num.trees = 400,              # Number of trees
    trControl = train_control,    # Cross-validation settings
    metric = "ROC",               # Optimize for ROC-AUC
    importance = 'impurity'       # Compute feature importance based on impurity
  )

  # Stop parallel processing
  stopCluster(cl)
  registerDoSEQ()
})

# Print training time
print(time_taken)

# Save the trained model to a file
save(ranger_model_41_3, file = "ranger_model_41_3.RData")

# Display model results and best hyperparameter values
print(ranger_model_41_3)
ranger_model_41_3$results
ranger_model_41_3$bestTune

# Extract and display the final trained model
final_model <- ranger_model_41_3$finalModel
print(final_model)

# Calculate and print feature importance
importance_values <- importance(ranger_model_41_3$finalModel)
print(importance_values)

#############################################
# Random modeling 4
# all valuables + + all interactions + 400 trees + parameters
#############################################

# Assign loan status to the dataset
train_data_new$loan_status <- v_loan_status

# Generate interaction terms between selected variables
train_data_new$interaction1 <- train_data_new$loan_percent_income * train_data_new$person_home_ownershipMORTGAGE
train_data_new$interaction2 <- train_data_new$loan_percent_income * train_data_new$person_home_ownershipOTHER
train_data_new$interaction3 <- train_data_new$loan_percent_income * train_data_new$person_home_ownershipRENT
# (Further interactions follow a similar pattern for other combinations of variables)

# Set up cross-validation control
train_control <- trainControl(
  method = 'repeatedcv',          # Use repeated cross-validation
  number = 10,                   # 10-fold cross-validation
  repeats = 5,                   # Repeat the process 5 times
  search = 'random',             # Perform random hyperparameter search
  classProbs = TRUE,             # Enable probability predictions
  verboseIter = TRUE,            # Display progress during training
  summaryFunction = custom_summary,  # Custom metric for evaluation
  savePredictions = "final"      # Save final predictions
)

# Convert loan_status to a factor for classification
train_data_new$loan_status <- factor(
  train_data_new$loan_status,
  levels = c("0", "1"),
  labels = c("No", "Yes")
)

# Set up a tuning grid for hyperparameter optimization
tuneGrid <- expand.grid(
  mtry = seq(10, ncol(train_data_new) / 2, by = 10),  # Number of variables considered at each split
  splitrule = "gini",                                # Splitting criterion
  min.node.size = 19                                # Minimum size of terminal nodes
)

# Set random seed for reproducibility
set.seed(123)

# Train the Random Forest model using the ranger package
time_taken <- system.time({
  
  # Enable parallel processing
  cl <- makeCluster(detectCores() - 1)  # Use all cores except one
  registerDoParallel(cl)
  
  # Train the model
  ranger_model_41_4 <- train(
    x = train_data_new[, -1],          # Use all predictors except the first column
    y = train_data_new$loan_status,   # Response variable
    method = 'ranger',                # Random Forest algorithm
    num.trees = 400,                  # Number of trees
    trControl = train_control,        # Cross-validation settings
    metric = "ROC",                   # Optimize for ROC-AUC
    importance = 'impurity'           # Calculate variable importance based on impurity
  )

  # Stop parallel processing
  stopCluster(cl)
  registerDoSEQ()
})

# Print the time taken for training
print(time_taken)

# Save the trained model to a file
save(ranger_model_41_4, file = "ranger_model_41_4.RData")

# Print model results and best hyperparameters
print(ranger_model_41_4)
ranger_model_41_4$results
ranger_model_41_4$bestTune

# Extract and display the final trained model
final_model <- ranger_model_41_4$finalModel
print(final_model)

# Calculate and display variable importance
importance_values <- importance(ranger_model_41_4$finalModel)
print(importance_values)

#############################################
# XGBoost modeling 1
# all valuables + custom parameters
#############################################

# Convert the target variable (loan_status) to a factor for classification
train_data_new$loan_status <- factor(
  train_data_new$loan_status,
  levels = c("0", "1"),
  labels = c("No", "Yes")
)

# Load necessary libraries
library(caret)     # For machine learning workflow
library(xgboost)   # For gradient boosting model

# Set a random seed for reproducibility
set.seed(123)

# Prepare data for training
y <- train_data_new$loan_status                          # Response variable
X <- as.matrix(train_data_new %>% dplyr::select(-loan_status)) # Predictor variables converted to matrix

# Define the hyperparameter tuning grid for XGBoost
tune_grid <- expand.grid(
  nrounds = c(50, 100, 150),           # Number of boosting rounds
  max_depth = c(4, 6, 8),             # Maximum tree depth
  eta = c(0.05, 0.1),                 # Learning rate
  colsample_bytree = c(0.6, 0.8),     # Subsample ratio of columns per tree
  subsample = c(0.6, 0.8),            # Subsample ratio of rows
  gamma = c(0, 1, 5),                 # Minimum loss reduction required for split
  min_child_weight = c(1, 5, 10)      # Minimum sum of instance weights in a child node
)

# Set up cross-validation control
ctrl <- trainControl(
  method = "cv",                     # Use cross-validation
  number = 10,                       # Perform 10-fold cross-validation
  summaryFunction = custom_summary,  # Use custom evaluation metric
  classProbs = TRUE,                 # Enable probability predictions
  verboseIter = TRUE                 # Display progress during training
)

# Train the XGBoost model with hyperparameter tuning
time_taken <- system.time({

  # Enable parallel processing
  cl <- makeCluster(detectCores() - 1)  # Use all but one core
  registerDoParallel(cl)  

  # Train the model
  xgb_tune <- train(
    x = X,
    y = as.factor(y),              # Convert response variable to factor
    method = "xgbTree",            # Use XGBoost tree-based method
    metric = "ROC",                # Optimize for ROC-AUC metric
    trControl = ctrl,              # Cross-validation settings
    tuneGrid = tune_grid           # Hyperparameter grid
  )

  # Stop parallel processing
  stopCluster(cl)
  registerDoSEQ()

})

# Print the time taken for training
print(time_taken)

# Save and load the tuned model
save(xgb_tune, file = "xgb_tune.RData")
load("xgb_tune.RData")

# Print the best hyperparameter combination
print(xgb_tune$bestTune)

# Save the tuning results to a CSV file
write.csv(xgb_tune$results, "xgb_tune_results.csv")

# Predict probabilities using the trained model
xgb_pred <- predict(xgb_tune, newdata = X, type = "prob")

# Display the first few predictions
head(xgb_pred)

# Visualize the model tuning results
plot(xgb_tune)

###########################
# ROC curve
# 
###########################

# Load the pROC library for calculating and visualizing ROC curves
library(pROC)

# Compute the ROC curve
# The target variable (loan_status) is compared against the predicted probabilities for class "Yes" (second column of xgb_pred)
roc_curve <- roc(train_data_new$loan_status, as.numeric(xgb_pred[, 2]))

# Plot the ROC curve
plot(roc_curve, 
     col = "blue",              # Set line color to blue
     lwd = 2,                   # Set line width
     main = "ROC Curve for XGBoost Model") # Title of the plot

# Add a diagonal reference line (random classifier)
abline(a = 0, b = 1, col = "gray", lty = 2)  # Gray dashed line

# Display the AUC value on the plot
text(0.6, 0.4,                             # Position of the text
     paste("AUC =", round(auc(roc_curve), 3)), # Calculate and display AUC rounded to 3 decimal places
     col = "red",                          # Text color
     cex = 1.2)                            # Text size

###########################
# confusion matrixs
# 
###########################

# Load the caret library for model evaluation
library(caret)

# Predict the class labels for the training data using the trained XGBoost model
xgb_class <- predict(xgb_tune, newdata = X)

# Generate the confusion matrix to evaluate the model's performance
confusionMatrix(xgb_class, train_data_new$loan_status)

###########################
# important feature plot
# 
###########################

# Load the xgboost library for model interpretation and visualization
library(xgboost)

# Calculate feature importance using the final XGBoost model
# `xgb.importance` computes the importance score for each feature
importance <- xgb.importance(feature_names = colnames(X), model = xgb_tune$finalModel)

# Plot the top 150 most important features based on their "Gain" contribution
# `xgb.plot.importance` creates a bar plot of feature importance
xgb.plot.importance(importance, top_n = 150, measure = "Gain", main = "Feature Importance")


###########################
# Residual analysis
# 
###########################


# Generate predicted class values from the tuned XGBoost model
xgb_pred_class <- predict(xgb_tune, newdata = X)

# Compute residuals as the difference between actual and predicted values
residuals <- as.numeric(train_data_new$loan_status) - as.numeric(xgb_pred_class)

# Add jitter to predicted class values to avoid overlap and improve visualization
jittered_pred_class <- jitter(as.numeric(xgb_pred_class), factor = 0.2) 

# Add jitter to residuals to avoid overlap and improve visualization
jittered_residuals <- jitter(residuals, factor = 0.2)  

# Create a residual plot with jitter and transparency for better visualization
plot(jittered_pred_class, jittered_residuals, col = rgb(0, 0, 1, alpha = 0.4), pch = 16,
     main = "Residual Plot for XGBoost Model with Jitter and Transparency",
     xlab = "Predicted Class (Numeric, with Jitter)", ylab = "Residuals (with Jitter)")

# Add a horizontal reference line at y = 0 to indicate no residual difference
abline(h = 0, col = "red", lwd = 2)  

# Add a legend to explain the meaning of the points in the plot
legend("topright", legend = c("Residual Points"), col = rgb(0, 0, 1, alpha = 0.4), 
       pch = 16, cex = 0.8)


#############################################
# XGBoost modeling 2
# all valuables + random parameters
#############################################

# Convert loan status to a factor with meaningful labels
train_data_new$loan_status <- factor(train_data_new$loan_status,
                                     levels = c("0", "1"),
                                     labels = c("No", "Yes"))

# Load necessary libraries
library(caret)
library(xgboost)

# Set seed for reproducibility
set.seed(123)

# Define target variable (y) and features (X) for training
y <- train_data_new$loan_status
X <- as.matrix(train_data_new %>% dplyr::select(-loan_status))  # Convert features to matrix

# Define cross-validation and model tuning controls
train_control <- trainControl(
  method = "cv",                   # Use k-fold cross-validation
  number = 10,                     # Number of folds
  search = "random",               # Random search for hyperparameter tuning
  summaryFunction = custom_summary, # Custom function for evaluation metrics
  classProbs = TRUE,               # Enable class probabilities for classification
  savePredictions = "final"        # Save final predictions
)

# Set seed again for consistent results during parallel processing
set.seed(123)

# Measure the time taken for model training
time_taken <- system.time({

  # Set up parallel computing for faster training
  cl <- makeCluster(detectCores() - 1)  # Use all available cores except one
  registerDoParallel(cl)               # Register parallel backend
  
  # Train an XGBoost model using caret's train function
  xgb_tune_2 <- train(
    x = X,                             # Input features
    y = as.factor(y),                  # Target variable
    method = "xgbTree",                # Use XGBoost tree-based method
    metric = "ROC",                    # Optimize for ROC metric
    trControl = train_control,         # Cross-validation settings
    tuneLength = 20                    # Number of random tuning combinations
  )

  # Stop parallel processing and revert to sequential execution
  stopCluster(cl)
  registerDoSEQ()

})

# Print the time taken for training
print(time_taken)

# Save the trained model to a file for later use
save(xgb_tune_2, file = "xgb_tune_2.RData")

# Print the best hyperparameters found during tuning
print(xgb_tune_2$bestTune)

# Save the tuning results to a CSV file for review
write.csv(xgb_tune_2$results, "xgb_tune_results_2.csv")

# Visualize the results of model tuning (e.g., ROC performance for each parameter combination)
plot(xgb_tune_2)



##################################################################################
#THe following code is for unsupervised learning
##################################################################################
# load the necessary packages
library(base)
library(datasets)
library(graphics)
library(grDevices)
library(methods)
library(stats)
library(utils)

library(ggplot2)     # for ggplot
library(tidyverse)
library(dplyr)
library(VIM)
library(dbscan)     #for density based clustering

############################## Dataset ############################################

## load the dataset
creditRisk <- read.csv("dataset/credit_risk_dataset.csv")

## convert character types to factors
creditRisk <- creditRisk %>%
  dplyr::mutate(across(where(is.character), as.factor))

######################## Handling Missing Values #########################################

## Only two variables have missing values: `person_emp_length` and `loan_int_rate`

# perform kNN imputation kNN using k=5 i.e 5 neighbors
library(VIM)
creditRisk <- kNN(creditRisk, variable = c("person_emp_length", "loan_int_rate"), 
                  k=5, 
                  imp_var = FALSE   # no additional indicator column is created
)


#### Remove missing values either using predictive modelling/other methods
# In general you can remove rows if the missing percentage is less than 5%
# probably can just remove rows corresponding to employment length but not for loan interest rate

######################### Removing Outliers ###############################################

# histogram of employment length -- The distribution is right skewed
ggplot(data=creditRisk, aes(x=person_emp_length))+
  geom_histogram(fill="blue", color="black", alpha=0.5)

png("images/outliers.png", width = 700, height = 500)
ggplot(data = creditRisk, aes(x=person_age, y=person_emp_length, color= person_emp_length < person_age))+
  geom_point(position = "jitter", alpha=0.2)+
  labs(title = "Employment length vs. Age", 
       x="Applicant's age", y="Applicant's employment period",
       color = "Emp_length < Age")+
  theme(panel.background = element_rect(color="black", fill="white"),
        panel.grid.major = element_line(color = "grey"),                                        # sets the color for major and minor grid lines       
        panel.grid.minor = element_line(color = 'grey'),
        plot.title = element_text(hjust = 0.5))
dev.off() 



## Checking outliers -- clearly employment length cannot be greater than a person's age
# logical vector to check for events whose emp_length > person_age
is_outlier <- creditRisk$person_emp_length[!is.na(creditRisk$person_emp_length)] > creditRisk$person_age[!is.na(creditRisk$person_emp_length)]
print(sum(is_outlier))   # prints total number of outliers that satisfy above condition

# To get the correct indices to remove such outliers, update the logical vector using all rows
is_outlier <- creditRisk$person_emp_length > creditRisk$person_age
sum(is.na(creditRisk$person_emp_length))
sum(is.na(creditRisk$loan_int_rate))

## Remove outliers
creditRisk <- creditRisk[!is_outlier, ]   # removing rows with an outlier

# Remove rows where all values are NA
creditRisk <- creditRisk[!apply(is.na(creditRisk), 1, all), ]

######################### DBSCAN clustering ################################
## scale the numeric dataset
numericScaled <- data.frame(scale(creditRisk[ , -c(3, 5, 6, 9, 11)]))

## converting the numeric dataset into matrix form
numMatrix <- as.matrix(numericScaled)

kNN_distances <- kNNdist(numMatrix, k=9, search="kd") # k  minPts-1
kNNdistplot(numMatrix, k=9)
abline(h=2, col="red")
dev.off()

set.seed(1234)
db <- dbscan(numMatrix, eps = 2, minPts = 10)    #minPts >= dimensions+1
db
hullplot(numMatrix, db$cluster)
pairs(numMatrix, col = db$cluster+1L)

creditRisk$dbCluster <- db$cluster
table(creditRisk$loan_status, creditRisk$dbCluster)
#        0     1     2
#  0    48 25424     0
#  1    23     0  7084

######################## Interpretation for clusters ########################################
#Let's look at the top ten data points in each clusters
head(creditRisk[creditRisk$dbCluster==0, -c(3, 5, 6, 11)], 10)
head(creditRisk[creditRisk$dbCluster==1, -c(3, 5, 6, 11)], 10)
head(creditRisk[creditRisk$dbCluster==2, -c(3, 5, 6, 11)], 10)

mean(creditRisk[creditRisk$dbCluster==0, "person_income"]) #642329.6
mean(creditRisk[creditRisk$dbCluster==1, "person_income"]) #69395.97
mean(creditRisk[creditRisk$dbCluster==2, "person_income"]) #48363.21

mean(creditRisk[creditRisk$dbCluster==0, "loan_int_rate"]) #11.60268
mean(creditRisk[creditRisk$dbCluster==1, "loan_int_rate"]) #10.4387
mean(creditRisk[creditRisk$dbCluster==2, "loan_int_rate"]) #13.04181

mean(creditRisk[creditRisk$dbCluster==0, "loan_percent_income"]) #0.09690141
mean(creditRisk[creditRisk$dbCluster==1, "loan_percent_income"]) #0.1489809
mean(creditRisk[creditRisk$dbCluster==2, "loan_percent_income"]) #0.2470553

mean(creditRisk[creditRisk$dbCluster==0, "cb_person_cred_hist_length"]) #16.66197
mean(creditRisk[creditRisk$dbCluster==1, "cb_person_cred_hist_length"]) #5.817928
mean(creditRisk[creditRisk$dbCluster==2, "cb_person_cred_hist_length"]) #5.64681

mean(creditRisk[creditRisk$dbCluster==0, "loan_amnt"]) #15526.76
mean(creditRisk[creditRisk$dbCluster==1, "loan_amnt"]) #9229.599
mean(creditRisk[creditRisk$dbCluster==2, "loan_amnt"]) #10816

########################## Evaluation metrics for DBSCAN ###################### 
# Since the ground truth labels are available, we are going to use ARI and F1 score/Precision/Recall
# as evaluation metrics, if they are not present, then use Silhouette score or Davies-Bouldin Index

## Adjusted Rand Index
install.packages("mclust")
library(mclust)
ari <- adjustedRandIndex(creditRisk$loan_status, creditRisk$dbCluster)
print(paste("ARI:", ari))
#[1] "ARI: 0.994207333730753"

# Interpretation: Value of ARI close to 1 implies there is a near perfect matching in 
# almost all of the cases. Some values that contribute to imperfect matching are the noise points

##F1 Score/Precision/Recall

# Predicted cluster labels
predicted_labels <- creditRisk$dbCluster

# Handle noise - DBSCAN assigns noise points to cluster 0. You can treat these as a separate "noise" 
#class or ignore them if noise isn’t part of the ground truth.

# Exclude noise points or treat them as a separate "noise" class
predicted_labels[predicted_labels == 0] <- NA  # Assign NA for noise if ignoring it

# Use a matching algorithm (e.g., Hungarian algorithm) to align cluster labels with true labels
confusion_matrix <- table(creditRisk$loan_status, predicted_labels)
cluster_mapping <- apply(confusion_matrix, 2, which.max)  # Map clusters to labels
mapped_labels <- cluster_mapping[predicted_labels]

# Confusion matrix for evaluation
confusion_matrix <- table(creditRisk$loan_status, predicted_labels)

# Precision, Recall, F1
precision <- diag(confusion_matrix) / colSums(confusion_matrix)  # TP / (TP + FP)
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)    # TP / (TP + FN)
f1 <- 2 * (precision * recall) / (precision + recall)

## Print results

#High precision means the clustering algorithm rarely misclassifies points into the wrong clusters.
print(paste("Precision:", mean(precision, na.rm = TRUE)))
#[1] "Precision: 1"

#High recall indicates that most true cluster members are correctly identified.
print(paste("Recall:", mean(recall, na.rm = TRUE)))
#[1] "Recall: 1"

# F1 score balances precision and recall.
print(paste("F1 Score:", mean(f1, na.rm = TRUE)))
#[1] "F1 Score: 1"

