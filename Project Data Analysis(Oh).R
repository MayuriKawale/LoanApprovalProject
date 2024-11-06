# Load necessary libraries
library(dplyr)
library(ggplot2)
library(mice)      # For predictive imputation model
library(outliers)  # For Grubbs' test on outliers
library(VIM) # For KNN imputation
library(mice) # For PMM imputation

# Set working directory and load the data
setwd("/Users/peteroh/Desktop/Oh/OU/Fall 2024/DSA : ISE 5103/Final Project")
data <- read.csv("credit_risk_dataset 2.csv")

# Display data structure and summary statistics
str(data)
summary(data)

# 1. Data Quality Checks ----
# 1.1 Missing Values
# Check for missing values in each column
missing_values <- sapply(data, function(x) sum(is.na(x)))
cat("Missing Values in Each Column:\n")
print(missing_values) # We have missing values in person_emp_length and loan_int_rate

# 1.2 Negative Values Check in Numeric Columns
# Check for unrealistic negative values in numeric columns
negative_values_check <- sapply(data, function(x) if(is.numeric(x)) sum(x < 0, na.rm = TRUE) else NA)
cat("Negative Values in Each Column:\n")
print(negative_values_check) # No unrealistic negative values (e.g., age should not be negative)

# 1.3 Check for Unusually High Values in Key Columns
# Define thresholds for high values based on domain knowledge
age_high_threshold <- 122            # Max realistic age
income_high_threshold <- 1000000      # High income threshold
loan_amount_high_threshold <- 50000   # Upper bound for loan amount
credit_hist_high_threshold <- 100     # Max credit history length

# High values check for each specified column
high_values <- list(
  person_age = data %>% filter(person_age > age_high_threshold),
  person_income = data %>% filter(person_income > income_high_threshold),
  loan_amnt = data %>% filter(loan_amnt > loan_amount_high_threshold),
  cb_person_cred_hist_length = data %>% filter(cb_person_cred_hist_length > credit_hist_high_threshold)
)

# Print the number of high-value observations and the details of each
cat("High Values Check for Specified Columns:\n")
for (col_name in names(high_values)) {
  cat(paste("\nColumn:", col_name))
  cat(paste("\nCount of High Values:", nrow(high_values[[col_name]])))
  cat("\nDetails of High Values:\n")
  print(high_values[[col_name]])
}

# 1.4 Categorical Variables Check
# Convert relevant columns to factors if they aren't already
data$loan_intent <- as.factor(data$loan_intent)
data$loan_grade <- as.factor(data$loan_grade)
data$person_home_ownership <- as.factor(data$person_home_ownership)
data$cb_person_default_on_file <- as.factor(data$cb_person_default_on_file)

# Check the number of unique levels for each factor
category_counts <- sapply(data[, sapply(data, is.factor)], nlevels)
cat("Number of Categories in Each Factor Column:\n")
print(category_counts)

# 2. Handling Missing Values ----
# 2.1 Missing Values in person_emp_length (Length of Employment in Years)

# We have several options for handling missing values in person_emp_length:
# Option 1: Replace missing values with overall median
data$person_emp_length[is.na(data$person_emp_length)] <- median(data$person_emp_length, na.rm = TRUE)

# Option 2: Replace missing values based on loan_intent and person_home_ownership groups
# Step 1: Perform ANOVA to check if mean employment length differs by person_home_ownership
# We conduct an ANOVA to determine if person_home_ownership significantly affects employment length
anova_home_ownership <- aov(person_emp_length ~ person_home_ownership, data = data)
summary(anova_home_ownership)

# Step 2: Perform ANOVA to check if mean employment length differs by loan_intent
# Conducting a second ANOVA to check if loan_intent is associated with significant differences in employment length
anova_loan_intent <- aov(person_emp_length ~ loan_intent, data = data)
summary(anova_loan_intent)

# Interpretation: 
# Both ANOVA tests show statistically significant results, indicating that both loan_intent and person_home_ownership
# are associated with different average employment lengths. This statistical significance justifies using both 
# loan_intent and person_home_ownership as grouping variables when imputing missing values for person_emp_length.

# Step 3: Impute missing values in person_emp_length based on loan_intent and person_home_ownership groups
# Here, we replace missing values in person_emp_length with the median employment length within each subgroup
# defined by the combination of loan_intent and person_home_ownership. This respects the variation in employment
# length that these groups capture.
data <- data %>%
  group_by(loan_intent, person_home_ownership) %>%
  mutate(person_emp_length = ifelse(is.na(person_emp_length), 
                                    median(person_emp_length, na.rm = TRUE), 
                                    person_emp_length)) %>%
  ungroup()

# Option 3: Apply KNN imputation to the 'person_emp_length' column
data_imputed_knn <- kNN(data, variable = "person_emp_length", k = 5, imp_var = FALSE)

# Check for remaining missing values in 'person_emp_length'
sum(is.na(data_imputed_knn$person_emp_length))
sum(is.na(data_imputed_knn$loan_int_rate))


# 2.2 Missing Values in loan_int_rate (Interest Rate)
# Similar to person_emp_length, we use a combination of methods based on loan characteristics.

# Option 1: Replace missing values with the overall median
data$loan_int_rate[is.na(data$loan_int_rate)] <- median(data$loan_int_rate, na.rm = TRUE)

# Option 2: Replace missing values with the median within each loan_grade group
# Since loan_grade often reflects credit risk, which can correlate with interest rate, we use group-based median imputation
data <- data %>%
  group_by(loan_grade) %>%
  mutate(loan_int_rate = ifelse(is.na(loan_int_rate), 
                                median(loan_int_rate, na.rm = TRUE), 
                                loan_int_rate)) %>%
  ungroup()

# Option 3: Use a predictive model to estimate missing loan_int_rate values
# We create a predictive imputation model using MICE and relevant predictors
imputed_data <- mice(data[, c("loan_int_rate", "loan_amnt", "loan_grade", "person_income")], 
                     method = "norm.predict", m = 1, seed = 123)
data$loan_int_rate <- complete(imputed_data)$loan_int_rate

# 2.3 Dealing with Missing Values in person_emp_length and loan_int_rate at once

# Option1: Apply KNN imputation on both 'loan_int_rate' and 'person_emp_length' (or any other columns with missing values)
data_imputed_knn <- kNN(data, variable = c("loan_int_rate", "person_emp_length"), k = 5, imp_var = FALSE)
# Check for remaining missing values in the dataset
colSums(is.na(data_imputed_knn)) # no remaining mssing values

# Option 2: Apply PMM imputation on any other columns with missing values)
imputed_data <- mice(data, method = "pmm", m = 1, maxit = 5) 
# Extract the complete dataset with imputed values
data_imputed_pmm <- complete(imputed_data)
# Check for remaining missing values in 'loan_int_rate'
sum(is.na(data_imputed_pmm$loan_int_rate))

# 3. Outlier Detection ----
# Outliers can impact our analysis and model, so we perform checks

# 3.1 Grubbs' Test for Outliers in person_income
# Grubbs' test helps detect outliers by comparing the most extreme value to the mean of the remaining values.
grubbs_test_income <- grubbs.test(data$person_income)
print(grubbs_test_income)

# 3.2 Z-score for person_income: flagging values above/below 3 standard deviations
# Using Z-scores, we flag observations more than 3 standard deviations from the mean as outliers
data$z_score_income <- scale(data$person_income)
data$outlier_income <- abs(data$z_score_income) > 3
cat("Outliers in person_income identified by Z-score:\n")
table(data$outlier_income)

# 3.3 Visual outlier check with boxplots for major numerical columns
boxplot(data$person_age, main="Person Age", ylab="Age")
boxplot(data$person_income, main="Person Income", ylab="Income")
boxplot(data$loan_amnt, main="Loan Amount", ylab="Loan Amount")

# 4. Initial Data Exploration ----
# Visualize distributions and relationships in the dataset to understand patterns and trends

# 4.1 Loan Status distribution
ggplot(data, aes(x = factor(loan_status))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Loan Status Distribution", x = "Loan Status (0 = Not Approved, 1 = Approved)", y = "Count")

# 4.2 Credit history length distribution
ggplot(data, aes(x = cb_person_cred_hist_length)) +
  geom_histogram(binwidth = 1, fill = "purple", color = "black") +
  labs(title = "Distribution of Credit History Length", x = "Credit History Length (years)", y = "Count")

# 4.3 Age distribution
ggplot(data, aes(x = person_age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Applicant Age", x = "Age", y = "Count")

# 4.4 Income distribution
ggplot(data, aes(x = person_income)) +
  geom_histogram(binwidth = 10000, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Applicant Income", x = "Income", y = "Count") +
  scale_x_continuous(labels = scales::comma)

# 4.5 Loan amount distribution
ggplot(data, aes(x = loan_amnt)) +
  geom_histogram(binwidth = 1000, fill = "lightcoral", color = "black") +
  labs(title = "Distribution of Loan Amount", x = "Loan Amount", y = "Count") +
  scale_x_continuous(labels = scales::comma)

# 4.6 Loan Approval Rate by Loan Intent
loan_intent_approval <- data %>%
  group_by(loan_intent) %>%
  summarize(approval_rate = mean(loan_status == 1, na.rm = TRUE)) %>%
  arrange(desc(approval_rate))

# Print the calculated approval rates
print(loan_intent_approval)

# Generate the plot with approval rate labels
loan_intent_plot <- ggplot(loan_intent_approval, aes(x = loan_intent, y = approval_rate, fill = loan_intent)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(approval_rate, accuracy = 0.1)), 
            vjust = -0.5, size = 3) +
  labs(title = "Loan Approval Rate by Loan Intent", x = "Loan Intent", y = "Approval Rate") +
  theme_minimal() +
  theme(legend.position = "none")

# Save the plot as a PNG file
ggsave("loan_intent_approval.png", plot = loan_intent_plot, width = 8, height = 6, dpi = 300, bg = "white")
