library(dplyr)
library(ggplot2)
library(mice)  # For imputation model


setwd("/Users/peteroh/Desktop/Oh/OU/Fall 2024/DSA : ISE 5103/Final Project")
data <- read.csv("credit_risk_dataset 2.csv")

# Display basic structure and summary
str(data)
summary(data)

# Check for missing values in each column
sapply(data, function(x) sum(is.na(x)))

# missing values from person_emp_length: 895 / loan_int_rate:3116
# Strategy
# person_emp_length (Length of Employment in Years)
# Option 1: Replace missing values with the overall median.
data$person_emp_length[is.na(data$person_emp_length)] <- median(data$person_emp_length, na.rm = TRUE)

# Option 2: Replace missing values with the median based on specific groups such as loan_intent and person_home_ownership, assuming employment length may vary by these categories.
data <- data %>%
  group_by(loan_intent, person_home_ownership) %>%
  mutate(person_emp_length = ifelse(is.na(person_emp_length), 
                                    median(person_emp_length, na.rm = TRUE), 
                                    person_emp_length)) %>%
  ungroup()
# Option 3: Option 3: Treat missing values as a separate category by setting them to 0, allowing them to represent "Unknown" employment length.
data$person_emp_length[is.na(data$person_emp_length)] <- 0


# loan_int_rate (Interest Rate)
# Option 1: Replace missing values with the overall median to reduce bias while keeping the central trend of the data.
data$loan_int_rate[is.na(data$loan_int_rate)] <- median(data$loan_int_rate, na.rm = TRUE)

# Option 2: Replace missing values with the median within each loan_grade group, as interest rates may differ significantly by loan grade.
data <- data %>%
  group_by(loan_grade) %>%
  mutate(loan_int_rate = ifelse(is.na(loan_int_rate), 
                                median(loan_int_rate, na.rm = TRUE), 
                                loan_int_rate)) %>%
  ungroup()
# Option 3: Use a predictive model to estimate missing values based on other relevant features (e.g., loan_amnt, loan_grade, person_income), which could enhance accuracy by capturing relationships within the data.
imputed_data <- mice(data[, c("loan_int_rate", "loan_amnt", "loan_grade", "person_income")], 
                     method = "norm.predict", m = 1)

# Fill in the loan_int_rate column with the imputed values
data$loan_int_rate <- complete(imputed_data)$loan_int_rate

# Verify that missing values have been handled
sapply(data, function(x) sum(is.na(x)))

# outliers
# Checking for outliers in key numerical columns
boxplot(data$person_age, main="Person Age", ylab="Age")
boxplot(data$person_income, main="Person Income", ylab="Income")
boxplot(data$loan_amnt, main="Loan Amount", ylab="Loan Amount")





# skews
# Visualizing skewness using histograms
hist(data$person_income, main="Income Distribution", xlab="Income", breaks=50)
hist(data$loan_amnt, main="Loan Amount Distribution", xlab="Loan Amount", breaks=50)
hist(data$loan_int_rate, main="Interest Rate Distribution", xlab="Interest Rate", breaks=20)



# factors, and/or other data issues
# Convert relevant columns to factors
data$loan_intent <- as.factor(data$loan_intent)
data$loan_grade <- as.factor(data$loan_grade)
data$person_home_ownership <- as.factor(data$person_home_ownership)
data$cb_person_default_on_file <- as.factor(data$cb_person_default_on_file)

# Check levels and frequencies
summary(data$loan_intent)
summary(data$loan_grade)
summary(data$person_home_ownership)
summary(data$cb_person_default_on_file)



# Initial visual exploration
# Age distribution
ggplot(data, aes(x = person_age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Applicant Age", x = "Age", y = "Count")

# Income distribution
ggplot(data, aes(x = person_income)) +
  geom_histogram(binwidth = 10000, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Applicant Income", x = "Income", y = "Count") +
  scale_x_continuous(labels = scales::comma)

# Loan amount distribution
ggplot(data, aes(x = loan_amnt)) +
  geom_histogram(binwidth = 1000, fill = "lightcoral", color = "black") +
  labs(title = "Distribution of Loan Amount", x = "Loan Amount", y = "Count") +
  scale_x_continuous(labels = scales::comma)

# Loan Status distribution
ggplot(data, aes(x = factor(loan_status))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Loan Status Distribution", x = "Loan Status (0 = Not Approved, 1 = Approved)", y = "Count")

# Explore credit history length
ggplot(data, aes(x = cb_person_cred_hist_length)) +
  geom_histogram(binwidth = 1, fill = "purple", color = "black") +
  labs(title = "Distribution of Credit History Length", x = "Credit History Length (years)", y = "Count")