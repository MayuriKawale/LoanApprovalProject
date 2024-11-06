

library(ggplot2)
library(dplyr)
library(parallel)
library(doParallel)
library(randomForest)
library(fastDummies)
library(ggcorrplot)
library(gridExtra)
library(reshape2)

train_data <- read.csv("credit_risk_dataset.csv")
train_data_new <- train_data

is_missing <- function(x) {
  is.na(x) | x == "" | trimws(x) == ""
}

missing_values <- 
  sapply(train_data_new, function(x) {
    sum(is_missing(x)) 
  })

missing_rate <- 
  colMeans(sapply(train_data_new, is_missing)) * 100

data_types <- sapply(train_data_new, class)

data_quality_report <- data.frame(
  Column = names(train_data_new),
  DataType = data_types,
  MissingValues = missing_values,
  MissingRates = missing_rate
)

print(data_quality_report)

numeric_data <- train_data_new %>% select_if(is.numeric)
colnames(numeric_data)

# Outliers and extreme values

# Plot boxplots for each variable and save them as separate PNG files
plot_outliers_save <- function(data) {
  # Select numeric columns
  numeric_cols <- sapply(data, is.numeric)
  data_numeric <- data[, numeric_cols]
  
  for (col_name in names(data_numeric)) {
    # Dynamically set filename, including variable name in filename
    png_filename <- paste0("boxplot_", col_name, ".png")
    
    # Open PNG device
    png(filename = png_filename, width = 600, height = 400)
    
    # Generate boxplot
    p <- ggplot(data, aes(y = .data[[col_name]])) +
         geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
         labs(title = paste("Boxplot of", col_name), y = col_name) +
         theme_minimal()
    
    # Render and save image
    print(p)
    
    # Close PNG device
    dev.off()
  }
}

# Call function to save each variable's boxplot as PNG files
plot_outliers_save(train_data_new)

plot_outliers_save_combined <- function(data) {
  # Select numeric columns
  numeric_cols <- sapply(data, is.numeric)
  data_numeric <- data[, numeric_cols & names(data) != "loan_status"]
  
  # Create an empty list to store all boxplots
  plot_list <- list()
  
  for (col_name in names(data_numeric)) {
    # Generate boxplot and add to list
    p <- ggplot(data, aes(y = .data[[col_name]])) +
      geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
      labs(title = paste("Boxplot of", col_name), y = col_name) +
      theme_minimal()
    
    plot_list[[col_name]] <- p
  }
  
  # Arrange all boxplots in one image and save as PNG file
  png("combined_boxplots_numeric.png", width = 1200, height = 800)
  grid.arrange(grobs = plot_list, ncol = 3)  # Adjust ncol parameter to control number of plots per row
  dev.off()
}

# Call function to arrange all boxplots in one image
plot_outliers_save_combined(train_data_new)

# Define a function to find outliers for a column
find_outliers <- function(data, column_name) {
  # Calculate Q1, Q3, and IQR
  Q1 <- quantile(data[[column_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column_name]], 0.75, na.rm = TRUE)
  IQR_value <- IQR(data[[column_name]], na.rm = TRUE)
  
  # Define lower and upper bounds for outliers
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  print(lower_bound)
  print(upper_bound)
  
  # Filter out outliers
  outliers <- data[data[[column_name]] < lower_bound | data[[column_name]] > upper_bound, ]
  
  return(outliers)
}

# Define a function to find extreme values for a column
find_extreme_values <- function(data, column_name) {
  # Calculate Q1, Q3, and IQR
  Q1 <- quantile(data[[column_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column_name]], 0.75, na.rm = TRUE)
  IQR_value <- IQR(data[[column_name]], na.rm = TRUE)
  
  # Define lower and upper bounds for extreme values
  lower_bound_extreme <- Q1 - 3 * IQR_value
  upper_bound_extreme <- Q3 + 3 * IQR_value
  
  # Filter out extreme values
  extreme_values <- data[data[[column_name]] < lower_bound_extreme | data[[column_name]] > upper_bound_extreme, ]
  
  return(extreme_values)
}

# `person_income`

# Keep outliers and extreme values

# Use function to find outliers in the `person_income` column
outliers_person_income <-
  find_outliers(train_data_new, "person_income")

# View outliers
print(outliers_person_income)

# Use function to find extreme values in the `person_income` column
extreme_values_person_income <- find_extreme_values(train_data_new, "person_income")

# View extreme values
print(extreme_values_person_income)

# Remove extreme values over 123 years
# train_data_new <- train_data_new[train_data_new$person_emp_length <= 100, ]

# `person_emp_length`

# Keep outliers and extreme values

# Use function to find outliers in the `person_income` column
outliers_person_emp_length <-
  find_outliers(train_data_new, "person_emp_length")

# View outliers
print(outliers_person_emp_length)

# Use function to find extreme values in the `person_emp_length` column
extreme_values_person_emp_length <- find_extreme_values(train_data_new, "person_emp_length")

# View extreme values
print(extreme_values_person_emp_length)

# Remove extreme values over 123 years
# train_data_new <- train_data_new[train_data_new$person_emp_length <= 100, ]

# `person_age`

# Keep outliers and extreme values

# Use function to find outliers in the `person_income` column
outliers_person_age <-
  find_outliers(train_data_new, "person_age")

# View outliers
print(outliers_person_age)

# Use function to find extreme values in the `person_age` column
extreme_values_person_age <- find_extreme_values(train_data_new, "person_age")

# View extreme values
print(extreme_values_person_age)

# Remove extreme values above 100 years
# train_data_new <- train_data_new[train_data_new$person_age <= 100, ]

# `loan_percent_income`

# Keep outliers and extreme values

# Use function to find outliers in the `person_income` column
outliers_loan_percent_income <-
  find_outliers(train_data_new, "loan_percent_income")

# View outliers
print(outliers_loan_percent_income)

# Use function to find extreme values in the `loan_percent_income` column
extreme_values_loan_percent_income <- find_extreme_values(train_data_new, "loan_percent_income")

# View extreme values
print(extreme_values_loan_percent_income)

# Remove extreme values above 100 years
# train_data_new <- train_data_new[train_data_new$loan_percent_income <= 100, ]

# `loan_int_rate`

# Keep outliers and extreme values

# Use function to find outliers in the `person_income` column
outliers_loan_int_rate <-
  find_outliers(train_data_new, "loan_int_rate")

# View outliers
print(outliers_loan_int_rate)

# Use function to find extreme values in the `loan_int_rate` column
extreme_values_loan_int_rate <- find_extreme_values(train_data_new, "loan_int_rate")

# View extreme values
print(extreme_values_loan_int_rate)

# Remove extreme values above 100 years
# train_data_new <- train_data_new[train_data_new$loan_int_rate >= 20, ]

# `loan_amnt`

# Keep outliers and extreme values

# Use function to find outliers in the `person_income` column
outliers_loan_amnt <-
  find_outliers(train_data_new, "loan_amnt")

# View outliers
print(outliers_loan_amnt)

# Use function to find extreme values in the `loan_amnt` column
extreme_values_loan_amnt <- find_extreme_values(train_data_new, "loan_amnt")

# View extreme values
print(extreme_values_loan_amnt)

# Remove extreme values above 100 years
# train_data_new <- train_data_new[train_data_new$loan_amnt >= 20, ]

# `cb_person_cred_hist_length`

# Keep outliers and extreme values

# Use function to find outliers in the `cb_person_cred_hist_length` column
outliers_cb_person_cred_hist_length <-
  find_outliers(train_data_new, "cb_person_cred_hist_length")

# View outliers
print(outliers_cb_person_cred_hist_length)

# Use function to find extreme values in the `cb_person_cred_hist_length` column
extreme_values_cb_person_cred_hist_length <- find_extreme_values(train_data_new
                                                                 
                                                                 , "cb_person_cred_hist_length")

# View extreme values
print(extreme_values_cb_person_cred_hist_length)

# Remove extreme values above 100 years
# train_data_new <- train_data_new[train_data_new$cb_person_cred_hist_length >= 20, ]

non_numeric_data <- train_data_new %>% select_if(~ !is.numeric(.))
colnames(non_numeric_data)

# Use table() to view the frequency of each category
table(train_data_new$person_home_ownership)

# Use dplyr's count() function as an alternative
# train_data_new %>%
#   count(person_home_ownership) %>%
#   arrange(desc(n)) # Arrange in descending order of occurrences

# Find categories with very low frequency
# rare_categories <- train_data_new %>%
#   count(person_home_ownership) %>%
#   filter(n < 200) # Set threshold as per specific requirement, e.g., less than 5 times

# Use table() to view the frequency of each category
table(train_data_new$loan_intent)

# Use dplyr's count() function as an alternative
# train_data_new %>%
#   count(loan_intent) %>%
#   arrange(desc(n)) # Arrange in descending order of occurrences

# Find categories with very low frequency
# rare_categories <- train_data_new %>%
#   count(loan_intent) %>%
#   filter(n < 200) # Set threshold as per specific requirement, e.g., less than 5 times

# Use table() to view the frequency of each category
table(train_data_new$loan_grade)

# Use dplyr's count() function as an alternative
# train_data_new %>%
#   count(loan_grade) %>%
#   arrange(desc(n)) # Arrange in descending order of occurrences

# Find categories with very low frequency
# rare_categories <- train_data_new %>%
#   count(loan_grade) %>%
#   filter(n < 200) # Set threshold as per specific requirement, e.g., less than 5 times

# Use table() to view the frequency of each category
table(train_data_new$cb_person_default_on_file)

# Use dplyr's count() function as an alternative
# train_data_new %>%
#   count(cb_person_default_on_file) %>%
#   arrange(desc(n)) # Arrange in descending order of occurrences

# Find categories with very low frequency
# rare_categories <- train_data_new %>%
#   count(cb_person_default_on_file) %>%
#   filter(n < 200) # Set threshold as per specific requirement, e.g., less than 5 times

# Analyze the relationship between person_home_ownership and target variable

train_data_new$person_home_ownership <- as.factor(train_data_new$person_home_ownership)
train_data_new$loan_status <- as.factor(train_data_new$loan_status)

png("test.png", width = 600, height = 400)
plot1 = ggplot(train_data_new, aes(x = person_home_ownership, fill = loan_status)) +
  geom_bar(position = "fill") + 
  labs(x = "Home Ownership", y = "Proportion", 
       title = "Relationship between Home Ownership and Loan Status") +
  theme_minimal()
dev.off()

# Analyze the relationship between loan_intent and target variable

train_data_new$loan_intent <- as.factor(train_data_new$loan_intent)
train_data_new$loan_status <- as.factor(train_data_new$loan_status)

# png("test.png", width = 600, height = 400)
plot2 = ggplot(train_data_new, aes(x = loan_intent, fill = loan_status)) +
  geom_bar(position = "fill") + 
  labs(x = "Loan Intent", y = "Proportion", 
       title = "Relationship between Loan Intent and Loan Status") +
  theme_minimal()
# dev.off()

# Analyze the relationship between loan_grade and target variable

train_data_new$loan_grade <- as.factor(train_data_new$loan_grade)
train_data_new$loan_status <- as.factor(train_data_new$loan_status)

# png("test.png", width = 600, height = 400)
plot3 = ggplot(train_data_new, aes(x = loan_grade, fill = loan_status)) +
  geom_bar(position = "fill") + 
  labs(x = "Loan Grade", y = "Proportion", 
       title = "Relationship between Loan Grade and Loan Status") +
  theme_minimal()
# dev.off()

# Analyze the relationship between cb_person_default_on_file and target variable

train_data_new$cb_person_default_on_file <- as.factor(train_data_new$cb_person_default_on_file)
train_data_new$loan_status <- as.factor(train_data_new$loan_status)

# png("test.png", width = 600, height = 400)
plot4 = ggplot(train_data_new, aes(x = cb_person_default_on_file, fill = loan_status)) +
  geom_bar(position = "fill") + 
  labs(x = "Default_on_File", y = "Proportion", 
       title = "Relationship between Default_on_File and Loan Status") +
  theme_minimal()
# dev.off()

png("combined_plot_factor.png", width = 800, height = 800)
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)
dev.off()

train_data_new_clean <- 
  subset(train_data_new, 
         person_emp_length >= 0 & person_emp_length <= 100)

summary(train_data_new_clean$person_emp_length)

train_data_new_clean <- 
  subset(train_data_new_clean, 
         person_income >= 0 & person_income <= 3000000)

summary(train_data_new_clean$person_income)

train_data_new_clean <- 
  subset(train_data_new_clean, 
         person_age >= 0 & person_age <= 100)

summary(train_data_new_clean$person_age)

# Filter rows with missing values in loan_int_rate
missing_loan_int_rate_rows <- train_data_new_clean[is.na(train_data_new_clean$loan_int_rate), ]

# View rows with missing values
print(missing_loan_int_rate_rows)

data_with_rate <- train_data_new_clean %>% filter(!is.na(loan_int_rate))
data_missing_rate <- train_data_new_clean %>% filter(is.na(loan_int_rate))

train_data_temp <- data_with_rate %>%
  select(person_income, loan_amnt, loan_grade, cb_person_cred_hist_length, loan_int_rate)

# Remove rows with missing values (ensure training data is complete)
train_data_temp <- na.omit(train_data_temp)

train_data_temp$loan_grade <- as.factor(train_data_temp$loan_grade)
data_missing_rate$loan_grade <- as.factor(data_missing_rate$loan_grade)

lm_model <- 
  lm(loan_int_rate ~ person_income + loan_amnt + loan_grade + cb_person_cred_hist_length, 
     data = train_data_temp)
summary(lm_model)

predict_data <- data_missing_rate %>%
  select(person_income, loan_amnt, loan_grade, cb_person_cred_hist_length)

# Use model to predict missing loan_int_rate values
predicted_rates <- predict(lm_model, newdata = predict_data)

# Fill missing data with predicted values
data_missing_rate$loan_int_rate <- predicted_rates

final_data <- bind_rows(data_with_rate, data_missing_rate)

# View merged dataset
summary(final_data)

train_data_new_clean <- final_data

is_missing <- function(x) {
  is.na(x) | x == "" | trimws(x) == ""
}

missing_values <- 
  sapply(train_data_new_clean, function(x) {
    sum(is_missing(x)) 
  })

missing_rate <- 
  colMeans(sapply(train_data_new_clean, is_missing)) * 100

data_types <- sapply(train_data_new_clean, class)

data_quality_report <- data.frame(
  Column = names(train_data_new_clean),
  DataType = data_types,
  MissingValues = missing_values,
  MissingRates = missing_rate
)

print(data_quality_report)

# person_emp_length  2.7%
# loan_int_rate 9.5%

# Create a histogram
histogram_plot <- ggplot(train_data_new_clean, aes(x = person_emp_length)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Employment Length", x = "Employment Length (years)", y = "Frequency") +
  theme_minimal()

# Create QQ plot
qq_plot <- ggplot(train_data_new_clean, aes(sample = person_emp_length)) +
  stat_qq() +
  stat_qq_line(color = "red", lwd = 1.2) +
  labs(title = "QQ Plot of Employment Length") +
  theme_minimal()

# Combine both plots into one image
png("combined_plot_person_emp_length.png", width = 800, height = 400)
grid.arrange(histogram_plot, qq_plot, ncol = 2)
dev.off()

# Add constant 1 to avoid errors in log transformation
train_data_new_clean$log_person_emp_length <- log(train_data_new_clean$person_emp_length + 1)

# Create histogram for Log-transformed Employment Length
histogram_log_plot <- ggplot(train_data_new_clean, aes(x = log_person_emp_length)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histogram of Log-transformed Employment Length", x = "Log(Employment Length + 1)", y = "Frequency") +
  theme_minimal()

# Create QQ plot
qq_log_plot <- ggplot(train_data_new_clean, aes(sample = log
                                                
                                                _person_emp_length)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "QQ Plot of Log-transformed Employment Length") +
  theme_minimal()

# Combine both plots into one image
png("combined_plots_log_person_emp_length.png", width = 800, height = 400)
grid.arrange(histogram_log_plot, qq_log_plot, ncol = 2)
dev.off()

# Create a histogram
histogram_plot <- ggplot(train_data_new_clean, aes(x = loan_int_rate)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Loan Interest Rate", x = "Interest Rate (%)", y = "Frequency") +
  theme_minimal()

# Create QQ plot
qq_plot <- ggplot(train_data_new_clean, aes(sample = loan_int_rate)) +
  stat_qq() +
  stat_qq_line(color = "red", lwd = 1.2) +
  labs(title = "QQ Plot of Loan Interest Rate") +
  theme_minimal()

# Combine both plots into one image
png("combined_interest_rate_plots.png", width = 800, height = 400)
grid.arrange(histogram_plot, qq_plot, ncol = 2)
dev.off()

# Add constant 1 to avoid errors in log transformation
train_data_new_clean$log_loan_int_rate <- log(train_data_new_clean$loan_int_rate + 1)

# View distribution of transformed variable
# Create histogram for Log-transformed Loan Interest Rate
histogram_log_plot <- ggplot(train_data_new_clean, aes(x = log_loan_int_rate)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histogram of Log-transformed Loan Interest Rate", x = "Log(Loan Interest Rate + 1)", y = "Frequency") +
  theme_minimal()

# Create QQ plot
qq_log_plot <- ggplot(train_data_new_clean, aes(sample = log_loan_int_rate)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "QQ Plot of Log-transformed Loan Interest Rate") +
  theme_minimal()

# Combine both plots into one image
png("combined_log_loan_int_rate_plots.png", width = 800, height = 400)
grid.arrange(histogram_log_plot, qq_log_plot, ncol = 2)
dev.off()

data_encoded <- 
  dummy_cols(train_data_new_clean, 
             select_columns = c("person_home_ownership", 
                                "loan_intent", "loan_grade", 
                                "cb_person_default_on_file"),
             remove_first_dummy = FALSE, # FALSE
             remove_selected_columns = TRUE)

str(data_encoded)
data_encoded[] <- lapply(data_encoded, function(x) as.numeric(as.character(x)))
cor_matrix <- cor(data_encoded, use = "complete.obs")

# Set threshold, retain only values with correlation greater than 0.5 or less than -0.5
cor_matrix_filtered <- cor_matrix
cor_matrix_filtered[abs(cor_matrix) < 0.5] <- NA  # Replace lower correlation values with NA, not displayed

png("cor_matrix_filtered.png", width = 1200, height = 1200)
ggcorrplot(cor_matrix_filtered, 
           method = "circle", 
           type = "lower", 
           lab = TRUE, 
           lab_size = 7,  # Increase label font size
           title = "Filtered Correlation Matrix (|correlation| > 0.5)", 
           colors = c("red", "white", "blue")) +
  ggtitle("Filtered Correlation Matrix (|correlation| > 0.5)") +
  theme(plot.title = element_text(size = 20),     # Adjust title font size
        axis.text = element_text(size = 15),      # Adjust x, y-axis label font size
        axis.title = element_text(size = 18))     # Adjust x, y-axis title font size
dev.off()

# Example data
set.seed(0)
# data <- data.frame(
#   loan_amnt = rnorm(100, mean = 10000, sd = 2000),
#   loan_int_rate = rnorm(100, mean = 5, sd = 1) + 0.003 * rnorm(100, mean = 10000, sd = 2000)
# )

png("loan_amnt_rate_plot.png", width = 1200, height = 1200)
# Plot scatter plot with regression line
ggplot(data_encoded, aes(x = loan_amnt, y = loan_int_rate)) +
  geom_point(color = "orange") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Scatter Plot of Loan Amount vs Loan Interest Rate with Regression Line",
       x = "Loan Amount", y = "Loan Interest Rate") +
  theme_minimal(base_size = 15)
dev.off()

# Example data
# data_encoded$person_home_ownership_MORTGAGE <- sample(0:1, 100, replace = TRUE)
# data_encoded$cb_person_default_on_file_N <- sample(0:1, 100, replace = TRUE)
# data_encoded$cb_person_default_on_file_Y <- 1 - data$cb_person_default_on_file_N  # Complementary binary variable

# Calculate correlation matrix
cor_matrix <- cor(data_encoded)

# Filter correlation matrix, retain only values where |correlation| > 0.5
cor_matrix_filtered <- ifelse(abs(cor_matrix) > 0.5, cor_matrix, 0)

# Convert matrix to long format for plotting
melted_cor_matrix <- melt(cor_matrix_filtered)
colnames(melted_cor_matrix) <- c("Var1", "Var2", "value")

png("scale_fill_gradient2.png", width = 1200, height = 1200)
# Plot heatmap
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       limit = c(-1, 1), name = "Correlation") +
  theme_minimal(base_size = 15) +
  labs(title = "Heatmap of Filtered Correlation Matrix (|correlation| > 0.5)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

# Plot density plot
png("person_home_ownership_MORTGAGE_rate1.png", width = 1200, height = 1200)
ggplot(data_encoded, aes(x = loan_int_rate, fill = as.factor(person_home_ownership_MORTGAGE))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Loan Interest Rate by Home Ownership (Mortgage vs Others)",
       x = "Loan Interest Rate",
       y = "Density") +
  scale_fill_manual(values = c("lightgreen", "skyblue"), labels = c("Others", "Mortgage"), name = "Home Ownership") +
  theme_minimal(base_size = 15)

dev.off()

colnames(cor_matrix_filtered)
loan_status_corr <- cor_matrix["loan_status", ]
high_corr_vars <- sort(loan_status_corr[abs(loan_status_corr) > 0.2], decreasing = TRUE)
print(high_corr_vars)

png("test01.png", width = 600, height = 400)
high_corr_matrix <- cor_matrix[names(high_corr_vars), names(high_corr_vars)]
high_corr_melted <- melt(high_corr_matrix)
ggplot(data = high_corr_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "High Correlation Variables with Loan Status", x = "Variables", y = "Variables")
dev.off()

person_age_corr <- cor_matrix["person_age", ]
person_age_vars <- sort(person_age_corr[abs(person_age_corr) > 0.2], decreasing = TRUE)
print(person_age_vars)

person_income_corr <- cor_matrix["person_income", ]
person_income_vars <- sort(person_income_corr[abs(person_income_corr) > 0.2], decreasing = TRUE)
print(person_income_vars)

person_emp_length_corr <- cor_matrix["person_emp_length", ]
person_emp_length_vars <- sort(person_emp_length_corr[abs(person_emp_length_corr) > 0.2], decreasing = TRUE)
print(person_emp_length_vars)

loan_amnt_corr <- cor_matrix["loan_amnt", ]
loan_amnt_vars <- sort(loan_amnt_corr[abs(loan_amnt_corr) > 0.2], decreasing = TRUE)
print(loan_amnt_vars)

loan_int_rate_corr <- cor_matrix["loan_int_rate", ]
loan_int_rate_vars <- sort(loan_int_rate_corr[abs(loan_int_rate_corr) > 0.2], decreasing = TRUE)
print(loan_int_rate_vars)

loan_percent_income_corr <- cor_matrix["loan_percent_income", ]
loan_percent_income_vars <- sort(loan_percent_income_corr[abs(loan_percent_income_corr) > 0.2], decreasing =
                                   
                                   TRUE)
print(loan_percent_income_vars)

cb_person_cred_hist_length_corr <- cor_matrix["cb_person_cred_hist_length", ]
cb_person_cred_hist_length_vars <- sort(cb_person_cred_hist_length_corr[abs(cb_person_cred_hist_length_corr) > 0.2], decreasing = TRUE)
print(cb_person_cred_hist_length_vars)

person_home_ownership_MORTGAGE_corr <- cor_matrix["person_home_ownership_MORTGAGE", ]
person_home_ownership_MORTGAGE_vars <- sort(person_home_ownership_MORTGAGE_corr[abs(person_home_ownership_MORTGAGE_corr) > 0.2], decreasing = TRUE)
print(person_home_ownership_MORTGAGE_vars)

person_home_ownership_OTHER_corr <- cor_matrix["person_home_ownership_OTHER", ]
person_home_ownership_OTHER_vars <- sort(person_home_ownership_OTHER_corr[abs(person_home_ownership_OTHER_corr) > 0.2], decreasing = TRUE)
print(person_home_ownership_OTHER_vars)

person_home_ownership_OWN_corr <- cor_matrix["person_home_ownership_OWN", ]
person_home_ownership_OWN_vars <- sort(person_home_ownership_OWN_corr[abs(person_home_ownership_OWN_corr) > 0.2], decreasing = TRUE)
print(person_home_ownership_OWN_vars)

person_home_ownership_RENT_corr <- cor_matrix["person_home_ownership_RENT", ]
person_home_ownership_RENT_vars <- sort(person_home_ownership_RENT_corr[abs(person_home_ownership_RENT_corr) > 0.2], decreasing = TRUE)
print(person_home_ownership_RENT_vars)

loan_intent_DEBTCONSOLIDATION_corr <- cor_matrix["loan_intent_DEBTCONSOLIDATION", ]
loan_intent_DEBTCONSOLIDATION_vars <- sort(loan_intent_DEBTCONSOLIDATION_corr[abs(loan_intent_DEBTCONSOLIDATION_corr) > 0.2], decreasing = TRUE)
print(loan_intent_DEBTCONSOLIDATION_vars)

loan_intent_EDUCATION_corr <- cor_matrix["loan_intent_EDUCATION", ]
loan_intent_EDUCATION_vars <- sort(loan_intent_EDUCATION_corr[abs(loan_intent_EDUCATION_corr) > 0.2], decreasing = TRUE)
print(loan_intent_EDUCATION_vars)

loan_intent_HOMEIMPROVEMENT_corr <- cor_matrix["loan_intent_HOMEIMPROVEMENT", ]
loan_intent_HOMEIMPROVEMENT_vars <- sort(loan_intent_HOMEIMPROVEMENT_corr[abs(loan_intent_HOMEIMPROVEMENT_corr) > 0.2], decreasing = TRUE)
print(loan_intent_HOMEIMPROVEMENT_vars)

loan_intent_MEDICAL_corr <- cor_matrix["loan_intent_MEDICAL", ]
loan_intent_MEDICAL_vars <- sort(loan_intent_MEDICAL_corr[abs(loan_intent_MEDICAL_corr) > 0.2], decreasing = TRUE)
print(loan_intent_MEDICAL_vars)

loan_intent_PERSONAL_corr <- cor_matrix["loan_intent_PERSONAL", ]
loan_intent_PERSONAL_vars <- sort(loan_intent_PERSONAL_corr[abs(loan_intent_PERSONAL_corr) > 0.2], decreasing = TRUE)
print(loan_intent_PERSONAL_vars)

loan_intent_VENTURE_corr <- cor_matrix["loan_intent_VENTURE", ]
loan_intent_VENTURE_vars <- sort(loan_intent_VENTURE_corr[abs(loan_intent_VENTURE_corr) > 0.2], decreasing = TRUE)
print(loan_intent_VENTURE_vars)

loan_grade_A_corr <- cor_matrix["loan_grade_A", ]
loan_grade_A_vars <- sort(loan_grade_A_corr[abs(loan_grade_A_corr) > 0.2], decreasing = TRUE)
print(loan_grade_A_vars)

loan_grade_B_corr <- cor_matrix["loan_grade_B", ]
loan_grade_B_vars <- sort(loan_grade_B_corr[abs(loan_grade_B_corr) > 0.2], decreasing = TRUE)
print(loan_grade_B_vars)

loan_grade_C_corr <- cor_matrix["loan_grade_C", ]
loan_grade_C_vars <- sort(loan_grade_C_corr[abs(loan_grade_C_corr) > 0.2], decreasing = TRUE)
print(loan_grade_C_vars)

loan_grade_D_corr <- cor_matrix["loan_grade_D", ]
loan_grade_D_vars <- sort(loan_grade_D_corr[abs(loan_grade_D_corr) > 0.2], decreasing = TRUE)
print(loan_grade_D_vars)

loan_grade_E_corr <- cor_matrix["loan_grade_E", ]
loan_grade_E_vars <- sort(loan_grade_E_corr[abs(loan_grade_E_corr) > 0.2], decreasing = TRUE)
print(loan_grade_E_vars)

loan_grade_F_corr <- cor_matrix["loan_grade_F", ]
loan_grade_F_vars <- sort(loan_grade_F_corr[abs(loan_grade_F_corr) > 0.2], decreasing = TRUE)
print(loan_grade_F_vars)

loan_grade_G_corr <- cor_matrix["loan_grade_G", ]
loan_grade_G_vars <- sort(loan_grade_G_corr[abs(loan_grade_G_corr) > 0.2], decreasing = TRUE)
print(loan_grade_G_vars)

cb_person_default_on_file_N_corr <- cor_matrix["cb_person_default_on_file_N", ]
cb_person_default_on_file_N_vars <- sort(cb_person_default_on_file_N_corr[abs(cb_person_default_on_file_N_corr) > 0.2], decreasing = TRUE)
print(cb_person_default_on_file_N_vars)

cb_person_default_on_file_Y_corr <- cor_matrix["cb_person_default_on_file_Y", ]
cb_person_default_on_file_Y_vars <- sort(cb_person_default_on_file_Y_corr[abs(cb_person_default_on_file_Y_corr) > 0.2], decreasing = TRUE)
print(cb_person_default_on_file_Y_vars)

# `person_age`  `cb_person_cred_hist_length`

png("test6.png", width = 600, height = 400)
ggplot(data_encoded, aes(x = person_age, y = cb_person_cred_hist_length)) +
  geom_point(alpha = 0.5) +          
  geom_smooth(method = "lm", col = "blue") +  
  labs(title = "Scatter Plot of Person Age vs. Credit History Length",
       x = "Person Age",
       y = "Credit History Length") +
  theme_minimal() 
dev.off()

data_encoded_temp <- data_encoded
data_encoded_temp$home_ownership_combination <- with(data_encoded, 
                                                     paste(person_home_ownership_RENT, 
                                                           person_home_ownership_MORTGAGE, 
                                                           sep = "_"))

png("test7.png", width = 600, height = 400)
ggplot(data_encoded_temp, aes(x = home_ownership_combination)) +
  geom_bar(aes(fill = home_ownership_combination), color = "black") +
  labs(title = "Distribution of Home Ownership Combinations",
       x = "Home Ownership (RENT_MORTGAGE)",
       y = "Count") +
  scale_fill_manual(values = c("0_0" = "grey", "1_0" = "blue", "0_1" = "green", "1_1" = "red"),
                    labels = c("Neither", "Rent Only", "Mortgage Only", "Both")) +
  theme_minimal()
dev.off()

png("test8.png", width = 600, height = 400)
ggplot(data_encoded, aes(x = factor(loan_grade_A), y = loan_int_rate)) +
  geom_boxplot(fill = c("lightblue", "lightgreen")) +
  labs(title = "Box Plot of Loan Interest Rate by Loan Grade A",
       x = "Loan Grade A (0 = No, 1 = Yes)",
       y = "Loan Interest Rate") +
  theme_minimal()
dev.off()

png("test9.png", width = 600, height = 400)
ggplot(data_encoded, aes(x = loan_amnt, y = loan_percent_income)) +
  geom_point(alpha = 0.5) +          
  geom_smooth(method = "lm", col = "blue") +  
  labs(title = "Scatter Plot of Loan Amount vs. Loan Percent of Income",
       x = "Loan Amount",
       y = "Loan Percent of Income") +
  theme_minimal()                   
dev.off()

png("test10.png", width = 600, height = 400)
ggplot(data_encoded, aes(x = factor(cb_person_default_on_file_Y), y = loan_int_rate)) +
  geom_boxplot(fill = c("lightblue", "lightpink")) +
  labs(title = "Box Plot of Loan Interest Rate by Default on File Status",
       x = "Default on File (0 = No, 1 = Yes)",
       y = "Loan Interest Rate") +
  theme_minimal()
dev.off()

png("test11.png", width = 600, height = 400)
ggplot(data_encoded, aes(x = factor(cb_person_default_on_file_N), y = loan_int_rate)) +
  geom_boxplot(fill = c("lightblue", "lightpink")) +
  labs(title = "Box Plot of Loan Interest Rate by No Default on File Status",
       x = "No Default on File (0 = Has Default, 1 = No Default)",
       y = "Loan Interest Rate") +
  theme_minimal()
dev.off()

data_encoded_temp$grade_C_default_combination <- with(data_encoded, 
                                                      paste(loan_grade_C, cb_person_default_on_file_N, sep = "_"))

png("test12.png", width = 600, height = 400)
ggplot(data_encoded_temp, aes(x = grade_C_default_combination)) +
  geom_bar(aes(fill = grade_C_default_combination), color = "black") +
  labs(title = "Distribution of Loan Grade C and No Default on File Combinations",
       x = "Loan Grade C and No Default (C_N)",
       y = "Count") +
  scale_fill_manual(values = c("0_0" = "grey", "1_0" = "blue", "0_1" = "green", "1_1" = "red"),
                    labels = c("Neither C nor No Default", "C Only", "No Default Only", "Both C and No Default")) +
  theme_minimal()
dev.off()

png("test13.png", width = 600, height = 400)
ggplot(data_encoded, aes(x = factor(loan_grade_D), y = loan_int_rate)) +
  geom_boxplot(fill = c("lightblue", "lightpink")) +
  labs(title = "Box Plot of Loan Interest Rate by Loan Grade D",
       x = "Loan Grade D (0 = No, 1 = Yes)",
       y = "Loan Interest Rate") +
  theme_minimal()
dev.off()

png("test14.png", width = 600, height = 400)
ggplot(data_encoded, aes(x = factor(loan_grade_D), y = loan_int_rate, fill = factor(cb_person_default_on_file_Y))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Box Plot of Loan Interest Rate by Loan Grade D and Default Status",
       x = "Loan Grade D (0 = Non-D Grade, 1 = D Grade)",
       y = "Loan Interest Rate",
       fill = "Default on File (1 = Yes, 0 = No)") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "pink"), 
                    labels = c("No Default", "Default")) +
  theme_minimal()
dev.off()

png("test15.png", width = 600, height = 400)
# Create groups based on loan amount
data_encoded_temp$loan_amnt_group <- cut(data_encoded$loan_amnt, 
                                         breaks = c(-Inf, 10000, 20000, Inf), 
                                         labels = c("Low", "Medium", "High"))

# Plot faceted box plot
ggplot(data_encoded_temp, aes(x = factor(loan_grade_D), y = loan_int_rate, fill = factor(cb_person_default_on_file_Y))) +
  geom_boxplot(width = 0.6, position = position_dodge(0.8), alpha = 0.7) +  # Use box plot only
  facet_wrap(~ loan_amnt_group, scales = "free_y") +  # Group by loan amount, facet display
  labs(title = "Faceted Box Plot of Loan Interest Rate by Loan Grade D, Default Status, and Loan Amount Group",
       x = "Loan Grade D (0 = Non-D Grade, 1 = D Grade)",
       y = "Loan Interest Rate",
       fill = "Default on File (1 = Yes, 0 = No)") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "pink"), 
                    labels = c("No Default", "Default")) +
  theme_minimal()
dev.off()

png("test16.png", width = 600, height = 400)
# Plot faceted box plot
ggplot(data_encoded, aes(x = cb_person_cred_hist_length, y = loan_int_rate, color = factor(cb_person_default_on_file_Y))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter Plot of Loan Interest Rate vs. Credit History Length by Default Status",
       x = "Credit History Length",
       y = "Loan Interest Rate",
       color = "Default on File (1 = Yes, 0 = No)") +
  scale_color_manual(values = c("0" = "blue", "1" = "red")) +
  theme_minimal()
dev.off()

png("test17.png", width = 600, height = 400)
ggplot(train_data_new, aes(x = factor(loan_intent), y = loan_int_rate, fill = factor(loan_intent))) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~ cut(loan_amnt, breaks = c(-Inf, 10000, 20000, Inf), labels = c("Low", "Medium", "High"))) +
  labs(title = "Box Plot of Loan Interest Rate by Loan Intent and Loan Amount Group",
       x = "Loan Intent",
       y = "Loan Interest Rate",
       fill = "Loan Intent") +
  theme_minimal()
dev.off()

png("test18.png", width = 600, height = 400)
ggplot(train_data_new, aes(x = loan_percent_income, y = loan_int_rate, color = factor(loan_grade))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter Plot of Loan Interest Rate vs. Loan Percent Income by Loan Grade",
       x = "Loan Percent Income",
       y = "Loan Interest Rate",
       color = "Loan Grade") +
  theme_minimal()
dev.off()

png("test19.png", width = 600, height = 400)
ggplot(data_encoded, aes(x = factor(cb_person_default_on_file_Y), y = loan_int_rate, fill = factor(person_age > 30))) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  labs(title = "Violin Plot of Loan Interest Rate by Age Group and Default Status",
       x = "Default on File (1 = Yes, 0 = No)",
       y = "Loan Interest Rate",
       fill = "Age Group (Over 30)") +
  theme_minimal()
dev.off()

png("test20.png", width = 600, height = 400)
ggplot(train_data_new, aes(x = factor(loan_status), y = loan_int_rate, fill = factor(loan_grade))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Box Plot of Loan Interest Rate by Loan Status and Loan Grade",
       x = "Loan Status",
       y = "Loan Interest Rate",
       fill = "Loan Grade") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()
dev.off()

png("test21.png", width = 600, height = 400)
ggplot(train_data_new, aes(x = loan_amnt, y = factor(loan_status), color = factor(loan_intent))) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ loan_intent, scales = "free_y") +
  labs(title = "Scatter Plot of Loan Amount vs. Loan Status by Loan Intent",
       x = "Loan Amount",
       y = "Loan Status",
       color = "Loan Intent") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()
dev.off()

