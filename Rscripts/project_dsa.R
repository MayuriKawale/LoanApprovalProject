library(tidyverse)    # For data manipulation and visualization
library(ggplot2)      # For plotting
library(corrplot)     # For correlation matrix plots

library(dplyr)        # For data manipulation

data <- read_csv("credit_risk_dataset.csv")

summary(data)

Q1 <- function(x, na.rm = TRUE) {
  quantile(x, na.rm= na.rm) [2]
  
}

Q3 <- function(x, na.rm = TRUE) {
  quantile(x, na.rm = na.rm) [4]
}

# Creating a tibble that contains only numeric variables from loan data
loanNumeric <- data %>%
  dplyr::select(where(is.numeric))

# View the numeric tibble
print(loanNumeric)

# creating a new function for making summary
myNumericSummary <-function(x) {
  c(length(x), n_distinct(x), sum(is.na(x)), mean(x, na.rm = TRUE),
    min(x, na.rm = TRUE), Q1(x, na.rm = TRUE), median(x, na.rm= TRUE), 
    Q3(x, na.rm = TRUE),
    max(x, na.rm = TRUE), sd(x, na.rm = TRUE))
}

# making a tibble and saving the results from myNumericSummary
numericSummary <- loanNumeric %>%
  dplyr::summarise(across(everything(), myNumericSummary))


# binding the labels
numericSummary <- cbind(stat = c( "n", "unique", "missing", "mean", "min", 
                                  "Q1", "median", "Q3" ,"max", "sd"), numericSummary)

colnames(numericSummary)

numericSummaryFinal <- numericSummary %>%
  pivot_longer(cols = -stat, names_to = "variable", values_to = "value") %>%   # Exclude 'stat' column
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(missing_pct = 100 * missing / n,
         unique_pct = 100 * unique / n) %>%
  dplyr::select(variable, n, missing, missing_pct, unique, unique_pct, everything())


library(knitr)
options(digits=3)
options(scipen=99)

library(kableExtra)
numericSummaryFinal %>%
  kable("latex", booktabs = TRUE, longtable = FALSE) %>%
  kable_styling(latex_options = c("striped", "scale_down"), font_size = 7)

html_file <- "numeric_summary_table.html"
writeLines(as.character(html_table), html_file)

# Open the HTML file in default web browser
browseURL(html_file)

# for categorical data


# making new function (frequency modes)
getmodes <- function(v , type = 1) {
  tb1 <- table(v)
  m1 <- which.max(tb1)
  
  if (type == 1) {
    return(names(m1))
  }
  else if (type == 2) { 
    return(names(which.max(tb1[-m1])))
  }
  else if (type == -1) {
    return (names(which.min(tb1)))
  }
  else {
    stop("Invalid Type selected")
  }
  
}

getmodesCnt <- function(v , type = 1){
  tb1 <- table(v)
  m1 <- which.max(tb1)
  
  if (type == 1){
    return (max(tb1))    # 1st mode freq
  }
  
  else if (type == 2) {
    return(max(tb1[-m1]))  #2nd mode
  }
  
  else if(type == -1){
    return(min(tb1))  # least common freq
  }
  
  else {
    stop("Ïnvalid Type selected")
    
  }
  
}


# Creating a tibble that contains character variables converted to factors from loan data
loanFactor <- data %>%
  dplyr::transmute(across(where(is.character), as.factor))

# View the factor tibble
print(loanFactor)

loanCategorical <- data %>%
  dplyr::transmute(across(where(~ is.character(.) | is.factor(.)), as.factor))

print(loanCategorical)

# Categorical data summary function
myCategoricalSummary <- function(x) {
  c(n = length(x), 
    missing = sum(is.na(x)), 
    missing_pct = 100 * sum(is.na(x)) / length(x),
    unique = n_distinct(x), 
    unique_pct = 100 * n_distinct(x) / length(x), 
    freqRatio = getmodesCnt(x, 1) / getmodesCnt(x, 2),  # Frequency ratio
    mode_1st = getmodes(x, 1),  # Most frequent (1st mode)
    mode_1st_freq = getmodesCnt(x, 1),  # 1st mode frequency
    mode_2nd = getmodes(x, 2),  # 2nd most frequent
    mode_2nd_freq = getmodesCnt(x, 2),  # 2nd mode frequency
    least_common = getmodes(x, -1),  # Least frequent
    least_common_freq = getmodesCnt(x, -1)  # Least common frequency
  )
}

# Apply the function to categorical data
categoricalSummary <- loanCategorical %>%
  dplyr::summarise(across(everything(), myCategoricalSummary))

# Add row names and reshape the summary
categoricalSummary <- cbind(stat = c("n", "missing", "missing_pct", "unique", "unique_pct", 
                                     "freqRatio", "1st_mode", "1st_mode_freq", 
                                     "2nd_mode", "2nd_mode_freq", 
                                     "least_common", "least_common_freq"), 
                            categoricalSummary)

colnames(categoricalSummary)
categoricalSummaryFinal <- categoricalSummary %>%
  pivot_longer(cols = -stat, names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value)


# Convert frequency ratio to numeric and round
categoricalSummaryFinal$freqRatio <- as.numeric(categoricalSummaryFinal$freqRatio)
categoricalSummaryFinal$freqRatio <- round(categoricalSummaryFinal$freqRatio, 2)


html_table <- categoricalSummaryFinal %>%
  kable("html", booktabs = TRUE, longtable = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F, font_size = 12)

# Save the table as an HTML file
html_file <- "categorical_summary_table.html"
writeLines(as.character(html_table), html_file)

# Open the HTML file in default web browser
browseURL(html_file)



# Data Cleaning

# Fix unrealistic values in person_age
data <- data %>%
  dplyr::mutate(person_age = ifelse(person_age>100, 100, person_age))

# fix employment length

data <- data %>%
  dplyr::mutate(person_emp_length = ifelse(person_emp_length > 60, 60, person_emp_length))

# Impute missing values
median_emp_length <- median(data$person_emp_length, na.rm = TRUE)

data <- data %>%
  mutate(person_emp_length = ifelse(is.na(person_emp_length), median_emp_length, person_emp_length))


median_int_rate <- median(data$loan_int_rate, na.rm = TRUE)
data <- data %>%
  mutate(loan_int_rate = ifelse(is.na(loan_int_rate), median_int_rate, loan_int_rate))

# Handling Outliers

income_cap <- quantile(data$person_income, 0.99)  # 99th percentile
data <- data %>%
  mutate(person_income = ifelse(person_income > income_cap, income_cap, person_income))

summary(data)

# distribution of loan amounts
ggplot(data, aes(x = loan_amnt)) +
  geom_histogram(bins = 30, fill = "red", color = "black") +
  labs(title = "Distribution of Loan Amounts", x = "Loan Amount", y = "Count")



# Pie chart for loan intent distribution
loan_intent_data <- data %>%
  count(loan_intent) %>%
  mutate(pct = n / sum(n) * 100)  # Calculate percentage

ggplot(loan_intent_data, aes(x = "", y = pct, fill = loan_intent)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Distribution of Loan Intent", x = NULL, y = NULL) +
  theme_void() + 
  theme(legend.title = element_blank())







