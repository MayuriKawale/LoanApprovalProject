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
library(forcats)     # for fct_lump

############################## Dataset ############################################

## load the dataset
creditRisk <- read.csv("dataset/credit_risk_dataset.csv")

##################### Exploratory Data Analysis/Visualizations ###################

# Boxplot of loan approval status using % income allocated towards loan repayment vs home ownership type

png("images/percentIncome_homeOwnership.png", width = 700, height = 500)
ggplot(data = creditRisk, aes(x=person_home_ownership, y=loan_percent_income, color = loan_status > 0))+
  geom_boxplot()+
  labs(title = "Boxplot for loan approval status", 
       x="Home ownership type", y="% income allocated towards loan repayment",
       color = "Loan Approved") +
  theme(panel.background = element_rect(color="black", fill="white"),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 10),  # Adjust legend title size
        legend.text = element_text(size = 8),    # Adjust legend item text size
        legend.key.size = unit(0.5, "cm"))       # Reduce the size of legend keys)
dev.off() 


#Histogram for checking skewness: person_income
png("images/annualIncome.png", width = 700, height = 500)
ggplot(data=creditRisk, aes(x=person_income))+
  geom_histogram(fill="blue", color="black", alpha=0.5)+
  labs(title="Original Data",
       x="Annual income of applicant in USD",
       y="Count")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()  


# Pie chart for loan intent distribution 
loan_intent_data <- creditRisk %>%
  count(loan_intent) %>%          # counts unique values of the variable
  mutate(pct = n / sum(n) * 100)  # Calculate percentage

png("images/loanIntent.png", width = 700, height = 500)
ggplot(loan_intent_data, aes(x = "", y = pct, fill = loan_intent)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  labs(title = "Distribution of Loan Intent", x = NULL, y = NULL, 
       fill = "Purpose of the loan") +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 0),
        legend.title = element_text())
dev.off()






######################## Handling Missing Values #########################################

## Only two variables have missing values: `person_emp_length` and `loan_int_rate`

creditRisk$person_emp_length[is.na(creditRisk$person_emp_length)] <- median(creditRisk$person_emp_length, na.rm=TRUE)
creditRisk$loan_int_rate[is.na(creditRisk$loan_int_rate)] <- median(creditRisk$loan_int_rate, na.rm=TRUE)

#use predictive mean matching or kNN for handling missing values


######################### Outliers ###############################################

# histogram of employment length -- The distribution is right skewed
ggplot(data=creditRisk, aes(x=person_emp_length))+
  geom_histogram(fill="blue", color="black", alpha=0.5)

# Removing outliers -- clearly employment length cannot be greater than a person's age
is_outlier <- creditRisk$person_emp_length > creditRisk$person_age   # logical vector to check for events whose emp_length > person_age
print(sum(is_outlier))

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

# remove outliers
creditRisk <- creditRisk[!is_outlier, ]   # removing rows with a outlier

######################## Skewness ###########################################
# The histogram for annual income is right skewed, so we use log transformation to 
# make a normal distribution and remove any skewness

png("images/LogAnnualIncome.png", width = 700, height = 500)
ggplot(data=creditRisk, aes(x=log(creditRisk$person_income)))+
  geom_histogram(fill="blue", color="black", alpha=0.5)+
  labs(title="Log transformation of data",
       x="Log(Annual income of an applicant in USD)",
       y="Count")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()


######################### Feature transformation ################################

#log transformation of annual income
creditRisk$person_income <- log(creditRisk$person_income)


####################### Dummy Variables #########################################

# Factors with various levels increases dimensions through dummy variables 
# Reduce the levels using fct_lump

creditRisk$loan_intent <- fct_lump(creditRisk$loan_intent, n=3) # choosing top 4 i.e. (n+1) levels
creditRisk$loan_grade <- fct_lump(creditRisk$loan_grade, n=3)   # choosing top 4 levels
