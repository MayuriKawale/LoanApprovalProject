# load the necessary packages
library(base)
library(datasets)
library(graphics)
library(grDevices)
library(methods)
library(stats)
library(utils)

library(ggplot2)     # for ggplot
library(gridExtra)   # for grid.arrange
library(ggcorrplot)  # for ggcorplot
library(reshape2)    # for melted correlation matrix
library(tidyverse)
library(dplyr)
library(forcats)     # for fct_lump
library(VIM)         # for kNN
library(fastDummies) # for dummy_cols

############################## Dataset ############################################

## load the dataset
creditRisk <- read.csv("dataset/credit_risk_dataset.csv")

creditRisk <- creditRisk %>%
  dplyr::mutate(across(where(is.character), as.factor))

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

#---------------------------------------------------------------------------------#

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

#---------------------------------------------------------------------------------#

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

#---------------------------------------------------------------------------------#

## Bar plot for loan approval vs loan intent

# Loan Approval Rate by Loan Intent
loan_intent_approval <- creditRisk %>%
  group_by(loan_intent) %>%
  summarize(approval_rate = mean(loan_status == 1, na.rm = TRUE)) %>%
  arrange(desc(approval_rate))

# Print the calculated approval rates
print(loan_intent_approval)

#plot
png("images/LoanApproval_Intent.png", width = 700, height = 500)
ggplot(loan_intent_approval, aes(x = loan_intent, y = approval_rate, fill = loan_intent)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(approval_rate, accuracy = 0.1)), 
            vjust = -0.5, size = 3) +
  labs(title = "Loan Approval Rate by Loan Intent", 
       x = "Purpose of loan", 
       y = "Loan Approval Rate") +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))
dev.off()

#---------------------------------------------------------------------------------#

# homeownership vs approval rate
plot1 <- ggplot(creditRisk, aes(x = person_home_ownership, fill = as.factor(loan_status))) +
  geom_bar(position = "fill") + 
  labs(x = "Home Ownership type", y = "Proportion of loan approval", 
       title = "Relationship between Home Ownership and Loan Status", 
       fill = "Loan status") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 10),  # Adjust legend title size
        legend.text = element_text(size = 8),    # Adjust legend item text size
        legend.key.size = unit(0.5, "cm"))       # Reduce the size of legend keys


#loan intent vs approval rate
plot2 <- ggplot(creditRisk, aes(x = loan_intent, fill = as.factor(loan_status))) +
  geom_bar(position = "fill") + 
  labs(x = "Purpose of loan", y = "Proportion", 
       title = "Relationship between Loan Intent and Loan Status",
       fill = "Loan status") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 10, size=6, vjust = 0.6, hjust = 0.5),   # angle rotates the tick labels; vjust and hjust control the vertical and horizontal justification
        legend.title = element_text(size = 10),                           # Adjust legend title size
        legend.text = element_text(size = 8),                             # Adjust legend item text size
        legend.key.size = unit(0.5, "cm"))                                # Reduce the size of legend keys
  

#loan grade vs approval rate
plot3 <- ggplot(creditRisk, aes(x = loan_grade, fill = as.factor(loan_status))) +
  geom_bar(position = "fill") + 
  labs(x = "Loan Grade", y = "Proportion", 
       title = "Relationship between Loan Grade and Loan Status",
       fill="Loan status") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 10),                           # Adjust legend title size
        legend.text = element_text(size = 8),                             # Adjust legend item text size
        legend.key.size = unit(0.5, "cm"))

#default history vs approval rate

plot4 <- ggplot(creditRisk, aes(x = cb_person_default_on_file, fill = as.factor(loan_status))) +
  geom_bar(position = "fill") + 
  labs(x = "Default History", y = "Proportion", 
       title = "Relationship between Default history and Loan Status",
       fill="Loan status") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 10),                           # Adjust legend title size
        legend.text = element_text(size = 8),                             # Adjust legend item text size
        legend.key.size = unit(0.5, "cm"))

png("images/loanApprovalRelations.png", width = 800, height = 800)
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)
dev.off()

#---------------------------------------------------------------------------------#

# Heatmap/ correlation map
corr_data <- dummy_cols(creditRisk, 
             select_columns = c("person_home_ownership", 
                                "loan_intent", "loan_grade", 
                                "cb_person_default_on_file"),
             remove_first_dummy = FALSE, 
             remove_selected_columns = TRUE)


corr_data[] <- lapply(corr_data, function(x) as.numeric(as.character(x)))
cor_matrix <- cor(corr_data, use = "complete.obs")

# Filter correlation matrix, retain only values where |correlation| > 0.5
cor_matrix_filtered <- ifelse(abs(cor_matrix) > 0.5, cor_matrix, 0)

# Convert matrix to long format for plotting
melted_cor_matrix <- melt(cor_matrix_filtered)

png("images/corrMatrix.png", width = 1000, height = 800)
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "black") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       limit = c(-1, 1), name = "Correlation") +
  theme_minimal(base_size = 15) +
  labs(title = "Heatmap of Filtered Correlation Matrix (|correlation| > 0.5)",
       x = "", y="") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

  
######################## Handling Missing Values #########################################

## Only two variables have missing values: `person_emp_length` and `loan_int_rate`

#creditRisk$person_emp_length[is.na(creditRisk$person_emp_length)] <- median(creditRisk$person_emp_length, na.rm=TRUE)
#creditRisk$loan_int_rate[is.na(creditRisk$loan_int_rate)] <- median(creditRisk$loan_int_rate, na.rm=TRUE)

# perform kNN imputation kNN using k=5 i.e 5 neighbors
creditRisk <- kNN(creditRisk, variable = c("person_emp_length", "loan_int_rate"), k=5, 
                  imp_var = FALSE   # no additional indicator column is created
                  )



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
