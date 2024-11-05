

library(ggplot2)
library(dplyr)
library(parallel)
library(doParallel)
library(randomForest)
library(fastDummies)
library(ggcorrplot)





# getwd()
# setwd("C:/Users/JHM/Documents/Feng-HW1/HW7")

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

# nrow(train_data_new)

data_types <- sapply(train_data_new, class)

data_quality_report <- data.frame(
  Column = names(train_data_new),
  DataType = data_types,
  MissingValues = missing_values,
  MissingRates = missing_rate
)

print(data_quality_report)

##
# person_emp_length  2.7%
# loan_int_rate 9.5%




png("test1.png", width = 600, height = 400)
ggplot(train_data_new, aes(x = person_emp_length)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Employment Length", x = "Employment Length (years)", y = "Frequency") +
  theme_minimal()
dev.off()

png("test2.png", width = 600, height = 400)
qqnorm(train_data_new$person_emp_length, main = "QQ Plot of Employment Length")
qqline(train_data_new$person_emp_length, col = "red", lwd = 2)
dev.off()

data_with_emp_length <- train_data_new %>% filter(!is.na(person_emp_length))
data_missing_emp_length <- train_data_new %>% filter(is.na(person_emp_length))

X <- data_with_emp_length %>% select(person_age, person_income, cb_person_cred_hist_length)
y <- data_with_emp_length$person_emp_length

set.seed(42)  # 设置随机种子以确保结果可复现

cl <- makeCluster(detectCores() - 1)  
registerDoParallel(cl)

rf_model <- randomForest(X, y, ntree = 100)

stopCluster(cl)
registerDoSEQ()

X_missing <- data_missing_emp_length %>% select(person_age, person_income, cb_person_cred_hist_length)

# 预测缺失的person_emp_length
predicted_emp_length <- predict(rf_model, X_missing)

# 将预测值填入缺失数据集
data_missing_emp_length$person_emp_length <- predicted_emp_length

# 合并有值和填充后的数据集
final_data <- bind_rows(data_with_emp_length, data_missing_emp_length)

# 查看合并后的数据集
summary(final_data)

train_data_new <- final_data






png("test3.png", width = 600, height = 400)
ggplot(train_data_new, aes(x = loan_int_rate)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Loan Interest Rate", x = "Interest Rate (%)", y = "Frequency") +
  theme_minimal()
dev.off()

png("test4.png", width = 600, height = 400)
qqnorm(train_data_new$loan_int_rate, main = "QQ Plot of Loan Interest Rate")
qqline(train_data_new$loan_int_rate, col = "red", lwd = 2)
dev.off()


data_with_rate <- train_data_new %>% filter(!is.na(loan_int_rate))
data_missing_rate <- train_data_new %>% filter(is.na(loan_int_rate))

train_data_temp <- data_with_rate %>%
  select(person_income, loan_amnt, loan_grade, cb_person_cred_hist_length, loan_int_rate)

# 去除含有缺失值的行（确保训练数据完整）
train_data_temp <- na.omit(train_data_temp)


train_data_temp$loan_grade <- as.factor(train_data_temp$loan_grade)
data_missing_rate$loan_grade <- as.factor(data_missing_rate$loan_grade)

lm_model <- 
  lm(loan_int_rate ~ person_income + loan_amnt + loan_grade + cb_person_cred_hist_length, 
     data = train_data_temp)
summary(lm_model)

predict_data <- data_missing_rate %>%
  select(person_income, loan_amnt, loan_grade, cb_person_cred_hist_length)

# 使用模型预测缺失的 loan_int_rate
predicted_rates <- predict(lm_model, newdata = predict_data)

# 将预测值填充到缺失的数据中
data_missing_rate$loan_int_rate <- predicted_rates

final_data <- bind_rows(data_with_rate, data_missing_rate)

# 查看合并后的数据集
summary(final_data)

train_data_new <- final_data






data_encoded <- 
  dummy_cols(train_data_new, 
             select_columns = c("person_home_ownership", 
                                "loan_intent", "loan_grade", 
                                "cb_person_default_on_file"),
             remove_first_dummy = FALSE, # FALSE
             remove_selected_columns = TRUE)

cor_matrix <- cor(data_encoded, use = "complete.obs")

png("test5.png", width = 600, height = 400)
ggcorrplot(cor_matrix, method = "circle", type = "lower", 
           lab = TRUE, lab_size = 3, 
           title = "Correlation Matrix (After Encoding)", 
           colors = c("red", "white", "blue"))
dev.off()

colnames(cor_matrix)
# [1] "person_age"                     "person_income"                  "person_emp_length"             
# [4] "loan_amnt"                      "loan_int_rate"                  "loan_status"                   
# [7] "loan_percent_income"            "cb_person_cred_hist_length"     "person_home_ownership_MORTGAGE"
# [10] "person_home_ownership_OTHER"    "person_home_ownership_OWN"      "person_home_ownership_RENT"    
# [13] "loan_intent_DEBTCONSOLIDATION"  "loan_intent_EDUCATION"          "loan_intent_HOMEIMPROVEMENT"   
# [16] "loan_intent_MEDICAL"            "loan_intent_PERSONAL"           "loan_intent_VENTURE"           
# [19] "loan_grade_A"                   "loan_grade_B"                   "loan_grade_C"                  
# [22] "loan_grade_D"                   "loan_grade_E"                   "loan_grade_F"                  
# [25] "loan_grade_G"                   "cb_person_default_on_file_N"    "cb_person_default_on_file_Y" 

loan_status_corr <- cor_matrix["loan_status", ]
high_corr_vars <- sort(loan_status_corr[abs(loan_status_corr) > 0.2], decreasing = TRUE)
print(high_corr_vars)
#  loan_status        loan_percent_income              loan_int_rate 
#    1.0000000                  0.3793665                  0.3337667 
# loan_grade_D person_home_ownership_RENT               loan_grade_A 
#    0.3189985                  0.2384300                 -0.2018908 

person_age_corr <- cor_matrix["person_age", ]
person_age_vars <- sort(person_age_corr[abs(person_age_corr) > 0.2], decreasing = TRUE)
print(person_age_vars)
# person_age cb_person_cred_hist_length 
#  1.0000000                  0.8591332

person_income_corr <- cor_matrix["person_income", ]
person_income_vars <- sort(person_income_corr[abs(person_income_corr) > 0.2], decreasing = TRUE)
print(person_income_vars)
#       person_income                      loan_amnt person_home_ownership_MORTGAGE 
#            1.000000                       0.266820                       0.203546 
# loan_percent_income 
#           -0.254471 

person_emp_length_corr <- cor_matrix["person_emp_length", ]
person_emp_length_vars <- sort(person_emp_length_corr[abs(person_emp_length_corr) > 0.2], decreasing = TRUE)
print(person_emp_length_vars)
# person_emp_length person_home_ownership_MORTGAGE     person_home_ownership_RENT 
#         1.0000000                      0.2209623                     -0.2282401 

loan_amnt_corr <- cor_matrix["loan_amnt", ]
loan_amnt_vars <- sort(loan_amnt_corr[abs(loan_amnt_corr) > 0.2], decreasing = TRUE)
print(loan_amnt_vars)
# loan_amnt loan_percent_income       person_income 
# 1.0000000           0.5726115           0.2668200 

loan_int_rate_corr <- cor_matrix["loan_int_rate", ]
loan_int_rate_vars <- sort(loan_int_rate_corr[abs(loan_int_rate_corr) > 0.2], decreasing = TRUE)
print(loan_int_rate_vars)
# loan_int_rate cb_person_default_on_file_Y                loan_grade_D 
#     1.0000000                   0.5035216                   0.4783609 
#  loan_grade_C                 loan_status                loan_grade_E 
#     0.3789681                   0.3337667                   0.3254305 
#  loan_grade_F cb_person_default_on_file_N                loan_grade_A 
#     0.2037136                  -0.5035216                  -0.8041353 

loan_percent_income_corr <- cor_matrix["loan_percent_income", ]
loan_percent_income_vars <- sort(loan_percent_income_corr[abs(loan_percent_income_corr) > 0.2], decreasing = TRUE)
print(loan_percent_income_vars)
# loan_percent_income           loan_amnt         loan_status       person_income 
#           1.0000000           0.5726115           0.3793665          -0.2544710 

cb_person_cred_hist_length_corr <- cor_matrix["cb_person_cred_hist_length", ]
cb_person_cred_hist_length_vars <- sort(cb_person_cred_hist_length_corr[abs(cb_person_cred_hist_length_corr) > 0.2], decreasing = TRUE)
print(cb_person_cred_hist_length_vars)
# cb_person_cred_hist_length                 person_age 
#                  1.0000000                  0.8591332 

person_home_ownership_MORTGAGE_corr <- cor_matrix["person_home_ownership_MORTGAGE", ]
person_home_ownership_MORTGAGE_vars <- sort(person_home_ownership_MORTGAGE_corr[abs(person_home_ownership_MORTGAGE_corr) > 0.2], decreasing = TRUE)
print(person_home_ownership_MORTGAGE_vars)
# person_home_ownership_MORTGAGE              person_emp_length                  person_income 
#                      1.0000000                      0.2209623                      0.2035460 
#      person_home_ownership_OWN     person_home_ownership_RENT 
#                     -0.2459998                     -0.8461999 

person_home_ownership_OTHER_corr <- cor_matrix["person_home_ownership_OTHER", ]
person_home_ownership_OTHER_vars <- sort(person_home_ownership_OTHER_corr[abs(person_home_ownership_OTHER_corr) > 0.2], decreasing = TRUE)
print(person_home_ownership_OTHER_vars)

person_home_ownership_OWN_corr <- cor_matrix["person_home_ownership_OWN", ]
person_home_ownership_OWN_vars <- sort(person_home_ownership_OWN_corr[abs(person_home_ownership_OWN_corr) > 0.2], decreasing = TRUE)
print(person_home_ownership_OWN_vars)
# person_home_ownership_OWN person_home_ownership_MORTGAGE     person_home_ownership_RENT 
#                 1.0000000                     -0.2459998                     -0.2963146 

person_home_ownership_RENT_corr <- cor_matrix["person_home_ownership_RENT", ]
person_home_ownership_RENT_vars <- sort(person_home_ownership_RENT_corr[abs(person_home_ownership_RENT_corr) > 0.2], decreasing = TRUE)
print(person_home_ownership_RENT_vars)
# person_home_ownership_RENT                    loan_status              person_emp_length 
#                  1.0000000                      0.2384300                     -0.2282401 
#  person_home_ownership_OWN person_home_ownership_MORTGAGE 
#                 -0.2963146                     -0.8461999 

loan_intent_DEBTCONSOLIDATION_corr <- cor_matrix["loan_intent_DEBTCONSOLIDATION", ]
loan_intent_DEBTCONSOLIDATION_vars <- sort(loan_intent_DEBTCONSOLIDATION_corr[abs(loan_intent_DEBTCONSOLIDATION_corr) > 0.2], decreasing = TRUE)
print(loan_intent_DEBTCONSOLIDATION_vars)
# loan_intent_DEBTCONSOLIDATION           loan_intent_VENTURE           loan_intent_MEDICAL 
#                     1.0000000                    -0.2013556                    -0.2088325 
#         loan_intent_EDUCATION 
#                    -0.2168705 

loan_intent_EDUCATION_corr <- cor_matrix["loan_intent_EDUCATION", ]
loan_intent_EDUCATION_vars <- sort(loan_intent_EDUCATION_corr[abs(loan_intent_EDUCATION_corr) > 0.2], decreasing = TRUE)
print(loan_intent_EDUCATION_vars)
# loan_intent_EDUCATION loan_intent_DEBTCONSOLIDATION          loan_intent_PERSONAL 
#             1.0000000                    -0.2168705                    -0.2244775 
#   loan_intent_VENTURE           loan_intent_MEDICAL 
#            -0.2293077                    -0.2378225 

loan_intent_HOMEIMPROVEMENT_corr <- cor_matrix["loan_intent_HOMEIMPROVEMENT", ]
loan_intent_HOMEIMPROVEMENT_vars <- sort(loan_intent_HOMEIMPROVEMENT_corr[abs(loan_intent_HOMEIMPROVEMENT_corr) > 0.2], decreasing = TRUE)
print(loan_intent_HOMEIMPROVEMENT_vars)

loan_intent_MEDICAL_corr <- cor_matrix["loan_intent_MEDICAL", ]
loan_intent_MEDICAL_vars <- sort(loan_intent_MEDICAL_corr[abs(loan_intent_MEDICAL_corr) > 0.2], decreasing = TRUE)
print(loan_intent_MEDICAL_vars)
# loan_intent_MEDICAL loan_intent_DEBTCONSOLIDATION          loan_intent_PERSONAL 
#           1.0000000                    -0.2088325                    -0.2161575 
# loan_intent_VENTURE         loan_intent_EDUCATION 
#          -0.2208087                    -0.2378225 

loan_intent_PERSONAL_corr <- cor_matrix["loan_intent_PERSONAL", ]
loan_intent_PERSONAL_vars <- sort(loan_intent_PERSONAL_corr[abs(loan_intent_PERSONAL_corr) > 0.2], decreasing = TRUE)
print(loan_intent_PERSONAL_vars)
# loan_intent_PERSONAL   loan_intent_VENTURE   loan_intent_MEDICAL loan_intent_EDUCATION 
#            1.0000000            -0.2084183            -0.2161575            -0.2244775 

loan_intent_VENTURE_corr <- cor_matrix["loan_intent_VENTURE", ]
loan_intent_VENTURE_vars <- sort(loan_intent_VENTURE_corr[abs(loan_intent_VENTURE_corr) > 0.2], decreasing = TRUE)
print(loan_intent_VENTURE_vars)
# loan_intent_VENTURE loan_intent_DEBTCONSOLIDATION          loan_intent_PERSONAL 
#           1.0000000                    -0.2013556                    -0.2084183 
# loan_intent_MEDICAL         loan_intent_EDUCATION 
#          -0.2208087                    -0.2293077 

loan_grade_A_corr <- cor_matrix["loan_grade_A", ]
loan_grade_A_vars <- sort(loan_grade_A_corr[abs(loan_grade_A_corr) > 0.2], decreasing = TRUE)
print(loan_grade_A_vars)
# loan_grade_A cb_person_default_on_file_N                 loan_status 
#    1.0000000                   0.3252872                  -0.2018908 
# loan_grade_D cb_person_default_on_file_Y                loan_grade_C 
#   -0.2487900                  -0.3252872                  -0.3495573 
# loan_grade_B               loan_int_rate 
#   -0.4831356                  -0.8041353 

loan_grade_B_corr <- cor_matrix["loan_grade_B", ]
loan_grade_B_vars <- sort(loan_grade_B_corr[abs(loan_grade_B_corr) > 0.2], decreasing = TRUE)
print(loan_grade_B_vars)
#                loan_grade_B cb_person_default_on_file_N                loan_grade_D 
#                   1.0000000                   0.3179614                  -0.2431870 
# cb_person_default_on_file_Y                loan_grade_C                loan_grade_A 
#                  -0.3179614                  -0.3416848                  -0.4831356 

loan_grade_C_corr <- cor_matrix["loan_grade_C", ]
loan_grade_C_vars <- sort(loan_grade_C_corr[abs(loan_grade_C_corr) > 0.2], decreasing = TRUE)
print(loan_grade_C_vars)
# loan_grade_C cb_person_default_on_file_Y               loan_int_rate 
#    1.0000000                   0.4277349                   0.3789681 
# loan_grade_B                loan_grade_A cb_person_default_on_file_N 
#   -0.3416848                  -0.3495573                  -0.4277349 

loan_grade_D_corr <- cor_matrix["loan_grade_D", ]
loan_grade_D_vars <- sort(loan_grade_D_corr[abs(loan_grade_D_corr) > 0.2], decreasing = TRUE)
print(loan_grade_D_vars)
#                loan_grade_D               loan_int_rate                 loan_status 
#                   1.0000000                   0.4783609                   0.3189985 
# cb_person_default_on_file_Y                loan_grade_B                loan_grade_A 
#                   0.3166824                  -0.2431870                  -0.2487900 
# cb_person_default_on_file_N 
#                  -0.3166824 

loan_grade_E_corr <- cor_matrix["loan_grade_E", ]
loan_grade_E_vars <- sort(loan_grade_E_corr[abs(loan_grade_E_corr) > 0.2], decreasing = TRUE)
print(loan_grade_E_vars)
# loan_grade_E loan_int_rate 
#    1.0000000     0.3254305 

loan_grade_F_corr <- cor_matrix["loan_grade_F", ]
loan_grade_F_vars <- sort(loan_grade_F_corr[abs(loan_grade_F_corr) > 0.2], decreasing = TRUE)
print(loan_grade_F_vars)
# loan_grade_F loan_int_rate 
#    1.0000000     0.2037136 

loan_grade_G_corr <- cor_matrix["loan_grade_G", ]
loan_grade_G_vars <- sort(loan_grade_G_corr[abs(loan_grade_G_corr) > 0.2], decreasing = TRUE)
print(loan_grade_G_vars)

cb_person_default_on_file_N_corr <- cor_matrix["cb_person_default_on_file_N", ]
cb_person_default_on_file_N_vars <- sort(cb_person_default_on_file_N_corr[abs(cb_person_default_on_file_N_corr) > 0.2], decreasing = TRUE)
print(cb_person_default_on_file_N_vars)
# cb_person_default_on_file_N                loan_grade_A                loan_grade_B 
#                   1.0000000                   0.3252872                   0.3179614 
#                loan_grade_D                loan_grade_C               loan_int_rate 
#                  -0.3166824                  -0.4277349                  -0.5035216 
# cb_person_default_on_file_Y 
#                  -1.0000000 

cb_person_default_on_file_Y_corr <- cor_matrix["cb_person_default_on_file_Y", ]
cb_person_default_on_file_Y_vars <- sort(cb_person_default_on_file_Y_corr[abs(cb_person_default_on_file_Y_corr) > 0.2], decreasing = TRUE)
print(cb_person_default_on_file_Y_vars)
# cb_person_default_on_file_Y               loan_int_rate                loan_grade_C 
#                   1.0000000                   0.5035216                   0.4277349 
#                loan_grade_D                loan_grade_B                loan_grade_A 
#                   0.3166824                  -0.3179614                  -0.3252872 
# cb_person_default_on_file_N 
#                  -1.0000000 





# person_age  cb_person_cred_hist_length

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
# 根据贷款金额创建分组
data_encoded_temp$loan_amnt_group <- cut(data_encoded$loan_amnt, 
                                         breaks = c(-Inf, 10000, 20000, Inf), 
                                         labels = c("Low", "Medium", "High"))

# 绘制分面箱线图
ggplot(data_encoded_temp, aes(x = factor(loan_grade_D), y = loan_int_rate, fill = factor(cb_person_default_on_file_Y))) +
  geom_boxplot(width = 0.6, position = position_dodge(0.8), alpha = 0.7) +  # 仅使用箱线图
  facet_wrap(~ loan_amnt_group, scales = "free_y") +  # 按贷款金额分组，分面展示
  labs(title = "Faceted Box Plot of Loan Interest Rate by Loan Grade D, Default Status, and Loan Amount Group",
       x = "Loan Grade D (0 = Non-D Grade, 1 = D Grade)",
       y = "Loan Interest Rate",
       fill = "Default on File (1 = Yes, 0 = No)") +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "pink"), 
                    labels = c("No Default", "Default")) +
  theme_minimal()
dev.off()







png("test16.png", width = 600, height = 400)
# 绘制分面箱线图
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











