


library(ggplot2)
library(dplyr)
library(parallel)
library(doParallel)
library(randomForest)
library(fastDummies)
library(ggcorrplot)
library(gridExtra)
library(reshape2)






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





numeric_data <- train_data_new %>% select_if(is.numeric)
colnames(numeric_data)






# # 异常值 极端值
# 
# # 绘制每个变量的箱线图并保存为单独的 PNG 文件
# plot_outliers_save <- function(data) {
#   # 选择数值列
#   numeric_cols <- sapply(data, is.numeric)
#   data_numeric <- data[, numeric_cols]
#   
#   for (col_name in names(data_numeric)) {
#     # 动态设置文件名，将变量名包含在文件名中
#     png_filename <- paste0("boxplot_", col_name, ".png")
#     
#     # 打开PNG设备
#     png(filename = png_filename, width = 600, height = 400)
#     
#     # 生成箱线图
#     p <- ggplot(data, aes(y = .data[[col_name]])) +
#          geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
#          labs(title = paste("Boxplot of", col_name), y = col_name) +
#          theme_minimal()
#     
#     # 绘制并保存图像
#     print(p)
#     
#     # 关闭PNG设备
#     dev.off()
#   }
# }
# 
# # 调用函数，将每个变量的箱线图保存为 PNG 文件
# plot_outliers_save(train_data_new)





plot_outliers_save_combined <- function(data) {
  # 选择数值列
  # numeric_cols <- sapply(data, is.numeric)
  # data_numeric <- data[, numeric_cols]
  
  numeric_cols <- sapply(data, is.numeric)
  data_numeric <- data[, numeric_cols & names(data) != "loan_status"]
  
  # 创建一个空的列表来存储所有箱线图
  plot_list <- list()
  
  for (col_name in names(data_numeric)) {
    # 生成箱线图并添加到列表中
    p <- ggplot(data, aes(y = .data[[col_name]])) +
      geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
      labs(title = paste("Boxplot of", col_name), y = col_name) +
      theme_minimal()
    
    plot_list[[col_name]] <- p
  }
  
  # 将所有箱线图放在一张图中，并保存为一个 PNG 文件
  png("combined_boxplots_numeric.png", width = 1200, height = 800)
  grid.arrange(grobs = plot_list, ncol = 3)  # 可以调整 ncol 参数来控制每行的图数
  dev.off()
}

# 调用函数，将所有箱线图放在一张图中
plot_outliers_save_combined(train_data_new)







# 定义一个函数来查找某个列的异常值
find_outliers <- function(data, column_name) {
  # 计算Q1, Q3和IQR
  Q1 <- quantile(data[[column_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column_name]], 0.75, na.rm = TRUE)
  IQR_value <- IQR(data[[column_name]], na.rm = TRUE)
  
  # 定义异常值的上下界
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  print(lower_bound)
  print(upper_bound)
  
  # 筛选出异常值
  outliers <- data[data[[column_name]] < lower_bound | data[[column_name]] > upper_bound, ]
  
  return(outliers)
}

# 定义一个函数来查找某个列的极端值
find_extreme_values <- function(data, column_name) {
  # 计算Q1, Q3和IQR
  Q1 <- quantile(data[[column_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column_name]], 0.75, na.rm = TRUE)
  IQR_value <- IQR(data[[column_name]], na.rm = TRUE)
  
  # 定义极端值的上下界
  lower_bound_extreme <- Q1 - 3 * IQR_value
  upper_bound_extreme <- Q3 + 3 * IQR_value
  
  # 筛选出极端值
  extreme_values <- data[data[[column_name]] < lower_bound_extreme | data[[column_name]] > upper_bound_extreme, ]
  
  return(extreme_values)
}








# person_income

# 保留异常值和极端值

# 使用函数查找 `person_income` 列的异常值
outliers_person_income <-
  find_outliers(train_data_new, "person_income")

# 查看异常值
print(outliers_person_income)

# 使用函数查找 `person_income` 列的极端值
extreme_values_person_income <- find_extreme_values(train_data_new, "person_income")

# 查看极端值
print(extreme_values_person_income)

# 删除极端值 123年
# train_data_new <- train_data_new[train_data_new$person_emp_length <= 100, ]






# person_emp_length

# 保留异常值和极端值


# 使用函数查找 `person_income` 列的异常值
outliers_person_emp_length <-
  find_outliers(train_data_new, "person_emp_length")

# 查看异常值
print(outliers_person_emp_length)

# 使用函数查找 `person_emp_length` 列的极端值
extreme_values_person_emp_length <- find_extreme_values(train_data_new, "person_emp_length")

# 查看极端值
print(extreme_values_person_emp_length)

# 删除极端值 123年
# train_data_new <- train_data_new[train_data_new$person_emp_length <= 100, ]





# person_age

# 保留异常值和极端值


# 使用函数查找 `person_income` 列的异常值
outliers_person_age <-
  find_outliers(train_data_new, "person_age")

# 查看异常值
print(outliers_person_age)

# 使用函数查找 `person_age` 列的极端值
extreme_values_person_age <- find_extreme_values(train_data_new, "person_age")

# 查看极端值
print(extreme_values_person_age)

# 删除极端值 100岁 以上
# train_data_new <- train_data_new[train_data_new$person_age <= 100, ]






# loan_percent_income

# 保留异常值和极端值


# 使用函数查找 `person_income` 列的异常值
outliers_loan_percent_income <-
  find_outliers(train_data_new, "loan_percent_income")

# 查看异常值
print(outliers_loan_percent_income)

# 使用函数查找 `loan_percent_income` 列的极端值
extreme_values_loan_percent_income <- find_extreme_values(train_data_new, "loan_percent_income")

# 查看极端值
print(extreme_values_loan_percent_income)

# 删除极端值 100岁 以上
# train_data_new <- train_data_new[train_data_new$loan_percent_income <= 100, ]






# loan_int_rate

# 保留异常值和极端值

# 使用函数查找 `person_income` 列的异常值
outliers_loan_int_rate <-
  find_outliers(train_data_new, "loan_int_rate")

# 查看异常值
print(outliers_loan_int_rate)

# 使用函数查找 `loan_int_rate` 列的极端值
extreme_values_loan_int_rate <- find_extreme_values(train_data_new, "loan_int_rate")

# 查看极端值
print(extreme_values_loan_int_rate)

# 删除极端值 100岁 以上
# train_data_new <- train_data_new[train_data_new$loan_int_rate >= 20, ]







# loan_amnt

# 保留异常值和极端值

# 使用函数查找 `person_income` 列的异常值
outliers_loan_amnt <-
  find_outliers(train_data_new, "loan_amnt")

# 查看异常值
print(outliers_loan_amnt)

# 使用函数查找 `loan_amnt` 列的极端值
extreme_values_loan_amnt <- find_extreme_values(train_data_new, "loan_amnt")

# 查看极端值
print(extreme_values_loan_amnt)

# 删除极端值 100岁 以上
# train_data_new <- train_data_new[train_data_new$loan_amnt >= 20, ]





# cb_person_cred_hist_length

# 保留异常值和极端值

# 使用函数查找 `cb_person_cred_hist_length` 列的异常值
outliers_cb_person_cred_hist_length <-
  find_outliers(train_data_new, "cb_person_cred_hist_length")

# 查看异常值
print(outliers_cb_person_cred_hist_length)

# 使用函数查找 `cb_person_cred_hist_length` 列的极端值
extreme_values_cb_person_cred_hist_length <- find_extreme_values(train_data_new, "cb_person_cred_hist_length")

# 查看极端值
print(extreme_values_cb_person_cred_hist_length)

# 删除极端值 100岁 以上
# train_data_new <- train_data_new[train_data_new$cb_person_cred_hist_length >= 20, ]





non_numeric_data <- train_data_new %>% select_if(~ !is.numeric(.))
colnames(non_numeric_data)





# 使用 table() 查看每个类别的频数
table(train_data_new$person_home_ownership)

# # 或者使用 dplyr 包中的 count() 函数
# train_data_new %>%
#   count(person_home_ownership) %>%
#   arrange(desc(n)) # 按出现次数降序排列

# # 查找频数很低的类别
# rare_categories <- train_data_new %>%
#   count(person_home_ownership) %>%
#   filter(n < 200) # threshold可以根据具体情况设置，如5次以下
# 
# print(rare_categories)





# 使用 table() 查看每个类别的频数
table(train_data_new$loan_intent)

# # 或者使用 dplyr 包中的 count() 函数
# train_data_new %>%
#   count(loan_intent) %>%
#   arrange(desc(n)) # 按出现次数降序排列

# # 查找频数很低的类别
# rare_categories <- train_data_new %>%
#   count(loan_intent) %>%
#   filter(n < 200) # threshold可以根据具体情况设置，如5次以下
# 
# print(rare_categories)




# 使用 table() 查看每个类别的频数
table(train_data_new$loan_grade)
# 
# # 或者使用 dplyr 包中的 count() 函数
# train_data_new %>%
#   count(loan_grade) %>%
#   arrange(desc(n)) # 按出现次数降序排列

# # 查找频数很低的类别
# rare_categories <- train_data_new %>%
#   count(loan_grade) %>%
#   filter(n < 200) # threshold可以根据具体情况设置，如5次以下
# 
# print(rare_categories)





# 使用 table() 查看每个类别的频数
table(train_data_new$cb_person_default_on_file)

# # 或者使用 dplyr 包中的 count() 函数
# train_data_new %>%
#   count(cb_person_default_on_file) %>%
#   arrange(desc(n)) # 按出现次数降序排列

# # 查找频数很低的类别
# rare_categories <- train_data_new %>%
#   count(cb_person_default_on_file) %>%
#   filter(n < 200) # threshold可以根据具体情况设置，如5次以下
# 
# print(rare_categories)





# 分析 person_home_ownership 和 目标变量的关系

train_data_new$person_home_ownership <- as.factor(train_data_new$person_home_ownership)
train_data_new$loan_status <- as.factor(train_data_new$loan_status)

png("test.png", width = 600, height = 400)
plot1 = ggplot(train_data_new, aes(x = person_home_ownership, fill = loan_status)) +
  geom_bar(position = "fill") + 
  labs(x = "Home Ownership", y = "Proportion", 
       title = "Relationship between Home Ownership and Loan Status") +
  theme_minimal()
dev.off()






# 分析 loan_intent 和 目标变量的关系

train_data_new$loan_intent <- as.factor(train_data_new$loan_intent)
train_data_new$loan_status <- as.factor(train_data_new$loan_status)

# png("test.png", width = 600, height = 400)
plot2 = ggplot(train_data_new, aes(x = loan_intent, fill = loan_status)) +
  geom_bar(position = "fill") + 
  labs(x = "Loan Intent", y = "Proportion", 
       title = "Relationship between Loan Intent and Loan Status") +
  theme_minimal()
# dev.off()





# 分析 loan_grade 和 目标变量的关系

train_data_new$loan_grade <- as.factor(train_data_new$loan_grade)
train_data_new$loan_status <- as.factor(train_data_new$loan_status)

# png("test.png", width = 600, height = 400)
plot3 = ggplot(train_data_new, aes(x = loan_grade, fill = loan_status)) +
  geom_bar(position = "fill") + 
  labs(x = "Loan Grade", y = "Proportion", 
       title = "Relationship between Loan Grade and Loan Status") +
  theme_minimal()
# dev.off()




# 分析 cb_person_default_on_file 和 目标变量的关系

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


# 筛选出 loan_int_rate 缺失值所在的行
missing_loan_int_rate_rows <- train_data_new_clean[is.na(train_data_new_clean$loan_int_rate), ]

# 查看缺失值所在的行
print(missing_loan_int_rate_rows)


data_with_rate <- train_data_new_clean %>% filter(!is.na(loan_int_rate))
data_missing_rate <- train_data_new_clean %>% filter(is.na(loan_int_rate))

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

# nrow(train_data_new_clean)

data_types <- sapply(train_data_new_clean, class)

data_quality_report <- data.frame(
  Column = names(train_data_new_clean),
  DataType = data_types,
  MissingValues = missing_values,
  MissingRates = missing_rate
)

print(data_quality_report)

##
# person_emp_length  2.7%
# loan_int_rate 9.5%










# 创建直方图
histogram_plot <- ggplot(train_data_new_clean, aes(x = person_emp_length)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Employment Length", x = "Employment Length (years)", y = "Frequency") +
  theme_minimal()

# 创建 QQ 图
qq_plot <- ggplot(train_data_new_clean, aes(sample = person_emp_length)) +
  stat_qq() +
  stat_qq_line(color = "red", lwd = 1.2) +
  labs(title = "QQ Plot of Employment Length") +
  theme_minimal()

# 将两个图表组合到一个图中
png("combined_plot_person_emp_length.png", width = 800, height = 400)
grid.arrange(histogram_plot, qq_plot, ncol = 2)
dev.off()


# 加上常数 1 避免对数函数在零值上出错
train_data_new_clean$log_person_emp_length <- log(train_data_new_clean$person_emp_length + 1)

# 创建 Log-transformed Employment Length 的直方图
histogram_log_plot <- ggplot(train_data_new_clean, aes(x = log_person_emp_length)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histogram of Log-transformed Employment Length", x = "Log(Employment Length + 1)", y = "Frequency") +
  theme_minimal()

# 创建 QQ 图
qq_log_plot <- ggplot(train_data_new_clean, aes(sample = log_person_emp_length)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "QQ Plot of Log-transformed Employment Length") +
  theme_minimal()

# 将两个图表组合到一个图中
png("combined_plots_log_person_emp_length.png", width = 800, height = 400)
grid.arrange(histogram_log_plot, qq_log_plot, ncol = 2)
dev.off()







# 创建直方图
histogram_plot <- ggplot(train_data_new_clean, aes(x = loan_int_rate)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Loan Interest Rate", x = "Interest Rate (%)", y = "Frequency") +
  theme_minimal()

# 创建 QQ 图
qq_plot <- ggplot(train_data_new_clean, aes(sample = loan_int_rate)) +
  stat_qq() +
  stat_qq_line(color = "red", lwd = 1.2) +
  labs(title = "QQ Plot of Loan Interest Rate") +
  theme_minimal()

# 将两个图表组合到一个图中
png("combined_interest_rate_plots.png", width = 800, height = 400)
grid.arrange(histogram_plot, qq_plot, ncol = 2)
dev.off()



# 加上常数 1 避免对数函数在零值上出错
train_data_new_clean$log_loan_int_rate <- log(train_data_new_clean$loan_int_rate + 1)

# 查看转换后的变量分布
# 绘制直方图
# 创建 Log-transformed Loan Interest Rate 的直方图
histogram_log_plot <- ggplot(train_data_new_clean, aes(x = log_loan_int_rate)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histogram of Log-transformed Loan Interest Rate", x = "Log(Loan Interest Rate + 1)", y = "Frequency") +
  theme_minimal()

# 创建 QQ 图
qq_log_plot <- ggplot(train_data_new_clean, aes(sample = log_loan_int_rate)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "QQ Plot of Log-transformed Loan Interest Rate") +
  theme_minimal()

# 将两个图表组合到一个图中
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


# 设置阈值，仅保留相关性大于0.5或小于-0.5的值
cor_matrix_filtered <- cor_matrix
cor_matrix_filtered[abs(cor_matrix) < 0.5] <- NA  # 将较低的相关性值替换为 NA，不显示

png("cor_matrix_filtered.png", width = 1200, height = 1200)
ggcorrplot(cor_matrix_filtered, 
           method = "circle", 
           type = "lower", 
           lab = TRUE, 
           lab_size = 7,  # 增大标签字体
           title = "Filtered Correlation Matrix (|correlation| > 0.5)", 
           colors = c("red", "white", "blue")) +
  ggtitle("Filtered Correlation Matrix (|correlation| > 0.5)") +
  theme(plot.title = element_text(size = 20),     # 调整标题字体大小
        axis.text = element_text(size = 15),      # 调整x、y轴标签字体大小
        axis.title = element_text(size = 18))     # 调整x、y轴标题字体大小
dev.off()





# 示例数据
set.seed(0)
# data <- data.frame(
#   loan_amnt = rnorm(100, mean = 10000, sd = 2000),
#   loan_int_rate = rnorm(100, mean = 5, sd = 1) + 0.003 * rnorm(100, mean = 10000, sd = 2000)
# )

png("loan_amnt_rate_plot.png", width = 1200, height = 1200)
# 绘制散点图并添加回归线
ggplot(data_encoded, aes(x = loan_amnt, y = loan_int_rate)) +
  geom_point(color = "orange") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Scatter Plot of Loan Amount vs Loan Interest Rate with Regression Line",
       x = "Loan Amount", y = "Loan Interest Rate") +
  theme_minimal(base_size = 15)
dev.off()






# 示例数据
# data_encoded$person_home_ownership_MORTGAGE <- sample(0:1, 100, replace = TRUE)
# data_encoded$cb_person_default_on_file_N <- sample(0:1, 100, replace = TRUE)
# data_encoded$cb_person_default_on_file_Y <- 1 - data$cb_person_default_on_file_N  # 互补的二分类变量

# 计算相关矩阵
cor_matrix <- cor(data_encoded)

# 过滤相关性矩阵，仅保留 |correlation| > 0.5 的值
cor_matrix_filtered <- ifelse(abs(cor_matrix) > 0.5, cor_matrix, 0)




# 将矩阵转换为长格式以便绘图
melted_cor_matrix <- melt(cor_matrix_filtered)
colnames(melted_cor_matrix) <- c("Var1", "Var2", "value")

png("scale_fill_gradient2.png", width = 1200, height = 1200)
# 绘制热力图
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       limit = c(-1, 1), name = "Correlation") +
  theme_minimal(base_size = 15) +
  labs(title = "Heatmap of Filtered Correlation Matrix (|correlation| > 0.5)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()






# 绘制密度图
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











