# 导入数据
library(readr)
install.packages("readr")
library(dplyr)
library(tidyr)
install.packages("tidyr")
library(ggplot2)



getwd()
setwd("C:/Users/LENOVO/Desktop/93大作业")



# 加载数据
consumption_data <- read.csv("electric power consumption per capita.csv", skip = 4)
population_data <- read.csv("total population.csv", skip = 4)
GDP_data <- read_csv("GDP.csv", skip = 4)
View(GDP_data)

# 筛选印尼的数据
indonesia_electric <- consumption_data %>%
  filter(Country.Name == "Indonesia") %>%
  select(-Country.Code, -Indicator.Name, -Indicator.Code)

indonesia_population <- population_data %>%
  filter(Country.Name == "Indonesia") %>%
  select(-Country.Code, -Indicator.Name, -Indicator.Code)

# 替换列名中的空格为下划线
colnames(GDP_data) <- gsub(" ", ".", colnames(GDP_data))

# 替换后使用新的列名
GDP_data <- GDP_data %>%
  filter(Country.Name == "Indonesia")

indonesia_GDP <- GDP_data %>%
  filter(Country.Name == "Indonesia") %>%
  select(-Country.Code, -Indicator.Name, -Indicator.Code)

# 转置数据框并整理年份列
indonesia_electric <- indonesia_electric %>%
  pivot_longer(cols = -Country.Name, names_to = "Year", values_to = "Electricity_Consumption")

View(indonesia_electric)

indonesia_population <- indonesia_population %>%
  pivot_longer(cols = -Country.Name, names_to = "Year", values_to = "Population")

View(indonesia_population)

indonesia_GDP <- indonesia_GDP %>%
  pivot_longer(cols = -Country.Name, names_to = "Year", values_to = "GDP")

View(indonesia_GDP)

# 合并两个数据框
indonesia_data <- indonesia_electric %>%
  inner_join(indonesia_population, by = c("Country.Name", "Year"))
View(indonesia_data)

# 移除年份前的 "X" 前缀
indonesia_population$Year <- gsub("X", "", indonesia_population$Year)
indonesia_population$Year <- as.numeric(indonesia_population$Year)  # 转换为数字格式

indonesia_electric$Year <- gsub("X", "", indonesia_electric$Year)
indonesia_electric$Year <- as.numeric(indonesia_electric$Year)  # 转换为数字格式

# 去除空值
indonesia_electric <- indonesia_electric %>%
  filter(!is.na(Electricity_Consumption))

indonesia_population <- indonesia_population %>%
  filter(!is.na(Population))

indonesia_GDP <- indonesia_GDP %>%
  filter(!is.na(GDP))

# 合并后再次检查并去除空值
indonesia_data <- indonesia_electric %>%
  inner_join(indonesia_population, by = c("Country.Name", "Year")) %>%
  mutate(
    Total_Electricity_Demand = Electricity_Consumption * Population / 1e9  # 计算总电力需求 (TWh)
  ) %>%
  filter(!is.na(Total_Electricity_Demand))  # 去除合并后含有空值的行

indonesia_GDP <- indonesia_GDP %>%
  mutate(
    Total_Electricity_Demand2 = GDP * 150 / 1e12  # 计算总电力需求 (TWh)
  ) %>%
  filter(!is.na(Total_Electricity_Demand2))  # 去除合并后含有空值的行

# 查看处理后的数据
print(head(indonesia_data))
print(head(indonesia_GDP))

# 保存为新的 CSV 文件
write.csv(indonesia_data, "indonesia_electricity_demand.csv", row.names = FALSE)
write.csv(indonesia_GDP, "indonesia_2040_GDP.csv", row.names = FALSE)






# 构建多项式回归模型（如二次或三次）(First hypothesis)
poly_model <- lm(Total_Electricity_Demand ~ poly(Year, 3), data = indonesia_data)

# 生成 2020 到 2040 年的预测数据
future_years <- data.frame(Year = 2020:2040)  # 未来年份
future_predictions <- predict(poly_model, newdata = future_years)

# 预测2040年的总电力需求
future_year <- data.frame(Year = 2040)
predicted_2040 <- predict(poly_model, newdata = future_year)

# 打印2040年的预测值
cat("预测2040年的总电力需求 (TWh):", predicted_2040, "\n")

# 合并预测数据
future_data <- data.frame(Year = future_years$Year, Predicted_Demand = future_predictions)

# 可视化：历史数据与预测数据
ggplot() +
  # 历史数据点
  geom_point(data = indonesia_data, aes(x = Year, y = Total_Electricity_Demand), color = "blue", size = 2) +
  # 历史数据趋势线（多项式回归）
  geom_smooth(data = indonesia_data, aes(x = Year, y = Total_Electricity_Demand), method = "lm", formula = y ~ poly(x, 3), color = "purple", se = FALSE) +
  # 预测数据点和趋势线
  geom_point(data = future_data, aes(x = Year, y = Predicted_Demand), color = "red", size = 2) +
  geom_line(data = future_data, aes(x = Year, y = Predicted_Demand), color = "green", size = 1,linetype = "dashed") +
  
  # 添加标题和轴标签
  labs(
    title = "Forecast of total electricity demand in Indonesia (2020-2040)",
    x = "Year",
    y = "Total Electricity Demand (TWh)"
  ) +
  theme_minimal()



# 计算预测值和残差
indonesia_data$Predicted <- predict(poly_model, newdata = indonesia_data)  # 使用模型生成预测值
indonesia_data$Residuals <- indonesia_data$Total_Electricity_Demand - indonesia_data$Predicted  # 计算残差

# 绘制残差 vs 预测值图
ggplot(indonesia_data, aes(x = Predicted, y = Residuals)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "残差 vs 预测值",
    x = "预测值",
    y = "残差"
  ) +
  theme_minimal()

# 绘制残差 vs 自变量 (Year) 图
ggplot(indonesia_data, aes(x = Year, y = Residuals)) +
  geom_point(color = "green") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "残差 vs 年份",
    x = "Year",
    y = "残差"
  ) +
  theme_minimal()

# 残差直方图
ggplot(indonesia_data, aes(x = Residuals)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "white") +
  labs(
    title = "残差分布直方图",
    x = "残差",
    y = "频率"
  ) +
  theme_minimal()

# 残差 Q-Q 图
qqnorm(indonesia_data$Residuals)
qqline(indonesia_data$Residuals, col = "red")

#符合正态分布




# 删除 GDP 列
data <- indonesia_GDP %>% select(-GDP)
data$Year <- as.numeric(indonesia_GDP$Year)  # 转换为数字格式
View(data)


# 构建基于 GDP 的回归模型(Second hypothesis)

# 构建基于年份的多项式回归模型
# 假设我们选择 3 次多项式
Poly_model <- lm(Total_Electricity_Demand2 ~ poly(Year, 3), data = data)

# 打印模型摘要
summary(Poly_model)

# 生成 2020 到 2040 年的预测数据
future_years2 <- data.frame(Year = 2020:2040)  # 未来年份
future_predictions2 <- predict(Poly_model, newdata = future_years2)

# 预测2040年的总电力需求
future_year2 <- data.frame(Year = 2040)
Predicted_2040 <- predict(Poly_model, newdata = future_year2)

# 打印2040年的预测值
cat("基于多项式回归模型预测2040年的总电力需求 (TWh):", Predicted_2040, "\n")

# 可视化：历史数据与多项式回归预测趋势
ggplot(data, aes(x = Year, y = Total_Electricity_Demand2)) +
  geom_point(color = "blue") +  # 历史数据点
  stat_smooth(method = "lm", formula = y ~ poly(x, 3), color = "red", se = FALSE) +  # 多项式回归趋势线
  geom_point(data = future_years2, aes(x = Year, y = future_predictions2), color = "green", size = 1) +  # 未来预测点
  annotate("point", x = 2040, y = Predicted_2040, color = "purple", size = 3) +  # 2040年预测点
  
  labs(
    title = "Forecast of total electricity demand in Indonesia (2020-2040)",
    x = "Year",
    y = "Total Electricity Demand (TWh)"
  ) +
  theme_minimal()


















# 参数设置
predicted_2040  # 总电力需求预测 (TWh)
Predicted_2040  # 总电力需求预测 (TWh)

existing_renewable_capacity_GW <- 18.958092  # 已有或规划的可再生能源容量 (GW)

# 计算参数
hours_per_year <- 365 * 24  # 全年小时数
capacity_factors <- c(renewable = 0.24, non_renewable = 0.45)  # 容量因子
energy_shares <- c(renewable = 0.38, non_renewable = 0.62)  # 能源比例

# Step 1: 计算总装机容量 (MW)
predicted_2040 <- predicted_2040 * 1e6  # 转换为 MWh
predicted_2040

Predicted_2040 <- Predicted_2040 * 1e6  # 转换为 MWh
Predicted_2040

total_installed_capacity_MW <- predicted_2040 / hours_per_year
total_installed_capacity_MW

total_installed_capacity2_MW <- Predicted_2040 / hours_per_year
total_installed_capacity2_MW

# Step 2: 计算装机容量 c
# 0.38c * 0.24 + 0.62c * 0.45 = total_installed_capacity_MW
c <- total_installed_capacity_MW / (energy_shares["renewable"] * capacity_factors["renewable"] +
                                      energy_shares["non_renewable"] * capacity_factors["non_renewable"])
c

c2 <- total_installed_capacity2_MW / (energy_shares["renewable"] * capacity_factors["renewable"] +
                                      energy_shares["non_renewable"] * capacity_factors["non_renewable"])
c2

# Step 3: 计算可再生能源容量 (MW)
renewable_capacity_MW <- c * energy_shares["renewable"]
renewable_capacity_MW

renewable_capacity2_MW <- c2 * energy_shares["renewable"]
renewable_capacity2_MW

# Step 4: 计算太阳能需要新增的容量
solar_capacity_MW <- renewable_capacity_MW - existing_renewable_capacity_GW * 1e3  # 转换为 MW
solar_capacity_GW <- solar_capacity_MW / 1e3  # 转换为 GW

solar_capacity2_MW <- renewable_capacity2_MW - existing_renewable_capacity_GW * 1e3  # 转换为 MW
solar_capacity2_GW <- solar_capacity2_MW / 1e3  # 转换为 GW


# 输出结果
#First hypothesis
cat("总电力需求预测 (TWh):", predicted_2040 / 1e6, "\n")
cat("总容量 (MW):", total_installed_capacity_MW, "\n")
cat("总装机容量 c (MW):", c, "\n")
cat("可再生能源装机容量 (MW):", renewable_capacity_MW, "\n")
cat("新增太阳能发电厂需要提供的容量 (GW):", solar_capacity_GW, "\n")

#Second hypothesis
cat("总电力需求预测 (TWh):", Predicted_2040 / 1e6, "\n")
cat("总容量 (MW):", total_installed_capacity2_MW, "\n")
cat("总装机容量 c (MW):", c2, "\n")
cat("可再生能源装机容量 (MW):", renewable_capacity2_MW, "\n")
cat("新增太阳能发电厂需要提供的容量 (GW):", solar_capacity2_GW, "\n")







