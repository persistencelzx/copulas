library(readxl)
library(writexl)

# 设置文件路径和年份范围
file_path <- "D:/r_session/test/2/"  # 修改为你的文件路径
years <- 1960:2024
file_names <- paste0(file_path, "晓桥逐日气象数据-", years, ".xls")

# 初始化结果数据框
results <- data.frame(
  Year = integer(),
  Total_Rainfall = numeric(),
  Total_Runoff = numeric(),
  Relative_Humidity = numeric()
)

# 遍历每个文件
for (year in years) {
  file_name <- file_names[year - min(years) + 1]
  data <- read_excel(file_name)
  
  # 打印列名以确认列名是否正确
  print(paste("Reading file for year", year))
  print(colnames(data))
  
  # 将字符型数据转换为数值型数据
  data$`降水量(mm)` <- as.numeric(as.character(data$`降水量(mm)`))
  data$`径流(mm)` <- as.numeric(as.character(data$`径流(mm)`))
  data$`相对湿度(%)` <- as.numeric(as.character(data$`相对湿度(%)`))
  
  
  # 计算每年的总和
  total_rainfall <- sum(data$`降水量(mm)`, na.rm = TRUE)
  total_runoff <- sum(data$`径流(mm)`, na.rm = TRUE)
  relative_humidity <- mean(data$`相对湿度(%)`, na.rm = TRUE)
  
  # 将结果添加到数据框中
  results <- rbind(results, data.frame(
    Year = year,
    Total_Rainfall = total_rainfall,
    Total_Runoff = total_runoff,
    Relative_humidity = relative_humidity
  ))
}

# 保存结果到CSV文件
output_file <- "晓桥1960到2024年每年总数据.csv"
write.csv(results, file = output_file, row.names = FALSE)


