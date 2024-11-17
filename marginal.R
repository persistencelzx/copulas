library(fitdistrplus)
library(ggplot2)
library(dplyr)
library(MASS)
library(VineCopula)
library(copula)
# 读取CSV文件
file_path <- "晓桥1960到2024年每年总数据.csv"
data <- read.csv(file_path)
data_matrix <- as.matrix(data)

# 数据标准化到 [0, 1] 区间
#最大最小值
data_standardized <- (data[,-1] - min(data[,-1])) / (max(data[,-1]) - min(data[,-1]))
cor(data_standardized)



# 对第一个变量进行正态分布拟合 #logis gamma norm
fit_rainfall <- fitdist(data_standardized[,1], distr = 'logis')
summary(fit_rainfall)
# 对第二个变量进行伽玛分布拟合 #exp cauchy
fit_runoff <- fitdist(data_standardized[,2], "exp")
summary(fit_runoff)
# 对第三个变量进行泊松分布拟合 #exp
fit_humidity <- fitdist(data_standardized[,3], "gamma")
summary(fit_humidity)
# 对降雨量进行KS检验
ks_rainfall <- ks.test(data_standardized[,1], "plogis", 
                       location = fit_rainfall$estimate["location"], 
                       scale = fit_rainfall$estimate["scale"])
# 对径流量进行KS检验
ks_runoff <- ks.test(data_standardized[,2], "pexp", 
                     rate = fit_runoff$estimate["rate"])
# 对湿度进行KS检验
ks_humidity <- ks.test(data_standardized[,3], "pgamma", 
                       rate = fit_humidity$estimate["rate"], shape = fit_humidity$estimate['shape'])




# 对降雨量（逻辑斯特分布）进行均匀转换
u_rainfall <- plogis(data_standardized[, 1], location = fit_rainfall$estimate["location"], scale = fit_rainfall$estimate["scale"])
# 对径流量（指数分布）进行均匀转换
u_runoff <- pexp(data_standardized[, 2], rate = fit_runoff$estimate["rate"])
# 对湿度（gamma分布）进行均匀转换
u_humidity <- pgamma(data_standardized[, 3], rate = fit_humidity$estimate["rate"], shape = fit_humidity$estimate['shape'])



data_vine <- cbind(u_rainfall, u_runoff, u_humidity)
# 使用RVineStructureSelect自动选择vine结构
family_set <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 13, 16, 18, 20, 23, 24, 26, 27, 28, 29, 30, 33, 34, 36, 37, 38, 39,40, 104)
vine_model <- RVineStructureSelect(data_vine, familyset = family_set)
summary(vine_model)



# 生成模拟数据
# 使用RVineCopula函数来模拟数据
simulated_data <- RVineSim(1000, vine_model)
cor(simulated_data)



# 使用 pairs 绘制散点图矩阵
pairs(simulated_data, main = "Scatterplot Matrix of Simulated Data")
ggpairs(as.data.frame(simulated_data), 
        title = "Scatterplot Matrix of Simulated Data")

plot(simulated_data[,1],simulated_data[,2])
plot(simulated_data[,1],simulated_data[,3])
plot(simulated_data[,2],simulated_data[,3])




