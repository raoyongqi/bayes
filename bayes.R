# 加载必要的包
library(readxl)
library(dplyr)
library(randomForest)
library(Boruta)
set.seed(42)
# library(readr)


setwd("C:/Users/r/Desktop/bayes")

# 1. 读取 Excel 文件
file_path <- "selection.csv"  # 替换为你的文件路径
data <- read.csv(file_path)
library(brms)

# 假设 data 已经加载为数据框
# 使用 brm() 函数创建贝叶斯回归模型
model <- brm(
  formula = RATIO ~ SARD + LON + S_SAND + WIND + VAPR + ELEV + LAT + MAXMAT + AVGMAT + MINMAT + MAP + T_SAND + MU_GLOBAL + T_REF_BULK + S_CLAY + S_REF_BULK + T_GRAVEL,
  data = data,
  family = gaussian(),
  chains = 4,
  iter = 1000,
  warmup = 1000
)


# 查看模型的摘要
summary(model)

# 绘制模型的后验分布
plot(model)

# 绘制某个参数的后验分布
plot(model, pars = "SARD")
# 将列名转为小写