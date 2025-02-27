library(seminr)
setwd("C:/Users/r/Desktop/bayes")

# 读取数据
file_path = "C:/Users/r/Desktop/bayes/data/selection.csv"  # 替换为你的文件路径
data <- read.csv(file_path)
library(dplyr)
# 假设你的数据框是 data
data <- data %>%
  rename(`Pathogen Load` = RATIO)

  # 定义测量项
Climate <- c("SRAD", "PSEA", "WIND", "VAPR", "MAX_MAT", "TSEA", "MIN_MAT", "MAP")
Soil <- c("S_SAND", "S_CLAY", "T_SAND", "T_BULK_DEN", "T_REF_BULK")
Geo <- c("LAT", "LON", "ELEV")

# 创建测量模型（反射性和复合指标）
measurements <- constructs(
  composite("Climate", Climate),  # 定义反射性构念 "Climate"
  composite("Soil", Soil),  # 定义复合构念 "Soil"
  composite("Geo", Geo),  # 定义复合构念 "Geo"
  composite("Pathogen Load",  single_item("Pathogen Load"))
)
str(Geo)
str(multi_items("CUEX", 1:3)
)
# 结构关系
structure <- relationships(
  paths(from = c("Climate", "Geo"), to = "Soil"),  # 气候和地理影响土壤
  
  paths(from = c("Climate", "Geo", "Soil"), to = "Pathogen Load")  # 气候、地理和土壤共同影响 Pathogen Load
)
# 检查数据类型
str(data)
# 如果需要，转换某些列为数值型
data$SRAD <- as.numeric(data$SRAD)

# 估计 PLS 模型
pls_model <- estimate_pls(data = data, measurements, structure)
pls_model
# 绘制 PLS 模型
png("PLS_Model.png", width = 10, height = 8, units = "in", res = 300)
plot(pls_model, main = "PLS Model")

dev.off()

ggsave("PLS_Model.png", plot = last_plot(), dpi = 300, width = 10, height = 8)
