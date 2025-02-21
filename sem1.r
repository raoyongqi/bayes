library(seminr)
setwd("C:/Users/r/Desktop/bayes")

# 读取数据
file_path = "C:/Users/r/Desktop/bayes/data/selection.csv"  # 替换为你的文件路径
data <- read.csv(file_path)


# 定义测量项
Climate <- c("SRAD", "PSEA", "WIND", "VAPR", "MAX_MAT", "TSEA", "MIN_MAT", "MAP")
Soil <- c("S_SAND", "S_CLAY", "T_SAND", "T_BULK_DEN", "T_REF_BULK")
Geo <- c("LAT", "LON", "ELEV")

# 创建测量模型（反射性和复合指标）
measurements <- constructs(
  composite("Climate", Climate),  # 定义反射性构念 "Climate"
  composite("Soil", Soil),  # 定义复合构念 "Soil"
  composite("Geo", Geo),  # 定义复合构念 "Geo"
  composite("RATIO",  single_item("RATIO"))
)
str(Geo)
str(multi_items("CUEX", 1:3)
)
# 结构关系
structure <- relationships(
  paths(from = c("Climate", "Geo"), to = "Soil"),  # 气候和地理影响土壤
  
  paths(from = c("Climate", "Geo", "Soil"), to = "RATIO")  # 气候、地理和土壤共同影响 RATIO
)
# 检查数据类型
str(data)
# 如果需要，转换某些列为数值型
data$SRAD <- as.numeric(data$SRAD)

# 估计 PLS 模型
pls_model <- estimate_pls(data = data, measurements, structure)

# 绘制 PLS 模型
plot(pls_model, title = "PLS Model")

