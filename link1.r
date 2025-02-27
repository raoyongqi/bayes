# Load necessary libraries
library(vegan)

# 读取数据
file_path <- "C:/Users/r/Desktop/bayes/data/selection.csv"  # 替换为你的文件路径

df <- read.csv(file_path)

# 删除 'RATIO' 列
df$Plant.Disease <- df$RATIO
df <- df[, !names(df) %in% c("RATIO")]

# 查看数据的前几行
cat("Data preview:\n")
print(head(df))

# 假设 CSV 文件中有多个变量和一个因变量
# 选择所有自变量（除去因变量）
X <- df[, setdiff(names(df), "Plant.Disease")]

# 提取因变量
y <- df$Plant.Disease

# 定义 Mantel 检验函数
mantel_test <- function(dist_matrix_1, dist_matrix_2) {
  # 执行 Mantel 检验并返回相关系数及 p 值
  mantel_result <- mantel(dist_matrix_1, dist_matrix_2, method = "pearson")
  return(c(mantel_result$statistic, mantel_result$signif))
}

# 创建一个空的数据框来存储结果
results <- data.frame(
  spec.variable = character(),
  variable = character(),
  Mantel.r = numeric(),
  p.value = character(),
  Significance = character(),
  stringsAsFactors = FALSE
)

# 遍历每个自变量，计算与因变量的 Mantel r
for (col in names(X)) {
  # 计算自变量和因变量的欧几里得距离矩阵
  dist_matrix_X <- dist(X[[col]], method = "euclidean")
  dist_matrix_y <- dist(y, method = "euclidean")
  
  # 执行 Mantel 检验
  result <- mantel_test(dist_matrix_X, dist_matrix_y)
  
  # 保留两位小数
  correlation <- round(result[1], 2)
  p_value <- round(result[2], 2)
  
  # 显著性判断
  significance <- ifelse(p_value < 0.05, "Significant", "Not Significant")
  
  # 将结果添加到结果列表中
  results <- rbind(results, data.frame(
    spec.variable = "Plant Disease",  # 因变量的列名固定为 "Plant Disease"
    variable = col,                   # 当前自变量列名
    Mantel.r = correlation,
    p.value = ifelse(p_value < 0.05, paste0("\\textbf{", p_value, "}"), p_value),  # 显著的 p-value 加粗
    Significance = significance,
    stringsAsFactors = FALSE
  ))
}

# 查看结果
print(results)


# 查看结果
print(results)
