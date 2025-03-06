# 加载必要的库
library(vegan)
library(linkET)
library(corrr)
library(ggplot2)

# 读取数据
file_path <- "C:/Users/r/Desktop/bayes/data/selection.csv"  # 替换为你的文件路径
df <- read.csv(file_path)

# 删除 'RATIO' 列
df$Plant.Disease <- df$RATIO
df <- df[, !names(df) %in% c("RATIO")]

# 选择所有自变量（除去因变量）
X <- df[, setdiff(names(df), "Plant.Disease")]

# 提取因变量
y <- df$Plant.Disease

# 计算因变量和所有自变量的欧几里得距离矩阵
dist_matrix_y <- dist(y, method = "euclidean")

# 创建存储 Mantel 相关性的数据框
mantel_results <- data.frame(
  variable = names(X),
  Mantel.r = numeric(length(names(X))),  # 预分配相同长度的向量
  p.value = numeric(length(names(X))),
  stringsAsFactors = FALSE
)

# 计算 Mantel 检验
for (col in names(X)) {
  dist_matrix_X <- dist(X[[col]], method = "euclidean")
  mantel_result <- mantel(dist_matrix_X, dist_matrix_y, method = "pearson")
  
  mantel_results[mantel_results$variable == col, ] <- c(
    col,
    round(mantel_result$statistic, 2), 
    round(mantel_result$signif, 3)
  )
}

# 转换成相关性矩阵
mantel_matrix <- as.matrix(mantel_results[, c("Mantel.r")])

# Add an extra row and column for the Plant.Disease variable to create a square matrix
mantel_matrix <- rbind(mantel_matrix, c(NA))
mantel_matrix <- cbind(mantel_matrix, c(NA))
rownames(mantel_matrix) <- c(mantel_results$variable, "Plant.Disease")
colnames(mantel_matrix) <- c("Plant.Disease", "Plant.Disease")

# 使用 qcorrplot 绘制 Mantel 相关性矩阵
p <- qcorrplot(mantel_cormat) +
  theme_minimal()

# 显示图形
print(p)
