# 假设数据框是 df
library("bayesplot")
library("rstanarm")
library("ggplot2")
library("xtable")

setwd("C:/Users/r/Desktop/bayes")

file_path <- "data/selection.csv"  # 替换为你的文件路径
selection <- read.csv(file_path)

selection_scaled[ , -which(names(selection) == "RATIO")] <- scale(selection[ , -which(names(selection) == "RATIO")])
# 删除 MIN_MAT 和 MAX_MAT 列
selection_scaled <- selection_scaled[, !(colnames(selection_scaled) %in% c("MIN_MAT", "MAX_MAT"))]

# 假设数据框是 df
# 将列名中的 "S_SAND" 替换为 "SAND"
colnames(selection_scaled) <- gsub("S_SAND", "SAND", colnames(selection_scaled))
M <- cor(selection_scaled[, 1:10])

colnames(M) <- gsub("_", " ", colnames(M))

corrplot(M, order = 'AOE', type = 'upper', method = 'square', tl.pos = 'd')
corrplot(M, add = TRUE, type = 'lower', method = 'number', order = 'AOE',diag = FALSE, tl.pos = 'n', cl.pos = 'n')

