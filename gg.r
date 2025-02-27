library(linkET)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

# 读取 CSV 数据
file_path <- "C:/Users/r/Desktop/bayes/data/selection.csv"  # 替换为你的文件路径
data <- read.csv(file_path)

# 这里假设你的数据中有两个数据框，类似于 'varechem' 和 'varespec'
# 需要根据你的 CSV 文件结构调整这些变量
# 以下假设 data 中包含名为 'spec' 和 'chem' 的列，分别用于物种和环境变量数据

# 假设数据框 'data' 中有名为 'spec' 和 'chem' 的数据框，分别代表物种和环境数据
# 根据你的实际数据修改这些变量
mantel <- mantel_test(data$spec, data$chem,
                      spec_select = list(Spec01 = 1:7,
                                         Spec02 = 8:18,
                                         Spec03 = 19:37,
                                         Spec04 = 38:44)) %>% 
  mutate(rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf),
                  labels = c("< 0.2", "0.2 - 0.4", ">= 0.4")),
         pd = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf),
                  labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")))

# 创建相关性矩阵图
qcorrplot(correlate(data$chem), type = "lower", diag = FALSE) +
  geom_square() +
  geom_couple(aes(colour = pd, size = rd), 
              data = mantel, 
              curvature = nice_curvature()) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu")) +
  scale_size_manual(values = c(0.5, 1, 2)) +
  scale_colour_manual(values = color_pal(3)) +
  guides(size = guide_legend(title = "Mantel's r",
                             override.aes = list(colour = "grey35"), 
                             order = 2),
         colour = guide_legend(title = "Mantel's p", 
                               override.aes = list(size = 3), 
                               order = 1),
         fill = guide_colorbar(title = "Pearson's r", order = 3))
