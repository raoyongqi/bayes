# 加载必要的库

library(vegan)

library(linkET)
library(corrr)
library(ggplot2)
library(unix)
getwd()
# 读取数据
file_path = "data/selection.csv"  # 替换为你的文件路径
data <- read.csv(file_path)
library(dplyr)
# 假设你的数据框是 data
data <- data %>%
  rename(`Pathogen Load` = RATIO)
# 获取所有列名
other_columns <- setdiff(names(data), "Pathogen Load")
ncol(data)
colnames(data)
# 使用其他列名从数据框中选取这些列
xxxx <- data[, other_columns]

# 提取因变量
yyyy <- data$`Pathogen Load`

#> 载入程辑包：'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
# 定义测量项
column_names <- c("SRAD", "HAND", "LON", "S_SAND", "WIND", "PSEA", "VAPR", "ELEV", 
                  "MIN_MAT", "TSEA", "LAT", "AVG_MAT", "MAP", "T_SAND", "MAX_MAT", "S_REF_BULK")

# 定义 Climate, Soil, Geo 列表
Climate <- c("SRAD", "PSEA", "WIND", "VAPR", "MAX_MAT", "TSEA","AVG_MAT", "MIN_MAT", "MAP")
Soil <- c("S_SAND",  "T_SAND", "S_REF_BULK")
Geo <- c("LAT", "LON", "ELEV","HAND")

# 假设 df 是你原来的数据框
ncol(xxxx)
# 根据列列表重新排序
xxxx <- xxxx %>%
  select(all_of(c(Climate, Soil, Geo)))
ncol(xxxx)

# yyyy <- df_sorted  # 假设这个数据框是自变量
# xxxx <- df$`Pathogen Load`  # 假设这个是因变量

# 使用 mantel_test 进行 Mantel 检验
mantel <- mantel_test(xxxx, yyyy,
                      spec_select = list(`Pathogen Load` = 1:1)
                                     )%>% 
  mutate(rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf),
                  labels = c("< 0.2", "0.2 - 0.4", ">= 0.4")),
         pd = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf),
                  labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")))
# devtools::install_github("krlmlr/ulimit")
#> `mantel_test()` using 'bray' dist method for 'spec'.
#> `mantel_test()` using 'euclidean' dist method for 'env'.
output_file <- "C:/Users/r/Desktop/mantel_results.xlsx"
library(openxlsx)
# 使用 openxlsx 将数据框写入 Excel
write.xlsx(mantel, output_file)
correlate(xxxx)
memory.limit(size = 16000)  # 16GB（根据你的电脑内存大小调整）

qcorrplot(correlate(xxxx), type = "lower", diag = FALSE) +
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


traceback()

# 在 R 启动时增加 C stack 的大小
options(expressions = 500000)  # 增加最大表达式数限制
