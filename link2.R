# 加载必要的库

Sys.setenv(LD_LIBRARY_PATH = "/usr/lib/x86_64-linux-gnu")
LD_LIBRARY_PATH 
install.packages("vegan", dependencies=TRUE)
Sys.getenv("LD_LIBRARY_PATH")

library(vegan)

library(linkET)
library(corrr)
library(ggplot2)
library(unix)
rlimit_stack(cur = Inf)  # 设置栈大小为无限制

# 读取数据
setwd("~/Documents$")

# 读取数据
file_path = "selection.csv"  # 替换为你的文件路径
data <- read.csv(file_path)
library(dplyr)
# 假设你的数据框是 data
data <- data %>%
  rename(`Pathogen Load` = RATIO)
# 获取所有列名
other_columns <- setdiff(names(data), "Pathogen Load")

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

# 定义 Climate, Soil, Geo 列表
Climate <- c("SRAD", "PSEA", "WIND", "VAPR", "MAX_MAT", "TSEA", "MIN_MAT", "MAP")
Soil <- c("S_SAND", "S_CLAY", "T_SAND", "T_BULK_DEN", "T_REF_BULK")
Geo <- c("LAT", "LON", "ELEV")

# 假设 df 是你原来的数据框

# 根据列列表重新排序
xxxx <- xxxx %>%
  select(all_of(c(Climate, Soil, Geo)))

# yyyy <- df_sorted  # 假设这个数据框是自变量
# xxxx <- df$`Pathogen Load`  # 假设这个是因变量

# 使用 mantel_test 进行 Mantel 检验
mantel <- mantel_test(xxxx, yyyy,
                      spec_select = list(Climate = 1:8,
                                         Soil = 9:13,
                                         Geo = 14:16)
                                     )%>% 
  mutate(rd = cut(r, breaks = c(-Inf, 0.2, 0.4, Inf),
                  labels = c("< 0.2", "0.2 - 0.4", ">= 0.4")),
         pd = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf),
                  labels = c("< 0.01", "0.01 - 0.05", ">= 0.05")))

#> `mantel_test()` using 'bray' dist method for 'spec'.
#> `mantel_test()` using 'euclidean' dist method for 'env'.

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
# 在 R 启动时增加 C stack 的大小
options(expressions = 500000)  # 增加最大表达式数限制
