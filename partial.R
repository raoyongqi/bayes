# 加载所需的库
library(vegan)
library(linkET)
library(corrr)
library(ggplot2)
library(dplyr)
library(randomForestSRC)  # 加载 randomForestSRC 包

# 读取数据
file_path <- "C:/Users/r/Desktop/bayes/data/selection.csv"  # 替换为你的文件路径
df <- read.csv(file_path)

# 删除 'RATIO' 列，并重新排列
df$Pathogen.Load <- df$RATIO
df <- df[, !names(df) %in% c("RATIO")]
df <- df %>%
  select(Pathogen.Load, everything())

# 创建随机森林模型
rfsrc_Pathogen.Load <- rfsrc(Pathogen.Load ~ ., data = df)
rfsrc_Pathogen.Load$xvar$LON
# 使用 partial 函数计算每个变量的部分依赖

partial_LON  <- partial(rfsrc_Pathogen.Load,
                       partial.xvar = "LON",  # 指定变量名
                       partial.values = rfsrc_Pathogen.Load$xvar$LON)  # 指定部分依赖的值
# 将每个变量的部分依赖数据转为数据框


partial_LAT <- partial(rfsrc_Pathogen.Load,
                       partial.xvar = "LAT",  # 指定变量名
                       partial.values = rfsrc_Pathogen.Load$xvar$LAT)  # 指定部分依赖的值

partial_AVG_MAT <- partial(rfsrc_Pathogen.Load,
                           partial.xvar = "AVG_MAT",  # 指定变量名
                           partial.values = rfsrc_Pathogen.Load$xvar$AVG_MAT)  # 指定部分依赖的值

partial_MAP <- partial(rfsrc_Pathogen.Load,
                       partial.xvar = "MAP",  # 指定变量名
                       partial.values = rfsrc_Pathogen.Load$xvar$MAP)  # 指定部分依赖的值
partial_LON_data <- get.partial.plot.data(partial_LON, m.target = "Pathogen.Load")
partial_LAT_data <- get.partial.plot.data(partial_LAT, m.target = "Pathogen.Load")
partial_AVG_MAT_data <- get.partial.plot.data(partial_AVG_MAT, m.target = "Pathogen.Load")
partial_MAP_data <- get.partial.plot.data(partial_MAP, m.target = "Pathogen.Load")

# 查看部分依赖数据的结构
str(partial_LON_data)
str(partial_LAT_data)
str(partial_AVG_MAT_data)
str(partial_MAP_data)
# 将每个变量的部分依赖数据转为数据框
# 将每个变量的部分依赖数据转为数据框
df_LON <- data.frame(Variable = "LON", 
                     Value = partial_LON_data$x, 
                     PartialDependence = partial_LON_data$yhat)

df_LAT <- data.frame(Variable = "LAT", 
                     Value = partial_LAT_data$x, 
                     PartialDependence = partial_LAT_data$yhat)

df_AVG_MAT <- data.frame(Variable = "AVG_MAT", 
                         Value = partial_AVG_MAT_data$x, 
                         PartialDependence = partial_AVG_MAT_data$yhat)

df_MAP <- data.frame(Variable = "MAP", 
                     Value = partial_MAP_data$x, 
                     PartialDependence = partial_MAP_data$yhat)
# 合并所有变量的部分依赖数据框
df_all <- rbind(df_LON, df_LAT, df_AVG_MAT, df_MAP)
theme_publication <- function(base_size = 12, base_family = "Helvetica", ...) {
  require(grid)
  require(ggthemes)
  (theme_foundation(base_size = base_size, base_family = base_family)
    + theme(plot.title = element_text(face = "bold", size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(color = NA),
            plot.background = element_rect(color = NA),
            panel.border = element_rect(color = "black", size = 1),
            axis.title = element_text(face = "plain", size = 9),
            axis.title.y = element_text(angle=90, vjust = 2, margin = margin(r=0)),
            axis.title.x = element_text(vjust = -0.2, margin = margin(t=7)),
            axis.text = element_text(size = 7), 
            #axis.line.y = element_line(color="black"),
            #axis.line.x = element_line(color="black"),
            axis.ticks = element_line(),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank() ,
            # explicitly set the horizontal lines (or they will disappear too)
            #panel.grid.major.x = element_line(size=.5, color="#f0f0f0"),
            #legend.key = element_rect(color = NA),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.key.size = unit(0.4, "cm"),
            legend.spacing = unit(0, "cm"),
            legend.title = element_text(size = 8),
            legend.text = element_text(size = 7),
            plot.margin = unit(c(10,5,5,5),"mm"),
            strip.text = element_text(size = 8),
            strip.background = element_blank()
    ))
}
library(ggplot2)

ggplot(df_all, aes(x = Value, y = PartialDependence, color = Variable)) +
  geom_point(size = 1.5, aes(group = Variable), alpha = 0.1) +
  geom_smooth(method = "loess", size = 1.5, aes(group = Variable), se = FALSE) +  
  facet_wrap_paginate(~ Variable, scales = "free") +  
  
  scale_color_manual(values = c("darkred", "coral", "darkblue", "darkgreen")) +  # 设定颜色
  
  labs(title = "Partial Dependence Plots",
       x = "Variable Value",
       y = "Partial Dependence",
       color = "Variable") +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),  # 调整 x 轴文本字体大小
    axis.text.y = element_text(size = 16),  # 调整 y 轴文本字体大小
    strip.text.x = element_text(face = "bold", size = 18),  # facet 标题加粗并增大字体
    strip.text.y = element_text(face = "bold", size = 18),  # facet 标题加粗并增大字体
    plot.title = element_text(size = 20, face = "bold"),  # 增大标题字体
    plot.subtitle = element_text(size = 18),  # 增大副标题字体（如果有）
    plot.caption = element_text(size = 14),  # 增大图注字体（如果有）
    legend.text = element_text(size = 14),  # 增大图例文本字体
    legend.title = element_text(size = 16)  # 增大图例标题字体
  ) + 
  theme_publication(base_size = 16,face = "bold")  # 使用出版风格，并调整基础字体大小


ggsave("partial.png", plot = last_plot(), dpi = 300, width = 10, height = 8)


