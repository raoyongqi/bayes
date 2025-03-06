# 加载必要的库
library(vegan)
library(linkET)
library(corrr)
library(ggplot2)
library(dplyr)
# 读取数据
file_path <- "C:/Users/r/Desktop/bayes/data/selection.csv"  # 替换为你的文件路径
df <- read.csv(file_path)

# 删除 'RATIO' 列
df$Pathogen.Load <- df$RATIO
df <- df[, !names(df) %in% c("RATIO")]
df <- df %>%
  select(Pathogen.Load, everything())
library("ranger")
pl_ranger_model <- ranger(Pathogen.Load~., data = df, num.trees = 50)
explainer_ranger <- DALEX::explain(
  pl_ranger_model, 
  data = df[, -1],  # 预测变量
  y = df$Pathogen.Load,  # 目标变量
  label = "Ranger Pathogen.Load"
)

model_parts_ranger_aps <- model_parts(explainer_ranger, type = "raw")
model_parts_ranger_aps

head(model_parts_ranger_aps, 8)
plot(model_parts_ranger_aps)

ggsave("rf.png", plot = last_plot(), dpi = 300, width = 10, height = 8)
