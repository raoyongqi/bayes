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


df_sf <- st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

# 将经纬度转换为 UTM 坐标系（根据经度选择合适的 UTM 带号）
# 例如，这里我们假设使用带 50N（EPSG:32650），你可以根据需要选择正确的带号
df_sf_utm <- st_transform(df_sf, crs = EPSG:4326)

# 提取转换后的 UTM 坐标，并将其添加到原始数据框中
df$UTMx <- st_coordinates(df_sf_utm)[, 1]
df$UTMy <- st_coordinates(df_sf_utm)[, 2]

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
