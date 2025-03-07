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


df_sf <- st_as_sf(df, coords = c("LON", "LAT"), crs = 4326, remove = FALSE)

# 将经纬度转换为 UTM 坐标系（根据经度选择合适的 UTM 带号）
# 例如，这里我们假设使用带 50N（EPSG:32650），你可以根据需要选择正确的带号
df_sf_utm <- st_transform(df_sf, crs = 32650)

# 提取转换后的 UTM 坐标，并将其添加到原始数据框中
df$UTMx <- st_coordinates(df_sf_utm)[, 1]
df$UTMy <- st_coordinates(df_sf_utm)[, 2]
df
vars_of_interest = c("ELEV",  "S_SAND" ,  "SRAD" )  # 根据需要修改

# 创建一个空的列表来存储每个变量的图形
bin_list <- list()
env_list <- list()
test = dist(data_geoR_tmp[,c("x", "y")])
bla = bin.var(test, bins = 100, method = c("proportions"), labels = NULL)

for(var_of_interest in vars_of_interest) {
  # 获取目标变量的列索引
  var_index <- which(names(df) == var_of_interest)
  
  # 准备数据
  data_geoR_tmp = data.frame(x = df[,c("UTMx")], y = df[,c("UTMy")], z = df[,var_index])
  if(var_of_interest != "Pathogen.Load") { 
    data_geoR_tmp$z = scale(data_geoR_tmp$z, center = TRUE, scale = TRUE) 
  }
  print(data_geoR_tmp$z)
  coords <- data.frame(x = data_geoR_tmp$x, y = data_geoR_tmp$y)
  # 使用 jitterDupCoords 给重复的点加扰动
  coords_jittered <- jitterDupCoords(coords, max = 0.01)
  data_geoR_tmp$x <- coords_jittered$x
  data_geoR_tmp$y <- coords_jittered$y
  data_geoR = as.geodata(data_geoR_tmp, coords.col = 1:2, data.col = 3)
  
  # 计算半变异

  
  dtmp = c()
  for (i in 1:length(levels(bla)))  
  {
    if(i==1){
      lev = as.character(levels(bla)[i])
      tmp1 = strsplit(lev, "[[]")[[1]][2]
      bound1 = as.numeric(strsplit(tmp1, ",")[[1]][1])
      tmp2 = strsplit(tmp1, ",")[[1]][2]
      bound2 = as.numeric(strsplit(tmp2, "[]]")[[1]][1])
      dtmp = c(dtmp, bound1+(bound2-bound1)/2)
    }
    if(i!=1){
      lev = as.character(levels(bla)[i])
      tmp1 = strsplit(lev, "[(]")[[1]][2]
      bound1 = as.numeric(strsplit(tmp1, ",")[[1]][1])
      tmp2 = strsplit(tmp1, ",")[[1]][2]
      bound2 = as.numeric(strsplit(tmp2, "[]]")[[1]][1])
      dtmp = c(dtmp, bound1+(bound2-bound1)/2)
    }
  }
  bin <- variog(data_geoR, uvec = dtmp)

  env  = variog.mc.env(data_geoR, obj.variog = bin)

  # 存储 bin 和 env 数据
  bin_list[[var_of_interest]] <- bin
  env_list[[var_of_interest]] <- env
}
# 将所有变量的半变异数据合并到一个数据框中
bin_combined <- do.call(rbind, lapply(names(bin_list), function(var_of_interest) {
  data.frame(
    Distance = bin_list[[var_of_interest]]$u,
    Semivariance = bin_list[[var_of_interest]]$v,
    Variable = var_of_interest
  )
}))
env_list[1]
env_list[2]
env_list[3]

env_combined <- do.call(rbind, lapply(names(env_list), function(var_of_interest) {
  data.frame(
    Distance = env_list[[var_of_interest]]$u,
    Lower = env_list[[var_of_interest]]$v.lower,
    Upper = env_list[[var_of_interest]]$v.upper,
    Variable = var_of_interest
  )
}))
# 使用 ggplot 绘制所有变量的半变异图
library(ggplot2)


library(ggplot2)

# 检查 Variable 列的唯一值
unique_variables_bin <- unique(bin_combined$Variable)
unique_variables_env <- unique(env_combined$Variable)

# 查看所有变量
print(unique_variables_bin)
print(unique_variables_env)

# 颜色映射
color_palette <- c("green" = "#2ca02c", "lightblue" = "#1f77b4", "brown" = "#8c564b")

# 确保变量和颜色的映射正确
library(ggthemes)
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
p22 <-ggplot() +
  geom_ribbon(data = env_combined, aes(x = Distance, ymin = Lower, ymax = Upper, fill = Variable), 
              alpha = 0.1) +
  geom_smooth(data = bin_combined, aes(x = Distance, y = Semivariance, color = Variable),
              size = 4, se = FALSE) +
  scale_colour_manual(values = c("#2ca02c",  "lightblue",  "darkred"))+
  theme_publication() +  # 使用 ggplot2 内置主题
  labs(x = "Distance (m)", y = "Semivariance", ) +
  theme(
    text = element_text(size = 14, face = "bold"),  # 加粗字体
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 14, face = "bold"),  # 加粗坐标轴的数字
    axis.title = element_text(size = 14, face = "bold"),  # 加粗坐标轴标题
    legend.position = c(0.85, 0.15),  # 将图例放到右下角
    legend.text = element_text(size = 16, face = "bold")  # 加粗图例文本
    
  ) +
  guides(
    fill = guide_legend(ncol = 1, title = NULL),  # 去除图例标题
    color = guide_legend(ncol = 1, title = NULL)  # 去除图例标题
  )
library(patchwork)

# 假设你有两个图 ggplot1 和 ggplot2
# 使用 patchwork 拼接图形
plot11
plot11 + p22  # 默认是并排显示
