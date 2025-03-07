# 加载必要的库

require(geoR)
require(randomForest)
require(pls)
require(caret)
require(parallel)
require(doParallel)
require(raster)
require(sf)
require(pracma)
require(data.table)
require(ggplot2)
require(ggpubr)
library(ggplot2)
library(dplyr)
# 读取数据

rm(list=ls())
# 删除 'RATIO' 列
file_path <- "C:/Users/r/Desktop/bayes/data/selection.csv"  # 替换为你的文件路径
df <- read.csv(file_path)

df$Pathogen.Load <- df$RATIO
df <- df[, !names(df) %in% c("RATIO")]
library(sp)


df_sf <- st_as_sf(df, coords = c("LON", "LAT"), crs = 4326, remove = FALSE)

# 将经纬度转换为 UTM 坐标系（根据经度选择合适的 UTM 带号）
# 例如，这里我们假设使用带 50N（EPSG:32650），你可以根据需要选择正确的带号
df_sf_utm <- st_transform(df_sf, crs = 32650)

# 提取转换后的 UTM 坐标，并将其添加到原始数据框中
df$UTMx <- st_coordinates(df_sf_utm)[, 1]
df$UTMy <- st_coordinates(df_sf_utm)[, 2]
df
vars_of_interest = c("ELEV",  "S_SAND" , "T_SAND","SRAD","PSEA" )  # 根据需要修改

# 创建一个空的列表来存储每个变量的图形
bin_list <- list()
env_list <- list()
dir_path = "C:/Users/r/Desktop/bayes/data/Toy_dataset.rds"
dir_path
# load data (toy example provided, named "DATA_review.rds")
valuetable = readRDS(dir_path)

# Source functions
source("C:/Users/r/Desktop/bayes/Functions_Ploton-et-al_2020.R")
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

env_combined
bin_combined
library(ggplot2)
p113<-ggplot() +
  geom_ribbon(data = env_combined, aes(x = Distance, ymin = Lower, ymax = Upper, fill = Variable), 
              alpha = 0.1) +
  geom_smooth(data = bin_combined, aes(x = Distance, y = Semivariance, color = Variable),
              size = 4, se = FALSE)  +
  scale_colour_manual(values = c("ELEV" = "#2ca02c",  # "Geo" 组用绿色
                                 "SRAD" = "lightblue",  # "Climate" 组用浅蓝色
                                 "PSEA" = "lightblue",  # "Climate" 组用浅蓝色
                                 
                                 "S_SAND" = "darkred",  # "Sand" 组用深红色
                                 "T_SAND" = "darkred")) +  # 其他组用深红色
  scale_fill_manual(values = c("ELEV" = "#2ca02c", 
                               "SRAD" = "lightblue",
                               "PSEA" = "lightblue",
                               
                               "S_SAND" = "darkred", 
                               "T_SAND" = "darkred"))+
 labs(x = "Distance (m)", y = "Semivariance", ) + 
  # 设置主题
  theme_publication() +
  theme(
    text = element_text(size = 14, face = "bold"),  # 加粗字体
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 14, face = "bold"),  # 加粗坐标轴的数字
    axis.title = element_text(size = 14, face = "bold"),  # 加粗坐标轴标题
    legend.position = "none",  # 将图例放到右下角
    legend.background = element_blank(),
    

  )
plot11+p113
library(dplyr)
env_combined <- env_combined %>%
  mutate(VariableGroup = ifelse(Variable %in% c("S_SAND", "T_SAND"), "Sand", 
                                ifelse(Variable %in% c("SRAD", "PSEA"), "Climate", 
                                       ifelse(Variable %in% c("ELEV"), "Geo", "Other"))))

bin_combined <- bin_combined %>%
  mutate(VariableGroup = ifelse(Variable %in% c("S_SAND", "T_SAND"), "Sand", 
                                ifelse(Variable %in% c("SRAD", "PSEA"), "Climate", 
                                       ifelse(Variable %in% c("ELEV"), "Geo", "Other"))))

library(ggplot2)
p2 <-ggplot() +
  geom_smooth(data = bin_combined, aes(x = Distance, y = Semivariance, color = VariableGroup),
              size = 4, se = FALSE)  +
  scale_colour_manual(values = c("Geo" = "green",  # "Geo" 组用绿色
                                 "Climate" = "lightblue",  # "Climate" 组用浅蓝色
                                 "Sand" = "darkred",  # "Sand" 组用深红色
                                 "Other" = "darkred")) +  # 其他组用深红色
  scale_fill_manual(values = c("Geo" = "green", 
                               "Climate" = "lightblue", 
                               "Sand" = "darkred", 
                               "Other" = "darkred"))+
  labs(x = "Distance (m)", y = "Semivariance", ) + 
  theme_publication()+
theme(
    text = element_text(size = 14, face = "bold"),  # 加粗字体
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 14, face = "bold"),  # 加粗坐标轴的数字
    axis.title = element_text(size = 14, face = "bold"),  # 加粗坐标轴标题
    legend.background = element_blank(),
    legend.position = c(0.9, 0.1),        # 右下角 (x = 1, y = 0)
    legend.justification = c(0.9, 0.1),    # 图例对齐方式 (右对齐, 下对齐)
    legend.title = element_blank() ,
    legend.key.size = unit(1, "cm"),  # 控制图例项的大小
    legend.direction = "vertical",
    legend.text = element_text(size = 16)  # 增大图例文本的字体
    
    # 每个legend项竖直排列# 移除图例标题
  )
p2
remotes::install_github('jbryer/likert')
library(grid)
library(gtable)
dev.off()

p1
complex_legend <- gTree(children = gList(
  rectGrob(x = 0.2, y = 0.8, width = 0.1, height = 0.1, gp = gpar(fill = "red")),
  textGrob("Red Square", x = 0.4, y = 0.8, just = "left"),
  
  circleGrob(x = 0.2, y = 0.6, r = 0.05, gp = gpar(fill = "blue")),
  textGrob("Blue Circle", x = 0.4, y = 0.6, just = "left"),
  
  linesGrob(x = c(0.15, 0.25), y = c(0.4, 0.4), gp = gpar(col = "green", lwd = 2)),
  textGrob("Green Line", x = 0.4, y = 0.4, just = "left")
))


Get_plot_guides = function(){
  
  # Set scalings manual
  res = c('Younger\nadults' = '#999999',
          'Older\nadults' = '#56B4E9',
          'Low' = '#0072B2',
          'Mid' = '#009E73',
          'High' = '#E69F00',
          'Free\nchoices' = 'solid',
          'Guided\nchoices' = 'dashed',
          'RW' = '#A36A2C',
          'Uncertainty' = '#D2C08E',
          'Valence' = '#2586A0',
          'Uncertainty +\nValence' = '#A7544B',
          'Unc+Valence' = '#A7544B',
          'Surprise' = '#B7C7B8',
          'Uncertainty +\nSurprise' = '#2F4858',
          'Unc+Surprise' = '#2F4858',
          'Valence+Surprise' = '#bf9fcc')
  
  return(res)
  
}
}
library(ggplot2)
  
  # 创建一些示例数据
  data <- data.frame(
    x = c(1, 2, 3),
    y = c(3, 2, 1),
    category = c("A", "B", "C")
  )
  
  # 创建一个空的图形，只显示图例
  p <- ggplot(data, aes(x = x, y = y, color = category)) +
    geom_point() +  # 绘制一些点
    theme_void() +  # 移除背景和坐标轴
    theme(legend.position = "right") +  # 显示图例
    guides(color = guide_legend(title = "Category Legend"))  # 只保留图例
  
  print(p)
  

p22
library(patchwork)

# 假设你有两个图 ggplot1 和 ggplot2
# 使用 patchwork 拼接图形
p1 <- ggplot2::ggplotGrob	(p2) |>  
  gtable::gtable_filter("guide") |>  
  grid.draw()  # 在当前绘图页面上绘制这个图例

plot11 + p113  # 默认是并排显示
# 导出用命令还是不OK
ggsave("var.png", plot = last_plot(), dpi = 300, width = 12, height = 8)
