library(ggplot2)
library(dplyr)
library(sf)
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
names(df)
var_of_interest = "Pathogen.Load"   # change the variable of interest if needed 
which(names(df) == var_of_interest)

df[,which(names(df) == var_of_interest)]
data_geoR_tmp = data.frame(x = df[,c("UTMx")], y = df[,c("UTMy")], z = df[,which(names(df) == var_of_interest)])

colnames(data_geoR_tmp)
if(var_of_interest !="Pathogen.Load") { data_geoR_tmp$RES = scale(data_geoR_tmp$RES, center = TRUE, scale = TRUE) }
colnames(data_geoR_tmp)


coords <- data.frame(x = data_geoR_tmp$x, y = data_geoR_tmp$y)

# 使用 jitterDupCoords 给重复的点加扰动
coords_jittered <- jitterDupCoords(coords,max=0.01)
data_geoR_tmp$x <-coords_jittered$x
data_geoR_tmp$y <-coords_jittered$y
str(topo)
data_geoR = as.geodata(data_geoR_tmp, coords.col = 1:2, data.col = 3)

str(data_geoR_tmp)
test = dist(data_geoR_tmp[,c("x", "y")])
bla = bin.var(test, bins = 100, method = c("proportions"),labels = NULL)
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
# plot(bin, envelope = env, pch=21, bg="grey", xlab="Distance (m)", ylab="Semivariance", main = "Semivarigoram on selected variable", xlim=c(0,1000*1000))
library(ggplot2)

# 提取半变异数据
library(ggplot2)

# 提取半变异数据
bin_df <- data.frame(
  Distance = bin$u,          # 采样距离
  Semivariance = bin$v       # 计算的半变异值
)

# 提取置信区间数据，并转换为长数据格式
env_df <- data.frame(
  Distance = bin$u,          # 采样距离
  Lower = env$v.lower,       # 置信区间下界
  Upper = env$v.upper        # 置信区间上界
)
env_df
# 使用 ggplot 绘制
plot11 <- ggplot() +
  # 置信区间
  geom_ribbon(data = env_df, aes(x = Distance, ymin = Lower, ymax = Upper, fill = "Pathogen Load"), 
              alpha = 0.1) +  # 使用一个常量值来映射颜色
  # 半变异数据点
  geom_smooth(data = bin_df, aes(x = Distance, y = Semivariance, color = "Pathogen Load"), 
              linewidth = 3, se = FALSE) +
  # 设置主题
  theme_publication() +
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
    fill = guide_legend( title = NULL),  # 去除图例标题
    color = guide_legend( title = NULL)  # 去除图例标题
  ) +
  scale_fill_manual(values = "coral") +  # 设置fill颜色
  scale_color_manual(values = "coral")  # 设置color颜色

plot11


ggsave("var.png", plot = plot, dpi = 300, width = 12, height = 8)

getwd()



env_df
mdist <- dist(df[c("UTMx","UTMy")])
hc <- hclust(mdist, method="complete")
d=500*1000  # the maximum distance between pixels within clusters (150 m * 1000 m = 150 km)                   
df$Clust_500km = cutree(hc, h=d) 
plot(df$UTMx, df$UTMy, col=as.factor(df$Clust_500km), xlab="Longitude", ylab="Latitude")
