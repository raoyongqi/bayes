# 加载必要的 R 包

library(caret)       # 用于机器学习和交叉验证
library(ggplot2)     # 用于绘图
library(dplyr)       # 用于数据操作
library(tidyr)       # 用于数据整理

# 设置随机种子，保证结果可复现
set.seed(123)

# 加载必要的 R 包
library(caret)       # 用于机器学习和交叉验证
library(ggplot2)     # 用于绘图
library(dplyr)       # 用于数据操作
library(tidyr)       # 用于数据整理

# 设置随机种子，保证结果可复现
set.seed(123)
setwd("C:/Users/r/Desktop/bayes")

# 读取数据
file_path = "C:/Users/r/Desktop/bayes/data/selection.csv"  # 替换为你的文件路径
data <- read.csv(file_path)

# 定义 10 折 10 重复交叉验证
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

# 选取一个响应变量作为示例 (你可以换成其他变量)
response_var <- 'RATIO'
formula <- as.formula(paste(response_var, "~ ."))

# 定义模型列表
model_names <- c(
  "cubist", "rf", "xgbLinear", "gamSpline", "glmnet", 
  "penalized", "lm", "gaussprPoly", "rqnc", "BstLm", "simpls", 
  "widekernelpls", "pcr"
)

# 创建一个空的列表来保存训练的模型
models <- list()

# 循环训练每个模型
for (model_name in model_names) {
  # 定义模型文件名
  model_file <- paste0(model_name, "_model.Rdata")
  
  # 检查模型文件是否已存在
  if (file.exists(model_file)) {
    cat("Model", model_name, "already exists. Skipping training.\n")
    # 加载现有模型
    load(model_file)
    models[[model_name]] <- model  # 将加载的模型存储到列表中
  } else {
    # 训练模型如果不存在
    model <- train(formula, data = data, method = model_name, trControl = train_control)
    # 保存训练的模型到文件
    save(model, file = model_file)
    cat("Model", model_name, "trained and saved.\n")
    models[[model_name]] <- model  # 将训练的模型存储到列表中
  }
}



# 确保模型顺序与模型列表的顺序一致
save(models, file = "models.Rdata")

# 提取模型评估结果（R² 和 RMSE）
results <- resamples(models)
dotplot(results)
summary_stats <- summary(results)
str(summary_stats$statistics)
library(dplyr)
library(tidyr)
# 将数据转换为长格式
library(ggthemes)

# Check if summary_stats is a list or data frame
str(summary_stats)

# If it's a list, convert it into a data frame
print(summary_stats)
summary_stats
summary_stats[[1]]
stats_r <-  as.data.frame(summary_stats$statistics$Rsquared)
stats_r
stats_r$metric <- "R-squared"
stats_r$model <- row.names(stats_r)
stats_r <- stats_r %>%
  mutate(model = reorder(model, Mean, FUN = median))  # 根据 Mean 排序
stats_RMSE <-  as.data.frame(summary_stats$statistics$RMSE)
stats_RMSE$metric <- "RMSE"
stats_RMSE$model <- row.names(stats_r)
stats_RMSE
stats_RMSE <- stats_RMSE %>%
  mutate(model = reorder(model, Mean, FUN = median))  # 根据 Mean 排序

# 假设 stats_RMSE 和 stats_r 行数相同
summ_stats <- rbind(stats_RMSE,stats_r)
summ_stats


# 查看结果
theme_publication <- function(base_size = 12, base_family = "Helvetica", ...) {
  require(grid)
  require(ggthemes)
  (theme_foundation(base_size = base_size, base_family = base_family)
    + theme(plot.title = element_text(face = "bold", size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(color = NA),
            plot.background = element_rect(color = NA),
            panel.border = element_rect(color = "black", size = 1),
            axis.title = element_text(face = "plain", size = rel(1)),
            axis.title.y = element_text(angle=90, vjust = 2, margin = margin(r=7)),
            axis.title.x = element_text(vjust = -0.2, margin = margin(t=10)),
            axis.text = element_text(size = rel(0.9)),
            #axis.line.y = element_line(color="black"),
            #axis.line.x = element_line(color="black"),
            axis.ticks = element_line(),
            panel.grid.minor = element_blank(),
            panel.grid.major.y = element_line(size=.5, color="#f0f0f0"),
            # explicitly set the horizontal lines (or they will disappear too)
            panel.grid.major.x = element_blank(),
            panel.spacing = unit(0.1, "lines"),
            #legend.key = element_rect(color = NA),
            legend.position = "none",
            #legend.direction = "horizontal",
            legend.key.size = unit(0.5, "cm"),
            legend.spacing = unit(0, "cm"),
            #legend.title = element_text(face="italic"),
            legend.text = element_text(size = 8),
            plot.margin = unit(c(10,5,5,5),"mm"),
            # strip.text = element_blank(),
            strip.background = element_blank()
    ))
}

rownames(summ_stats) <- NULL
summ_stats
scale_fill_publication <- function(...){
  require(scales)
  discrete_scale("fill", "Publication", manual_pal(values =
                                                     c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
}

scale_color_publication <- function(...){
  require(scales)
  discrete_scale("colour", "Publication", manual_pal(values =
                                                       c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
}


# --------------------------------------------------------------------------------------------------------------------------------
# functions

simpleCap <- function(x) {
  s <- tolower(x)
  s <- strsplit(s, " ")[[1]]
  paste0(toupper(substring(s, 1,1)), substring(s, 2), collapse=" ")
}

# tools::toTitleCase(tolower(x))

rm_box_outliers <- function(dat, form, limit_var, by_vars) {
  # get boxplot outlier limits
  form <- formula(paste(limit_var, "~", paste(by_vars, collapse = " + ")))
  box_stats <- boxplot(form, data = dat, plot = FALSE)
  sts_df <- data.frame(t(rbind(box_stats$stats[1, ], box_stats$stats[5, ])))
  colnames(sts_df) <- c("lower", "upper")
  sts_df$level <- box_stats$names
  # exclude data outside of limits
  dat$level <- interaction(dat[, by_vars])
  dat <- merge(dat, sts_df, by = "level", all.x = TRUE)
  dat_split <- split(dat, f = dat$level)
  dat_split <- lapply(dat_split, function(x) {
    x[x[, limit_var] >= x[, "lower"] & x[, limit_var] <= x[, "upper"], ]})
  dat_limits <- do.call(rbind.data.frame, dat_split)
  rownames(dat_limits) <- NULL
  return(dat_limits)
}
summ_stats <- summ_stats %>%
  mutate(model = reorder(model, Mean, FUN = min))  # 根据 Mean 排序

summ_stats
cv_comp <- ggplot(summ_stats, aes(x = model, y = Mean)) +
  geom_point(size = 1.4, shape = 16) +  # 绘制均值点
  geom_linerange(aes(ymin = Min., ymax = Max.)) +  # 绘制误差线
  coord_flip() +  # 翻转坐标轴，使模型名在 y 轴
  facet_wrap(~ metric, scales = "free_x") +  # 按不同的指标（RMSE, R²）分面
  labs(x = "", y = "Predictive Accuracy") +  # 设置轴标签
  theme_publication() +  # 使用清爽的主题
  theme(
    axis.title.x = element_text(size = 10, margin = margin(t = 5, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 10, margin = margin(t = 0, r = -3, b = 0, l = 0)),
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    strip.text = element_text(size = 7),
    panel.border = element_rect(size = 0.5, fill = NA),  # 添加边框
    panel.grid.major.y = element_blank(),  # 移除 y 轴网格线
    panel.grid.major.x = element_line(size = 0.5, color = "#f0f0f0"),  # 添加浅色 x 轴网格线
    plot.margin = unit(c(0, 0.1, 0.2, 0), "cm")  # 调整边距
  )


ggsave("cv_comp_plot.png", plot = cv_comp, width = 8, height = 6, dpi = 600)

