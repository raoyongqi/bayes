# 加载必要的 R 包
library(caret)       # 用于机器学习和交叉验证
library(ggplot2)     # 用于绘图
library(dplyr)       # 用于数据操作
library(tidyr)       # 用于数据整理

# 设置随机种子，保证结果可复现
set.seed(123)

# 生成模拟数据 (100 个样本, 7 个响应变量 + 5 个预测变量)
data <- data.frame(
  SpeciesT = rnorm(100),
  GenusT = rnorm(100),
  FamilyT = rnorm(100),
  PhyloD = rnorm(100),
  MeanPhyloD = rnorm(100),
  SES_PhyloD = rnorm(100),
  SES_MeanPhyloD = rnorm(100),
  Predictor1 = rnorm(100),
  Predictor2 = rnorm(100),
  Predictor3 = rnorm(100),
  Predictor4 = rnorm(100),
  Predictor5 = rnorm(100)
)

# 定义 10 折 10 重复交叉验证
train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

# 选取一个响应变量作为示例 (你可以换成其他变量)
response_var <- "SpeciesT"
formula <- as.formula(paste(response_var, "~ ."))

# 训练多个机器学习模型
models <- list(
  RF = train(formula, data = data, method = "rf", trControl = train_control),
  GLM = train(formula, data = data, method = "glm", trControl = train_control),
  GAM = train(formula, data = data, method = "gam", trControl = train_control),
  PLS = train(formula, data = data, method = "pls", trControl = train_control),
  XGB = train(formula, data = data, method = "xgbTree", trControl = train_control),
  Ridge = train(formula, data = data, method = "ridge", trControl = train_control),
  Lasso = train(formula, data = data, method = "lasso", trControl = train_control)
)

# 提取模型评估结果（R² 和 RMSE）
results <- resamples(models)
dotplot(results)
library(dplyr)
library(tidyr)
colnames(summary_df)
# 将数据转换为长格式
library(ggthemes)

# Check if summary_stats is a list or data frame
str(summary_stats)

# If it's a list, convert it into a data frame
print(summary_stats)

stats_r <-  as.data.frame(summary_stats$Rsquared)
stats_r$metric <- "R-squared"
stats_r$model <- row.names(stats_r)
stats_r <- stats_r[order(stats_r$Mean), ]
stats_RMSE <-  as.data.frame(summary_stats$RMSE)
stats_RMSE$metric <- "RMSE"
stats_RMSE$model <- row.names(stats_r)
stats_RMSE
stats_RMSE <- stats_RMSE[order(stats_RMSE$Mean), ]
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
cv_comp <- ggplot(summ_stats, aes(x = model, y = Mean)) +
  geom_point(size = 1.4, shape = 16) +  # 绘制均值点
  geom_linerange(aes(ymin = Min., ymax = Max.)) +  # 绘制误差线
  coord_flip() +  # 翻转坐标轴，使模型名在 y 轴
  facet_wrap(~ metric, scales = "free_x") +  # 按不同的指标（RMSE, R²）分面
  labs(x = "", y = "Predictive Accuracy") +  # 设置轴标签
  theme_minimal() +  # 使用清爽的主题
  theme(
    axis.title.x = element_text(size = 8, margin = margin(t = 5, r = 0, b = 0, l = 0)),
    axis.title.y = element_text(size = 8, margin = margin(t = 0, r = -3, b = 0, l = 0)),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    strip.text = element_text(size = 7),
    panel.border = element_rect(size = 0.5, fill = NA),  # 添加边框
    panel.grid.major.y = element_blank(),  # 移除 y 轴网格线
    panel.grid.major.x = element_line(size = 0.5, color = "#f0f0f0"),  # 添加浅色 x 轴网格线
    plot.margin = unit(c(0, 0.1, 0.2, 0), "cm")  # 调整边距
  )
print(cv_comp)

