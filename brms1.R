library(brms)
library(bayesplot)
library(ggplot2)
rm(list=ls())
setwd("C:/Users/r/Desktop/bayes")

file_path <- "data/selection.csv"  # 替换为你的文件路径
selection <- read.csv(file_path)
selection_scaled <- selection

selection_scaled[ , -which(names(selection) == "RATIO")] <- scale(selection[ , -which(names(selection) == "RATIO")])

selection_scaled$Plant.Disease <- selection_scaled$RATIO
selection_scaled <- selection_scaled[, !names(selection_scaled) %in% c("RATIO")]


# 使用标准化后的数据进行拟合
fit <- brm(Plant.Disease ~ ., data =selection_scaled)

# 提取后验分布
posterior <- as.matrix(fit)
params <- colnames(posterior)

# 要排除的参数
exclude_params <- c("LAT", "MAX_MAT", "MIN_MAT", "AVG_MAT", "(Intercept)", "sigma")

# 过滤掉要排除的参数
params_to_plot <- setdiff(params, exclude_params)

# 设置颜色
color_scheme <- c("darkred", "coral")

# 绘制后验分布
plot_title <- ggtitle("Posterior Distributions of Regression Coefficients",
                      "with medians and 80% intervals")

# Create the plot
plot <- mcmc_areas(posterior,
                   pars = params_to_plot,
                   prob = 0.8) + 
  plot_title + 
  theme(
    panel.background = element_rect(fill = "white"),  # Set background to white
    plot.background = element_rect(fill = "white")   # Set entire plot background to white
  ) +
  scale_fill_manual(values = color_scheme)  # Set color for the positive and negative intervals


print(plot)
ggsave("high_res_plot.png", plot = plot, dpi = 300, width = 12, height = 8)
