# 安装lavaan和semPlot包
install.packages("lavaan")
install.packages("semPlot")

# 加载包
library(lavaan)
library(semPlot)
# 构建SEM模型
model <- ' # 测量模型
              F1 =~ x1 + x2 + x3
              F2 =~ y1 + y2 + y3
              
            # 结构模型
              F2 ~ F1 '

# 拟合模型
fit <- sem(model, data = your_data)  # 替换为实际数据
