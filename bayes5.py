import pandas as pd
import numpy as np
import pymc as pm
import matplotlib.pyplot as plt

# 设置工作目录并加载数据
file_path = "selection.csv"  # 替换为你的文件路径
selection = pd.read_csv(file_path)

# 对自变量进行标准化，保持因变量不变
X = selection.drop(columns='RATIO')
y = selection['RATIO']

X_scaled = (X - X.mean()) / X.std()  # 标准化自变量

# 使用PyMC v5构建贝叶斯线性回归模型
with pm.Model() as model:
    # 定义先验
    intercept = pm.Normal('intercept', mu=0, sigma=10)
    coefficients = pm.Normal('coefficients', mu=0, sigma=10, shape=X_scaled.shape[1])
    sigma = pm.HalfNormal('sigma', sigma=1)

    # 线性回归模型
    mu = intercept + pm.math.dot(X_scaled, coefficients)
    
    # 定义似然函数
    likelihood = pm.Normal('RATIO', mu=mu, sigma=sigma, observed=y)

    # 采样，限制为单核
    trace = pm.sample(2000, cores=1, return_inferencedata=False)

# 绘制后验分布图
pm.plot_trace(trace, var_names=['intercept'] + [f'coefficients[{i}]' for i in range(X_scaled.shape[1])])
plt.show()

# 计算回归系数的中位数、标准误差和95%置信区间
summary = pm.summary(trace, var_names=['intercept'] + [f'coefficients[{i}]' for i in range(X_scaled.shape[1])])

# 打印回归系数的中位数、标准误差和95%置信区间
print(summary[['mean', 'hdi_2.5%', 'hdi_97.5%', 'sd']])

# 计算每个自变量的贡献度（即回归系数的绝对值）
contribution = np.abs(trace['coefficients']).mean(axis=0)

# 打印每个自变量的贡献度和标准误差
contribution_df = pd.DataFrame({
    'Variable': X_scaled.columns,
    'Contribution': contribution,
    'Est.Error': np.std(trace['coefficients'], axis=0)
})

# 按贡献度降序排列并打印结果
contribution_df = contribution_df.sort_values(by='Contribution', ascending=False)
print(contribution_df)
