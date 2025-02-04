import pymc as pm
import numpy as np
import pandas as pd

# 示例数据
data = pd.DataFrame({
    'count': np.random.poisson(lam=5, size=100),
    'age': np.random.randn(100),
    'group': np.random.choice(['A', 'B'], size=100)
})

# 定义混合效应模型
with pm.Model() as model:
    # 先验分布
    alpha = pm.Normal('alpha', mu=0, sigma=10)
    beta = pm.Normal('beta', mu=0, sigma=10)
    sigma = pm.HalfNormal('sigma', sigma=1)
    group_effect = pm.Normal('group_effect', mu=0, sigma=10, shape=2)
    
    # 线性回归模型
    mu = alpha + beta * data['age'] + group_effect[data['group'].map({'A': 0, 'B': 1})]
    
    # 观测模型
    Y_obs = pm.Poisson('Y_obs', mu=mu, observed=data['count'])
    
    # 采样
    trace = pm.sample(2000, tune=1000, return_inferencedata=True)
    
# 查看结果
pm.summary(trace)
