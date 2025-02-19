from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import r2_score, mean_squared_error
from hebo.sklearn_tuner import sklearn_tuner
from sklearn.datasets import fetch_california_housing

# 加载数据集
housing = fetch_california_housing()
X, y = housing.data, housing.target  # 获取特征和目标变量

# 定义超参数搜索空间
space_cfg = [
    {'name': 'max_depth', 'type': 'int', 'lb': 1, 'ub': 20},  # 树的最大深度
    {'name': 'min_samples_leaf', 'type': 'int', 'lb': 1, 'ub': 50},  # 叶节点的最小样本数，调整为整数
    {'name': 'max_features', 'type': 'cat', 'categories': ['auto', 'sqrt', 'log2']},  # 特征选择方式
    {'name': 'bootstrap', 'type': 'bool'},  # 是否使用自助采样
    {'name': 'min_impurity_decrease', 'type': 'pow', 'lb': 1e-4, 'ub': 1.0},  # 节点分裂时的最小不纯度减少
]

# 使用 HEBO 自动调优 RandomForestRegressor 的超参数，最大迭代次数为 16
tuner = sklearn_tuner(RandomForestRegressor, space_cfg, X, y, metric=r2_score, max_iter=16)

# 获取优化结果
best_model = tuner.best_model
best_params = tuner.best_params

# 打印最优超参数
print(f'Best hyperparameters found: {best_params}')

# 评估最优模型
y_pred = best_model.predict(X)
print(f'R² score of the best model: {r2_score(y, y_pred):.4f}')
print(f'Mean Squared Error of the best model: {mean_squared_error(y, y_pred):.4f}')
