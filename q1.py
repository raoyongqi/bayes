import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import r2_score
from sklearn.model_selection import GridSearchCV
# 设置随机种子
random_state = 42

# 读取数据
file_path = "data/selection.csv"  # 替换为你的文件路径
selection = pd.read_csv(file_path)

# 处理自变量和因变量
X = selection.drop(columns='RATIO')
y = selection['RATIO']

# 数据集划分
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.1, random_state=random_state)

# 训练 QRF 模型（使用随机森林回归器模拟 QRF）
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import r2_score

# 设置随机种子
random_state = 42

# 读取数据
file_path = "data/selection.csv"  # 替换为你的文件路径
selection = pd.read_csv(file_path)

# 处理自变量和因变量
X = selection.drop(columns='RATIO')
y = selection['RATIO']

# 数据集划分
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.1, random_state=random_state)

# 训练 QRF 模型（使用随机森林回归器模拟 QRF）
param_grid = {
    'n_estimators': [100, 200, 300],
    'max_depth': [5, 10, 20, None],
    'min_samples_split': [2, 5, 10],
    'min_samples_leaf': [1, 3, 5],
    'max_features': ['auto', 'sqrt']
}

grid_search = GridSearchCV(RandomForestRegressor(random_state=random_state, n_jobs=-1), 
                           param_grid, cv=5, scoring='r2', verbose=1)
grid_search.fit(X_train, y_train)

# 最优参数
best_params = grid_search.best_params_
print("Best parameters:", best_params)

# 使用最优参数训练模型
qrf_best = RandomForestRegressor(**best_params, random_state=random_state)
qrf_best.fit(X_train, y_train)

# 计算 R²
y_pred_best = qrf_best.predict(X_test)
r2_best = r2_score(y_test, y_pred_best)
print(f'Optimized R² Score: {r2_best:.4f}')
