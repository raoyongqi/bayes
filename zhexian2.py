import pandas as pd
import matplotlib.pyplot as plt

# 读取 RMSE 和 R² 结果
rmse_df = pd.read_csv("rmse_results.csv", index_col="ntree")
r2_df = pd.read_csv("r2_results.csv", index_col="ntree")

# 获取 ntree 和 mtry 值
ntree_values = rmse_df.index
mtry_values = rmse_df.columns.astype(int)  # 确保 mtry 以整数形式显示

# 创建折线图
fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14, 6))

# 绘制 RMSE 折线图
for mtry in mtry_values:
    ax1.plot(ntree_values, rmse_df[str(mtry)], label=f'mtry={mtry}')
ax1.set_title('RMSE vs ntree and mtry')
ax1.set_xlabel('ntree')
ax1.set_ylabel('RMSE')
ax1.legend(title='mtry')

# 绘制 R² 折线图
for mtry in mtry_values:
    ax2.plot(ntree_values, r2_df[str(mtry)], label=f'mtry={mtry}')
ax2.set_title('R² vs ntree and mtry')
ax2.set_xlabel('ntree')
ax2.set_ylabel('R²')
ax2.legend(title='mtry')

# 调整布局
plt.tight_layout()
plt.savefig("ntreemtry.png")  # 你可以选择不同的文件格式，例如 .png, .jpg, .pdf, .svg
# 显示图表
plt.show()
