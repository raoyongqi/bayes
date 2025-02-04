import pandas as pd
import numpy as np
from sklearn.metrics import pairwise_distances
from scipy.spatial.distance import squareform
from scipy.stats import pearsonr

file_path = "C:/Users/r/Desktop/bayes/selection.csv"  # 替换为你的文件路径

# 读取数据
df = pd.read_csv(file_path)

# 删除 'RATIO' 列
df['Plant Disease'] = df['RATIO']
df = df.drop(columns=['RATIO'])

# 查看数据的前几行
print("Data preview:")
print(df.head())

# 假设 CSV 文件中有多个变量和一个因变量
# 选择所有自变量（除去因变量）
X = df.drop(columns=['Plant Disease'])

# 提取因变量
y = df['Plant Disease'].values.reshape(-1, 1)

# 定义 Mantel 检验函数
def mantel_test(dist_matrix_1, dist_matrix_2):
    # 将距离矩阵转换为 1D 向量
    dist_vector_1 = squareform(dist_matrix_1)
    dist_vector_2 = squareform(dist_matrix_2)
    
    # 计算 Pearson 相关系数及 p 值
    correlation, p_value = pearsonr(dist_vector_1, dist_vector_2)
    return correlation, p_value

# 创建一个列表来存储结果
results = []

# 遍历每个自变量，计算与因变量的 Mantel r
for col in X.columns:
    # 计算自变量与因变量的距离矩阵
    X_col = X[[col]].values.reshape(-1, 1)
    
    # 计算自变量和因变量的距离矩阵
    dist_matrix_X = pairwise_distances(X_col, metric="euclidean")
    dist_matrix_y = pairwise_distances(y, metric="euclidean")
    
    # 强制将距离矩阵对称
    dist_matrix_X = (dist_matrix_X + dist_matrix_X.T) / 2
    dist_matrix_y = (dist_matrix_y + dist_matrix_y.T) / 2
    
    # 将对角线元素设为零
    np.fill_diagonal(dist_matrix_X, 0)
    np.fill_diagonal(dist_matrix_y, 0)
    
    # 执行 Mantel 检验
    correlation, p_value = mantel_test(dist_matrix_X, dist_matrix_y)
    
    # 保留两位小数
    correlation = round(correlation, 2)
    p_value = round(p_value, 2)
    
    # 将显著的 p-value 加粗显示
    significance = "Significant" if p_value < 0.05 else "Not Significant"
    
    # 将结果添加到列表中
    results.append({
        "spec.variable": "Plant Disease",  # 因变量的列名固定为 "PL"
        "variable": col,    # 当前自变量列名
        "Mantel r": correlation,
        "p-value": f"\\textbf{{{p_value}}}" if p_value < 0.05 else p_value,  # 显著的 p-value 加粗
        "Significance": significance
    })

# 将结果转换为 DataFrame
mantel_results = pd.DataFrame(results)

# 打印 Mantel 检验结果
print("\nMantel Test Results:")
print(mantel_results)
simplified_names = {
    'SARD': 'Solar Radiation',
    'S_SAND': 'Soil Sand',
    'LON': 'Longitude',
    'VAPR': 'Vapor Pressure',
    'WIND': 'Wind Speed',
    'LAT': 'Latitude',
    'ELEV': 'Elevation',
    'MAX_MAT': 'Maximum Temperature',
    'AVG_MAT': 'Average Temperature',
    'MIN_MAT': 'Minimum Temperature',
    'MAP': 'MeanAnnual Precipitation',
    'T_SAND': 'Topsoil Sand',
    'MU_GLOBAL': 'MU Global',
    'T_REF_BULK': 'Topsoil ReferenceBulk Density',
    'S_CLAY': 'Soil Clay',
    'S_REF_BULK': 'Soil Reference Bulk Density',
    'T_GRAVEL': 'Topsoil Gravel',
    'lon': 'Longitude',
    'lat': 'Latitude',
    'PL': 'Plant Disease',

}

# 创建统计量 DataFrame


# 添加 Simplified Name 列
mantel_results['env.variable'] = mantel_results['variable'].map(lambda x: x.replace('_', ' '))
# 打印 Mantel 检验的三线表 LaTeX 格式

mantel_results['Mantel r'] = mantel_results['Mantel r'].apply(lambda x: f"{x:.2f}")
mantel_results['p-value'] = mantel_results['p-value'].apply(lambda x: f"{x:.2f}" if isinstance(x, float) else x)


latex_table = mantel_results[["spec.variable", "env.variable", "Mantel r", "p-value"]].to_latex(
    index=False, 
    header=True, 
    column_format='cccc',  # 修改为居中对齐
    escape=False
)

# 修改 LaTeX 输出为三线表格式
latex_table = latex_table.replace(r'\begin{tabular}{|l|l|l|l|}', r'\begin{tabular}{l l l l}')
latex_table = latex_table.replace(r'\hline', r'\toprule', 1)  # Top rule for the table header
latex_table = latex_table.replace(r'\hline', r'\midrule', 1)  # Mid rule for the body of the table
latex_table = latex_table.replace(r'\hline', r'\bottomrule', 1)  # Bottom rule for the table footer

# 添加标题
latex_table_with_title = r"\begin{table}[H]" + "\n"
latex_table_with_title += r"\centering" + "\n"
latex_table_with_title += r"\caption{Mantel Test Results for Specimens and Environmental Variables}" + "\n"
latex_table_with_title += latex_table
latex_table_with_title += r"\end{table}"

# 打印带标题的 LaTeX 三线表
print("\nMantel Test Results (Three-line LaTeX Table with Title):")
print(latex_table_with_title)

# 可选：将带标题的 LaTeX 三线表保存到文件
with open("mantel_test_results_three_line_with_title.tex", "w") as file:
    file.write(latex_table_with_title)
