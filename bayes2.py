import pandas as pd
import numpy as np
from scipy.stats import spearmanr
import re
# 文件路径
file_path = "C:/Users/r/Desktop/bayes/data/selection.csv"  # 替换为你的文件路径

# 读取数据
selection = pd.read_csv(file_path)
# 删除 'RATIO' 列

selection['PL'] =selection['RATIO']
selection = selection.drop(columns=['RATIO'])
selection.columns = selection.columns.map(lambda x: x.replace('_', r'\par '))
# 计算 Spearman 相关系数矩阵和 p 值矩阵
corr_matrix = selection.corr(method='spearman')  # Spearman 相关系数矩阵
p_matrix = selection.corr(method=lambda x, y: spearmanr(x, y)[1])  # p 值矩阵

# 格式化相关系数矩阵
formatted_matrix = corr_matrix.copy()
for i in range(len(corr_matrix)):
    for j in range(len(corr_matrix.columns)):
        # 保留两位小数并添加显著性标记
        value = f"{corr_matrix.iloc[i, j]:.2f}"
        
        # 检查相关系数是否为负数，若为负数则加粗
        if corr_matrix.iloc[i, j] < 0:
            value = f"\\textbf{{{value}}}"
        
        # 添加显著性标记
        if p_matrix.iloc[i, j] < 0.05:
            value += "*"
        
        formatted_matrix.iloc[i, j] = value

# 创建掩码，只显示下三角部分
mask = np.triu(np.ones_like(corr_matrix, dtype=bool), k=0)  # k=0 包括对角线

# 提取下三角部分的相关系数（不包括对角线）
lower_triangle_corr = formatted_matrix.where(~mask)

heatmap_data = lower_triangle_corr.dropna(how='all', axis=0).dropna(how='all', axis=1)
formatted_df = heatmap_data.fillna("").astype(str)



output_file = "C:/Users/r/Desktop/bayes/formatted_correlation_matrix.xlsx"  # 选择输出文件路径
formatted_df.to_excel(output_file, index=False)

modified_columns = []
for item in formatted_df.columns:
    parts = re.split(r'\\par', item)
    bold_parts = [f"\\textbf{{{part.strip()}}}" for part in parts]
    modified_text = " \\par ".join(bold_parts)
    modified_columns.append(modified_text)
print(modified_columns)
formatted_df.columns = modified_columns
formatted_df.index = formatted_df.index.map(lambda x: re.sub(r'\\par', '', x))

formatted_df.index = formatted_df.index.map(lambda x: f"\\textbf{{{x}}}")
# 输出为Excel文件

print(f"Excel file has been saved at: {output_file}")
latex_table = formatted_df.to_latex(
    index=True,
    header=True,
    escape=False,  # 允许 LaTeX 命令（如 \textbf）不被转义
    caption="Spearman Correlation Matrix with Significance Markers",  # 可选标题
    label="tab:correlation_matrix",  # 可选标签,
)

# 在 \begin{tabular} 之前插入 \hspace*{-3.17cm}
latex_table_with_space = latex_table.replace("\\begin{tabular}", "\\tiny \n\\hspace*{-3.17cm}\n \\begin{tabular}")

# 将表格按行分割
latex_lines = latex_table_with_space.splitlines()


# 假设你有一个名为 `selection` 的 DataFrame，你要生成 LaTeX 表格
for i, line in enumerate(latex_lines):
    if '\\begin{tabular}' in line:
        # 创建一个新的列格式字符串：第一个列（索引列）宽度为 'p{2cm}'，后续列的宽度为 'p{1cm}'
        # 可以根据需要调整 'p{2cm}' 和 'p{1cm}' 的值
        latex_lines[i] = '\\begin{tabular}{' + 'p{1.6cm}' + 'p{0.68cm}' * (len(selection.columns)-1) + '}'


for i, line in enumerate(latex_lines):
    if '\\toprule' in line:
        # 在紧跟着 \toprule 的下一行插入 "Spearman r &"

        latex_lines[i + 1] = "\\multicolumn{1}{c}{Spearman r} " + latex_lines[i + 1]

# # 重新组合表格
modified_latex_table = "\n".join(latex_lines)

# 保存到文件
output_path = "C:/Users/r/Desktop/bayes/correlation_table.tex"
with open(output_path, "w") as f:
    f.write(modified_latex_table)

print("LaTeX 表格已保存到:", output_path)