import pandas as pd
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_squared_error, r2_score
import joblib  # 用于保存模型
import os  # 用于处理文件和目录
from boruta import BorutaPy
import numpy as np
import random
from boruta import BorutaPy

random.seed(42)
np.random.seed(42)
# 1. 读取Excel文件
file_path = 'data/hand_data.xlsx'  # 替换为你的文件路径
data = pd.read_excel(file_path)
# data.drop(columns=['Province', 'City', 'District'], inplace=True)

data.columns = data.columns.str.lower()

# 找出所有列名中包含下划线的列，并检查前后部分是否相同
data.columns = [col.replace('_resampled', '') if '_resampled' in col else col for col in data.columns]
data.columns = [col.replace('wc2.1_5m_', '') if col.startswith('wc2.1_5m_') else col for col in data.columns]
new_columns = []
for col in data.columns:
    if '_' in col:  # 如果列名中有下划线
        parts = col.split('_')  # 用下划线拆分列名
        if len(parts) > 1 and parts[0] == parts[-1]:  # 如果前后部分相同
            # 将拆分后的第一部分和最后一部分合并
            new_columns.append('_'.join(parts[:1]))  # 取前两个部分作为列名
        elif len(parts) > 2 and parts[1] == parts[-1]:  # 如果前后部分相同
            # 将拆分后的第一部分和最后一部分合并
            new_columns.append('_'.join(parts[:2]))  # 取前两个部分作为列名
        elif len(parts) > 3 and parts[2] == parts[-1]:  # 如果前后部分相同
            # 将拆分后的第一部分和最后一部分合并
            new_columns.append('_'.join(parts[:2]))  # 取前两个部分作为列名
        else:
            new_columns.append(col)  # 否则保留原列名
    else:
        new_columns.append(col)  # 如果没有下划线，直接保留原列名

# 更新 DataFrame 的列名
data.columns = new_columns
# 2. 筛选特征列

# 将所有 'prec_*' 列加总为 MAP
data['MAP'] = data.filter(like='prec_').sum(axis=1)
data['WIND'] = data.filter(like='wind_').mean(axis=1)
data['MAX_MAT'] = data.filter(like='tmax_').mean(axis=1)
data['MIN_MAT'] = data.filter(like='tmin_').mean(axis=1)
data['AVG_MAT'] = data.filter(like='tavg_').mean(axis=1)

data['SRAD'] = data.filter(like='srad_').mean(axis=1)
data['VAPR'] = data.filter(like='vapr_').mean(axis=1)
data['TSEA'] = data['bio_4']
data['PSEA'] =data['bio_15']

# 删除 'prec_*' 列
data = data.drop(columns=data.filter(like='prec_').columns)
data = data.drop(columns=data.filter(like='srad_').columns)
data = data.drop(columns=data.filter(like='tmax_').columns)
data = data.drop(columns=data.filter(like='tmin_').columns)
data = data.drop(columns=data.filter(like='tavg_').columns)
data = data.drop(columns=data.filter(like='vapr_').columns)

data = data.drop(columns=data.filter(like='wind_').columns)
data = data.drop(columns=data.filter(like='bio_').columns)
data.columns = data.columns.str.upper()
data = data.drop(columns=['MU_GLOBAL','REF_DEPTH', 'LANDMASK', 'ROOTS', 'ISSOIL'])

feature_columns = [col for col in data.columns if col != 'RATIO']
# 3. 分离特征变量和目标变量
X = data[feature_columns]
y = data['RATIO']  # 目标变量

# 4. 分割数据集为训练集和测试集
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=42)

# 5. 初始化并训练随机森林回归模型
rf = RandomForestRegressor(n_estimators=100, random_state=42)

# 6. 使用 Boruta 进行特征选择
feat_selector = BorutaPy(rf, n_estimators='auto',max_iter=10, alpha=0.05, random_state=42, verbose=2)


feat_selector.fit(X_train.values, y_train.values)
# 按照ranking_的顺序排序特征名
sorted_features = [feature for _, feature in sorted(zip(feat_selector.ranking_, feature_columns))]
df = data[[*sorted_features[:16]]]
df['RATIO'] = data['RATIO']  # 目标变量

# 保存为 CSV 文件
df.to_csv('data/selection.csv', index=False)
simplified_names = {
    'SRAD': 'Solar Radiation',
    'S_SAND': 'Soil Sand',
    'LON': 'Longitude',
    'VAPR': 'Vapor Pressure',
    'WIND': 'Wind Speed',
    'LAT': 'Latitude',
    'ELEV': 'Elevation',
    'MAX_MAT': 'Maximum Temperature',
    'AVG_MAT': 'Average Temperature',
    'MIN_MAT': 'Minimum Temperature',
    'TSEA': 'Temperature Seasonality',
    'PSEA': 'Precipitation Seasonality',
    'MAT': 'MeanAnnual Temperature',
    'MAP': 'MeanAnnual Precipitation',
    'T_SAND': 'Topsoil Sand',    
    'T_BULK_DEN': 'Topsoil Bulk Density',
    'T_REF_BULK': 'Topsoil ReferenceBulk Density',
    'S_CLAY': 'Soil Clay',
    'S_REF_BULK': 'Soil Reference Bulk Density',
    'T_GRAVEL': 'Topsoil Gravel',
    'lon': 'Longitude',
    'lat': 'Latitude',
    'PL': 'Plant Disease',
    'ratio': 'Ratio',
    'bio_1': 'Bio1',
    'bio_2': 'Bio2',
    'bio_3': 'Bio3',
    'bio_4': 'Bio4',
    'bio_5': 'Bio5',
    'bio_6': 'Bio6',
    'bio_7': 'Bio7',
    'bio_8': 'Bio8',
    'bio_9': 'Bio9',
    'bio_10': 'Bio10',
    'bio_11': 'Bio11',
    'bio_12': 'Bio12',
    'bio_13': 'Bio13',
    'bio_14': 'Bio14',
    'bio_15': 'Bio15',
    'bio_16': 'Bio16',
    'bio_17': 'Bio17',
    'bio_18': 'Bio18',
    'bio_19': 'Bio19',
    'elev': 'Elevation',
    "prec_01": "Precipitation Jan",
    "prec_02": "Precipitation Feb",
    "prec_03": "Precipitation Mar",
    "prec_04": "Precipitation Apr",
    "prec_05": "Precipitation May",
    "prec_06": "Precipitation Jun",
    "prec_07": "Precipitation Jul",
    "prec_08": "Precipitation Aug",
    "prec_09": "Precipitation Sep",
    "prec_10": "Precipitation Oct",
    "prec_11": "Precipitation Nov",
    "prec_12": "Precipitation Dec",
    "srad_01": "SolarRadiation Jan",
    "srad_02": "SolarRadiation Feb",
    "srad_03": "SolarRadiation Mar",
    "srad_04": "SolarRadiation Apr",
    "srad_05": "SolarRadiation May",
    "srad_06": "SolarRadiation Jun",
    "srad_07": "SolarRadiation Jul",
    "srad_08": "SolarRadiation Aug",
    "srad_09": "SolarRadiation Sep",
    "srad_10": "SolarRadiation Oct",
    "srad_11": "SolarRadiation Nov",
    "srad_12": "SolarRadiation Dec",
    "tavg_01": "TemperatureAvg Jan",
    "tavg_02": "TemperatureAvg Feb",
    "tavg_03": "TemperatureAvg Mar",
    "tavg_04": "TemperatureAvg Apr",
    "tavg_05": "TemperatureAvg May",
    "tavg_06": "TemperatureAvg Jun",
    "tavg_07": "TemperatureAvg Jul",
    "tavg_08": "TemperatureAvg Aug",
    "tavg_09": "TemperatureAvg Sep",
    "tavg_10": "TemperatureAvg Oct",
    "tavg_11": "TemperatureAvg Nov",
    "tavg_12": "TemperatureAvg Dec",
    "tmax_01": "MaxTemperature Jan",
    "tmax_02": "MaxTemperature Feb",
    "tmax_03": "MaxTemperature Mar",
    "tmax_04": "MaxTemperature Apr",
    "tmax_05": "MaxTemperature May",
    "tmax_06": "MaxTemperature Jun",
    "tmax_07": "MaxTemperature Jul",
    "tmax_08": "MaxTemperature Aug",
    "tmax_09": "MaxTemperature Sep",
    "tmax_10": "MaxTemperature Oct",
    "tmax_11": "MaxTemperature Nov",
    "tmax_12": "MaxTemperature Dec",
    "tmin_01": "Min Temperature Jan",
    "tmin_02": "Min Temperature Feb",
    "tmin_03": "Min Temperature Mar",
    "tmin_04": "Min Temperature Apr",
    "tmin_05": "Min Temperature May",
    "tmin_06": "Min Temperature Jun",
    "tmin_07": "Min Temperature Jul",
    "tmin_08": "Min Temperature Aug",
    "tmin_09": "Min Temperature Sep",
    "tmin_10": "Min Temperature Oct",
    "tmin_11": "Min Temperature Nov",
    "tmin_12": "Min Temperature Dec",
    "vapr_01": "Vapor Pressure Jan",
    "vapr_02": "Vapor Pressure Feb",
    "vapr_03": "Vapor Pressure Mar",
    "vapr_04": "Vapor Pressure Apr",
    "vapr_05": "Vapor Pressure May",
    "vapr_06": "Vapor Pressure Jun",
    "vapr_07": "Vapor Pressure Jul",
    "vapr_08": "Vapor Pressure Aug",
    "vapr_09": "Vapor Pressure Sep",
    "vapr_10": "Vapor Pressure Oct",
    "vapr_11": "Vapor Pressure Nov",
    "vapr_12": "Vapor Pressure Dec",
    "wind_01": "Wind Speed Jan",
    "wind_02": "Wind Speed Feb",
    "wind_03": "Wind Speed Mar",
    "wind_04": "Wind Speed Apr",
    "wind_05": "Wind Speed May",
    "wind_06": "Wind Speed Jun",
    "wind_07": "Wind Speed Jul",
    "wind_08": "Wind Speed Aug",
    "wind_09": "Wind Speed Sep",
    "wind_10": "Wind Speed Oct",
    "wind_11": "Wind Speed Nov",
    "wind_12": "Wind Speed Dec",
    "awc_class": "AWC Class",
    "awt_s_soc_sum_s_c_1": "AWT Soil Organic Carbon Sum S C 1",
    "awt_t_soc_sum_t_c_12": "AWT Soil Organic Carbon Sum T C 12",
    "issoil": "IS Soil",
    "mu_global": "MU Global",
    "ref_depth": "Reference Depth",
    "roots": "Roots",
    "s_bulk": "Soil Bulk",
    "s_cec": "Soil CEC",
    "s_clay": "Soil Clay",
    "s_c": "Soil C",
    "s_gravel": "Soil Gravel",
    "s_oc": "Soil Organic Carbon",
    "s_ph": "Soil pH",
    "s_ref": "Soil Reference",
    "s_sand": "Soil Sand",
    "s_silt": "Soil Silt",
    "t_bulk": "Topsoil Bulk",
    "t_cec": "Topsoil CEC",
    "t_clay": "Topsoil Clay",
    "t_c": "Topsoil C",
    "t_gravel": "Topsoil Gravel",
    "t_oc": "Topsoil Organic Carbon",
    "t_ph": "Topsoil pH",
    "t_ref": "Topsoil Reference",
    "t_sand": "Topsoil Sand",
    "t_silt": "Topsoil Silt",
    "t_silt": "Topsoil Silt",
    "HAND": "Height Above the Nearest Drainage",

}

# 创建统计量 DataFrame

# Assuming you have a DataFrame df and a dictionary 'simplified_names'
range_df = pd.DataFrame({
    'Variable': df.columns,               # 获取所有列名
    'Mean': df.mean().round(2).astype(str),           # 计算每列的平均值并四舍五入到两位小数，转换为字符串
    'Std Dev': df.std().round(2).astype(str),         # 计算每列的标准差并四舍五入到两位小数，转换为字符串
    'Range': [f"{df[col].min():.2f} - {df[col].max():.2f}" for col in df.columns],  # 将 Min 和 Max 合并为 Range 列，四舍五入到两位小数，转换为字符串
})
# 添加 Simplified Name 列
range_df['Simplified Name'] = range_df['Variable'].map(simplified_names)
range_df  = range_df[['Variable', 'Simplified Name', 'Mean', 'Std Dev', 'Range']]
range_df['Variable'] = range_df['Variable'].map(lambda x: x.replace('_', r' '))
# 生成 LaTeX 三线表格式
latex_table = range_df.to_latex(index=False, header=True, 
                                 caption='Variable, Mean, and Std Dev of Each Column', 
                                 label='tab:range')

# 手动修改 LaTeX 三线表
# latex_table = latex_table.replace(r'\hline', r'\toprule', 1)  # 顶部分隔线
# latex_table = latex_table.replace(r'\\ \hline', r'\\ \midrule', 1)  # 中间分隔线
# latex_table = latex_table.replace(r'\\', r'\\ \bottomrule', 1)  # 底部分隔线

# 打印 LaTeX 表格
units = {
    'SRAD': 'W/m²',
    'HAND': 'm',
    'LON': '°',
    'S SAND': '%',
    'WIND': 'm/s',
    'PSEA': '%',
    'VAPR': 'kPa',
    'ELEV': 'm',
    'MIN MAT': '°C',
    'TSEA': '°C',
    'LAT': '°',
    'AVG MAT': '°C',
    'MAP': 'mm',
    'T SAND': '%',
    'MAX MAT': '°C',
    'S REF BULK': 'g/cm³'
}

# 将单位添加到 Variable 列
range_df['Variable'] = range_df['Variable'].map(lambda x: f"{x} （{units.get(x, 'N/A')}）")

print(latex_table)

# 如果要保存到 .tex 文件中，可以这样做
with open('table_output.tex', 'w') as f:
    f.write(latex_table)
range_df.to_excel('range_df_output.xlsx', index=False)
