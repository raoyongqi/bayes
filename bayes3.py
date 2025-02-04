import re

text = "S\\par SAND"
# 使用正则表达式拆分字符串
parts = re.split(r'\\par', text)

# 对拆分后的部分加粗
bold_parts = [f"\\textbf{{{part.strip()}}}" for part in parts]

# 用 \par 重新连接
modified_text = " \\par ".join(bold_parts)

print(modified_text)
