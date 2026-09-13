# fcmsafety

食品接触材料（FCM）化学物质安全筛查的 R 包：以本地 SQLite 法规库为基础，
对化合物做 SVHC / CMR / IARC / EU SML / China SML 等法规清单匹配。
本文件是项目的术语表（glossary）。

## Language（语言）

### 分类工具与规则

**Toxtree**：
JRC 委托 Ideaconsult 开发的开源（GPL 2.0）毒理分类软件，按决策树对化合物做结构规则分类。
_Avoid_：ToxTree（大小写混用）、toxtree 软件之外的泛指

**Cramer rules**：
1978 年 Cramer 决策树规则，按化学结构把化合物分到三个毒性关注等级。
_Avoid_：Cramer 分类（指结果时）、TTC 规则（相关但不同物）

**Cramer class**：
Cramer 规则的分级结果：Class I（低）/ II（中）/ III（高毒性关注）。
_Avoid_：Cramer 组、Cramer 等级

**headless**：
Toxtree 的无界面命令行运行方式（CLI 参数 `-n`），输入输出都走 CSV 文件。
_Avoid_：batch mode（易与 GUI 菜单里的批处理混淆）

### 化学标识

**SMILES**：
分子结构的线性文本表示，本项目里是 Toxtree 分类的输入标识。
_Avoid_：SMILE、结构式（指图示）

**InChIKey**：
化学结构的固定长度唯一标识符，法规库匹配的主键。
_Avoid_：InChiKey（拼写）、CAS 号做主键（旧库格式不一致）

### 数据流文件

**toxtree_results.csv**：
Toxtree 分类结果文件，`assign_toxicity()` 的输入；关键列 `Cramer.rules`。
_Avoid_：toxtree 输出（泛指）、Cramer rules 列（带空格的原始列名）

**法规库（regulatory databases）**：
SQLite 中的 SVHC / CMR / IARC / EU SML / China SML 等清单表，是匹配结果的唯一事实来源。
_Avoid_：数据库（泛指时）、xlsx 装载旧模式（已退役）
