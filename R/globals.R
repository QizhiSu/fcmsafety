# =============================================================================
# R CMD check 的全局变量声明（唯一目的：消 NOTE）
#
# 为什么需要这个文件：
#   R CMD check 会扫描每个函数体，凡是"读了但没在函数内定义"的名字都报
#   `no visible binding for global variable` NOTE。本项目大量使用 dplyr 的
#   裸列名（mutate(CID = ...)）以及"数据库加载到全局环境"的对象（svhc、
#   cmr 等），check 无法知道它们从哪来，于是逐条报 NOTE。
#   把这些名字集中声明在这里，check 就安静了。
#
# 维护约定：
#   - **只影响 check 静态扫描，不影响运行时行为**。这里少写一个名字不会
#     让代码出错，只会多一条 NOTE；多写一个名字也不会有副作用。
#   - 新增 dplyr 裸列名或全局对象后如果 check 冒出 binding NOTE，
#     把名字加到下面的清单里即可，不需要改业务代码。
#   - 末尾几条带 "\r\n" 的是 xlsx 源表里"列名本身含换行"的原始列名
#     （EU SML 的 SML(T) 表头），必须按原样写才匹配得上，别改成单行。
# =============================================================================

utils::globalVariables(c(
  ".", "CID", "value", "Hazard Statement Code(s)", "<<-",
  "FCM substance No", "Group Restriction No", "InChIKey",
  "IsomericSMILES", "NAME", "CAS", "SMILES", "iarc_meta",
  "Flavornet", "CAS_retrieved", "ExactMass", "Cramer_rules",
  "Toxic_level", "Toxic_level_basis",
  "Group_hits", "Group_IARC", "Group_review",
  "eu_sml_meta", "china_sml_meta", "SML", "SML_group",
  "SML\r\n                     [mg/kg]",
  "SML (T)\r\n                     [mg/kg]",
  "SML(T)\r\n                     [mg/kg]\r\n                     (Group restriction No)",
  "Formula", "ExactMass", "MolecularFormula",
  # 数据库加载到全局环境的对象 + 元数据对象
  "svhc", "cmr", "cmr_suspect", "iarc", "eu_sml", "eu_sml_group",
  "edc", "china_sml", "China_SML", "China_SML_group",
  "svhc_meta", "cmr_meta", "cmr_suspect_meta", "edc_meta"
))
