# 0010 - IARC 组条目的识别与 (see X) 交叉引用

- 状态：已采纳
- 日期：2026-09-10
- 相关：[0009](0009-20260910-iarc-group-element-judge.md)（组条目的元素判据）、[0008](0008-20260910-report-export-and-failure-visibility.md)（组条目接入定级，默认关闭）

## 背景

用户拿 Progesterone 提问："它属于哪个法规的哪个组别？"

用它自己的 InChIKey（`RJKFOVLPORLFTN-LEKSSAKUSA-N`）跑完整流水线，七个数据源全部
0 命中，`Toxic_level = "-"`。当时据此答复"确实没有证据"。**这个答复是错的**：
Progesterone 属于 IARC 的 **Progestins 组，2B 级**（Suppl 7, 1987）。

`Progestins` 这条就在 `iarc` 表里：

```
agent = 'Progestins'   group_classification = '2B'   cas_no = NULL
InChIKey = 'PSGAAPLEWMOORI-PEINSRQWSA-N'   -- 与 Medroxyprogesterone acetate 完全相同
```

漏掉的**不是元素层判据**，而是**组条目的识别**：`query_iarc_group_registry()` 用一条
手写名称正则筛组条目：

```r
"(compounds|and its (salts|decay products)|metal without|metallic|fibres|fibers|dust|Cyclamates?|salts\\b)"
```

`Progestins` 一个词都不沾，于是它**从未进过注册表**。连 `layer = "manual"`（"这条
拿不出判据"）的标记都没打上 —— 那个标记只对已经入选的条目生效。它是**隐形**的，
不是"识别到但判不了"。

## 组条目的实际覆盖率

逐条人工核对 `iarc` 表（859 行 / 835 行有分组）后：

| 项 | 数 |
|---|---|
| 旧正则认出 | 24 |
| 名字启发式再扫出的真·类条目 | 15 |
| 实际类条目 | **39** |

漏掉的 15 条里有四条落在 1/2A 级，直接影响定级：

`Aflatoxins`(1)、`Polychlorinated biphenyls`(1)、`MOPP and other combined chemotherapy including alkylating agents`(1)、`Polybrominated biphenyls`(2A)、`Progestins`(2B)、`Hexachlorocyclohexanes`(2B)、`Bleomycins`(2B)、`Nodularins`(3)、`Sulfites`(3)、`Polyurethane foams`(3)、`Silica, amorphous`(3)、`Styrene-acrylonitrile copolymers`(3)、`Styrene-butadiene copolymers`(3)、`Vinyl chloride-vinyl acetate copolymers`(3)、`Vinylidene chloride-vinyl chloride copolymers`(3)

其中 `Aflatoxins`、`Polychlorinated biphenyls` 与食品接触材料直接相关
（纸/纸板霉菌毒素、再生纸污染）。

## Decision 1：识别改为三源并集，且人工表必须显式

```r
is_group <- grepl(.group_pattern, raw$agent, ignore.case = TRUE) |
            no_cas |                                  # cas_no 为空
            raw$agent %in% .iarc_extra_group_entries  # 人工核定表
is_group[raw$agent %in% .iarc_force_single] <- FALSE
```

- **不用模糊词法启发式。** "以 s 结尾的短名词"这条规则能找到 15 条真类条目，但同时
  会吞进 `Dichlorvos`、`Tetrachlorvinphos`、`Blue VRS` 三个具体物质。误收本身无害
  （它们抽不出特征元素，会落成 `layer = "manual"` 被跳过），但注册表是给人读的
  待办清单，往里塞噪音就失去了意义。宁要一份显式、可审、可追加的清单。
- **`cas_no` 为空单独成一条判据**：类条目通常没有唯一 CAS。当前 6 行空 CAS 里，
  4 行本来就是类条目，`Hypochlorite salts` 已被正则覆盖，唯一例外是 `Arecoline`
  —— 槟榔碱是具体物质，只是源表 CAS 列空着，用 `.iarc_force_single` 排除。

**这条决策不改变任何定级结果。** 15 条里 14 条抽不出特征元素（`layer = "manual"`，
`screen_iarc_groups()` 直接跳过），1 条 `Silica, amorphous` 抽到 Si 但 Si 在
`.skeletal_elements` 里，护栏一会把它压到 `manual_review`。补上它们的作用是让
"哪些类条目还没有自动判据"从隐形变成显式可见 —— 这是后续接成员清单 / 结构骨架层
的前提。**Progesterone 仍然返回 `-`。**

## Decision 2：读 (see X)，但只填空缺

`iarc` 表里有 **36 行**写成 `X (see Y)`，这是 IARC 官方的归属声明：X 的评价挂在
条目 Y 下面。此前没有任何代码读它。

先纠正一个数字：这 36 行里 `group_classification` 为空的 **24 行**，但它们**不是都
查不到**。`summarise_iarc_groups()` 按 InChIKey 汇总并取最严，而其中 **14 行与一条
已分级的行共用 InChIKey**（`Acetaminophen` 与 `Paracetamol (Acetaminophen)`、
`Bis(2-ethylhexyl) phthalate` 与 `Di(2-ethylhexyl)phthalate`、`Mustard gas` 与
`Sulfur mustard`、`Myleran` 与 `Busulfan` …），早就被"同键兄弟行"救回来了。
**真正静默的只有 10 行。**

解析规则：取**最后一个** `(see` / `(See also` 之后、**最外层收尾右括号之前**的内容。
目标名自身可能带括号（`Bis(chloromethyl)ether; chloromethyl methyl ether`），按第一个
`(see` 或最后一个 `)` 切都会切错。

比对前做名称归一化（去大小写、空格、连字符、括号、分号等），因为库里同一个条目
写法不统一：`Di(2-ethylhexyl) phthalate` 与 `Di(2-ethylhexyl)phthalate` 只差一个空格。
按原样比对，可解析数从 14 掉到 11。

**别名只填空缺，不覆盖已有分组：**

```r
covered <- unique(raw$InChIKey[has_cls & key_ok])
# 该行的 InChIKey 只要在 covered 里，就不补
```

理由：同一个 InChIKey 上可能叠着多条**不同**条目 —— 滑石的三种形态
（含石棉纤维 / 不含 / 用于会阴部爽身粉）、三种碳纳米管（MWCNT-7 / 其他 MWCNT /
SWCNT）都各自共用滑石和碳的键。别名只说明"**这一行**的评价挂在别处"，把它补到
键上等于给同键的全部物质硬套一个不属于它们的分级，比留空更糟。

实测：24 行静默中 **1 行**可补 —— `Gallium arsenide (see Arsenic and inorganic
arsenic compounds)` → 组 1，端到端从 `-` 变为 **V 级**（`group_membership = FALSE`
下也成立，确认是别名单独起作用）。

### 剩下 9 行为什么补不了

目标条目**根本不在 `iarc` 表里**：

| 源行 | 指向 | 缺 |
|---|---|---|
| `Iodine-131` | Radioiodines | 条目缺失 |
| `Strontium-90` | Fission products | 条目缺失 |
| `Chloromethyl methyl ether` | Bis(chloromethyl)ether | 条目缺失 |
| `CI Direct Black 38` / `CI Direct Blue 6` | Benzidine, dyes metabolized to | 条目缺失（库里只有取代联苯胺 `Benzidine` 组 1） |
| `Aldrin` / `Dieldrin` | Dieldrin, and aldrin metabolized to dieldrin | 条目缺失 |
| `Strong-inorganic-acid mists containing sulfuric acid` | Acid mists | 条目缺失 |
| `1,2:3,4-Diepoxybutane` | Monographs on 1,3-Butadiene | 指向的是**专著卷**，不是条目；即便库里有 `1,3-Butadiene`（组 1）也不能套用 |

这是**上游数据缺口**，代码层面无解，需要把目标条目补进 `iarc` 表。

## 已知未修的相邻问题

排查过程中发现三条**误分级**，成因同样是 InChIKey 共用，但属于"同键兄弟行"路径
而非别名路径，本轮未动：

| 条目 | 官方分级 | 当前得到 | 成因 |
|---|---|---|---|
| `Multiwalled carbon nanotubes other than MWCNT-7` | 3 | **2B**（偏高） | 三条碳纳米管条目共用碳的键，取最严取到 MWCNT-7 |
| `Single-walled carbon nanotubes` | 3 | **2B**（偏高） | 同上 |
| `Talc containing asbestiform fibres` | 1（并入 Asbestos） | **2B**（偏低） | 与"含滑石爽身粉"共用键，且 `Asbestos` 条目缺失 |

更广的一类：聚合物条目与其单体共键（`Vinyl chloride`(1) 与 `Polyvinyl chloride`(3)、
`Styrene`(2A) 与 `Polystyrene`(3)、`Acrylic acid`(3) 与 `Polyacrylic acid`(3) 等
共 28 组）。查 PVC 时按 InChIKey 会同时命中两行并取最严 → 把 PVC 当成氯乙烯定级。
这是**过严**方向的风险，需单独评估。

## 影响

- 注册表条目 24 → 39；`layer = "manual"` 从 5 条增至 19 条。
- 别名解析器新增 4 个内部函数：`.norm_iarc_name()`、`parse_iarc_see_target()`、
  `query_iarc_see_alias_map()`、`apply_iarc_see_aliases()`。
- 定级变化：`Gallium arsenide` 从 `-` 变为 V。其余物质结果不变。
- 测试：`tests/testthat/test-group-membership.R` 新增 5 组共 40+ 断言；
  全量测试 0 失败；`R CMD check --no-install` 0 ERROR / 1 WARNING（既有非 ASCII 字面量）。

## 后续（按价值排序）

1. **补上游缺失的 8 个目标条目**（上表），可再救回 9 行静默 + 修正 talc 误分级。
2. **处理共键导致的聚合物/单体混淆**（28 组），当前是过严方向的风险。
3. **给封闭集合组补成员清单或母核 SMARTS**，这才是让 Progesterone 这类物质
   真正被判出来的路径。元素层对它们无解 —— 有机族的定义是结构，不是元素。
