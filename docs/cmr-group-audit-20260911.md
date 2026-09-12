# P1-4 审计：CMR A 类覆盖型组条目的真实漏报面

> 审计日期 2026-09-11　方法：读 CLP 原始导出 `inst/clp.xlsx`（ATP23，4035 条）＋ 调真实
> `prepare_input()` / `assign_toxicity()`，不旁路复刻。临时脚本 `.workbuddy/_audit_cmr_groups*.py`、
> `.workbuddy/_probe_group_gap.R`、`.workbuddy/_probe_real_assign.R`。

## 1. 清单：24 条（不是先前记的 20 条）

判定规则（可复现）：CLP 条目名称含 `with the exception of` / `and its salts` /
以 `salts of X` / `X compounds` 开头；排除 `complex combination` / `reaction mass of`
与 index 648/649/650 段（UVCB）。再筛 `hazard_class` 含 `Carc.`/`Muta.`/`Repr.`。

先前记的「20 条」是更窄的规则得出的，本次把 `and its salts` 与 `salts of aniline` 等一并纳入 → 24 条。

| # | index_no | 名称 | 关键 H 码 | 主表可见 |
|---|---|---|---|---|
| 1 | 004-002-00-2 | beryllium compounds with the exception of… | H350i | ✗ |
| 2 | 007-014-00-6 | salts of hydrazine | H350 | ✗ |
| 3 | 024-007-00-3 | zinc chromates including zinc potassium chromate | H350i | ✗ |
| 4 | 024-017-00-8 | Chromium (VI) compounds, with the exception of… | H350i | ✗ |
| 5 | 033-005-00-1 | arsenic acid and its salts with the exception of… | H350 | ✗ |
| 6 | 050-008-00-3 | tributyltin compounds, with the exception of… | H360FD | ✗ |
| 7 | 082-001-00-6 | lead compounds with the exception of… | H360Df | ✗ |
| 8 | 082-002-00-1 | lead alkyls | H360Df | ✗ |
| 9 | 602-042-00-0 | 1,2,3,4,5,6-hexachlorcyclohexanes with the exception of… | H351 | ✗ |
| 10 | 607-230-00-6 | 2-ethylhexanoic acid and its salts, with the exception of… | H360D | ✗ |
| 11 | 608-065-00-2 | salts of bromoxynil with the exception of… | H361d | ✗ |
| 12 | 608-066-00-8 | salts of ioxynil with the exception of… | H361d | ✗ |
| 13 | 609-026-00-2 | salts and esters of dinoseb, with the exception of… | H360Df | ✗ |
| 14 | 611-024-00-1 | Benzidine based azo dyes; 4,4'-diarylazobiphenyl dyes… | H350 | ✗ |
| 15 | 611-029-00-9 | o-dianisidine based azo dyes… | H350 | ✗ |
| 16 | 611-030-00-4 | o-tolidine based dyes… | H350 | ✗ |
| 17 | 612-009-00-2 | salts of aniline | H351 | ✗ |
| 18 | 612-037-00-5 | salts of 3,3'-dimethoxybenzidine; salts of o-dianisidine | H350 | ✗ |
| 19 | 612-070-00-5 | salts of benzidine | H350 | ✗ |
| 20 | 612-071-00-0 | salts of 2-naphthylamine | H350 | ✗ |
| 21 | 612-073-00-1 | salts of biphenyl-4-ylamine; salts of xenylamine… | H350 | ✗ |
| 22 | 612-079-00-4 | salts of 2,2'-dichloro-4,4'-methylenedianiline… | H350 | ✗ |
| 23 | 612-081-00-5 | salts of 4,4'-bi-o-toluidine; salts of 3,3'-dimethylbenzidine… | H350 | ✗ |
| 24 | 612-097-00-2 | salts of 4,4'-carbonimidoylbis[N,N-dimethylaniline] | H351 | ✗ |

另有 2 条 `and its salts` 类条目已在主表（被当成单一物质存，"and its salts" 语义丢失）：
`612-198-00-1` 4,4'-thiodianiline and its salts、`612-199-00-7` 4,4'-oxydianiline and its salts。

**结论：24 条里 22 条对主表完全不可见**（`InChIKey NOT NULL` 决定它们进不来）。

## 2. 漏报分两种，性质完全不同

### 类型 A：物质在库内，但 CMR 证据查不到
量化（含该元素的库内物质 → cmr/cmr_suspect 命中率）：

| 元素 | 组条目 | 库内物质 | cmr 命中 | 查不到 |
|---|---|---|---|---|
| Pb | lead compounds / lead alkyls | 33 | 12 | 20 |
| Cr | Chromium (VI) compounds | 23 | 15 | 8（其中 3 个是 Cr(III)/金属铬，本不该报） |
| Sn | tributyltin compounds | 40 | 25 | 15（多为二/单辛基锡，不属该组） |
| As | arsenic acid and its salts | 15 | 8 | 7 |
| Be | beryllium compounds | 2 | 2 | 0 |

**但类型 A 基本不影响定级**。铅的 33 个物质里 **32 个已经是 V 级**（SVHC 兜住），
唯一例外是 1 个 IV 级。Cr / Sn / As 同理，多数有 SVHC 或 IARC 支撑。

→ 类型 A 的实际后果只是 **`CMR` 列显示 `-`，报告不精确**，级别结论基本正确。

### 类型 B：物质不在库内 → 全库无记录 → 定级留空 `-`
用真实链路实测（`prepare_input()` 算 InChIKey → `assign_toxicity()`）：

| 输入物质 | 正确结论 | 实测 CMR | SVHC | IARC | Toxic_level | Group_hits |
|---|---|---|---|---|---|---|
| 硫酸肼 `ZGCHATBSUIJLRL` | salts of hydrazine · H350 · 应 V | `-` | `-` | `-` | **`-`** | `-` |
| 二盐酸肼 `LIAWOTKNAVAKCX` | 同上 | `-` | `-` | `-` | **`-`** | `-` |
| 氯化铍 `LWBPNIJBHRISSS` | beryllium compounds · H350i · 应 V | `-` | `-` | `-` | **`-`** | `-` |
| 铬酸锌 `NDKWCCLKSWNDBG` | zinc chromates · H350i · 应 V | `-` | `-` | `-` | **`-`** | `-` |
| 一氧化铅（对照，在库） | 铅组 · 应 V | `-` | Y | `-` | V | `cmr: cmr_145; svhc: svhc_403` |
| TBTO（对照，在库） | 三丁基锡组 · 应 V | `-` | Y | `-` | V | `-` |

**关键**：类型 B 下 `Toxic_level = "-"`，而 `-` 的既定语义（ADR 0007）是
「全无证据」——**用户无法区分"查过、确实干净"与"库里压根没这条"**。
这正是 ADR 0012 里 `run_screening()` 为「整行未解析」设告警的同一类陷阱。

**分界线是"物质在不在 `chemicals` 表里"**，不是"组条目有没有建注册表"。

### 为什么在库内就能命中？
`extract_input_keywords()`（`group_membership.R` 1110–1150）按 InChIKey 去
`chemicals.IUPACName` / `svhc.substance_name` / `cmr.名称` **回查名称**再提关键词。
所以：
- 物质在库内 → 拿得到名称 → 元素关键词命中组条目（一氧化铅命中 `cmr_145 lead alkyls`）
- 物质不在库内 → **拿不到名称 → 连关键词都提不出来 → Group_hits 也是 `-`**

> 修正 9/10 的旧结论：A 类组条目（无 CAS）也会被 `is_uvcb_name()` 判为 UVCB 进注册表，
> 所以关键词层**并非**"只覆盖 C 类"。但它在库外物质上完全失效。

## 3. 最典型、且 FCM 高相关的实例：2-乙基己酸的金属盐

组条目 `607-230-00-6 2-ethylhexanoic acid and its salts`（H360D / Repr. 1B → **V 级**）。

| 物质 | 当前证据 | 当前级别 | 应判 |
|---|---|---|---|
| `C8H16O2` 2-乙基己酸 | cmr_suspect | IV | V |
| `C16H30CaO4` 2-乙基己酸钙 | 仅 China_SML | II/III | **V** |
| `C16H30O4Zn` 2-乙基己酸锌 | 仅 China_SML | II/III | **V** |
| `C16H30O4Sn` 2-乙基己酸锡 | 仅 China_SML | II/III | **V** |
| `C16H30MgO4` 2-乙基己酸镁 | 仅 China_SML | II/III | **V** |
| `C24H45CeO6` 2-乙基己酸铈 | 仅 China_SML | II/III | **V** |

**为什么这条比铅重要**：钙/锌/锡的 2-乙基己酸盐是 **PVC 热稳定剂**，食品接触用 PVC
里很常见，且**没有被 SVHC 兜住** → 漏的是实打实的级别，不只是报告列。

判据形态也最干净：**「2-乙基己酸骨架 + 任意金属/阳离子」= 整个盐类**，
用一条 SMARTS 即可覆盖无限集合。

## 4. 判据可行性分级（决定实现方式）

| 档 | 组条目 | 判据 | 可靠性 |
|---|---|---|---|
| 元素型 | lead compounds / lead alkyls、beryllium compounds、arsenic acid and its salts | Formula 含 Pb / Be / As | 高（组名即"X compounds"，除显式例外） |
| 骨架型 | **2-ethylhexanoic acid and its salts**、salts of aniline、salts of benzidine 系、Benzidine based azo dyes、salts of bromoxynil / ioxynil、dinoseb、hexachlorcyclohexanes | SMARTS 子结构 | 高 |
| 价态型 | **Chromium (VI) compounds** | 含 Cr **且**为 +6 价 | **低** |
| 难判 | tributyltin compounds | Sn + 三个丁基 | 中（SMARTS 可写，但易误伤二/单丁基锡） |

**Cr(VI) 不可用简单判据**：库内 `CrO3`（Cr VI）是 `O=[Cr](=O)=O`，
而 `Cr2O3`（Cr III）是 `O=[Cr]O[Cr]=O` —— **两者都含 `[Cr]=O`**，SMARTS 区分不了。
`[Cr+6]` 写法只有 2 条。→ 这条只能人工核定白名单（库内仅 23 个含 Cr 物质，一次核定即可）。

## 5. 未决
- 命中组条目后**是否参与定级**（会改变现有输出：2-乙基己酸盐 III→V 是修正；
  若 Cr(VI) 判据误伤则会过严）。
- 24 条是否全做，还是先做 FCM 高相关的少数几条。
