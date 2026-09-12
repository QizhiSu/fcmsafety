# 输入准备：只要求"名称 + SMILES"，结构标识全本地推导

## 背景

筛查流程原先要求输入表自带 `InChIKey`（`assign_toxicity()` 的匹配主键），而 Toxtree 又需要 `SMILES`。用户手上的检出数据通常只有物质名与结构式，缺 InChIKey 就整条流程跑不动；README 的旧指引是"再去装外部包 labtools 从 PubChem 抽取元数据"，等于把自动化断了。

## Decision

新增导出函数 `prepare_input()`，把"名称 + SMILES"补成可直接进入筛查流程的表，且**默认完全离线**。解析按优先级阶梯进行，能离线就不联网：

1. 输入自带 InChIKey → 原样采用（`given`）
2. SMILES 经 CDK InChI 模块本地算 InChIKey（`smiles_cdk`）
3. 按完整 InChIKey 查本地 `chemicals` 表取回 CID / 分子式 / 精确质量
4. 完整键未命中 → 按 InChIKey 前 14 位骨架再查一次（`db_skeleton`；多命中记 `db_skeleton_ambiguous`）
5. 按物质名在本地各业务表比对（`db_name`）
6. 仍未解决且 `online = TRUE` 时查 PubChem（`pubchem_smiles` / `pubchem_name`，默认关闭）

## 关键依据（2026-09-10 实测）

**离线是否可行**：rcdk 3.8.2 自身不导出任何 InChI 函数（`getNamespaceExports("rcdk")` 里含 inchi 的为零，53 个描述符里也没有），但 `rcdklibs` 的 `cont/` 目录带着 `cdk-inchi-2.9.jar` + `cdk-jniinchi-support-2.9.jar` + `jna-inchi-win32-x86-64-1.2.jar`——InChI 模块其实已在 classpath 上，只是 rcdk 没包 wrapper。用 rJava 直接调 `InChIGeneratorFactory` 即可。

**精度**：拿 `chemicals` 表真实数据自校验（用库内 SMILES 重算 InChIKey 与库内存储值比对），80/80 与随机 500/500 **完全一致，0 不一致 0 失败**，**27 ms/行**，全程零网络。故本地推导被采纳为主路径。

**立体化学**：SMILES 归一化**必须用 `Absolute` flavor**。实测 `Canonical` 会把 L-丙氨酸 `C[C@@H](N)C(=O)O`、D-丙氨酸 `C[C@H](N)C(=O)O`、无立体 `CC(N)C(=O)O` 全部归一成同一个字符串（`O=C(O)C(N)C`），造成立体异构体互相误配；`Absolute` 三者分别得到不同结果，且苯酚的 `Oc1ccccc1` 与 `c1ccc(O)cc1` 能正确归一到同一个。

**骨架降级的安全性**：2446 条 chemicals 中按前 14 位分组共 2419 个唯一骨架，**仅 20 个骨架（0.83%）对应多个立体异构体**。故唯一命中时采用库内完整键是安全的；多命中时只标记歧义、不静默选一个。

## Considered Options

- **本地 CDK InChI（选定）**：零网络、27 ms/行、可复算；代价是需要本机 Java（Toxtree 本来就要），且 rcdk 在 DESCRIPTION 里属 Suggests。
- **联网 PubChem 按 SMILES 查**：无需 Java，但把命脉押在网络与限速上（100 行 ≈ 35 秒），断网即断链。保留为 `online = TRUE` 的兜底。
- **继续要求输入自带 InChIKey**：最省事，但把补全责任推给用户，违背"除导入数据外全自动"的目标。

## Consequences

- 输入硬性要求降为 **名称 + SMILES 两列**（列名支持中英文变体自动识别，也可用 `name_col` / `smiles_col` 显式指定）；CAS 与 InChIKey 变为可选。
- 新增 `identity_method` 列（ASCII 机器可读码）与 `identity_source`（中文说明），逐行记录身份从哪来；`attr(x, "prepare_report")` 与 `print_prepare_report()` 提供补全报告。程序判断请用 `identity_method`——中文在 C locale 下不可靠。
- **中文列名匹配依赖会话 locale**：在无法表示中文的 locale（如 `LC_ALL=C`）下，R 读源码时会把非 ASCII 字符转义成 `<U+XXXX>` 字面量，导致中文候选名匹配失败并报错提示改用 `name_col` 显式指定。中文 Windows / UTF-8 locale 下正常（已实测）。
- 依赖 CDK 不可用时（无 rcdk/rJava）不报错，退化为"只处理自带 InChIKey 的行"，并 warning 说明。

## 顺带修复：Toxtree 的两处批处理脆弱性

排查本功能时实测出两处会让整批 Cramer 结果全丢的问题，一并修复（见 NEWS）：

1. **空 CAS 导致输出整行左移**：Toxtree 3.1.0 在输入行 CAS 字段为空时，输出会把该行整行左移一列（CAS 字段被整个丢弃，其后所有值前移），Cramer 分级值落进 `CRAMERFLAGS` 列、按列名读回为 NA。实测给空字段填任意非空占位符即可避免（行号 / CID / `N/A` 均可），故 `run_toxtree()` 统一填 `"N/A"`，归一化后回填为输入原值，占位符不泄进结果。这使"无 CAS 的检出数据"也能拿到完整 Cramer 分级（0909 的 F1 由此从"丢弃"变为"消除"）。
2. **非法 SMILES 毁掉整批**：Toxtree 会静默丢弃解析不了的 SMILES，输出行数随之少于输入，按行序对齐随即失败、整批 stop。现在先用 CDK 在本地做同一判断，把不能解析的行挡在批处理之外，跑完按原位置合并回去（行数、行序不变，这些行结果 NA）。判断时必须同时判 `is.null(m)` 与 `is.null(m[[1]])`——`parse.smiles()` 对坏输入返回的是"长度 1、元素为 NULL"的列表，只查 `length()` 会把坏行当好行。
