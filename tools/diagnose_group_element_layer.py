#!/usr/bin/env python3
"""审计 group_membership.R 的元素层粗筛：每条 IARC 组条目的特征元素是否成立。

⚠️ 保真度警告：本脚本只复刻了元素层（parse_agent_elements /
parse_formula_elements / parse_smiles_elements），**没有**复刻 screen_iarc_groups()
里的 layer 判定与置信度降级。因此它给出的"命中行数"高估了实际影响 ——
layer = "form" / "valence" / "scenario" 的条目命中只是 manual_review，不参与定级。
曾据此误判为"Silica 58 行误升 V 级"，实为 manual_review。详见
docs/adr/0009-20260910-iarc-group-element-judge.md。

**要判断命中与置信度的真实情况，用 tools/audit_group_hits.R（R 版，走真实函数）。**

本脚本的用途仅限一件事：快速查看"注册表给每条组条目抽出了什么特征元素，
以及这些元素在全库里多常见"。用来回答：
  - 特征元素为空      -> 该条目永远不会被命中（漏报）
  - 特征元素是骨架元素 -> 命中量与物质数无关，是误报

用法（仓库根目录下）：
  python tools/diagnose_group_element_layer.py
"""
import sqlite3, re, collections

two_char = ["Br","Cl","Si","Se","As","Co","Cr","Cd","Hg","Be","Pb","Ni","Ra","Rn","Th","Pu","Sr","U","Na"]

def formula_elements(f):
    if not f: return []
    f = re.sub(r"[+\-]", "", f)
    out = re.findall(r"[A-Z][a-z]?", f)
    return list(dict.fromkeys(out))

def smiles_elements(s):
    if not s: return []
    els, i, n = [], 0, len(s)
    while i < n:
        ch = s[i]
        if re.match(r"[0-9=#@%.\-\\/]", ch):
            i += 1; continue
        if ch in "()[]":
            i += 1; continue
        if i < n - 1 and s[i:i+2] in two_char:
            els.append(s[i:i+2]); i += 2; continue
        if re.match(r"[A-Z]", ch): els.append(ch)
        elif ch in "cnosp": els.append(ch.upper())
        i += 1
    return list(dict.fromkeys(els))

# parse_agent_elements 的映射表（复刻 R）
agent_map = {
 "arsenic":"As","beryllium":"Be","cadmium":"Cd","chromium":"Cr","cobalt":"Co",
 "mercury":"Hg","selenium":"Se","silica":"Si","talc":"Mg","nickel":"Ni",
 "radium":"Ra","radon":"Rn","thorium":"Th","lead":"Pb","plutonium":"Pu",
 "strontium":"Sr","uranium":"U","cyclamate":"C",
}
def agent_elements(a):
    if not a: return []
    words = re.findall(r"\b\w+\b", a)
    return list(dict.fromkeys(agent_map[w.lower()] for w in words if w.lower() in agent_map))

con = sqlite3.connect("inst/fcmsafety.db")
rows = con.execute("SELECT InChIKey, IUPACName, Formula, SMILES FROM chemicals").fetchall()
chems = []
for ik, nm, f, s in rows:
    els = list(dict.fromkeys(formula_elements(f) + smiles_elements(s)))
    chems.append((ik, nm or "", set(els)))

gpat = re.compile(r"(compounds|and its (salts|decay products)|metal without|metallic|fibres|fibers|dust|Cyclamates?|salts\b)", re.I)
seen = {}
for a, g in con.execute("SELECT agent, group_classification FROM iarc WHERE group_classification IS NOT NULL"):
    if a and gpat.search(a):
        seen.setdefault(a, g)
con.close()

print("%-62s %-4s %-14s %6s" % ("agent", "grp", "elements", "hits"))
print("-"*100)
detail = {}
for a in sorted(seen, key=lambda x: (seen[x] != "1", x)):
    els = agent_elements(a)
    if not els:
        print("%-62s %-4s %-14s %6s" % (a[:62], seen[a], "(none!)", "DEAD"))
        continue
    hits = [c for c in chems if set(e.lower() for e in els) & set(e.lower() for e in c[2])]
    detail[a] = (els, hits)
    print("%-62s %-4s %-14s %6d" % (a[:62], seen[a], ";".join(els), len(hits)))

print()
print("="*100)
print("组 1 / 2A / 2B 条目（会真正改变等级的）——命中样例")
print("="*100)
for a,(els,hits) in sorted(detail.items(), key=lambda kv: -len(kv[1][1])):
    if seen[a] == "3": continue
    if len(hits) == 0: continue
    print("\n[组 %s] %s   (特征元素 %s, 命中 %d 行)" % (seen[a], a, ";".join(els), len(hits)))
    for ik, nm, _ in hits[:8]:
        print("      -", nm[:70])
