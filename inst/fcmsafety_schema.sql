-- FCMSafety SQLite Database Schema
-- This schema replaces the xlsx-based database system with a comprehensive SQLite solution
-- Created: 2025-01-26

-- Enable foreign key constraints
PRAGMA foreign_keys = ON;

-- =============================================================================
-- CORE CHEMICAL METADATA TABLE
-- =============================================================================
-- Shared chemical information referenced by all regulatory databases
CREATE TABLE chemicals (
    InChIKey TEXT PRIMARY KEY,
    CID INTEGER,
    Formula TEXT,
    SMILES TEXT,
    IUPACName TEXT,
    ExactMass REAL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Indexes for chemical metadata
CREATE INDEX idx_chemicals_cid ON chemicals(CID);
CREATE INDEX idx_chemicals_formula ON chemicals(Formula);

-- =============================================================================
-- DUAL-STORAGE REGULATORY DATABASE TABLES
-- =============================================================================
-- Each regulatory database has two tables:
-- 1. _raw: Complete original data (for update comparisons)
-- 2. _filtered: InChIKey-filtered data (for toxicity assignment)

-- SVHC (Substances of Very High Concern) Database
-- Raw table: ALL original data from XLSX
CREATE TABLE svhc_raw (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    substance_name TEXT,
    description TEXT,
    ec_no TEXT,
    cas_no TEXT,
    reason_for_inclusion TEXT,
    date_of_inclusion TEXT,
    decision TEXT,
    iuclid_dataset TEXT,
    support_document TEXT,
    response_to_comments TEXT,
    remarks TEXT,
    CID INTEGER,
    Formula TEXT,
    SMILES TEXT,
    InChIKey TEXT,  -- Can be NULL
    IUPACName TEXT,
    ExactMass REAL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Filtered table: Only records with valid InChIKey (for toxicity assignment)
CREATE TABLE svhc (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    InChIKey TEXT NOT NULL,
    substance_name TEXT,
    description TEXT,
    ec_no TEXT,
    cas_no TEXT,
    reason_for_inclusion TEXT,
    date_of_inclusion TEXT,
    decision TEXT,
    iuclid_dataset TEXT,
    support_document TEXT,
    response_to_comments TEXT,
    remarks TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (InChIKey) REFERENCES chemicals(InChIKey)
);

-- CMR (Carcinogenic, Mutagenic, Reprotoxic) Database
-- Raw table: ALL original data from XLSX
CREATE TABLE cmr_raw (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    index_no TEXT,
    international_chemical_identification TEXT,
    ec_no TEXT,
    cas_no TEXT,
    hazard_class_and_category_codes TEXT,
    hazard_statement_codes TEXT,
    -- pictogram / signal_word_codes 与 specific_conc_limits / m_factors 在 CLP 官方
    -- 导出里各是一列（"Pictogram, Signal Word Code(s)"、"Specific Conc. Limits,
    -- M-factors"），必须拆开落库；旧迁移按子串取列，整串同时写进了两对列。
    -- 拆法见 R/update_other_dbs.R 的 split_clp_label_cell() / split_clp_limit_cell()。
    pictogram TEXT,
    signal_word_codes TEXT,
    hazard_statement_codes_alt TEXT,
    suppl_hazard_statement_codes TEXT,
    specific_conc_limits TEXT,
    m_factors TEXT,
    notes TEXT,
    atp_inserted_updated TEXT,
    CID INTEGER,
    Formula TEXT,
    SMILES TEXT,
    InChIKey TEXT,  -- Can be NULL
    IUPACName TEXT,
    ExactMass REAL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Filtered table: Only records with valid InChIKey (for toxicity assignment)
CREATE TABLE cmr (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    InChIKey TEXT NOT NULL,
    index_no TEXT,
    international_chemical_identification TEXT,
    ec_no TEXT,
    cas_no TEXT,
    hazard_class_and_category_codes TEXT,
    hazard_statement_codes TEXT,
    -- 图示/信号词、限值/M 系数是拆开的两对列（说明见上面 cmr_raw 的同名段）。
    -- 既有库里的重复值由 heal_cmr_split_cols() 在增量写库前自愈。
    pictogram TEXT,
    signal_word_codes TEXT,
    hazard_statement_codes_alt TEXT,
    suppl_hazard_statement_codes TEXT,
    specific_conc_limits TEXT,
    m_factors TEXT,
    notes TEXT,
    atp_inserted_updated TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (InChIKey) REFERENCES chemicals(InChIKey)
);

-- CMR Suspect Database
CREATE TABLE cmr_suspect (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    InChIKey TEXT NOT NULL,
    -- CLP 的官方标识（如 613-166-00-X），也是增量 diff 的主键。原先这张表没有
    -- 这一列，只能拿 substance_name 当主键 —— 而物质名是自由文本，上游改一个
    -- 连字符/逗号/省略号，就会被判成"删一条 + 加一条"。实测 45 行 removed 里
    -- 26 行是上游改了标点、15 行只差空白。同源的 cmr 表用 index_no 当键，
    -- removed 只有 2 行。
    index_no TEXT,
    substance_name TEXT,
    cas_no TEXT,
    ec_no TEXT,
    classification TEXT,
    source TEXT,
    notes TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (InChIKey) REFERENCES chemicals(InChIKey)
);

-- IARC (International Agency for Research on Cancer) Database
CREATE TABLE iarc (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    InChIKey TEXT NOT NULL,
    cas_no TEXT,
    agent TEXT,
    group_classification TEXT,
    -- volume / volume_publication_year 必须声明成 TEXT。IARC 官方值是多值/带后缀的
    -- 文本（"41, Sup 7, 71, 106"、"2022 online"）。声明成 INTEGER 时，SQLite 存
    -- 文本本身没问题，但 RSQLite 读回时按**首个非 NA 值的实际类型**决定整列类型：
    -- 线上 iarc.id = 1 的 volume 正是整数，于是整列按整数读，后面的多值文本被截成
    -- 前导数字（"41, Sup 7, 71, 106" -> 41），静默丢值 —— 增量 diff 因此每轮都判
    -- modified，永远收敛不了。声明 TEXT 后整列恒为文本，读取不再依赖行序。
    volume TEXT,
    volume_publication_year TEXT,
    evaluation_year INTEGER,
    additional_information TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (InChIKey) REFERENCES chemicals(InChIKey)
);

-- EU SML (Specific Migration Limits) Database
CREATE TABLE eu_sml (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    InChIKey TEXT NOT NULL,
    fcm_substance_no TEXT,
    ref_no TEXT,
    cas_no TEXT,
    substance_name TEXT,
    use_as_additive TEXT,
    use_as_monomer TEXT,
    frf_applicable TEXT,
    sml REAL,  -- Processed numeric value
    sml_group TEXT,  -- Processed group restriction
    restrictions_and_specifications TEXT,
    notes_on_verification TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (InChIKey) REFERENCES chemicals(InChIKey)
);

-- EU SML Group Restrictions Database
-- NOTE: InChIKey is nullable here because the source xlsx (eu10_2011.xlsx,
-- SML_group sheet) has no InChIKey column. Group restrictions are aggregated
-- by FCM substance number, not by chemical identity.
CREATE TABLE eu_sml_group (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    InChIKey TEXT,  -- NULL for group restrictions (no InChIKey in source)
    group_no TEXT,
    substance_name TEXT,
    cas_no TEXT,
    sml REAL,  -- Processed numeric value
    restrictions TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (InChIKey) REFERENCES chemicals(InChIKey)
);

-- EDC (Endocrine Disrupting Chemicals) Database
CREATE TABLE edc (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    InChIKey TEXT NOT NULL,
    substance_name TEXT,
    cas_no TEXT,
    ec_no TEXT,
    classification TEXT,
    evidence_level TEXT,
    source TEXT,
    notes TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (InChIKey) REFERENCES chemicals(InChIKey)
);

-- China SML Database
CREATE TABLE china_sml (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    InChIKey TEXT,  -- May be NULL for this database
    substance_name TEXT,
    cas_no TEXT,
    sml_value REAL,
    unit TEXT,
    food_type TEXT,
    regulation_reference TEXT,
    notes TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (InChIKey) REFERENCES chemicals(InChIKey)
);

-- =============================================================================
-- METADATA AND AUDIT TABLES
-- =============================================================================

-- Database metadata and version tracking
CREATE TABLE database_metadata (
    database_name TEXT PRIMARY KEY,
    version TEXT,
    last_updated TIMESTAMP,
    total_records INTEGER,
    source_url TEXT,
    update_method TEXT,
    notes TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Update history and audit trail
CREATE TABLE update_history (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    database_name TEXT NOT NULL,
    update_timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    update_type TEXT NOT NULL, -- 'full_refresh', 'incremental', 'manual'
    records_added INTEGER DEFAULT 0,
    records_removed INTEGER DEFAULT 0,
    records_modified INTEGER DEFAULT 0,
    source_file TEXT,
    user_notes TEXT,
    success BOOLEAN DEFAULT TRUE,
    error_message TEXT
);

-- Detailed change log for individual substance changes
CREATE TABLE change_log (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    update_history_id INTEGER NOT NULL,
    database_name TEXT NOT NULL,
    InChIKey TEXT,
    substance_identifier TEXT, -- name, CAS, or other identifier
    change_type TEXT NOT NULL, -- 'added', 'removed', 'modified'
    field_name TEXT, -- for modifications, which field changed
    old_value TEXT,
    new_value TEXT,
    timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (update_history_id) REFERENCES update_history(id),
    FOREIGN KEY (InChIKey) REFERENCES chemicals(InChIKey)
);

-- Registry of entries that could not be stored ("未分配条目簿")
--
-- 上游有相当一部分条目本体没有结构：IARC 评的是感染状态与职业暴露场景
-- （"Helicobacter pylori (infection with)"、"Acheson process, occupational
-- exposure associated with"）、A 类组条目（"salts of hydrazine"、
-- "Chromium (VI) compounds, with the exception of ..."）、UVCB 工业品
-- （"alcohols, aliphatic, monohydric, saturated, linear, primary (C4-C22)"）、
-- 反应产物混合物（"reaction mass of: ..."）。实测四库合计 414 行属于这一类。
--
-- 它们拿不到 InChIKey，而四张业务表的 InChIKey 是 NOT NULL + 外键指向
-- chemicals —— 插不进去；写库又是单事务，一行失败整批回滚（iarc 曾因此
-- 846 行全废）。这些行在写库前被摘出来登记在这里，让"上游给了什么但我们
-- 收不了"可见，而不是静默丢弃。
--
-- 唯一键 (database_name, entity_key)：同一轮或跨轮重复遇到只累加 seen_count，
-- 不会长出重复行。status 区分三种情况：
--   'open'     待处理，还没人工判断
--   'accepted' 人工确认结构性收不了（永远的边界，如感染状态、工种）
--   'resolved' 后来拿到了键并入库（如上游补了 CAS，或组条目成员被展开）
CREATE TABLE unassigned_entries (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    database_name TEXT NOT NULL,
    entity_key TEXT NOT NULL,      -- 与 diff 同一套键定义（key_of_df 的产物）
    substance_name TEXT,           -- 源里的名称，给人看
    cas_no TEXT,                   -- 源里给的 CAS（可能为空）
    reason TEXT NOT NULL,          -- 'no_cas' | 'no_structure_found' | 'not_looked_up'
    detail TEXT,                   -- 补充说明（如试过哪些候选 CAS）
    first_seen_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    last_seen_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    seen_count INTEGER DEFAULT 1,
    status TEXT DEFAULT 'open',    -- 'open' | 'accepted' | 'resolved'
    notes TEXT                     -- 人工批注，自动流程不覆盖
);

-- Data Quality Flags：已入库、但某个字段已知不可信的行
--
-- 和 unassigned_entries 的区别：那张表记"收不进来的行"，这张表记"收进来了、
-- 但某列已知有问题"的行。两者都不改业务表本身。
--
-- 为什么不把标记写进业务表的 notes 列：入库是 DELETE + INSERT，源 df 里没有的
-- 列一律填 NA（见 write_changes_to_db）。只要该行日后被判一次 modified，写在
-- 业务表 notes 里的标记就会被抹掉，而且悄无声息。旁路表不会被写库碰到。
CREATE TABLE data_quality_flags (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    database_name TEXT NOT NULL,   -- 业务表名（cmr / cmr_suspect / iarc / eu_sml）
    entity_key TEXT NOT NULL,      -- 该表的业务标识（cmr 用 index_no，iarc 用 cas_no）
    entity_name TEXT,              -- 源里的名称，给人看
    flag TEXT NOT NULL,            -- 问题类型，如 'unreliable_structure_key'
    detail TEXT,                   -- 具体说明（如"该键实际指向丁烷"）
    severity TEXT DEFAULT 'high',  -- 'high' | 'medium' | 'low'
    source TEXT,                   -- 问题出自哪一步（如 'clp_cmr_meta.xlsx'）
    first_seen_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    last_seen_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    seen_count INTEGER DEFAULT 1,
    status TEXT DEFAULT 'open',    -- 'open' | 'accepted' | 'resolved'
    notes TEXT                     -- 人工批注，自动流程不覆盖
);

-- =============================================================================
-- INDEXES FOR PERFORMANCE
-- =============================================================================

-- Primary lookup indexes
CREATE INDEX idx_svhc_inchikey ON svhc(InChIKey);
CREATE INDEX idx_svhc_cas ON svhc(cas_no);
CREATE INDEX idx_svhc_substance_name ON svhc(substance_name);

CREATE INDEX idx_cmr_inchikey ON cmr(InChIKey);
CREATE INDEX idx_cmr_cas ON cmr(cas_no);
CREATE INDEX idx_cmr_index_no ON cmr(index_no);

CREATE INDEX idx_cmr_suspect_inchikey ON cmr_suspect(InChIKey);
CREATE INDEX idx_cmr_suspect_cas ON cmr_suspect(cas_no);
CREATE INDEX idx_cmr_suspect_index_no ON cmr_suspect(index_no);

CREATE INDEX idx_iarc_inchikey ON iarc(InChIKey);
CREATE INDEX idx_iarc_cas ON iarc(cas_no);
CREATE INDEX idx_iarc_agent ON iarc(agent);

CREATE INDEX idx_eu_sml_inchikey ON eu_sml(InChIKey);
CREATE INDEX idx_eu_sml_cas ON eu_sml(cas_no);
CREATE INDEX idx_eu_sml_fcm_no ON eu_sml(fcm_substance_no);

CREATE INDEX idx_eu_sml_group_inchikey ON eu_sml_group(InChIKey);
CREATE INDEX idx_eu_sml_group_cas ON eu_sml_group(cas_no);

CREATE INDEX idx_edc_inchikey ON edc(InChIKey);
CREATE INDEX idx_edc_cas ON edc(cas_no);

CREATE INDEX idx_china_sml_inchikey ON china_sml(InChIKey);
CREATE INDEX idx_china_sml_cas ON china_sml(cas_no);

-- Audit trail indexes
CREATE INDEX idx_update_history_database ON update_history(database_name);
CREATE INDEX idx_update_history_timestamp ON update_history(update_timestamp);
CREATE INDEX idx_change_log_update_id ON change_log(update_history_id);
CREATE INDEX idx_change_log_database ON change_log(database_name);
CREATE INDEX idx_change_log_inchikey ON change_log(InChIKey);

-- 未分配条目簿：同一库同一键唯一（登记走 upsert，不靠调用方去重）
CREATE UNIQUE INDEX idx_unassigned_entries_key
    ON unassigned_entries(database_name, entity_key);
CREATE INDEX idx_unassigned_entries_status ON unassigned_entries(status);

CREATE UNIQUE INDEX idx_data_quality_flags_key
    ON data_quality_flags(database_name, entity_key, flag);
CREATE INDEX idx_data_quality_flags_status ON data_quality_flags(status);

-- =============================================================================
-- VIEWS FOR COMPATIBILITY
-- =============================================================================

-- Views that replicate the exact structure returned by load_databases()
-- These ensure backward compatibility with existing assign_toxicity() function

CREATE VIEW view_svhc AS
SELECT
    s.substance_name AS "Substance name",
    s.description AS "Description",
    s.ec_no AS "EC No.",
    s.cas_no AS "CAS No.",
    s.reason_for_inclusion AS "Reason for inclusion",
    s.date_of_inclusion AS "Date of inclusion",
    s.decision AS "Decision",
    s.iuclid_dataset AS "IUCLID dataset",
    s.support_document AS "Support document",
    s.response_to_comments AS "Response to comments",
    s.remarks AS "Remarks",
    c.CID,
    c.Formula,
    c.SMILES,
    c.InChIKey,
    c.IUPACName,
    c.ExactMass
FROM svhc s
JOIN chemicals c ON s.InChIKey = c.InChIKey
WHERE c.InChIKey IS NOT NULL;

CREATE VIEW view_cmr AS
SELECT
    c.index_no AS "Index No",
    c.international_chemical_identification AS "International Chemical Identification",
    c.ec_no AS "EC No",
    c.cas_no AS "CAS No",
    c.hazard_class_and_category_codes AS "Hazard Class and Category Code(s)",
    c.hazard_statement_codes AS "Hazard Statement Code(s)",
    c.pictogram AS "Pictogram",
    c.signal_word_codes AS "Signal Word Code(s)",
    c.hazard_statement_codes_alt AS "Hazard statement Code(s)",
    c.suppl_hazard_statement_codes AS "Suppl. Hazard statement Code(s)",
    c.specific_conc_limits AS "Specific Conc. Limits",
    c.m_factors AS "M-factors",
    c.notes AS "Notes",
    c.atp_inserted_updated AS "ATP inserted/ATP Updated",
    ch.CID,
    ch.Formula,
    ch.SMILES,
    ch.InChIKey,
    ch.IUPACName,
    ch.ExactMass
FROM cmr c
JOIN chemicals ch ON c.InChIKey = ch.InChIKey
WHERE ch.InChIKey IS NOT NULL;

-- Additional views for other databases would follow the same pattern...

-- =============================================================================
-- TRIGGERS FOR AUTOMATIC TIMESTAMP UPDATES
-- =============================================================================

-- Update timestamp triggers for all main tables
CREATE TRIGGER update_chemicals_timestamp
    AFTER UPDATE ON chemicals
    BEGIN
        UPDATE chemicals SET updated_at = CURRENT_TIMESTAMP WHERE InChIKey = NEW.InChIKey;
    END;

CREATE TRIGGER update_svhc_timestamp
    AFTER UPDATE ON svhc
    BEGIN
        UPDATE svhc SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id;
    END;

CREATE TRIGGER update_cmr_timestamp
    AFTER UPDATE ON cmr
    BEGIN
        UPDATE cmr SET updated_at = CURRENT_TIMESTAMP WHERE id = NEW.id;
    END;

-- Additional triggers for other tables would follow the same pattern...

-- =============================================================================
-- INITIAL METADATA SETUP
-- =============================================================================

-- Initialize database metadata
INSERT INTO database_metadata (database_name, version, last_updated, total_records, update_method, notes) VALUES
('svhc', '1.0.0', CURRENT_TIMESTAMP, 0, 'migration', 'Initial migration from xlsx'),
('cmr', '1.0.0', CURRENT_TIMESTAMP, 0, 'migration', 'Initial migration from xlsx'),
('cmr_suspect', '1.0.0', CURRENT_TIMESTAMP, 0, 'migration', 'Initial migration from xlsx'),
('iarc', '1.0.0', CURRENT_TIMESTAMP, 0, 'migration', 'Initial migration from xlsx'),
('eu_sml', '1.0.0', CURRENT_TIMESTAMP, 0, 'migration', 'Initial migration from xlsx'),
('eu_sml_group', '1.0.0', CURRENT_TIMESTAMP, 0, 'migration', 'Initial migration from xlsx'),
('edc', '1.0.0', CURRENT_TIMESTAMP, 0, 'migration', 'Initial migration from xlsx'),
('china_sml', '1.0.0', CURRENT_TIMESTAMP, 0, 'migration', 'Initial migration from xlsx');
