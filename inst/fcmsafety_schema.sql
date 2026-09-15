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
-- Complete data including substances without InChIKey
-- Query with WHERE InChIKey IS NOT NULL for toxicity matching
CREATE TABLE svhc (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    InChIKey TEXT,
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
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- CMR (Carcinogenic, Mutagenic, Reprotoxic) Database
-- Complete data including substances without InChIKey
CREATE TABLE cmr (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    InChIKey TEXT,
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
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- CMR Suspect Database
-- Complete data including substances without InChIKey
CREATE TABLE cmr_suspect (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    InChIKey TEXT,
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
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- IARC (International Agency for Research on Cancer) Database
-- Complete data including substances without InChIKey
CREATE TABLE iarc (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    InChIKey TEXT,
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
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- EU SML (Specific Migration Limits) Database
-- Complete data including substances without InChIKey
CREATE TABLE eu_sml (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    InChIKey TEXT,
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
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- EU SML Group Restrictions Database
CREATE TABLE eu_sml_group (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    InChIKey TEXT,  -- NULL for group restrictions (no InChIKey in source)
    group_no TEXT,
    substance_name TEXT,
    cas_no TEXT,
    sml REAL,  -- Processed numeric value
    restrictions TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- EDC (Endocrine Disrupting Chemicals) Database
-- Complete data including substances without InChIKey
CREATE TABLE edc (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    InChIKey TEXT,
    substance_name TEXT,
    cas_no TEXT,
    ec_no TEXT,
    classification TEXT,
    evidence_level TEXT,
    source TEXT,
    notes TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- China SML Database
CREATE TABLE china_sml (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    InChIKey TEXT,
    substance_name TEXT,
    cas_no TEXT,
    sml_value REAL,
    unit TEXT,
    food_type TEXT,
    regulation_reference TEXT,
    notes TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
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
-- 注意：由于业务表现在允许 InChIKey 为 NULL（单一表设计），
-- 这张表主要用于记录"无法进行毒性匹配"的原因（如无 CAS 也无结构）。
--
-- 唯一键 (database_name, entity_key)：同一库同一键唯一（登记走 upsert）。
-- status 区分三种情况：
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

CREATE UNIQUE INDEX idx_unassigned_entries_key ON unassigned_entries(database_name, entity_key);

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
