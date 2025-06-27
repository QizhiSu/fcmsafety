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
    volume INTEGER,
    volume_publication_year INTEGER,
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
CREATE TABLE eu_sml_group (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    InChIKey TEXT NOT NULL,
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
