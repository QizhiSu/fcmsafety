#!/usr/bin/env Rscript
# Launch FCMSafety Database Inspector

cat("🚀 Launching FCMSafety Database Inspector...\n")

# Load required modules
source('R/simple_migration.R')
source('R/database_inspector_app.R')

# Ensure database exists
db_path <- file.path(getwd(), "inst", "fcmsafety.db")
if (!file.exists(db_path)) {
  cat("📊 Creating database...\n")
  simple_migrate_xlsx_to_sqlite(force_recreate = TRUE)
}

# Launch the application
cat("📱 Opening in browser at http://localhost:3838\n")
cat("🛑 Press Ctrl+C to stop the application\n")

launch_database_inspector(port = 3838, launch_browser = TRUE)
