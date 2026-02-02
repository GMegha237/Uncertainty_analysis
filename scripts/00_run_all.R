# scripts/00_run_all.R
# Run the full pipeline from raw data -> cleaned data -> analysis -> tables/figures

rm(list = ls())
gc()

message("Starting full run...")

# Make sure we run from the project folder (where the .Rproj file is)
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveProject()))
}

# Create output folders if they don't exist
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# Run each step in order
source("scripts/01_import.R")
source("scripts/02_clean.R")
source("scripts/03_construct.R")
source("scripts/04_analysis.R")
source("scripts/05_outputs.R")

message("Full run complete ✅")
