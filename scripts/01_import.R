# scripts/01_import.R
# Read the raw survey export and save a processed copy.

library(tidyverse)

RAW_PATH <- "data/raw/survey_2026-02-02.csv"
STOP_IF_MISSING <- TRUE

if (STOP_IF_MISSING && !file.exists(RAW_PATH)) {
  stop("I can't find the raw data file at: ", RAW_PATH,
       "\nPut your raw survey export there and try again.")
}

survey_raw <- readr::read_csv(RAW_PATH, show_col_types = FALSE)

# Basic sanity checks
if (nrow(survey_raw) == 0) stop("Raw data has 0 rows.")
message("Imported rows: ", nrow(survey_raw))
message("Imported columns: ", ncol(survey_raw))

saveRDS(survey_raw, "data/processed/01_survey_raw.rds")
vb