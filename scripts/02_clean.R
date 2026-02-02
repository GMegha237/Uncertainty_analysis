# scripts/02_clean.R
library(tidyverse)

survey_raw <- readRDS("data/processed/01_survey_raw.rds")

# TODO: cleaning rules go here
survey_clean <- survey_raw

saveRDS(survey_clean, "data/processed/02_survey_clean.rds")
message("Cleaning step complete.")
