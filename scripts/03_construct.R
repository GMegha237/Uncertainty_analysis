# scripts/03_construct.R
library(tidyverse)

survey_clean <- readRDS("data/processed/02_survey_clean.rds")

# TODO: construct outcomes / scales / indices here
survey_analysis <- survey_clean

saveRDS(survey_analysis, "data/processed/03_survey_analysis.rds")
message("Construct step complete.")
