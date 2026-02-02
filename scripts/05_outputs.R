# scripts/05_outputs.R
library(tidyverse)

results <- readRDS("data/processed/04_results.rds")

# TODO: write tables and figures to outputs/
writeLines(paste("N =", results$n), "outputs/tables/summary.txt")

message("Outputs step complete.")
