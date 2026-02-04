# scripts/03_construct.R
library(tidyverse)

in_rds  <- "data/processed/02_survey_clean.rds"
out_rds <- "data/processed/03_survey_analysis.rds"

survey_clean <- readRDS(in_rds)
stopifnot(nrow(survey_clean) > 0)

# -----------------------------
# Helpers
# -----------------------------
num <- function(x) readr::parse_number(as.character(x))

row_mean_scale <- function(dat, cols) {
  cols <- intersect(cols, names(dat))
  if (length(cols) == 0) return(rep(NA_real_, nrow(dat)))
  mat <- dat %>% select(all_of(cols)) %>% mutate(across(everything(), num))
  out <- rowMeans(as.matrix(mat), na.rm = TRUE)
  out[is.nan(out)] <- NA_real_
  out
}

z_within <- function(x, sample_flag) {
  m <- mean(x[sample_flag == 1], na.rm = TRUE)
  s <- sd(x[sample_flag == 1], na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(NA_real_, length(x)))
  out <- rep(NA_real_, length(x))
  out[sample_flag == 1] <- (x[sample_flag == 1] - m) / s
  out
}

center_within <- function(x, sample_flag) {
  m <- mean(x[sample_flag == 1], na.rm = TRUE)
  out <- rep(NA_real_, length(x))
  out[sample_flag == 1] <- x[sample_flag == 1] - m
  out
}

winsorize_within <- function(x, sample_flag, p = 0.99) {
  cap <- as.numeric(quantile(x[sample_flag == 1], probs = p, na.rm = TRUE))
  out <- x
  out[out > cap] <- cap
  out
}

# -----------------------------
# 1) Define perception item sets (EDIT THESE ONCE)
# -----------------------------
# Replace with your real column names from survey_clean
perception_sets <- list(
  trust_info = c("trust_info_sources","trust_info_gov","trust_info_np","trust_info_ff","trust_info_sm","trust_info_nm"),
  thoughts_educ = c("thoughts_educ_1","thoughts_educ_2"),
  tot_int = c("tot_int_1","tot_int_2")
)

# -----------------------------
# 2) Construct variables
# -----------------------------
survey_analysis <- survey_clean %>%
  mutate(
    # time transforms (useful in regressions)
    log_time_reading = log1p(time_reading_sec),

    # optional: cap extreme reading times at p99 within analysis sample
    time_reading_sec_p99 = winsorize_within(time_reading_sec, analysis_sample_main, p = 0.99),

    # centered versions for heterogeneity interactions
    time_reading_c = center_within(time_reading_sec, analysis_sample_main),
    log_time_reading_c = center_within(log_time_reading, analysis_sample_main)
  )

# -----------------------------
# 3) Build perception scales + z-scores (if you define perception_sets)
# -----------------------------
if (length(perception_sets) > 0) {
  for (nm in names(perception_sets)) {
    cols <- perception_sets[[nm]]

    survey_analysis[[paste0(nm, "_mean")]] <- row_mean_scale(survey_analysis, cols)
    survey_analysis[[paste0(nm, "_z")]] <- z_within(survey_analysis[[paste0(nm, "_mean")]],
                                                   survey_analysis$analysis_sample_main)
    survey_analysis[[paste0(nm, "_c")]] <- center_within(survey_analysis[[paste0(nm, "_mean")]],
                                                        survey_analysis$analysis_sample_main)
  }

  z_cols <- paste0(names(perception_sets), "_z")
  survey_analysis <- survey_analysis %>%
    mutate(
      perceptions_index_z = rowMeans(across(all_of(z_cols)), na.rm = TRUE),
      perceptions_index_z = if_else(is.nan(perceptions_index_z), NA_real_, perceptions_index_z),
      missing_perceptions_flag = as.integer(is.na(perceptions_index_z))
    )
} else {
  survey_analysis <- survey_analysis %>%
    mutate(
      perceptions_index_z = NA_real_,
      missing_perceptions_flag = NA_integer_
    )
}

# -----------------------------
# 4) Quick sanity prints (console-friendly)
# -----------------------------
message("N clean: ", nrow(survey_clean))
message("N analysis_sample_main: ", sum(survey_clean$analysis_sample_main == 1, na.rm = TRUE))
message("Reading time p99 (main sample): ",
        as.numeric(quantile(survey_clean$time_reading_sec[survey_clean$analysis_sample_main == 1],
                            0.99, na.rm = TRUE)))

# -----------------------------
# 5) Save
# -----------------------------
saveRDS(survey_analysis, out_rds)
message("Construct step complete: ", out_rds)

