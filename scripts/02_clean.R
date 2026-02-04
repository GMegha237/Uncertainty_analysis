# scripts/02_clean.R
library(tidyverse)
library(lubridate)

raw_rds <- "data/processed/01_survey_raw.rds"
out_rds <- "data/processed/02_survey_clean.rds"
out_csv <- "data/processed/02_survey_clean.csv"
out_paradata_rds <- "data/processed/02_survey_paradata_qc.rds"

survey_raw <- readRDS(raw_rds)
stopifnot(nrow(survey_raw) > 0)

# Helper: robust numeric parsing
num <- function(x) readr::parse_number(as.character(x))

# 0) Trim whitespace in column names FIRST
names(survey_raw) <- trimws(names(survey_raw))

# 1) Drop Qualtrics metadata rows
survey_raw <- survey_raw |> slice(-(1:2))

message("Rows after metadata drop: ", nrow(survey_raw))
message("Cols: ", ncol(survey_raw))
if ("condition" %in% names(survey_raw)) {
  print(sort(unique(survey_raw$condition)))
}

# 2) Parse core meta fields early
survey_raw <- survey_raw |>
  mutate(
    consent = num(Consent),
    finished = num(Finished),
    progress = num(Progress),
    duration_sec = num(`Duration (in seconds)`)
  )

stopifnot(all(c("consent", "finished", "duration_sec") %in% names(survey_raw)))

# 3) Assignment parsing (arm/group/counterbalance/core_sample)
survey_raw <- survey_raw |>
  mutate(
    condition_raw = str_trim(str_to_lower(as.character(condition))),
    arm = case_when(
      str_detect(condition_raw, "^[1-5][ab]$") ~ condition_raw,
      condition_raw == "5c" ~ "5c",
      TRUE ~ NA_character_
    ),
    arm = factor(arm, levels = c("1a", "1b", "2a", "2b", "3a", "3b", "4a", "4b", "5a", "5b", "5c")),
    group = as.integer(str_sub(as.character(arm), 1, 1)),
    counterbalance = str_sub(as.character(arm), 2, 2),
    core_sample = group %in% 1:4
  )

stopifnot(!any(is.na(survey_raw$arm)))

# 4) Factorial treatments (confirmed by your table)
survey_raw <- survey_raw |>
  mutate(
    uncertainty_disclosed = case_when(
      group %in% c(1, 2) ~ 1L,
      group %in% c(3, 4) ~ 0L,
      TRUE ~ NA_integer_
    ),
    visual = case_when(
      group %in% c(2, 4) ~ 1L,
      group %in% c(1, 3) ~ 0L,
      TRUE ~ NA_integer_
    )
  )

# --- Helpers used below  ---

# coalesce a vector of column names (returns a single vector)
coalesce_cols <- function(dat, cols) {
  cols <- intersect(cols, names(dat))
  if (length(cols) == 0) return(rep(NA, nrow(dat)))
  purrr::reduce(dat[cols], dplyr::coalesce)
}

# attempt to parse Qualtrics timing fields to numeric "seconds"
ts_to_num <- function(x) {
  x_chr <- as.character(x)
  x_num <- suppressWarnings(readr::parse_number(x_chr))
  if (sum(!is.na(x_num)) >= sum(!is.na(x_chr)) * 0.9) return(x_num)

  x_dt <- suppressWarnings(lubridate::ymd_hms(x_chr, quiet = TRUE))
  as.numeric(x_dt)
}

# safe duration: submit - first; clamp negatives to 0
dur_sec <- function(first_ts, submit_ts) {
  d <- submit_ts - first_ts
  ifelse(is.na(d), NA_real_, pmax(d, 0))
}

# --- Chunk 5.0: coalesce timing fields across arms ---

# Build lists of timing columns across all arms
header_first_cols <- grep(
  "^(1[ab]|2[ab]|3[ab]|4[ab]|5[ab])_header_time_First Click$", 
  names(survey_raw), 
  value = TRUE
)
header_submit_cols <- grep(
  "^(1[ab]|2[ab]|3[ab]|4[ab]|5[ab])_header_time_Page Submit$", 
  names(survey_raw), 
  value = TRUE
)
header_click_cols <- grep(
  "^(1[ab]|2[ab]|3[ab]|4[ab]|5[ab])_header_time_Click Count$", 
  names(survey_raw), 
  value = TRUE
)

evid_first_cols <- grep(
  "^(1[ab]|2[ab]|3[ab]|4[ab]|5[ab])_both_evid_time_First Click$", 
  names(survey_raw), 
  value = TRUE
)
evid_submit_cols <- grep(
  "^(1[ab]|2[ab]|3[ab]|4[ab]|5[ab])_both_evid_time_Page Submit$", 
  names(survey_raw), 
  value = TRUE
)
evid_click_cols <- grep(
  "^(1[ab]|2[ab]|3[ab]|4[ab]|5[ab])_both_evid_time_Click Count$", 
  names(survey_raw), 
  value = TRUE
)

survey_raw <- survey_raw |>
  mutate(
    header_first_raw = coalesce_cols(survey_raw, header_first_cols),
    header_submit_raw = coalesce_cols(survey_raw, header_submit_cols),
    header_clicks_raw = coalesce_cols(survey_raw, header_click_cols),

    evidence_first_raw = coalesce_cols(survey_raw, evid_first_cols),
    evidence_submit_raw = coalesce_cols(survey_raw, evid_submit_cols),
    evidence_clicks_raw = coalesce_cols(survey_raw, evid_click_cols)
  )

# --- Chunk 5.1: compute reading time measures ---

survey_raw <- survey_raw |>
  mutate(
    header_first_ts = ts_to_num(header_first_raw),
    header_submit_ts = ts_to_num(header_submit_raw),
    evidence_first_ts = ts_to_num(evidence_first_raw),
    evidence_submit_ts = ts_to_num(evidence_submit_raw),

    header_clicks = num(header_clicks_raw),
    evidence_clicks = num(evidence_clicks_raw),

    time_header_sec = dur_sec(header_first_ts, header_submit_ts),
    time_evidence_sec = dur_sec(evidence_first_ts, evidence_submit_ts),

    time_reading_sec = dplyr::coalesce(time_header_sec, 0) + 
      dplyr::coalesce(time_evidence_sec, 0),
    time_reading_min = time_reading_sec / 60,
    reading_clicks = dplyr::coalesce(header_clicks, 0) + 
      dplyr::coalesce(evidence_clicks, 0),
    log_time_reading = log1p(time_reading_sec),

    zero_evidence_clicks = as.integer(!is.na(evidence_clicks) & evidence_clicks == 0)
  )

# Quick sanity summaries
summary(survey_raw$time_header_sec)
summary(survey_raw$time_evidence_sec)
summary(survey_raw$time_reading_sec)

# --- Chunk 5.2: reading-time flags (define within core sample & complete) ---

base_for_cutoffs <- survey_raw %>%
  filter(core_sample == TRUE, consent == 1, finished == 1) %>%
  pull(time_reading_sec)

p25 <- as.numeric(quantile(base_for_cutoffs, 0.25, na.rm = TRUE))
p10 <- as.numeric(quantile(base_for_cutoffs, 0.10, na.rm = TRUE))

survey_raw <- survey_raw %>%
  mutate(
    low_reading_time_p25 = as.integer(!is.na(time_reading_sec) & time_reading_sec < p25),
    very_low_reading_time_p10 = as.integer(!is.na(time_reading_sec) & time_reading_sec < p10)
  )

# --- Chunk 6.0: unify allocation into generic option slots ---

opt1_cols <- c(
  grep("^C[1-4][ab]_donation_1$", names(survey_raw), value = TRUE),
  grep("^C5[ab]_donation_1$", names(survey_raw), value = TRUE),
  grep("^C5[bc]_donation_4$", names(survey_raw), value = TRUE)
)

opt2_cols <- c(
  grep("^C[1-4][ab]_donation_2$", names(survey_raw), value = TRUE),
  grep("^C5[ab]_donation_2$", names(survey_raw), value = TRUE),
  grep("^C5[bc]_donation_5$", names(survey_raw), value = TRUE)
)

why_cols <- grep("^C[1-5][ab]_don_why$|^C5c_don_why$", names(survey_raw), value = TRUE)

survey_raw <- survey_raw %>%
  mutate(
    alloc_opt1 = num(coalesce_cols(., opt1_cols)),
    alloc_opt2 = num(coalesce_cols(., opt2_cols)),
    alloc_total = alloc_opt1 + alloc_opt2,
    alloc_sum500_flag = as.integer(!is.na(alloc_total) & alloc_total == 500),
    alloc_reason_text = coalesce_cols(., why_cols)
  )

# quick check
table(survey_raw$alloc_sum500_flag, useNA = "ifany")

# --- Section 7: order + truth (core 1–4) + allocation mapping ---

survey_raw <- survey_raw %>%
  mutate(
    # Order: groups 1-4 counterbalanced, group 5 always FI first
    first_shown = case_when(
      group == 5 ~ "fi",              # All group 5: FI first (no swap)
      counterbalance == "a" ~ "fi",   # Groups 1-4: FI then CT
      counterbalance == "b" ~ "ct",   # Groups 1-4: CT then FI
      TRUE ~ NA_character_
    ),
    second_shown = case_when(
      group == 5 ~ "ct",              # All group 5: CT second
      first_shown == "fi" ~ "ct",
      first_shown == "ct" ~ "fi",
      TRUE ~ NA_character_
    ),

    # Truth in core arms: FIRST option is always higher effect (and in truth more uncertain)
    high_effect_true    = if_else(core_sample, first_shown, NA_character_),
    more_uncertain_true = if_else(core_sample, first_shown, NA_character_),

    # Uncertainty was only SHOWN in groups 1–2
    uncertainty_applicable = as.integer(core_sample & uncertainty_disclosed == 1L),

    # Allocation mapping to FI/CT
    alloc_fi = case_when(
      first_shown == "fi" ~ alloc_opt1,
      first_shown == "ct" ~ alloc_opt2,
      TRUE ~ NA_real_
    ),
    alloc_ct = case_when(
      first_shown == "fi" ~ alloc_opt2,
      first_shown == "ct" ~ alloc_opt1,
      TRUE ~ NA_real_
    ),

    # Core truth-aligned allocations (core arms only)
    alloc_high_effect_true = if_else(core_sample, alloc_opt1, NA_real_),
    alloc_share_high_effect_true = alloc_high_effect_true / 500,
    alloc_more_uncertain_true = if_else(core_sample, alloc_opt1, NA_real_),
    alloc_share_more_uncertain_true = alloc_more_uncertain_true / 500,

    # "Visible uncertainty" allocation only when uncertainty was shown
    alloc_more_uncertain_visible = if_else(uncertainty_applicable == 1L, alloc_opt1, NA_real_),

    # --- D) extra allocation summaries ---
    alloc_diff_fi_ct = alloc_fi - alloc_ct,
    alloc_extreme_flag = as.integer(!is.na(alloc_fi) & (alloc_fi %in% c(0, 500)))
  )


# --- Section 8.0: coalesce effectiveness/uncertainty comparison items ---

eff_cols <- grep("^C[1-5][ab]_effectiveness$", names(survey_raw), value = TRUE)
unc_cols <- grep("^C[1-5][ab]_uncertainty$", names(survey_raw), value = TRUE)

survey_raw <- survey_raw %>%
  mutate(
    eff_comp_raw = coalesce_cols(., eff_cols),
    unc_comp_raw = coalesce_cols(., unc_cols),
    eff_comp_num = num(eff_comp_raw),
    unc_comp_num = num(unc_comp_raw)
  )

table(survey_raw$eff_comp_num, useNA = "ifany")
table(survey_raw$unc_comp_num, useNA = "ifany")


# --- Section 8.1: recode 1/2/3/4 into opt1/opt2/same/dk (EDIT ONCE to match Qualtrics) ---

eff_code_map <- c("1"="opt1", "2"="opt2", "3"="same", "4"="dk")
unc_code_map <- c("1"="opt1", "2"="opt2", "3"="same", "4"="dk")

recode_comp <- function(x_num, map_vec) {
  x_chr <- as.character(x_num)
  out <- unname(map_vec[x_chr])
  factor(out, levels = c("opt1","opt2","same","dk"))
}

survey_raw <- survey_raw %>%
  mutate(
    eff_comp_cat_opt = recode_comp(eff_comp_num, eff_code_map),
    unc_comp_cat_opt = recode_comp(unc_comp_num, unc_code_map)
  )


# --- Section 8.2: correctness scoring (core design logic) ---

survey_raw <- survey_raw %>%
  mutate(
    # Effectiveness is meaningful in all core arms (they see point estimates everywhere)
    eff_correct = as.integer(core_sample & eff_comp_cat_opt == "opt1"),

    # Uncertainty: primary score only where uncertainty was SHOWN (groups 1–2)
    unc_correct_visible = as.integer(uncertainty_applicable == 1L & unc_comp_cat_opt == "opt1"),

    # Optional: inference score across all core arms (interpretation differs in 3–4)
    unc_correct_infer = as.integer(core_sample & unc_comp_cat_opt == "opt1"),

    both_correct_visible = as.integer(eff_correct == 1L & unc_correct_visible == 1L),

    eff_dk = as.integer(eff_comp_cat_opt == "dk"),
    unc_dk = as.integer(unc_comp_cat_opt == "dk"),
    eff_same = as.integer(eff_comp_cat_opt == "same"),
    unc_same = as.integer(unc_comp_cat_opt == "same")
  )


# --- Section 9.0: completion and duplicate flags ---

stopifnot(all(c("prolific_pid","consent","finished") %in% names(survey_raw)) | all(c("PROLIFIC_PID","consent","finished") %in% names(survey_raw)))

# standardize prolific id name if not already done
if (!"prolific_pid" %in% names(survey_raw) && "PROLIFIC_PID" %in% names(survey_raw)) {
  survey_raw <- survey_raw %>% mutate(prolific_pid = PROLIFIC_PID)
}

survey_raw <- survey_raw %>%
  mutate(
    complete = as.integer(consent == 1 & finished == 1)
  )

dup_tbl <- survey_raw %>%
  count(prolific_pid, name = "pid_n") %>%
  mutate(duplicate_pid_flag = as.integer(!is.na(prolific_pid) & prolific_pid != "" & pid_n > 1)) %>%
  select(prolific_pid, duplicate_pid_flag)

survey_raw <- survey_raw %>%
  left_join(dup_tbl, by = "prolific_pid")

# --- Section 9.1: speed flags ---

stopifnot("duration_sec" %in% names(survey_raw))

dur_base <- survey_raw %>% filter(complete == 1) %>% pull(duration_sec)

p01 <- as.numeric(quantile(dur_base, 0.01, na.rm = TRUE))
p05 <- as.numeric(quantile(dur_base, 0.05, na.rm = TRUE))

survey_raw <- survey_raw %>%
  mutate(
    speed_flag_p1 = as.integer(!is.na(duration_sec) & duration_sec < p01),
    speed_flag_p5 = as.integer(!is.na(duration_sec) & duration_sec < p05)
  )

# --- Section 9.2: missing-outcome flags ---

survey_raw <- survey_raw %>%
  mutate(
    missing_perceptions_flag = NA_integer_,  # fill later when perceptions are built
    missing_allocation_flag = as.integer(is.na(alloc_fi) | is.na(alloc_ct)),
    missing_comp_checks_flag = as.integer(is.na(eff_comp_num) | is.na(unc_comp_num)),
    missing_main_outcomes_flag = as.integer(
      missing_allocation_flag == 1 | missing_comp_checks_flag == 1
    )
  )

# --- Section 9.3: analysis sample indicators (editable) ---

survey_raw <- survey_raw %>%
  mutate(
    analysis_sample_main = as.integer(complete == 1 & core_sample == TRUE),
    analysis_sample_strict = as.integer(complete == 1 & core_sample == TRUE &
                                          duplicate_pid_flag == 0 &
                                          speed_flag_p5 == 0 &
                                          alloc_sum500_flag == 1)
  )

# --- Section 10.0: assemble survey_clean ---

survey_clean <- survey_raw %>%
  transmute(
    # IDs
    response_id = ResponseId,
    prolific_pid,
    session_id = SESSION_ID,
    study_id = STUDY_ID,

    # assignment
    arm, group, counterbalance, core_sample,
    uncertainty_disclosed, visual,

    # completion/QC
    consent, finished, complete,
    duration_sec, progress,
    duplicate_pid_flag, speed_flag_p1, speed_flag_p5,
    alloc_sum500_flag,
    low_reading_time_p25, very_low_reading_time_p10, zero_evidence_clicks,
    analysis_sample_main, analysis_sample_strict,

    # baseline perceptions / covariates (carry through)
    trust_disposition, trust_info_sources, trust_info_gov, trust_info_np, trust_info_ff,
    trust_info_sm, trust_info_nm,
    thoughts_educ_1, thoughts_educ_2, Ed_activity,
    tot_int_1, tot_int_2,
    sd_und, SC0,
    Education, Age, Gender, `Statistical knowhow`, prof_backg,

    # engagement
    time_header_sec, time_evidence_sec, time_reading_sec, time_reading_min,
    header_clicks, evidence_clicks, reading_clicks, log_time_reading,

    # allocation outcomes (generic + aligned)
    alloc_opt1, alloc_opt2, alloc_fi, alloc_ct, alloc_total,
    alloc_high_effect_true, alloc_share_high_effect_true,
    alloc_more_uncertain_true, alloc_share_more_uncertain_true,
    alloc_more_uncertain_visible,
    alloc_diff_fi_ct, alloc_extreme_flag,
    alloc_reason_text,

    # comparative checks (option-space)
    eff_comp_num, unc_comp_num,
    eff_comp_cat_opt, unc_comp_cat_opt,
    eff_correct, unc_correct_visible, unc_correct_infer, both_correct_visible,
    eff_dk, unc_dk, eff_same, unc_same,

    # truth/order vars
    first_shown, second_shown,
    high_effect_true, more_uncertain_true,
    uncertainty_applicable
  )

# --- Section 10.1: paradata QC output ---

timing_cols <- grep("(header_time|both_evid_time)_(First Click|Last Click|Page Submit|Click Count)$",
                    names(survey_raw), value = TRUE)

survey_paradata_qc <- survey_raw %>%
  select(ResponseId, PROLIFIC_PID, arm, all_of(timing_cols))

# --- sanity checks before saving ---
stopifnot(nrow(survey_clean) > 0)
stopifnot(!any(duplicated(names(survey_clean))))
stopifnot(all(c("alloc_diff_fi_ct","alloc_extreme_flag") %in% names(survey_clean)))

# optional extra check (recommended)
stopifnot(all(!is.na(survey_clean$response_id)))

# --- Section 10.2: save outputs ---

saveRDS(survey_clean, "data/processed/02_survey_clean.rds")
saveRDS(survey_paradata_qc, "data/processed/02_survey_paradata_qc.rds")

readr::write_csv(survey_clean, "data/processed/02_survey_clean.csv")

message("Saved: data/processed/02_survey_clean.rds")
message("Saved: data/processed/02_survey_paradata_qc.rds")
message("Saved: data/processed/02_survey_clean.csv")

# --- END OF SCRIPT ---