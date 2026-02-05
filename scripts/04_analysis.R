# scripts/04_analysis.R

# Descriptive Statistics Check for survey_analysis
# This script generates comprehensive summaries to verify data processing

library(tidyverse)

survey_analysis <- readRDS("data/processed/03_survey_analysis.rds")

cat(strrep("=", 80), "\n")
cat("OVERALL DATASET CHECK\n")
cat(strrep("=", 80), "\n\n")

cat("Total N:", nrow(survey_analysis), "\n")
cat("Total variables:", ncol(survey_analysis), "\n\n")

# =============================================================================
# 1) EXPERIMENTAL ASSIGNMENT VARIABLES
# =============================================================================
cat("\n", strrep("=", 80), "\n")
cat("1) EXPERIMENTAL ASSIGNMENT\n")
cat(strrep("=", 80), "\n\n")

cat("ARM DISTRIBUTION:\n")
print(table(survey_analysis$arm, useNA = "ifany"))
cat("\n")

cat("GROUP DISTRIBUTION:\n")
print(table(survey_analysis$group, useNA = "ifany"))
cat("\n")

cat("COUNTERBALANCE DISTRIBUTION:\n")
print(table(survey_analysis$counterbalance, useNA = "ifany"))
cat("\n")

cat("CORE SAMPLE:\n")
print(table(survey_analysis$core_sample, useNA = "ifany"))
cat("\n")

cat("FACTORIAL DESIGN (Groups 1-4):\n")
cat("Uncertainty Disclosed:\n")
print(table(survey_analysis$uncertainty_disclosed, survey_analysis$group, useNA = "ifany"))
cat("\nVisual Treatment:\n")
print(table(survey_analysis$visual, survey_analysis$group, useNA = "ifany"))
cat("\n")

cat("2x2 FACTORIAL (should see 4 cells for groups 1-4):\n")
with(survey_analysis |> filter(core_sample == TRUE), 
     print(table(uncertainty_disclosed, visual, useNA = "ifany")))
cat("\n")

# =============================================================================
# 2) COMPLETION & QUALITY FLAGS
# =============================================================================
cat("\n", strrep("=", 80), "\n")
cat("2) COMPLETION & QUALITY FLAGS\n")
cat(strrep("=", 80), "\n\n")

cat("CONSENT:\n")
print(table(survey_analysis$consent, useNA = "ifany"))
cat("\n")

cat("FINISHED:\n")
print(table(survey_analysis$finished, useNA = "ifany"))
cat("\n")

cat("COMPLETE (consent=1 & finished=1):\n")
print(table(survey_analysis$complete, useNA = "ifany"))
cat("\n")

cat("ANALYSIS SAMPLES:\n")
cat("Main (complete + core):\n")
print(table(survey_analysis$analysis_sample_main, useNA = "ifany"))
cat("Strict (main + quality filters):\n")
print(table(survey_analysis$analysis_sample_strict, useNA = "ifany"))
cat("\n")

cat("QUALITY FLAGS:\n")
cat("Duplicate PID:", sum(survey_analysis$duplicate_pid_flag == 1, na.rm = TRUE), "\n")
cat("Speed flag p1:", sum(survey_analysis$speed_flag_p1 == 1, na.rm = TRUE), "\n")
cat("Speed flag p5:", sum(survey_analysis$speed_flag_p5 == 1, na.rm = TRUE), "\n")
cat("Allocation doesn't sum to 500:", sum(survey_analysis$alloc_sum500_flag == 0, na.rm = TRUE), "\n")
cat("Low reading time (p25):", sum(survey_analysis$low_reading_time_p25 == 1, na.rm = TRUE), "\n")
cat("Very low reading time (p10):", sum(survey_analysis$very_low_reading_time_p10 == 1, na.rm = TRUE), "\n")
cat("Zero evidence clicks:", sum(survey_analysis$zero_evidence_clicks == 1, na.rm = TRUE), "\n\n")

# =============================================================================
# 3) ORDER & TRUTH VARIABLES
# =============================================================================
cat("\n", strrep("=", 80), "\n")
cat("3) ORDER & TRUTH ASSIGNMENT\n")
cat(strrep("=", 80), "\n\n")

cat("FIRST SHOWN (should be 'fi' or 'ct'):\n")
print(table(survey_analysis$first_shown, survey_analysis$group, useNA = "ifany"))
cat("\n")

cat("SECOND SHOWN (should be opposite of first):\n")
print(table(survey_analysis$second_shown, survey_analysis$group, useNA = "ifany"))
cat("\n")

cat("CHECK: Group 5 should ALL have FI first:\n")
with(survey_analysis |> filter(group == 5),
     print(table(first_shown, counterbalance, useNA = "ifany")))
cat("\n")

cat("HIGH EFFECT TRUE (core sample only, should match first_shown):\n")
with(survey_analysis |> filter(core_sample == TRUE),
     print(table(high_effect_true, first_shown, useNA = "ifany")))
cat("\n")

# =============================================================================
# 4) TIME & ENGAGEMENT VARIABLES
# =============================================================================
cat("\n", strrep("=", 80), "\n")
cat("4) TIME & ENGAGEMENT MEASURES\n")
cat(strrep("=", 80), "\n\n")

cat("DURATION (entire survey, seconds):\n")
print(summary(survey_analysis$duration_sec))
cat("\n")

cat("TIME ON HEADER PAGE (seconds):\n")
print(summary(survey_analysis$time_header_sec))
cat("\n")

cat("TIME ON EVIDENCE PAGE (seconds):\n")
print(summary(survey_analysis$time_evidence_sec))
cat("\n")

cat("TOTAL READING TIME (seconds):\n")
print(summary(survey_analysis$time_reading_sec))
cat("In minutes:\n")
print(summary(survey_analysis$time_reading_min))
cat("\n")

cat("CLICKS:\n")
cat("Header clicks:\n")
print(summary(survey_analysis$header_clicks))
cat("Evidence clicks:\n")
print(summary(survey_analysis$evidence_clicks))
cat("Total reading clicks:\n")
print(summary(survey_analysis$reading_clicks))
cat("\n")

cat("TRANSFORMED TIME VARIABLES:\n")
cat("Log(1 + reading time):\n")
print(summary(survey_analysis$log_time_reading))
cat("Reading time (winsorized at p99):\n")
print(summary(survey_analysis$time_reading_sec_p99))
cat("Reading time (centered, analysis sample only):\n")
print(summary(survey_analysis$time_reading_c[survey_analysis$analysis_sample_main == 1]))
cat("\n")

# =============================================================================
# 5) ALLOCATION OUTCOMES
# =============================================================================
cat("\n", strrep("=", 80), "\n")
cat("5) ALLOCATION/DONATION OUTCOMES\n")
cat(strrep("=", 80), "\n\n")

cat("GENERIC ALLOCATIONS (option 1 & 2):\n")
cat("Option 1:\n")
print(summary(survey_analysis$alloc_opt1))
cat("Option 2:\n")
print(summary(survey_analysis$alloc_opt2))
cat("Total (should be 500 for valid responses):\n")
print(summary(survey_analysis$alloc_total))
cat("\n")

cat("ALLOCATIONS BY CHARITY (FI vs CT):\n")
cat("Forest Initiative (FI):\n")
print(summary(survey_analysis$alloc_fi))
cat("Clean Transport (CT):\n")
print(summary(survey_analysis$alloc_ct))
cat("Difference (FI - CT):\n")
print(summary(survey_analysis$alloc_diff_fi_ct))
cat("\n")

cat("TRUTH-ALIGNED ALLOCATIONS (core sample only):\n")
cat("Allocation to high-effect option:\n")
print(summary(survey_analysis$alloc_high_effect_true))
cat("Share to high-effect (0-1):\n")
print(summary(survey_analysis$alloc_share_high_effect_true))
cat("Allocation to more uncertain option:\n")
print(summary(survey_analysis$alloc_more_uncertain_true))
cat("Share to more uncertain (0-1):\n")
print(summary(survey_analysis$alloc_share_more_uncertain_true))
cat("\n")

cat("ALLOCATION TO VISIBLE UNCERTAINTY (groups 1-2 only):\n")
print(summary(survey_analysis$alloc_more_uncertain_visible))
cat("\n")

cat("EXTREME ALLOCATIONS (all-or-nothing):\n")
print(table(survey_analysis$alloc_extreme_flag, useNA = "ifany"))
cat("\n")

# =============================================================================
# 6) COMPREHENSION CHECKS
# =============================================================================
cat("\n", strrep("=", 80), "\n")
cat("6) COMPREHENSION CHECK OUTCOMES\n")
cat(strrep("=", 80), "\n\n")

cat("EFFECTIVENESS COMPARISON (numeric codes):\n")
print(table(survey_analysis$eff_comp_num, useNA = "ifany"))
cat("\n")

cat("EFFECTIVENESS COMPARISON (categorical):\n")
print(table(survey_analysis$eff_comp_cat_opt, useNA = "ifany"))
cat("\n")

cat("UNCERTAINTY COMPARISON (numeric codes):\n")
print(table(survey_analysis$unc_comp_num, useNA = "ifany"))
cat("\n")

cat("UNCERTAINTY COMPARISON (categorical):\n")
print(table(survey_analysis$unc_comp_cat_opt, useNA = "ifany"))
cat("\n")

cat("CORRECTNESS (core sample):\n")
cat("Effectiveness correct:", sum(survey_analysis$eff_correct == 1, na.rm = TRUE), 
    "/", sum(survey_analysis$core_sample == 1, na.rm = TRUE), "\n")
cat("Uncertainty correct (visible, groups 1-2):", 
    sum(survey_analysis$unc_correct_visible == 1, na.rm = TRUE),
    "/", sum(survey_analysis$uncertainty_applicable == 1, na.rm = TRUE), "\n")
cat("Both correct:", sum(survey_analysis$both_correct_visible == 1, na.rm = TRUE), "\n")
cat("\n")

cat("DON'T KNOW RESPONSES:\n")
cat("Effectiveness DK:", sum(survey_analysis$eff_dk == 1, na.rm = TRUE), "\n")
cat("Uncertainty DK:", sum(survey_analysis$unc_dk == 1, na.rm = TRUE), "\n")
cat("\n")

cat("SAME RESPONSES:\n")
cat("Effectiveness Same:", sum(survey_analysis$eff_same == 1, na.rm = TRUE), "\n")
cat("Uncertainty Same:", sum(survey_analysis$unc_same == 1, na.rm = TRUE), "\n\n")

# =============================================================================
# 7) PERCEPTION ITEMS
# =============================================================================
cat("\n", strrep("=", 80), "\n")
cat("7) PERCEPTION ITEMS (Charity-specific)\n")
cat(strrep("=", 80), "\n\n")

perception_items <- c("acc", "unc", "trust", "clear", "trans", "info", "dets")

for (item in perception_items) {
  cat(toupper(item), "PERCEPTIONS:\n")
  cat("FI:\n")
  print(summary(survey_analysis[[paste0(item, "_fi")]]))
  cat("CT:\n")
  print(summary(survey_analysis[[paste0(item, "_ct")]]))
  cat("First shown:\n")
  print(summary(survey_analysis[[paste0(item, "_first")]]))
  cat("Second shown:\n")
  print(summary(survey_analysis[[paste0(item, "_second")]]))
  cat("\n")
}

# =============================================================================
# 8) BASELINE PERCEPTIONS & COVARIATES
# =============================================================================
cat("\n", strrep("=", 80), "\n")
cat("8) BASELINE PERCEPTIONS & COVARIATES\n")
cat(strrep("=", 80), "\n\n")

cat("TRUST DISPOSITION:\n")
print(summary(survey_analysis$trust_disposition))
cat("\n")

cat("TRUST IN INFORMATION SOURCES (individual items):\n")
trust_items <- c("trust_info_sources", "trust_info_gov", "trust_info_np", 
                 "trust_info_ff", "trust_info_sm", "trust_info_nm")
for (item in trust_items) {
  cat(item, ":\n")
  print(summary(survey_analysis[[item]]))
}
cat("\n")

cat("EDUCATION-RELATED THOUGHTS:\n")
print(summary(survey_analysis$thoughts_educ_1))
print(summary(survey_analysis$thoughts_educ_2))
cat("\n")

cat("TIME-ON-TASK INTENTIONS:\n")
print(summary(survey_analysis$tot_int_1))
print(summary(survey_analysis$tot_int_2))
cat("\n")

cat("DEMOGRAPHICS:\n")
cat("Education:\n")
print(table(survey_analysis$Education, useNA = "ifany"))
cat("\nAge:\n")
print(summary(survey_analysis$Age))
cat("\nGender:\n")
print(table(survey_analysis$Gender, useNA = "ifany"))
cat("\nStatistical knowhow:\n")
print(summary(survey_analysis$`Statistical knowhow`))
cat("\nProfessional background:\n")
print(table(survey_analysis$prof_backg, useNA = "ifany"))
cat("\n")

# =============================================================================
# 9) CONSTRUCTED SCALES
# =============================================================================
cat("\n", strrep("=", 80), "\n")
cat("9) CONSTRUCTED PERCEPTION SCALES\n")
cat(strrep("=", 80), "\n\n")

cat("TRUST IN INFO SCALE:\n")
cat("Mean scale:\n")
print(summary(survey_analysis$trust_info_mean))
cat("Z-scored (analysis sample):\n")
print(summary(survey_analysis$trust_info_z[survey_analysis$analysis_sample_main == 1]))
cat("Centered:\n")
print(summary(survey_analysis$trust_info_c[survey_analysis$analysis_sample_main == 1]))
cat("\n")

cat("THOUGHTS ABOUT EDUCATION SCALE:\n")
cat("Mean scale:\n")
print(summary(survey_analysis$thoughts_educ_mean))
cat("Z-scored (analysis sample):\n")
print(summary(survey_analysis$thoughts_educ_z[survey_analysis$analysis_sample_main == 1]))
cat("\n")

cat("TIME-ON-TASK INTENTIONS SCALE:\n")
cat("Mean scale:\n")
print(summary(survey_analysis$tot_int_mean))
cat("Z-scored (analysis sample):\n")
print(summary(survey_analysis$tot_int_z[survey_analysis$analysis_sample_main == 1]))
cat("\n")

cat("OVERALL PERCEPTIONS INDEX:\n")
cat("Z-scored composite (analysis sample):\n")
print(summary(survey_analysis$perceptions_index_z[survey_analysis$analysis_sample_main == 1]))
cat("\nMissing perceptions flag:", 
    sum(survey_analysis$missing_perceptions_flag == 1, na.rm = TRUE), "\n\n")

# =============================================================================
# 10) CROSS-CHECKS & VALIDATION
# =============================================================================
cat("\n", strrep("=", 80), "\n")
cat("10) VALIDATION CHECKS\n")
cat(strrep("=", 80), "\n\n")

cat("CHECK 1: First + Second should always sum to FI + CT\n")
check1 <- survey_analysis %>%
  filter(!is.na(alloc_fi) & !is.na(alloc_ct)) %>%
  mutate(
    sum_fi_ct = alloc_fi + alloc_ct,
    sum_opt = alloc_opt1 + alloc_opt2,
    mismatch = abs(sum_fi_ct - sum_opt) > 0.01
  )
cat("Mismatches:", sum(check1$mismatch), "\n\n")

cat("CHECK 2: For core sample, alloc_high_effect_true should equal alloc_opt1\n")
check2 <- survey_analysis %>%
  filter(core_sample == TRUE, !is.na(alloc_high_effect_true)) %>%
  mutate(mismatch = abs(alloc_high_effect_true - alloc_opt1) > 0.01)
cat("Mismatches:", sum(check2$mismatch), "\n\n")

cat("CHECK 3: Group 5 should all have first_shown = 'fi'\n")
check3 <- survey_analysis %>%
  filter(group == 5) %>%
  summarise(
    all_fi = all(first_shown == "fi", na.rm = TRUE),
    n_fi = sum(first_shown == "fi", na.rm = TRUE),
    n_total = n()
  )
print(check3)
cat("\n")

cat("CHECK 4: Counterbalance distribution within groups 1-4\n")
check4 <- survey_analysis %>%
  filter(core_sample == TRUE) %>%
  count(group, counterbalance) %>%
  pivot_wider(names_from = counterbalance, values_from = n, values_fill = 0)
print(check4)
cat("\n")

cat("CHECK 5: Analysis sample composition\n")
check5 <- survey_analysis %>%
  group_by(group) %>%
  summarise(
    total = n(),
    complete = sum(complete == 1, na.rm = TRUE),
    analysis_main = sum(analysis_sample_main == 1, na.rm = TRUE),
    analysis_strict = sum(analysis_sample_strict == 1, na.rm = TRUE)
  )
print(check5)
cat("\n")

cat("CHECK 6: Missing data patterns in key outcomes\n")
check6 <- survey_analysis %>%
  summarise(
    N = n(),
    missing_alloc = sum(is.na(alloc_fi) | is.na(alloc_ct)),
    missing_eff_comp = sum(is.na(eff_comp_num)),
    missing_unc_comp = sum(is.na(unc_comp_num)),
    missing_time = sum(is.na(time_reading_sec)),
    missing_perceptions = sum(missing_perceptions_flag == 1, na.rm = TRUE)
  )
print(check6)
cat("\n")

# =============================================================================
# SUMMARY
# =============================================================================
cat("\n", strrep("=", 80), "\n")
cat("SUMMARY STATISTICS BY SAMPLE\n")
cat(strrep("=", 80), "\n\n")

summary_table <- survey_analysis %>%
  mutate(
    sample_type = case_when(
      analysis_sample_strict == 1 ~ "Strict Analysis Sample",
      analysis_sample_main == 1 ~ "Main Analysis Sample",
      complete == 1 & group == 5 ~ "Group 5 (Complete)",
      complete == 1 ~ "Other Complete",
      TRUE ~ "Incomplete"
    )
  ) %>%
  group_by(sample_type) %>%
  summarise(
    N = n(),
    mean_duration_min = mean(duration_sec / 60, na.rm = TRUE),
    mean_reading_min = mean(time_reading_min, na.rm = TRUE),
    mean_alloc_fi = mean(alloc_fi, na.rm = TRUE),
    pct_eff_correct = mean(eff_correct, na.rm = TRUE) * 100,
    pct_extreme = mean(alloc_extreme_flag, na.rm = TRUE) * 100
  )

print(summary_table)

cat("\n", strrep("=", 80), "\n")
cat("DESCRIPTIVE STATISTICS CHECK COMPLETE\n")
cat(strrep("=", 80), "\n")

# Statistical Significance Tests: Allocations & Perceptions Across Groups
# Tests treatment effects in the 2x2 factorial design (Groups 1-4)
# Also examines Group 5 separately

library(tidyverse)
library(broom)

survey_analysis <- readRDS("data/processed/03_survey_analysis.rds")

# Create analysis datasets
core_complete <- survey_analysis |>
  filter(analysis_sample_main == 1)

core_strict <- survey_analysis |>
  filter(analysis_sample_strict == 1)

group5_complete <- survey_analysis |>
  filter(group == 5, complete == 1)

cat(strrep("=", 80), "\n")
cat("STATISTICAL SIGNIFICANCE TESTS\n")
cat(strrep("=", 80), "\n\n")

cat("Sample sizes:\n")
cat("Core complete (groups 1-4):", nrow(core_complete), "\n")
cat("Core strict (quality filtered):", nrow(core_strict), "\n")
cat("Group 5 complete:", nrow(group5_complete), "\n\n")

# =============================================================================
# PART 1: ALLOCATION OUTCOMES - FACTORIAL DESIGN (GROUPS 1-4)
# =============================================================================

cat("\n", strrep("=", 80), "\n")
cat("PART 1: ALLOCATION OUTCOMES - 2x2 FACTORIAL DESIGN\n")
cat(strrep("=", 80), "\n\n")

cat("Treatment variables:\n")
cat("- uncertainty_disclosed (0=hidden, 1=shown)\n")
cat("- visual (0=text only, 1=visual representation)\n\n")

# --- 1.1: Allocation to high-effect option ---
cat("--- 1.1: ALLOCATION TO HIGH-EFFECT OPTION (Option 1 in core design) ---\n\n")

cat("DESCRIPTIVE STATISTICS by treatment:\n")
desc_high_effect <- core_complete |>
  group_by(uncertainty_disclosed, visual) |>
  summarise(
    n = n(),
    mean = mean(alloc_high_effect_true, na.rm = TRUE),
    sd = sd(alloc_high_effect_true, na.rm = TRUE),
    se = sd / sqrt(n),
    median = median(alloc_high_effect_true, na.rm = TRUE),
    .groups = "drop"
  )
print(desc_high_effect)
cat("\n")

cat("ANOVA: Main effects and interaction\n")
anova_high_effect <- lm(
  alloc_high_effect_true ~ uncertainty_disclosed * visual,
  data = core_complete
)
print(summary(anova_high_effect))
cat("\nANOVA table:\n")
print(anova(anova_high_effect))
cat("\n")

cat("PAIRWISE COMPARISONS (t-tests):\n")
cat("Uncertainty disclosed vs hidden (collapsing visual):\n")
t_unc_main <- t.test(
  alloc_high_effect_true ~ uncertainty_disclosed,
  data = core_complete
)
print(t_unc_main)
cat("\n")

cat("Visual vs text (collapsing uncertainty):\n")
t_vis_main <- t.test(
  alloc_high_effect_true ~ visual,
  data = core_complete
)
print(t_vis_main)
cat("\n")

# --- 1.2: Share allocated to high-effect option ---
cat("\n--- 1.2: SHARE ALLOCATED TO HIGH-EFFECT OPTION (proportion 0-1) ---\n\n")

cat("DESCRIPTIVE STATISTICS:\n")
desc_share <- core_complete |>
  group_by(uncertainty_disclosed, visual) |>
  summarise(
    n = n(),
    mean = mean(alloc_share_high_effect_true, na.rm = TRUE),
    sd = sd(alloc_share_high_effect_true, na.rm = TRUE),
    .groups = "drop"
  )
print(desc_share)
cat("\n")

cat("ANOVA:\n")
anova_share <- lm(
  alloc_share_high_effect_true ~ uncertainty_disclosed * visual,
  data = core_complete
)
print(summary(anova_share))
cat("\n")

# --- 1.3: Allocation to more uncertain option ---
cat("\n--- 1.3: ALLOCATION TO MORE UNCERTAIN OPTION (visible disclosure only) ---\n\n")
cat("NOTE: This only includes groups 1-2 where uncertainty was DISCLOSED\n\n")

disclosed_only <- core_complete |> filter(uncertainty_disclosed == 1)

cat("DESCRIPTIVE STATISTICS:\n")
desc_uncertain <- disclosed_only |>
  group_by(visual) |>
  summarise(
    n = n(),
    mean = mean(alloc_more_uncertain_visible, na.rm = TRUE),
    sd = sd(alloc_more_uncertain_visible, na.rm = TRUE),
    .groups = "drop"
  )
print(desc_uncertain)
cat("\n")

cat("T-TEST: Visual vs Text (within uncertainty-disclosed conditions):\n")
t_uncertain <- t.test(
  alloc_more_uncertain_visible ~ visual,
  data = disclosed_only
)
print(t_uncertain)
cat("\n")

# --- 1.4: Difference FI - CT ---
cat("\n--- 1.4: ALLOCATION DIFFERENCE (FI - CT) ---\n\n")

cat("DESCRIPTIVE STATISTICS:\n")
desc_diff <- core_complete |>
  group_by(uncertainty_disclosed, visual) |>
  summarise(
    n = n(),
    mean = mean(alloc_diff_fi_ct, na.rm = TRUE),
    sd = sd(alloc_diff_fi_ct, na.rm = TRUE),
    .groups = "drop"
  )
print(desc_diff)
cat("\n")

cat("ANOVA:\n")
anova_diff <- lm(
  alloc_diff_fi_ct ~ uncertainty_disclosed * visual,
  data = core_complete
)
print(summary(anova_diff))
cat("\n")

# --- 1.5: Extreme allocations (all-or-nothing) ---
cat("\n--- 1.5: EXTREME ALLOCATIONS (All-or-nothing: 0 or 500) ---\n\n")

cat("PROPORTIONS by treatment:\n")
prop_extreme <- core_complete |>
  group_by(uncertainty_disclosed, visual) |>
  summarise(
    n = n(),
    n_extreme = sum(alloc_extreme_flag == 1, na.rm = TRUE),
    prop_extreme = mean(alloc_extreme_flag, na.rm = TRUE),
    .groups = "drop"
  )
print(prop_extreme)
cat("\n")

cat("CHI-SQUARE TEST: Uncertainty disclosed\n")
tab_unc <- table(core_complete$uncertainty_disclosed, core_complete$alloc_extreme_flag)
print(tab_unc)
print(chisq.test(tab_unc))
cat("\n")

cat("CHI-SQUARE TEST: Visual\n")
tab_vis <- table(core_complete$visual, core_complete$alloc_extreme_flag)
print(tab_vis)
print(chisq.test(tab_vis))
cat("\n")

# --- 1.6: Allocation by group (all 4 groups separately) ---
cat("\n--- 1.6: ALLOCATION BY GROUP (Pairwise comparisons) ---\n\n")

cat("DESCRIPTIVE by group:\n")
desc_by_group <- core_complete |>
  group_by(group) |>
  summarise(
    n = n(),
    mean_high_effect = mean(alloc_high_effect_true, na.rm = TRUE),
    sd_high_effect = sd(alloc_high_effect_true, na.rm = TRUE),
    .groups = "drop"
  )
print(desc_by_group)
cat("\n")

cat("ONE-WAY ANOVA across 4 groups:\n")
anova_groups <- aov(alloc_high_effect_true ~ factor(group), data = core_complete)
print(summary(anova_groups))
cat("\n")

cat("POST-HOC PAIRWISE T-TESTS (Bonferroni correction):\n")
pairwise_groups <- pairwise.t.test(
  core_complete$alloc_high_effect_true,
  core_complete$group,
  p.adjust.method = "bonferroni"
)
print(pairwise_groups)
cat("\n")

# =============================================================================
# PART 2: PERCEPTION OUTCOMES - FACTORIAL DESIGN
# =============================================================================

cat("\n", strrep("=", 80), "\n")
cat("PART 2: PERCEPTION OUTCOMES - 2x2 FACTORIAL DESIGN\n")
cat(strrep("=", 80), "\n\n")

perception_vars <- list(
  "Accuracy (FI)" = "acc_fi",
  "Accuracy (CT)" = "acc_ct",
  "Uncertainty (FI)" = "unc_fi",
  "Uncertainty (CT)" = "unc_ct",
  "Trust (FI)" = "trust_fi",
  "Trust (CT)" = "trust_ct",
  "Clarity (FI)" = "clear_fi",
  "Clarity (CT)" = "clear_ct",
  "Transparency (FI)" = "trans_fi",
  "Transparency (CT)" = "trans_ct",
  "Informativeness (FI)" = "info_fi",
  "Informativeness (CT)" = "info_ct",
  "Details (FI)" = "dets_fi",
  "Details (CT)" = "dets_ct"
)

# Function to run factorial tests for a perception variable
test_perception <- function(data, var_name, var_label) {
  cat("\n--- ", var_label, " ---\n\n")

  # Descriptives
  desc <- data |>
    group_by(uncertainty_disclosed, visual) |>
    summarise(
      n = sum(!is.na(.data[[var_name]])),
      mean = mean(.data[[var_name]], na.rm = TRUE),
      sd = sd(.data[[var_name]], na.rm = TRUE),
      .groups = "drop"
    )
  cat("DESCRIPTIVES:\n")
  print(desc)
  cat("\n")

  # ANOVA
  formula_str <- paste(var_name, "~ uncertainty_disclosed * visual")
  model <- lm(as.formula(formula_str), data = data)

  cat("ANOVA:\n")
  anova_table <- anova(model)
  print(anova_table)

  # Extract p-values
  p_unc <- anova_table["uncertainty_disclosed", "Pr(>F)"]
  p_vis <- anova_table["visual", "Pr(>F)"]
  p_int <- anova_table["uncertainty_disclosed:visual", "Pr(>F)"]

  cat("\nKey p-values:\n")
  cat("Uncertainty disclosed:", format.pval(p_unc, digits = 3),
      ifelse(p_unc < 0.05, "***", ""), "\n")
  cat("Visual:", format.pval(p_vis, digits = 3),
      ifelse(p_vis < 0.05, "***", ""), "\n")
  cat("Interaction:", format.pval(p_int, digits = 3),
      ifelse(p_int < 0.05, "***", ""), "\n\n")

  return(list(
    var = var_label,
    p_uncertainty = p_unc,
    p_visual = p_vis,
    p_interaction = p_int
  ))
}

# Run tests for all perceptions
perception_results <- list()
for (i in seq_along(perception_vars)) {
  var_name <- perception_vars[[i]]
  var_label <- names(perception_vars)[i]
  perception_results[[i]] <- test_perception(core_complete, var_name, var_label)
}

# Summary table of all perception tests
cat("\n", strrep("=", 80), "\n")
cat("SUMMARY: All Perception Variables\n")
cat(strrep("=", 80), "\n\n")

summary_df <- do.call(rbind, lapply(perception_results, function(x) {
  data.frame(
    Variable = x$var,
    P_Uncertainty = format.pval(x$p_uncertainty, digits = 3),
    Sig_Unc = ifelse(x$p_uncertainty < 0.05, "***",
                     ifelse(x$p_uncertainty < 0.10, "*", "")),
    P_Visual = format.pval(x$p_visual, digits = 3),
    Sig_Vis = ifelse(x$p_visual < 0.05, "***",
                     ifelse(x$p_visual < 0.10, "*", "")),
    P_Interaction = format.pval(x$p_interaction, digits = 3),
    Sig_Int = ifelse(x$p_interaction < 0.05, "***",
                     ifelse(x$p_interaction < 0.10, "*", ""))
  )
}))

print(summary_df, row.names = FALSE)
cat("\n")

# =============================================================================
# PART 3: ORDER EFFECTS (FIRST VS SECOND)
# =============================================================================

cat("\n", strrep("=", 80), "\n")
cat("PART 3: ORDER EFFECTS (First shown vs Second shown)\n")
cat(strrep("=", 80), "\n\n")

cat("Testing whether perceptions differ based on presentation order\n\n")

order_vars <- list(
  "Accuracy" = c("acc_first", "acc_second"),
  "Uncertainty" = c("unc_first", "unc_second"),
  "Trust" = c("trust_first", "trust_second"),
  "Clarity" = c("clear_first", "clear_second"),
  "Transparency" = c("trans_first", "trans_second"),
  "Informativeness" = c("info_first", "info_second"),
  "Details" = c("dets_first", "dets_second")
)

for (var_name in names(order_vars)) {
  cat("---", var_name, "---\n")
  first_var <- order_vars[[var_name]][1]
  second_var <- order_vars[[var_name]][2]

  # Create long format for paired t-test
  paired_data <- core_complete |>
    select(response_id, all_of(c(first_var, second_var))) |>
    filter(!is.na(.data[[first_var]]) & !is.na(.data[[second_var]]))

  cat("N pairs:", nrow(paired_data), "\n")
  cat("Mean first:", mean(paired_data[[first_var]], na.rm = TRUE), "\n")
  cat("Mean second:", mean(paired_data[[second_var]], na.rm = TRUE), "\n")

  if (nrow(paired_data) > 0) {
    t_result <- t.test(paired_data[[first_var]],
                       paired_data[[second_var]],
                       paired = TRUE)
    cat("Paired t-test p-value:", format.pval(t_result$p.value, digits = 3), "\n")
    cat("Mean difference (first - second):", t_result$estimate, "\n\n")
  } else {
    cat("Insufficient data for paired test\n\n")
  }
}

# =============================================================================
# PART 4: GROUP 5 ANALYSES (Separate from factorial)
# =============================================================================

cat("\n", strrep("=", 80), "\n")
cat("PART 4: GROUP 5 COMPARISONS\n")
cat(strrep("=", 80), "\n\n")

if (nrow(group5_complete) > 0) {

  cat("Group 5 sample sizes:\n")
  g5_counts <- group5_complete |>
    count(arm) |>
    arrange(arm)
  print(g5_counts)
  cat("\n")

  # Compare 5a, 5b, 5c on allocations
  cat("--- ALLOCATION OUTCOMES across 5a, 5b, 5c ---\n\n")

  cat("Allocation to FI:\n")
  desc_g5_fi <- group5_complete |>
    group_by(arm) |>
    summarise(
      n = n(),
      mean = mean(alloc_fi, na.rm = TRUE),
      sd = sd(alloc_fi, na.rm = TRUE),
      .groups = "drop"
    )
  print(desc_g5_fi)
  cat("\n")

  if (length(unique(group5_complete$arm)) > 1) {
    cat("ONE-WAY ANOVA: Allocation to FI across 5a/5b/5c\n")
    anova_g5 <- aov(alloc_fi ~ arm, data = group5_complete)
    print(summary(anova_g5))
    cat("\n")

    cat("POST-HOC PAIRWISE COMPARISONS:\n")
    pairwise_g5 <- pairwise.t.test(group5_complete$alloc_fi,
                                   group5_complete$arm,
                                   p.adjust.method = "bonferroni")
    print(pairwise_g5)
    cat("\n")
  }

  # Perceptions in Group 5
  cat("--- PERCEPTION OUTCOMES in Group 5 ---\n\n")

  cat("Accuracy (FI) by arm:\n")
  desc_g5_acc <- group5_complete |>
    group_by(arm) |>
    summarise(
      n = sum(!is.na(acc_fi)),
      mean = mean(acc_fi, na.rm = TRUE),
      sd = sd(acc_fi, na.rm = TRUE),
      .groups = "drop"
    )
  print(desc_g5_acc)
  cat("\n")

  if (length(unique(group5_complete$arm)) > 1) {
    anova_g5_acc <- aov(acc_fi ~ arm, data = group5_complete)
    cat("ANOVA: Accuracy (FI)\n")
    print(summary(anova_g5_acc))
    cat("\n")
  }

} else {
  cat("No complete Group 5 observations\n\n")
}

# =============================================================================
# PART 5: ROBUSTNESS - STRICT SAMPLE
# =============================================================================

cat("\n", strrep("=", 80), "\n")
cat("PART 5: ROBUSTNESS CHECK - STRICT SAMPLE (Quality Filtered)\n")
cat(strrep("=", 80), "\n\n")

cat("Re-running key allocation tests on strict sample (N =", nrow(core_strict), ")\n\n")

cat("--- Allocation to high-effect option ---\n")
anova_strict <- lm(
  alloc_high_effect_true ~ uncertainty_disclosed * visual,
  data = core_strict
)
print(summary(anova_strict))
cat("\n")

cat("T-test: Uncertainty disclosed vs hidden\n")
t_strict_unc <- t.test(
  alloc_high_effect_true ~ uncertainty_disclosed,
  data = core_strict
)
print(t_strict_unc)
cat("\n")

cat("T-test: Visual vs text\n")
t_strict_vis <- t.test(
  alloc_high_effect_true ~ visual,
  data = core_strict
)
print(t_strict_vis)
cat("\n")

# =============================================================================
# SUMMARY OF KEY FINDINGS
# =============================================================================

cat("\n", strrep("=", 80), "\n")
cat("SUMMARY OF KEY STATISTICAL TESTS\n")
cat(strrep("=", 80), "\n\n")

cat("1. ALLOCATION TO HIGH-EFFECT OPTION (Primary outcome)\n")
cat("   Main sample (N =", nrow(core_complete), "):\n")
cat(
  "   - Uncertainty effect: p =",
  format.pval(anova(anova_high_effect)["uncertainty_disclosed", "Pr(>F)"], digits = 3),
  "\n"
)
cat(
  "   - Visual effect: p =",
  format.pval(anova(anova_high_effect)["visual", "Pr(>F)"], digits = 3),
  "\n"
)
cat(
  "   - Interaction: p =",
  format.pval(anova(anova_high_effect)["uncertainty_disclosed:visual", "Pr(>F)"], digits = 3),
  "\n\n"
)

cat("   Strict sample (N =", nrow(core_strict), "):\n")
cat(
  "   - Uncertainty effect: p =",
  format.pval(anova(anova_strict)["uncertainty_disclosed", "Pr(>F)"], digits = 3),
  "\n"
)
cat(
  "   - Visual effect: p =",
  format.pval(anova(anova_strict)["visual", "Pr(>F)"], digits = 3),
  "\n"
)
cat(
  "   - Interaction: p =",
  format.pval(anova(anova_strict)["uncertainty_disclosed:visual", "Pr(>F)"], digits = 3),
  "\n\n"
)

cat("2. PERCEPTION OUTCOMES\n")
sig_perceptions <- summary_df |>
  filter(Sig_Unc == "***" | Sig_Vis == "***" | Sig_Int == "***")
if (nrow(sig_perceptions) > 0) {
  cat("   Significant effects found for:\n")
  print(sig_perceptions |> 
        select(Variable, P_Uncertainty, P_Visual, P_Interaction),
        row.names = FALSE)
} else {
  cat("   No significant treatment effects on perceptions at p < 0.05\n")
}

cat("\n", strrep("=", 80), "\n")
cat("STATISTICAL TESTS COMPLETE\n")
cat(strrep("=", 80), "\n")