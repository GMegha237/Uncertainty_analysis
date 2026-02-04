# scripts/04_analysis.R
library(tidyverse)

survey_analysis <- readRDS("data/processed/03_survey_analysis.rds")

# TODO: main regressions / estimators here
# For now: placeholder
results <- list(
  n = nrow(survey_analysis)
)

saveRDS(results, "data/processed/04_results.rds")
message("Analysis step complete.")
library(dplyr)

dat <- survey_clean

# --- Choose sample for quick diagnostics ---
core <- dat %>% filter(core_sample == TRUE, complete == 1)

# --- Outcomes: allocations + perception items you have ---
# Add any perception vars here once you know the names (e.g., "perceived_fairness", etc.)
outcomes <- c(
  "alloc_fi", "alloc_ct", "alloc_diff_fi_ct",
  "alloc_share_high_effect_true", "alloc_share_more_uncertain_true",
  "eff_comp_num", "unc_comp_num",
  "time_reading_sec"
)
outcomes <- intersect(outcomes, names(core))

# --- Predictors aligned with design ---
preds <- c("uncertainty_disclosed", "visual", "counterbalance")
preds <- intersect(preds, names(core))

# ---------- helpers ----------
safe_anova <- function(df, y, x) {
  d <- df %>% select(all_of(c(y, x))) %>% filter(!is.na(.data[[y]]), !is.na(.data[[x]]))
  if (nrow(d) < 30) return(NULL)
  if (dplyr::n_distinct(d[[x]]) < 2) return(NULL)
  if (dplyr::n_distinct(d[[y]]) < 2) return(NULL)
  fit <- lm(reformulate(x, y), data = d)
  a <- anova(fit)
  tibble::tibble(
    outcome=y, predictor=x, test="anova",
    n=nrow(d),
    stat=a[1, "F value"], df=a[1, "Df"], p=a[1, "Pr(>F)"]
  )
}

safe_chisq <- function(df, y, x) {
  d <- df %>% select(all_of(c(y, x))) %>% filter(!is.na(.data[[y]]), !is.na(.data[[x]]))
  if (nrow(d) < 30) return(NULL)
  if (dplyr::n_distinct(d[[x]]) < 2) return(NULL)
  if (dplyr::n_distinct(d[[y]]) < 2) return(NULL)
  tab <- table(d[[x]], d[[y]])
  if (any(dim(tab) < 2)) return(NULL)
  tst <- suppressWarnings(chisq.test(tab))
  tibble::tibble(
    outcome=y, predictor=x, test="chisq",
    n=nrow(d),
    stat=unname(tst$statistic), df=unname(tst$parameter), p=tst$p.value
  )
}

run_1way <- function(df, y, x) {
  # decide binary vs numeric
  yy <- df[[y]]
  uniq <- sort(unique(yy[!is.na(yy)]))
  if (length(uniq) <= 2 && all(uniq %in% c(0,1))) safe_chisq(df, y, x) else safe_anova(df, y, x)
}

# ---------- main omnibus table ----------
res <- dplyr::bind_rows(lapply(outcomes, function(y) {
  dplyr::bind_rows(lapply(preds, function(x) run_1way(core, y, x)))
})) %>%
  dplyr::mutate(p_adj = p.adjust(p, method="BH")) %>%
  dplyr::arrange(p_adj)

print(res, n = 200)

# ---------- quick descriptives for allocations ----------
core %>%
  group_by(uncertainty_disclosed, visual, counterbalance) %>%
  summarise(
    n = n(),
    mean_alloc_fi = mean(alloc_fi, na.rm=TRUE),
    mean_alloc_ct = mean(alloc_ct, na.rm=TRUE),
    mean_diff_fi_ct = mean(alloc_diff_fi_ct, na.rm=TRUE),
    mean_reading_sec = mean(time_reading_sec, na.rm=TRUE),
    .groups="drop"
  ) %>%
  arrange(uncertainty_disclosed, visual, counterbalance) %>%
  print(n = 200)

# ---------- uncertainty correctness only where shown ----------
if ("unc_correct_visible" %in% names(core) && "uncertainty_applicable" %in% names(core)) {
  core %>%
    filter(uncertainty_applicable == 1) %>%
    group_by(visual, counterbalance) %>%
    summarise(
      n = n(),
      prop_unc_correct_visible = mean(unc_correct_visible == 1, na.rm=TRUE),
      .groups="drop"
    ) %>%
    print(n = 200)
}
