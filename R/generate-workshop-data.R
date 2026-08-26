# generate-workshop-data.R
# ------------------------------------------------------------------
# Builds the synthetic dataset used throughout the workshop.
#
# The data is FAKE. It is shaped like an ABCD-style longitudinal
# release -- one row per participant per visit, readable column
# names, and realistic missingness -- so that everything taught on
# it transfers to the real dataset without teaching real data here.
#
# Run from the project root:
#   Rscript R/generate-workshop-data.R
#
# The seed is fixed, so re-running overwrites the file with the
# identical contents.
# ------------------------------------------------------------------

library(readr)

set.seed(20260910)  # Module 1 date, for no better reason

n_subjects <- 200
visits     <- c("baseline", "year_1", "year_2")

# ---- Subject-level attributes (fixed across visits) --------------
subjects <- data.frame(
  subject_id = sprintf("S-%04d", seq_len(n_subjects)),
  site       = sample(sprintf("S%02d", 1:5), n_subjects, replace = TRUE),
  sex        = sample(c("F", "M"), n_subjects, replace = TRUE),
  # Baseline age in months (ABCD enrols at 9-10 years old)
  base_age   = sample(108:131, n_subjects, replace = TRUE),
  # Per-subject offsets, so repeated measures on the same child
  # are correlated -- this is what makes the data longitudinal
  # rather than three unrelated cross-sections.
  off_prob   = rnorm(n_subjects, 0, 6),
  off_cog    = rnorm(n_subjects, 0, 9),
  stringsAsFactors = FALSE
)

# ---- Expand to one row per subject per visit --------------------
abcd <- merge(subjects, data.frame(visit = visits, stringsAsFactors = FALSE))
abcd <- abcd[order(abcd$subject_id, match(abcd$visit, visits)), ]

visit_n <- match(abcd$visit, visits) - 1  # 0, 1, 2 years elapsed

abcd$age_months <- abcd$base_age + visit_n * 12 + sample(-1:1, nrow(abcd), replace = TRUE)

# Behaviour: mild decline in problems with age, plus subject offset
abcd$total_problems <- round(
  pmax(0, 26 - 1.8 * visit_n + abcd$off_prob + rnorm(nrow(abcd), 0, 5))
)

# Cognition: rises with age, standardised-ish around 100
abcd$cognition_score <- round(
  98 + 2.6 * visit_n + abcd$off_cog + rnorm(nrow(abcd), 0, 7)
)

# Screen time climbs through adolescence; sleep falls
abcd$screen_time_hrs <- round(pmax(0, 2.4 + 0.7 * visit_n + rnorm(nrow(abcd), 0, 1.1)), 1)
abcd$sleep_hrs       <- round(pmax(4, 9.4 - 0.35 * visit_n + rnorm(nrow(abcd), 0, 0.8)), 1)

# ---- Missingness ------------------------------------------------
# Item-level gaps: questionnaires skipped, tasks not completed.
abcd$total_problems[sample(nrow(abcd), round(0.08 * nrow(abcd)))]  <- NA
abcd$cognition_score[sample(nrow(abcd), round(0.12 * nrow(abcd)))] <- NA
abcd$screen_time_hrs[sample(nrow(abcd), round(0.05 * nrow(abcd)))] <- NA

# Attrition: some children never came back for the year_2 visit,
# so those rows are absent entirely rather than present-with-NA.
# Both kinds of missing show up in real longitudinal data.
y2 <- which(abcd$visit == "year_2")
abcd <- abcd[-sample(y2, round(0.10 * length(y2))), ]

# ---- Write ------------------------------------------------------
abcd <- abcd[, c(
  "subject_id", "visit", "site", "age_months", "sex",
  "total_problems", "cognition_score", "screen_time_hrs", "sleep_hrs"
)]

write_csv(abcd, "data/abcd-synthetic.csv")

cat("Wrote data/abcd-synthetic.csv:",
    nrow(abcd), "rows,", ncol(abcd), "columns\n")
