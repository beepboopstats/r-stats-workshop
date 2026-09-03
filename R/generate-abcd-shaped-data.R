# generate-abcd-shaped-data.R
# ==================================================================
# Builds `data/data.RDS` -- the ABCD-shaped synthetic dataset used in
# Modules 2, 3 and 4. (Module 1 uses the much simpler
# `data/abcd-synthetic.csv`, built by generate-workshop-data.R.)
#
# ------------------------------------------------------------------
# THE DATA IS FAKE.
# ------------------------------------------------------------------
# Every row is simulated. No real participant contributes a row, a
# value, or a record here.
#
# The *shape* was fitted from a de-identified extract of a real ABCD
# release: column names, variable types, factor level sets, marginal
# means and SDs by session, test-retest correlations, and per-session
# missingness rates were summarised into the constants below, and the
# extract was then set aside. Only those summary constants live in
# this file. It is a public repository and the ABCD Data Use
# Certification does not permit redistributing release derivatives,
# so nothing row-level may ever be committed under data/.
#
# Where the de-identification (jittering + rounding) had visibly
# destroyed a known property of a measure, this generator restores
# the psychometrically sensible value rather than copying the
# artifact. Those deliberate departures are marked `# RESTORED:`.
#
# ------------------------------------------------------------------
# PLANTED EFFECTS
# ------------------------------------------------------------------
# The dataset is not noise. These associations are real, sized to be
# findable at n = 500, and are what make the Module 3 and Module 4
# exercises resolve to something interpretable. Keep this list in
# sync with the `PLANTED` block below.
#
# The figure after each effect is what a straightforward model
# actually recovers from the file as it stands, not the coefficient
# fed to the simulation -- link functions and competing terms move
# the two apart. Re-check these if you change any constant.
#
#   E1  Age -> youth-reported screen time. +1.0 h/day per year on
#       weekdays, +1.8 h/day per year at weekends.
#   E2  Screen time -> CBCL internalizing, holding age and sex:
#       +0.27 points per weekday hour.
#   E3  Household income -> NIH Toolbox crystallized, picture
#       vocabulary and oral reading. About 0.6 SD between the lowest
#       and the highest income group at baseline.
#   E4  Sex. Males +1.5 externalizing, females +1.1 internalizing,
#       males ~7% larger total cortical volume.
#   E5  Site. Random intercepts on cognition (SD 2.5 standard-score
#       points) and screen time (SD 0.4 h) -- the reason site belongs
#       in a model rather than being ignored.
#   E6  Family conflict (FES) -> CBCL externalizing, ~+2.9 points
#       across the range of the conflict mean.
#   E7  Parental monitoring -> less substance-use curiosity; age ->
#       more curiosity.
#   E8  Age -> cortical thinning, -0.030 mm/year, and a slow decline
#       in total cortical volume after age 12.
#   E9  Household income -> perceived neighbourhood safety: 53% of
#       the highest income group strongly agree their neighbourhood
#       is safe, against 30% of the lowest.
#   E10 Missingness is structured, never uniform:
#         a. instrument schedules -- several measures are collected
#            only at ses-00A / ses-02A / ses-04A, three NIH Toolbox
#            subtests were retired after baseline, and the FES
#            cohesion scale was added at ses-04A only;
#         b. anthropometrics are missing at Remote visits, because
#            nobody can measure a waist over video;
#         c. MRI failure rises with age (movement in the scanner);
#         d. attrition is MNAR -- the 86 participants who do not
#            reach ses-04A had higher baseline externalizing (5.1 vs
#            4.1) and were more often in the lowest income group
#            (37% vs 25%). Complete-case analysis is therefore not
#            free, which is the point Module 3 has to make.
#
# ------------------------------------------------------------------
# Run from the project root:
#   Rscript R/generate-abcd-shaped-data.R
#
# The seed is fixed, so re-running overwrites the file with the
# identical contents.
# ==================================================================

set.seed(20260924)  # Module 2 date

N_PARTICIPANTS <- 500
SESSIONS       <- c("ses-00A", "ses-01A", "ses-02A", "ses-03A", "ses-04A")

# Sessions at which whole instruments are administered (E10a).
IMAGING_SESSIONS <- c("ses-00A", "ses-02A", "ses-04A")
YOUTH_SELF_REPORT_SESSIONS <- c("ses-00A", "ses-02A", "ses-04A")

# Effect sizes, gathered in one place so the header list above can be
# checked against the code.
PLANTED <- list(
  screen_wkdy_per_year   = 1.50,  # E1
  screen_wknd_per_year   = 2.50,  # E1
  screen_to_internal     = 0.30,  # E2
  income_to_cognition    = 0.45,  # E3, SD across the income range
  sex_to_external        = 1.50,  # E4
  sex_to_internal        = 1.20,  # E4
  sex_to_brain_volume    = 0.07,  # E4, proportional
  site_sd_cognition      = 2.50,  # E5
  site_sd_screen         = 0.40,  # E5
  conflict_to_external   = 2.00,  # E6
  monitoring_to_curious  = 0.35,  # E7
  age_to_curious         = 0.18,  # E7
  age_to_thickness       = -0.030 # E8, mm per year
)

# ---- Small helpers -----------------------------------------------

# Split a latent z into a stable participant part and wave-specific
# noise, so that the correlation between any two waves is `rho`.
mix_stable <- function(u, rho) {
  sqrt(rho) * u + sqrt(1 - rho) * rnorm(length(u))
}

# Cut a latent z into ordered codes hitting the target proportions.
ordinal_from_z <- function(z, probs, codes) {
  cuts <- stats::qnorm(cumsum(probs / sum(probs)))
  codes[findInterval(z, utils::head(cuts, -1)) + 1]
}

# An integer sum score with a floor and a ceiling, as questionnaire
# subscales always have.
sum_score <- function(z, mean, sd, lo, hi) {
  pmin(hi, pmax(lo, round(mean + sd * z)))
}

# Blank out a proportion `p` of a vector, optionally with the
# probability of being blanked driven by `weight` (MAR / MNAR).
punch_holes <- function(x, p, weight = NULL) {
  n <- length(x)
  if (p <= 0) return(x)
  if (is.null(weight)) {
    x[sample.int(n, size = round(p * n))] <- NA
  } else {
    w <- stats::plogis(scale(weight)[, 1])
    x[order(-w * stats::runif(n))[seq_len(round(p * n))]] <- NA
  }
  x
}

as_coded_factor <- function(x, codes) factor(x, levels = codes)

# ==================================================================
# 1. Participants
# ==================================================================

pid <- sprintf("sub-%04d", seq_len(N_PARTICIPANTS))

# ---- Family structure --------------------------------------------
# RESTORED: the source extract had its family IDs scrambled, leaving
# one participant per family and making the column useless. ABCD
# genuinely enrolled siblings and twins, so ~15% of participants here
# share a family with one other participant. That nesting is why
# Module 3 has to talk about non-independent observations.
n_shared   <- round(0.15 * N_PARTICIPANTS / 2) * 2
shared_idx <- sample.int(N_PARTICIPANTS, n_shared)
family_of  <- seq_len(N_PARTICIPANTS)
family_of[shared_idx[seq(2, n_shared, by = 2)]] <-
  family_of[shared_idx[seq(1, n_shared, by = 2)]]
family_id <- sprintf("fam-%04d", match(family_of, sort(unique(family_of))))

# ---- Site --------------------------------------------------------
# 22 sites, fitted proportions, constant within a participant. Site
# is a family-level attribute: siblings attend the same site.
site_codes <- as.character(1:22)
site_probs <- c(0.053, 0.038, 0.035, 0.042, 0.043, 0.051, 0.051, 0.042,
                0.046, 0.044, 0.035, 0.004, 0.033, 0.067, 0.051, 0.039,
                0.054, 0.032, 0.060, 0.063, 0.028, 0.090)
site_by_family <- sample(site_codes, max(family_of), replace = TRUE,
                         prob = site_probs)
site <- site_by_family[family_of]

# ---- Sex ---------------------------------------------------------
sex <- sample(c("1", "2"), N_PARTICIPANTS, replace = TRUE,
              prob = c(0.501, 0.499))     # 1 = Male, 2 = Female
is_male <- as.integer(sex == "1")

# ---- Race and ethnicity, kept internally consistent --------------
# race__nih drawn from its marginal; ethnicity conditional on race;
# ethnrace__leg then derived, with Hispanic outweighing race exactly
# as the dictionary says the legacy variable behaves.
race_codes <- c("2", "3", "4", "5", "6", "8", "13")
race <- sample(race_codes, N_PARTICIPANTS, replace = TRUE,
               prob = c(0.665, 0.164, 0.024, 0.007, 0.002, 0.111, 0.027))

p_hispanic <- c("2" = 0.214, "3" = 0.072, "4" = 0.020, "5" = 0.335,
                "6" = 0.335, "8" = 0.335, "13" = 0.335)
ethn <- ifelse(stats::runif(N_PARTICIPANTS) < p_hispanic[race], "1", "2")

ethnrace <- ifelse(
  ethn == "1", "1",
  ifelse(race == "2", "2",
         ifelse(race == "3", "3",
                ifelse(race == "4", "4", "13")))
)

# ---- Socioeconomic latent ----------------------------------------
# One latent per family drives caregiver education and household
# income, so the two agree the way they do in real data. It is also
# what E3 and E9 hang off.
ses_by_family <- rnorm(max(family_of))
ses <- ses_by_family[family_of]

# Baseline caregiver education, fitted proportions.
edu_base <- ordinal_from_z(ses, c(0.033, 0.084, 0.274, 0.263, 0.346),
                           c("1", "2", "3", "4", "5"))

# Baseline household income on the 6-level scale. "Don't know" (999)
# and "Decline to answer" (777) are drawn separately and are a little
# more common at lower income -- they are answers, not gaps, and
# Module 2 has to decide what to do with them.
income6_base <- ordinal_from_z(
  ses, c(0.130, 0.125, 0.115, 0.125, 0.362, 0.143),
  c("1", "2", "3", "4", "5", "6")
)
refuse_draw <- stats::runif(N_PARTICIPANTS) < stats::plogis(-3.0 - 0.35 * ses)
dontknow_draw <- stats::runif(N_PARTICIPANTS) < stats::plogis(-3.5 - 0.35 * ses)
income6_base[dontknow_draw] <- "999"
income6_base[refuse_draw]   <- "777"

# ---- Participant-level latents behind the repeated measures ------
# General ability, independent of SES: the whole SES gradient (E3)
# is applied explicitly per test below, so its size stays readable.
u_cognition  <- rnorm(N_PARTICIPANTS)
u_internal   <- rnorm(N_PARTICIPANTS)
u_external   <- rnorm(N_PARTICIPANTS)
u_screen     <- rnorm(N_PARTICIPANTS)
u_brain      <- rnorm(N_PARTICIPANTS)
u_height     <- rnorm(N_PARTICIPANTS)
u_bmi        <- rnorm(N_PARTICIPANTS)
u_impulsive  <- rnorm(N_PARTICIPANTS)
u_monitoring <- 0.20 * ses + sqrt(1 - 0.20^2) * rnorm(N_PARTICIPANTS)
u_conflict   <- -0.15 * ses + sqrt(1 - 0.15^2) * rnorm(N_PARTICIPANTS)

# Site random intercepts (E5).
site_cog_effect    <- stats::setNames(
  rnorm(22, 0, PLANTED$site_sd_cognition), site_codes)
site_screen_effect <- stats::setNames(
  rnorm(22, 0, PLANTED$site_sd_screen), site_codes)

# ---- Baseline age and visit date ---------------------------------
age_base <- sample(9:11, N_PARTICIPANTS, replace = TRUE,
                   prob = c(0.29, 0.44, 0.27)) + stats::runif(N_PARTICIPANTS)
date_base <- as.POSIXct("2016-10-19", tz = "UTC") +
  stats::runif(N_PARTICIPANTS, 0, 757) * 86400

# ==================================================================
# 2. Attrition, then the long frame
# ==================================================================
# Monotone dropout: once a participant leaves, they stay gone. The
# hazard rises with baseline externalizing and falls with income, so
# the people missing from later waves are not a random subset of the
# people present at baseline (E10d).

dropout_risk <- stats::plogis(-3.35 + 0.45 * u_external - 0.45 * ses)
last_session <- rep(length(SESSIONS), N_PARTICIPANTS)
for (i in seq_len(N_PARTICIPANTS)) {
  for (s in 2:length(SESSIONS)) {
    if (stats::runif(1) < dropout_risk[i]) {
      last_session[i] <- s - 1
      break
    }
  }
}

rows <- do.call(rbind, lapply(seq_len(N_PARTICIPANTS), function(i) {
  data.frame(i = i, s = seq_len(last_session[i]))
}))
i <- rows$i          # participant index for every row
s <- rows$s          # session index (1..5) for every row
n <- nrow(rows)

participant_id <- pid[i]
session_id     <- factor(SESSIONS[s], levels = SESSIONS)

# ==================================================================
# 3. Visit administration
# ==================================================================

# Visits are annual, but they slip -- scheduling, illness, a pandemic.
age_exact <- age_base[i] + (s - 1) * 1.05 + rnorm(n, 0, 0.30)
# Age in years as a double, matching the dictionary (`type_data`
# double, `unit` years). The source extract had binned it into whole
# years; binned age cannot carry the per-year effects E1 and E8, and
# would silently break `mean()` and any model treating age as
# continuous, so the exact age is kept here.
visit_age <- round(age_exact, 1)

visit_dtt <- date_base[i] + (age_exact - age_base[i]) * 365.25 * 86400
# Land every visit inside a plausible working day, to the minute.
visit_dtt <- as.POSIXct(
  round(as.numeric(visit_dtt) / 86400) * 86400 +
    stats::runif(n, 8 * 3600, 18 * 3600),
  origin = "1970-01-01", tz = "UTC"
)
visit_dtt <- as.POSIXct(round(as.numeric(visit_dtt) / 60) * 60,
                        origin = "1970-01-01", tz = "UTC")

# Visit type by session. ses-03A is overwhelmingly remote -- these are
# the 2020-21 visits -- and that single fact explains a large block of
# the missing anthropometrics later on.
visit_type_probs <- list(
  "ses-00A" = c(1.00, 0.00, 0.00),
  "ses-01A" = c(0.99, 0.01, 0.00),
  "ses-02A" = c(0.71, 0.15, 0.14),
  "ses-03A" = c(0.16, 0.79, 0.05),
  "ses-04A" = c(0.40, 0.13, 0.47)
)
visit_type <- vapply(seq_len(n), function(r) {
  sample(c("1", "2", "3"), 1, prob = visit_type_probs[[s[r]]])
}, character(1))
is_remote <- visit_type == "2"
visit_type <- as_coded_factor(visit_type, c("1", "2", "3"))

design_site <- as_coded_factor(site[i], site_codes)

# ==================================================================
# 4. Household variables (time-varying, mostly stable)
# ==================================================================
# Education and income are re-asked at every visit. They mostly stay
# put, but they do move -- which is exactly why they are `ab_g_dyn__`
# (dynamic) and not `ab_g_stc__` (static).

bump_ordinal <- function(base_by_pid, codes, p_change) {
  out <- base_by_pid[i]
  moved <- stats::runif(n) < p_change & s > 1
  pos <- match(out, codes) + sample(c(-1, 1), n, replace = TRUE)
  pos <- pmin(length(codes), pmax(1, pos))
  out[moved] <- codes[pos[moved]]
  out
}

edu <- bump_ordinal(edu_base, c("1", "2", "3", "4", "5"), 0.07)

income6 <- income6_base[i]
ordinary <- !income6 %in% c("999", "777")
moved <- ordinary & stats::runif(n) < 0.14 & s > 1
pos <- match(income6, c("1", "2", "3", "4", "5", "6")) +
  sample(c(-1, 1), n, replace = TRUE)
pos <- pmin(6, pmax(1, pos))
income6[moved] <- as.character(pos[moved])

# The 3-level variable is a collapse of the 6-level one, so it must
# agree with it row by row. Participants who spot that are right to.
income3 <- c("1" = "1", "2" = "1", "3" = "2", "4" = "2",
             "5" = "3", "6" = "3", "999" = "999", "777" = "777")[income6]
income3 <- unname(income3)

# ==================================================================
# 5. Family, community and parenting
# ==================================================================

# Parental monitoring: 5 items scored 1-5, reported as their mean.
pm_items <- 5
pm_z <- mix_stable(u_monitoring[i], 0.35)
pm_raw <- 4.42 + 0.62 * pm_z - 0.04 * (age_exact - 10)
fc_y_pm_mean <- round(
  pmin(5, pmax(1, round(pm_raw * pm_items) / pm_items)), 2)

# Family Environment Scale: 9 yes/no items per subscale, reported as
# the proportion endorsed.
# RESTORED: rounding in the source extract had collapsed both means to
# 0/1. They are proportions on a 9-item scale here, as the instrument
# defines them.
fes_items <- 9
confl_z <- mix_stable(u_conflict[i], 0.40)
fc_y_fes__confl_mean <- round(
  pmin(1, pmax(0, round((0.22 + 0.19 * confl_z) * fes_items) / fes_items)), 3)

cohes_z <- -0.35 * confl_z + sqrt(1 - 0.35^2) * rnorm(n)
fc_y_fes__cohes_mean <- round(
  pmin(1, pmax(0, round((0.78 + 0.18 * cohes_z) * fes_items) / fes_items)), 3)

# Neighbourhood safety, single ordinal item, tracking income (E9).
ns_z <- 0.30 * ses[i] + sqrt(1 - 0.30^2) * mix_stable(rnorm(n), 0.30)
fc_y_nsc__ns_003 <- ordinal_from_z(
  ns_z, c(0.023, 0.053, 0.173, 0.321, 0.430),
  c("1", "2", "3", "4", "5"))

# ==================================================================
# 6. Screen time (E1, E2, E5)
# ==================================================================
# Right-skewed, as hours-per-day always are, with a handful of very
# heavy users. That skew is deliberate: Module 3 needs a variable
# whose normality assumption visibly fails.

years_since_9 <- age_exact - 9
screen_z <- mix_stable(u_screen[i], 0.70)

wkdy_mu <- 3.4 + PLANTED$screen_wkdy_per_year * (years_since_9 - 1) * 0.62 +
  site_screen_effect[site[i]]
nt_y_stq__screen__wkdy_sum <- pmax(0, wkdy_mu * exp(0.42 * screen_z - 0.09))

wknd_mu <- 4.6 + PLANTED$screen_wknd_per_year * (years_since_9 - 1) * 0.62 +
  site_screen_effect[site[i]]
nt_y_stq__screen__wknd_sum <- pmax(
  0, wknd_mu * exp(0.42 * (0.90 * screen_z + 0.44 * rnorm(n)) - 0.09))

# ~3% very heavy users, the outliers a boxplot should show.
heavy <- sample.int(n, round(0.03 * n))
nt_y_stq__screen__wkdy_sum[heavy] <-
  nt_y_stq__screen__wkdy_sum[heavy] * stats::runif(length(heavy), 3, 6)
nt_y_stq__screen__wknd_sum[heavy] <-
  nt_y_stq__screen__wknd_sum[heavy] * stats::runif(length(heavy), 3, 6)

nt_y_stq__screen__wkdy_sum <- round(nt_y_stq__screen__wkdy_sum, 1)
nt_y_stq__screen__wknd_sum <- round(nt_y_stq__screen__wknd_sum, 1)

# Parents report the same construct and systematically under-report
# it, more so as the child gets older. Comparing the two reporters is
# a Module 2 exercise in its own right.
underreport <- 0.78 - 0.045 * (age_exact - 10)
nt_p_yst__screen__wkdy_sum <- round(pmax(
  0, nt_y_stq__screen__wkdy_sum * underreport + rnorm(n, 0, 0.9)), 1)
nt_p_yst__screen__wknd_sum <- round(pmax(
  0, nt_y_stq__screen__wknd_sum * underreport + rnorm(n, 0, 1.1)), 1)

# ==================================================================
# 7. Mental health
# ==================================================================

# CBCL, parent-reported. Sums of many 0-2 items, so heavily
# right-skewed with a floor at zero -- count-like, not normal.
# The scores are counts, so they are drawn from a Poisson whose rate
# is itself a lognormal of the linear predictor. Two consequences
# worth knowing before Module 3 runs a t-test on them: the marginal
# distribution is heavily right-skewed with a floor at zero, and the
# stable between-participant part has to carry most of the variance
# for repeated measures on the same child to correlate the way real
# CBCL scores do (r about 0.6-0.7 across two years).
int_lin <- 0.86 * mix_stable(u_internal[i], 0.80) +
  PLANTED$screen_to_internal * (nt_y_stq__screen__wkdy_sum - 6) / 7 +
  PLANTED$sex_to_internal * (sex[i] == "2") / 5.0 +
  0.05 * (age_exact - 12)
mh_p_cbcl__synd__int_sum <- pmin(
  60, stats::rpois(n, exp(1.27 + 0.82 * int_lin)))

ext_lin <- 0.88 * mix_stable(u_external[i], 0.86) +
  PLANTED$conflict_to_external * (fc_y_fes__confl_mean - 0.22) / 3.5 +
  PLANTED$sex_to_external * (sex[i] == "1") / 5.0 -
  0.04 * (age_exact - 12)
mh_p_cbcl__synd__ext_sum <- pmin(
  60, stats::rpois(n, exp(0.89 + 0.97 * ext_lin)))

# BIS/BAS: youth self-report, collected at the imaging waves only.
bis_z <- mix_stable(u_internal[i], 0.45)
bas_z <- mix_stable(u_impulsive[i], 0.45)
mh_y_bisbas__bis_sum     <- sum_score(bis_z, 7.9, 3.85, 0, 21)
mh_y_bisbas__bas__dr_sum <- sum_score(bas_z, 4.0, 2.85, 0, 12)
mh_y_bisbas__bas__fs_sum <- sum_score(
  0.7 * bas_z + 0.3 * rnorm(n), 5.0 - 0.15 * (age_exact - 10), 2.6, 0, 12)
mh_y_bisbas__bas__rr_sum <- sum_score(
  0.6 * bas_z + 0.4 * rnorm(n), 10.2, 4.2, 0, 20)

# UPPS-P short form: four items per subscale, each scored 1-4, so
# every subscale runs 4-16 exactly.
upps <- function(rho_z, mean, sd) sum_score(rho_z, mean, sd, 4, 16)
imp_z <- mix_stable(u_impulsive[i], 0.45)
mh_y_upps__nurg_sum <- upps(0.75 * imp_z + 0.25 * rnorm(n), 8.2, 2.5)
mh_y_upps__purg_sum <- upps(0.70 * imp_z + 0.30 * rnorm(n), 7.6, 2.7)
mh_y_upps__pers_sum <- upps(0.45 * imp_z + 0.55 * rnorm(n), 7.1, 2.2)
mh_y_upps__plan_sum <- upps(0.45 * imp_z + 0.55 * rnorm(n), 7.7, 2.2)
# Sensation seeking climbs steadily through adolescence.
mh_y_upps__sens_sum <- upps(
  0.55 * imp_z + 0.45 * rnorm(n), 9.3 + 0.22 * (age_exact - 10), 3.85)

# ==================================================================
# 8. Neurocognition (E3, E5)
# ==================================================================
# Age-corrected standard scores: normed to mean 100, SD 15, so a
# score does *not* climb with age. What varies is who scores well.

cog_z <- mix_stable(u_cognition[i], 0.68) + site_cog_effect[site[i]] / 15

nihtb <- function(mean, sd, ses_load, drift = 0) {
  z <- 0.75 * cog_z + 0.25 * rnorm(n) +
    # The three income groups span roughly 2 SD of the latent, so
    # half the coefficient separates the outer two groups by E3.
    ses_load * PLANTED$income_to_cognition / 2 * ses[i]
  round(pmin(180, pmax(45, mean + drift * (age_exact - 10) + sd * z)))
}

# Crystallized ability (vocabulary, reading) tracks the home
# environment hardest; fluid measures much less so.
nc_y_nihtb__comp__cryst__agecorr_score <- nihtb(107.5, 17.5, 1.00)
nc_y_nihtb__picvcb__agecor_score       <- nihtb(108.5, 16.8, 0.90)
nc_y_nihtb__readr__agecor_score        <- nihtb(104.0, 17.5, 0.85)
nc_y_nihtb__comp__fluid__agecorr_score <- nihtb(96.5, 17.8, 0.35)
nc_y_nihtb__comp__tot__agecor_score    <- nihtb(102.3, 17.9, 0.70)
nc_y_nihtb__crdst__agecorr_score       <- nihtb(97.5, 16.1, 0.30)
nc_y_nihtb__flnkr__agecor_score        <- nihtb(96.4, 15.0, 0.25, drift = 0.5)
nc_y_nihtb__lswmt__agecor_score        <- nihtb(101.4, 15.4, 0.45, drift = 0.7)
nc_y_nihtb__picsq__agecor_score        <- nihtb(100.4, 16.5, 0.35, drift = 1.9)
nc_y_nihtb__pttcp__agecor_score        <- nihtb(94.8, 21.8, 0.20, drift = 4.2)

# ==================================================================
# 9. Structural MRI (E4, E8)
# ==================================================================

brain_z <- mix_stable(u_brain[i], 0.97)
total_vol <- 595000 *
  (1 + PLANTED$sex_to_brain_volume * (is_male[i] - 0.5)) *
  (1 - 0.006 * pmax(0, age_exact - 12)) *
  exp(0.085 * brain_z)

# Hemispheres are close to symmetric, and by construction they sum to
# the total -- an internal consistency check participants can find.
lh_share <- 0.5 + rnorm(n, 0, 0.004)
mr_y_smri__vol__dsk__lh_sum <- round(total_vol * lh_share)
mr_y_smri__vol__dsk__rh_sum <- round(total_vol) - mr_y_smri__vol__dsk__lh_sum
mr_y_smri__vol__dsk_sum <- mr_y_smri__vol__dsk__lh_sum +
  mr_y_smri__vol__dsk__rh_sum

# RESTORED: rounding had flattened cortical thickness to the integers
# 2 and 3. It is a millimetre measurement with an SD near 0.09.
mr_y_smri__thk__dsk_mean <- round(
  2.74 + PLANTED$age_to_thickness * (age_exact - 10) +
    0.09 * mix_stable(u_brain[i], 0.80), 3)

# ==================================================================
# 10. Physical health
# ==================================================================
# Inches and pounds, as ABCD records them.

height_for_age <- 53.6 + 2.20 * (age_exact - 9) -
  0.075 * pmax(0, age_exact - 13)^2
# Girls hit their growth spurt first and are briefly taller; boys
# overtake around 14. An interaction worth plotting.
sex_height <- ifelse(sex[i] == "2",
                     1.0 - 0.55 * pmax(0, age_exact - 12),
                     -0.5 + 0.55 * pmax(0, age_exact - 12))
ph_y_anthr__height_mean <- round(
  height_for_age + sex_height + 3.8 * mix_stable(u_height[i], 0.88), 1)

bmi <- 18.6 + 0.72 * (age_exact - 9) + 4.3 * mix_stable(u_bmi[i], 0.88)
bmi <- pmax(12, bmi * exp(0.10 * rnorm(n)))   # right-skewed, as BMI is
ph_y_anthr__weight_mean <- round(
  bmi * ph_y_anthr__height_mean^2 / 703, 1)

ph_y_anthr__waist_001 <- round(
  18.5 + 0.100 * ph_y_anthr__weight_mean + rnorm(n, 0, 4.5), 1)

# ==================================================================
# 11. Substance-use curiosity (E7)
# ==================================================================
# Overwhelmingly "not at all curious" at these ages, drifting upward.

curious <- function(base_shift) {
  z <- base_shift +
    PLANTED$age_to_curious * (age_exact - 12) -
    PLANTED$monitoring_to_curious * (fc_y_pm_mean - 4.4) / 0.62 +
    0.35 * imp_z + rnorm(n, 0, 0.8)
  out <- ordinal_from_z(-z, c(0.008, 0.023, 0.130, 0.839),
                        c("1", "2", "3", "4"))
  # A small share answer "Don't know" or "Decline to answer".
  out[stats::runif(n) < 0.020] <- "999"
  out[stats::runif(n) < 0.005] <- "777"
  out
}
su_y_itu__alc_001 <- curious(0.00)
su_y_itu__mj_001  <- curious(-0.30)
su_y_itu__nic_001 <- curious(-0.45)

# ==================================================================
# 12. Missingness (E10)
# ==================================================================

in_session <- function(x, keep) {
  x[!SESSIONS[s] %in% keep] <- NA
  x
}

# -- a. Instrument schedules ---------------------------------------
# Youth self-report and imaging run at the odd waves only.
for (v in c("mh_y_bisbas__bis_sum", "mh_y_bisbas__bas__dr_sum",
            "mh_y_bisbas__bas__fs_sum", "mh_y_bisbas__bas__rr_sum",
            "mh_y_upps__nurg_sum", "mh_y_upps__purg_sum",
            "mh_y_upps__pers_sum", "mh_y_upps__plan_sum",
            "mh_y_upps__sens_sum")) {
  assign(v, in_session(get(v), YOUTH_SELF_REPORT_SESSIONS))
}
for (v in c("mr_y_smri__thk__dsk_mean", "mr_y_smri__vol__dsk__lh_sum",
            "mr_y_smri__vol__dsk__rh_sum", "mr_y_smri__vol__dsk_sum",
            "nc_y_nihtb__comp__cryst__agecorr_score",
            "nc_y_nihtb__comp__fluid__agecorr_score",
            "nc_y_nihtb__comp__tot__agecor_score",
            "nc_y_nihtb__crdst__agecorr_score",
            "nc_y_nihtb__flnkr__agecor_score",
            "nc_y_nihtb__lswmt__agecor_score",
            "nc_y_nihtb__picsq__agecor_score",
            "nc_y_nihtb__picvcb__agecor_score",
            "nc_y_nihtb__pttcp__agecor_score",
            "nc_y_nihtb__readr__agecor_score")) {
  assign(v, in_session(get(v), IMAGING_SESSIONS))
}

# The FES cohesion scale was only added to the battery at ses-04A.
# A whole column that is empty for four fifths of the file is not a
# bug, and Module 2 should not "fix" it.
fc_y_fes__cohes_mean <- in_session(fc_y_fes__cohes_mean, "ses-04A")

# Several NIH Toolbox subtests were retired after baseline.
retired_after_baseline <- c(
  "nc_y_nihtb__comp__fluid__agecorr_score",
  "nc_y_nihtb__comp__tot__agecor_score",
  "nc_y_nihtb__crdst__agecorr_score")
for (v in retired_after_baseline) {
  assign(v, in_session(get(v), "ses-00A"))
}
# List sorting skipped the middle imaging wave.
nc_y_nihtb__lswmt__agecor_score <- in_session(
  nc_y_nihtb__lswmt__agecor_score, c("ses-00A", "ses-04A"))

# -- b. Remote visits cannot measure a body -------------------------
remote_gap <- is_remote & stats::runif(n) < 0.82
ph_y_anthr__height_mean[remote_gap] <- NA
ph_y_anthr__weight_mean[remote_gap] <- NA
ph_y_anthr__waist_001[remote_gap]   <- NA
# Waist is the measure most often skipped even in person.
ph_y_anthr__waist_001 <- punch_holes(ph_y_anthr__waist_001, 0.06)

# -- c. Sessions lost at the scanner and the testing station -------
# Usable imaging falls away sharply across waves: older children move
# more in the scanner, and later visits were harder to schedule. The
# rate is set per wave and tilted by age within a wave, so the
# children with usable MRI at ses-04A are a younger, more compliant
# subset -- which is the whole point of the Module 3 discussion.
age_z_in_session <- unsplit(
  lapply(split(age_exact, s), function(a) (a - mean(a)) / stats::sd(a)), s)

drop_at_rate <- function(x, by_session, age_tilt = 0.30) {
  base <- by_session[SESSIONS[s]]
  keep <- !is.na(x) & !is.na(base)
  p <- stats::plogis(stats::qlogis(pmin(0.99, pmax(0.001, base))) +
                       age_tilt * age_z_in_session)
  x[keep & stats::runif(n) < p] <- NA
  x
}

scan_rate <- c("ses-00A" = 0.006, "ses-02A" = 0.247, "ses-04A" = 0.351)
scan_fail <- !is.na(mr_y_smri__vol__dsk_sum) &
  stats::runif(n) < stats::plogis(
    stats::qlogis(pmin(0.99, pmax(0.001, scan_rate[SESSIONS[s]]))) +
      0.30 * age_z_in_session)
scan_fail[is.na(scan_fail)] <- FALSE
for (v in c("mr_y_smri__thk__dsk_mean", "mr_y_smri__vol__dsk__lh_sum",
            "mr_y_smri__vol__dsk__rh_sum", "mr_y_smri__vol__dsk_sum")) {
  x <- get(v); x[scan_fail] <- NA; assign(v, x)
}

# Toolbox subtests were not all completed by everyone who sat down to
# do them, and completion drifted apart between tests over time.
nihtb_rate <- list(
  nc_y_nihtb__comp__cryst__agecorr_score = c("ses-00A" = 0.024, "ses-02A" = 0.305, "ses-04A" = 0.660),
  nc_y_nihtb__comp__fluid__agecorr_score = c("ses-00A" = 0.028),
  nc_y_nihtb__comp__tot__agecor_score    = c("ses-00A" = 0.030),
  nc_y_nihtb__crdst__agecorr_score       = c("ses-00A" = 0.014),
  nc_y_nihtb__flnkr__agecor_score        = c("ses-00A" = 0.016, "ses-02A" = 0.245, "ses-04A" = 0.234),
  nc_y_nihtb__lswmt__agecor_score        = c("ses-00A" = 0.020, "ses-04A" = 0.047),
  nc_y_nihtb__picsq__agecor_score        = c("ses-00A" = 0.014, "ses-02A" = 0.041, "ses-04A" = 0.047),
  nc_y_nihtb__picvcb__agecor_score       = c("ses-00A" = 0.018, "ses-02A" = 0.049, "ses-04A" = 0.042),
  nc_y_nihtb__pttcp__agecor_score        = c("ses-00A" = 0.018, "ses-02A" = 0.245, "ses-04A" = 0.237),
  nc_y_nihtb__readr__agecor_score        = c("ses-00A" = 0.016, "ses-02A" = 0.054, "ses-04A" = 0.047)
)
for (v in names(nihtb_rate)) {
  assign(v, drop_at_rate(get(v), nihtb_rate[[v]]))
}

# -- d. Ordinary item-level non-response ----------------------------
# Weighted by the latent it belongs to, so it is MAR rather than
# uniform: the people who skip the questionnaire are not a random
# sample of the people who answered it.
mh_p_cbcl__synd__int_sum <- punch_holes(mh_p_cbcl__synd__int_sum, 0.015, u_internal[i])
mh_p_cbcl__synd__ext_sum <- punch_holes(mh_p_cbcl__synd__ext_sum, 0.015, u_external[i])
fc_y_nsc__ns_003         <- punch_holes(fc_y_nsc__ns_003, 0.19, -ses[i])
fc_y_pm_mean             <- punch_holes(fc_y_pm_mean, 0.006)
fc_y_fes__confl_mean     <- punch_holes(fc_y_fes__confl_mean, 0.006)
su_y_itu__alc_001        <- punch_holes(su_y_itu__alc_001, 0.29, -ses[i])
su_y_itu__mj_001         <- punch_holes(su_y_itu__mj_001, 0.26, -ses[i])
su_y_itu__nic_001        <- punch_holes(su_y_itu__nic_001, 0.42, -ses[i])
nt_y_stq__screen__wkdy_sum <- punch_holes(nt_y_stq__screen__wkdy_sum, 0.010)
nt_y_stq__screen__wknd_sum <- punch_holes(nt_y_stq__screen__wknd_sum, 0.008)
nt_p_yst__screen__wkdy_sum <- punch_holes(nt_p_yst__screen__wkdy_sum, 0.018)
nt_p_yst__screen__wknd_sum <- punch_holes(nt_p_yst__screen__wknd_sum, 0.018)
edu     <- punch_holes(edu, 0.015)
income6 <- punch_holes(income6, 0.011)
income3[is.na(income6)] <- NA

# A couple of administrative gaps, because those happen too.
visit_dtt <- punch_holes(visit_dtt, 0.002)

# ==================================================================
# 13. Assemble
# ==================================================================

abcd <- data.frame(
  participant_id                         = participant_id,
  session_id                             = session_id,
  ab_g_dyn__visit_dtt                    = visit_dtt,
  ab_g_dyn__visit_age                    = visit_age,
  ab_g_dyn__visit_type                   = visit_type,
  ab_g_dyn__design_site                  = design_site,
  ab_g_dyn__cohort_edu__cgs              = as_coded_factor(edu, as.character(1:5)),
  ab_g_dyn__cohort_income__hhold__3lvl   = as_coded_factor(
    income3, c("1", "2", "3", "999", "777")),
  ab_g_dyn__cohort_income__hhold__6lvl   = as_coded_factor(
    income6, c(as.character(1:6), "999", "777")),
  ab_g_stc__design_id__fam               = family_id[i],
  ab_g_stc__cohort_ethn                  = as_coded_factor(ethn[i], c("1", "2")),
  ab_g_stc__cohort_ethnrace__leg         = as_coded_factor(
    ethnrace[i], c("1", "2", "3", "4", "13")),
  ab_g_stc__cohort_race__nih             = as_coded_factor(race[i], race_codes),
  ab_g_stc__cohort_sex                   = as_coded_factor(sex[i], c("1", "2")),
  fc_y_fes__cohes_mean                   = fc_y_fes__cohes_mean,
  fc_y_fes__confl_mean                   = fc_y_fes__confl_mean,
  fc_y_nsc__ns_003                       = as_coded_factor(
    fc_y_nsc__ns_003, c("1", "2", "3", "4", "5", "999", "777")),
  fc_y_pm_mean                           = fc_y_pm_mean,
  mh_p_cbcl__synd__ext_sum               = mh_p_cbcl__synd__ext_sum,
  mh_p_cbcl__synd__int_sum               = mh_p_cbcl__synd__int_sum,
  mh_y_bisbas__bas__dr_sum               = mh_y_bisbas__bas__dr_sum,
  mh_y_bisbas__bas__fs_sum               = mh_y_bisbas__bas__fs_sum,
  mh_y_bisbas__bas__rr_sum               = mh_y_bisbas__bas__rr_sum,
  mh_y_bisbas__bis_sum                   = mh_y_bisbas__bis_sum,
  mh_y_upps__nurg_sum                    = mh_y_upps__nurg_sum,
  mh_y_upps__pers_sum                    = mh_y_upps__pers_sum,
  mh_y_upps__plan_sum                    = mh_y_upps__plan_sum,
  mh_y_upps__purg_sum                    = mh_y_upps__purg_sum,
  mh_y_upps__sens_sum                    = mh_y_upps__sens_sum,
  mr_y_smri__thk__dsk_mean               = mr_y_smri__thk__dsk_mean,
  mr_y_smri__vol__dsk__lh_sum            = mr_y_smri__vol__dsk__lh_sum,
  mr_y_smri__vol__dsk__rh_sum            = mr_y_smri__vol__dsk__rh_sum,
  mr_y_smri__vol__dsk_sum                = mr_y_smri__vol__dsk_sum,
  nc_y_nihtb__comp__cryst__agecorr_score = nc_y_nihtb__comp__cryst__agecorr_score,
  nc_y_nihtb__comp__fluid__agecorr_score = nc_y_nihtb__comp__fluid__agecorr_score,
  nc_y_nihtb__comp__tot__agecor_score    = nc_y_nihtb__comp__tot__agecor_score,
  nc_y_nihtb__crdst__agecorr_score       = nc_y_nihtb__crdst__agecorr_score,
  nc_y_nihtb__flnkr__agecor_score        = nc_y_nihtb__flnkr__agecor_score,
  nc_y_nihtb__lswmt__agecor_score        = nc_y_nihtb__lswmt__agecor_score,
  nc_y_nihtb__picsq__agecor_score        = nc_y_nihtb__picsq__agecor_score,
  nc_y_nihtb__picvcb__agecor_score       = nc_y_nihtb__picvcb__agecor_score,
  nc_y_nihtb__pttcp__agecor_score        = nc_y_nihtb__pttcp__agecor_score,
  nc_y_nihtb__readr__agecor_score        = nc_y_nihtb__readr__agecor_score,
  nt_p_yst__screen__wkdy_sum             = nt_p_yst__screen__wkdy_sum,
  nt_p_yst__screen__wknd_sum             = nt_p_yst__screen__wknd_sum,
  nt_y_stq__screen__wkdy_sum             = nt_y_stq__screen__wkdy_sum,
  nt_y_stq__screen__wknd_sum             = nt_y_stq__screen__wknd_sum,
  ph_y_anthr__height_mean                = ph_y_anthr__height_mean,
  ph_y_anthr__weight_mean                = ph_y_anthr__weight_mean,
  ph_y_anthr__waist_001                  = ph_y_anthr__waist_001,
  su_y_itu__alc_001                      = as_coded_factor(
    su_y_itu__alc_001, c("1", "2", "3", "4", "999", "777")),
  su_y_itu__mj_001                       = as_coded_factor(
    su_y_itu__mj_001, c("1", "2", "3", "4", "999", "777")),
  su_y_itu__nic_001                      = as_coded_factor(
    su_y_itu__nic_001, c("1", "2", "3", "4", "999", "777")),
  stringsAsFactors = FALSE
)

abcd <- abcd[order(abcd$participant_id, abcd$session_id), ]
rownames(abcd) <- NULL

# The dictionary must describe exactly the columns that exist, or the
# data dictionary page will silently drop variables.
dict_names <- utils::read.csv("data/data_dictionary.csv")$name
stopifnot(
  setequal(setdiff(names(abcd), c("participant_id", "session_id")), dict_names),
  nrow(abcd) > 2000,
  # Hemispheres must sum to the total, exactly.
  all(abcd$mr_y_smri__vol__dsk__lh_sum + abcd$mr_y_smri__vol__dsk__rh_sum ==
        abcd$mr_y_smri__vol__dsk_sum, na.rm = TRUE)
)

saveRDS(abcd, "data/data.RDS")

cat(sprintf("Wrote data/data.RDS: %d rows x %d columns, %d participants\n",
            nrow(abcd), ncol(abcd), length(unique(abcd$participant_id))))
print(table(abcd$session_id))
