#######################################################################################
#                                                                                     #
#   Exposure to ideal appearance content on TikTok and body satisfaction              #
#   in female adolescents                                                             #
#                                                                                     #
#   Within-person:  H2: Start-of-day body satisfaction predicts subsequent content    #
#                   exposure throughout the day (morning prompt to 03:59).            #
#   Between-person: H4: Average start-of-day body satisfaction predicts average       #
#                   content exposure.                                                 #
#   Three exposure measures, one block pair each: thin, sexualised female bodies;     #
#   beauty filters; beauty content. Each block is titled with the hypotheses it tests.#
#   Body satisfaction = SSES_1_multipleChoice_index (morning prompt).                 #
#                                                                                     #
#   Preregistrations: OSF k7953 (thin, sexualised female bodies), OSF chbxw (beauty   #
#   filters, beauty content). One- or two-sided p-values as preregistered: see the    #
#   comment above the p-values in each block.                                         #
#                                                                                     #
#######################################################################################


# Load packages
library(dplyr); library(tidyr); library(nlme); library(mitml); library(parallel)

# Disable scientific notation of small values
options(scipen = 999)

# Data folder
data_dir <- "/Volumes/LaCie/0_IdealBodies_Final/1_Data/Processed_data"


# Exposure definition used throughout the paper (Methods 4.10, "Confirmatory exposure
# definition"): minimal/tight clothing + posing of any kind + thin body build.
# Morning-window thin sexualised female bodies exposure files are built by 02_exposure_building/02_build_computer_vision_exposure_morning.R



#######################################################################################################
#######################################################################################################
#######################################################################################################
# H2: Start-of-day body satisfaction predicts subsequent content exposure throughout the day.
#     Thin, sexualised female bodies. Lower start-of-day body satisfaction will predict female
#     adolescents spending more daily time (in seconds) watching TikTok videos containing at least
#     one thin (indexed by slim body stature) and sexualised female body (indexed by minimal
#     clothing and sexually suggestive posing of body or face). (Exploratory.)
# H4: Average start-of-day body satisfaction predicts average content exposure.
#     Female adolescents with lower average start-of-day body satisfaction will spend more daily
#     time (in seconds) – on average – watching TikTok videos containing at least one thin (indexed
#     by slim body stature) and sexualised female body (indexed by minimal clothing and sexually
#     suggestive posing of body or face). (Exploratory.)
#
# THIN, SEXUALISED FEMALE BODIES -- COMPLETE CASE
# (preregistered sensitivity analysis; manuscript reports beta_within = -0.091, p = .044, n = 615 days)

# Clear workspace
rm(list = ls()); data_dir <- "/Volumes/LaCie/0_IdealBodies_Final/1_Data/Processed_data"

# Load daily thin sexualised female bodies exposure on the 924-row participant-day dataset, morning window (66 participants x 14 days;
# window = morning EMA start -> 03:00 UTC next day; days without a morning EMA
# use a substituted start, see window_start_source)
daily <- readRDS(file.path(data_dir, "daily_exposure_morning_clothing_anyposing_thin.rds"))
daily$id <- as.character(daily$id)

# Body satisfaction under its original m-Path name: SSES item 1 ("I felt satisfied with the way my body looks"),
# morning prompt; the participant-day dataset stores it as sses_am
daily <- rename(daily, SSES_1_multipleChoice_index = sses_am)

daily    <- daily[order(daily$id, daily$date), ]

# Check the grid
stopifnot(nrow(daily) == 924, length(unique(daily$id)) == 66)
table(daily$window_start_source)

# Complete case: keep days with a morning body satisfaction rating AND derivable thin sexualised female bodies exposure
# (days outside donation coverage have NA thin sexualised female bodies exposure; non-use days have thin sexualised female bodies exposure 0, as registered)
complete_case <- subset(daily, !is.na(SSES_1_multipleChoice_index) & !is.na(thin_sexualised_female_bodies_seconds))
nrow(complete_case)

# Rescale thin sexualised female bodies exposure from seconds to minutes (numerical stability; betas are minutes per scale point)
complete_case$thin_sexualised_female_bodies_exposure <- complete_case$thin_sexualised_female_bodies_seconds / 60

# Person-mean centering of the predictor: between = person mean, within = daily deviation
complete_case <- complete_case %>%
      group_by(id) %>%
      mutate(SSES_1_multipleChoice_index_between = mean(SSES_1_multipleChoice_index),
             SSES_1_multipleChoice_index_within  = SSES_1_multipleChoice_index - SSES_1_multipleChoice_index_between) %>%
      ungroup() %>% as.data.frame()


###########################################################
### Registered model (OSF k7953), thin sexualised female bodies exposure as outcome

# Random intercept and random slope for within-person body satisfaction,
# AR(1) residual correlation across study days within participant.
# Registered code: corAR1(form = ~ 1 | id); implemented over day_index so that gaps between
# observed days are respected in the complete-case frame (identical on the full grid) -- S8
# Optimizer: optim (as in the reported run). With thin sexualised female bodies exposure as the outcome the random-slope
# variance is close to zero and nlminb stops with "singular convergence" on some data sets;
# optim converges. State this in Table S5 next to the nlminb entry for the end-of-day body satisfaction models.
m.eh1.complete_case <- lme(thin_sexualised_female_bodies_exposure ~ SSES_1_multipleChoice_index_within + SSES_1_multipleChoice_index_between,
                data        = complete_case,
                random      = ~ SSES_1_multipleChoice_index_within | id,
                correlation = corAR1(form = ~ day_index | id),
                control     = lmeControl(maxIter = 500, msMaxIter = 500, msMaxEval = 2000,
                                         niterEM = 50, opt = "optim"),
                na.action   = na.fail)

summary(m.eh1.complete_case)
intervals(m.eh1.complete_case, which = "fixed")

# p-values as preregistered (first registration, OSF k7953, "Inference criteria": one-sided
# p-values for its directional hypotheses): within-person ONE-sided and between-person
# ONE-sided, both predicting beta < 0
fixed_effects <- summary(m.eh1.complete_case)$tTable
pt(fixed_effects["SSES_1_multipleChoice_index_within",  "t-value"], df = fixed_effects["SSES_1_multipleChoice_index_within",  "DF"])   # within-person, one-sided
pt(fixed_effects["SSES_1_multipleChoice_index_between", "t-value"], df = fixed_effects["SSES_1_multipleChoice_index_between", "DF"])   # between-person, one-sided

# AR(1) autocorrelation
coef(m.eh1.complete_case$modelStruct$corStruct, unconstrained = FALSE)

# If the model above does not converge on the real data, do NOT silently simplify it:
# refit with random = ~ 1 | id (random intercept only) and record the change in Table S5.



###########################################################
### Registered robustness check: zero-inflated negative binomial on raw seconds
# (OSF k7953, "Reliability and Robustness Testing"; needs the glmmTMB package)

library(glmmTMB)

# Exposure as an integer count of seconds
complete_case$thin_sexualised_female_bodies_exposure_sec <- as.integer(round(complete_case$thin_sexualised_female_bodies_seconds))

m.eh1.zinb <- glmmTMB(thin_sexualised_female_bodies_exposure_sec ~ SSES_1_multipleChoice_index_within + SSES_1_multipleChoice_index_between + (1 | id),
                      ziformula = ~ 1,
                      family    = nbinom2,
                      data      = complete_case)

# The default optimiser (nlminb) can end in a false convergence with a non-positive-definite
# Hessian (standard errors NaN). Then refit with BFGS from the same start; if that fails too,
# stop -- a robustness check with NaN standard errors must not be reported as a result
m.eh1.zinb$sdr$pdHess                                   # TRUE = usable fit
if (!isTRUE(m.eh1.zinb$sdr$pdHess)) {
  cat("ZINB: default optimiser did not converge, refitting with BFGS\n")
  m.eh1.zinb <- glmmTMB(thin_sexualised_female_bodies_exposure_sec ~ SSES_1_multipleChoice_index_within + SSES_1_multipleChoice_index_between + (1 | id),
                        ziformula = ~ 1,
                        family    = nbinom2,
                        data      = complete_case,
                        control   = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))
  m.eh1.zinb$sdr$pdHess
}
if (!isTRUE(m.eh1.zinb$sdr$pdHess)) stop("ZINB robustness check: no converged fit (non-positive-definite Hessian with both optimisers); see vignette('troubleshooting', package = 'glmmTMB')")

summary(m.eh1.zinb)

# One-sided p-values from the two-sided Wald test, as preregistered (first registration, OSF k7953:
# one-sided for its directional hypotheses; direction beta < 0)
zinb_coefs <- summary(m.eh1.zinb)$coefficients$cond
ifelse(zinb_coefs["SSES_1_multipleChoice_index_within",  "Estimate"] < 0, zinb_coefs["SSES_1_multipleChoice_index_within",  "Pr(>|z|)"] / 2, 1 - zinb_coefs["SSES_1_multipleChoice_index_within",  "Pr(>|z|)"] / 2)   # within-person, one-sided
ifelse(zinb_coefs["SSES_1_multipleChoice_index_between", "Estimate"] < 0, zinb_coefs["SSES_1_multipleChoice_index_between", "Pr(>|z|)"] / 2, 1 - zinb_coefs["SSES_1_multipleChoice_index_between", "Pr(>|z|)"] / 2)   # between-person, one-sided




#######################################################################################################
#######################################################################################################
#######################################################################################################
# H2: Start-of-day body satisfaction predicts subsequent content exposure throughout the day.
#     Thin, sexualised female bodies. Lower start-of-day body satisfaction will predict female
#     adolescents spending more daily time (in seconds) watching TikTok videos containing at least
#     one thin (indexed by slim body stature) and sexualised female body (indexed by minimal
#     clothing and sexually suggestive posing of body or face). (Exploratory.)
# H4: Average start-of-day body satisfaction predicts average content exposure.
#     Female adolescents with lower average start-of-day body satisfaction will spend more daily
#     time (in seconds) – on average – watching TikTok videos containing at least one thin (indexed
#     by slim body stature) and sexualised female body (indexed by minimal clothing and sexually
#     suggestive posing of body or face). (Exploratory.)
#
# THIN, SEXUALISED FEMALE BODIES -- MULTIPLE IMPUTATION
# (preregistered PRIMARY analysis; manuscript Table 1)
#
# Deviations from the preregistration (documented in the Supplement):
#   - jomo engine instead of pan: household income (level 2) has 28/66 missing,
#     pan cannot impute level-2 variables
#   - burn-in 20,000 instead of 6,000 after inspection of trace plots
#   - gender omitted from the imputation model (sample is all female)
#
# Runtime: roughly 15-45 min on an M3 Ultra with 10 parallel chains.

# Clear workspace
rm(list = ls()); data_dir <- "/Volumes/LaCie/0_IdealBodies_Final/1_Data/Processed_data"

# Load daily thin sexualised female bodies exposure on the 924-row participant-day dataset, morning window
daily <- readRDS(file.path(data_dir, "daily_exposure_morning_clothing_anyposing_thin.rds"))
daily$id <- as.character(daily$id)

# Body satisfaction under its original m-Path name: SSES item 1 ("I felt satisfied with the way my body looks"),
# morning prompt; the participant-day dataset stores it as sses_am
daily <- rename(daily, SSES_1_multipleChoice_index = sses_am)

# Load person-level baseline covariates
covariates <- readRDS(file.path(data_dir, "participant_level_covariates.rds"))
covariates$id <- as.character(covariates$id)

# Check: one row per participant; SDQ, O2S3, IUS-C and age complete (they enter the
# imputation model as predictors); household income incomplete (imputed at level 2, see S5)
stopifnot(!any(duplicated(covariates$id)))
colSums(is.na(covariates[, c("SDQ_total", "O2S3_total", "IUSC_total", "age", "household_income")]))
stopifnot(all(complete.cases(covariates[, c("SDQ_total", "O2S3_total", "IUSC_total", "age")])))


### Preprocessing

# Lagged EMA covariates (most recent occasion before the morning prompt, i.e. the previous
# evening) entering the imputation model, Methods eq. (2). Same list as in 01_exposure_predicts_body_satisfaction.R;
# check the printed names against 01_preprocessing/04_build_participant_day_dataset_evening.R (the "prompts" block)
lag_cols <- grep("^lag_", names(daily), value = TRUE)
lag_cols <- setdiff(lag_cols, c("lag_occasion_time", "lag_prompt_type"))
lag_cols <- lag_cols[sapply(daily[lag_cols], is.numeric)]
lag_cols

# Drop lagged covariates without variance (would make the covariance matrix singular)
lag_sd      <- sapply(daily[lag_cols], sd, na.rm = TRUE)
lag_cols <- lag_cols[is.finite(lag_sd) & lag_sd > 1e-8]
lag_cols

# Analysis data set: AM body satisfaction, thin sexualised female bodies exposure (in minutes), lagged EMA covariates,
# baseline covariates
mi_data <- daily %>%
          select(id, date, day_index, SSES_1_multipleChoice_index, thin_sexualised_female_bodies_seconds, all_of(lag_cols)) %>%
          left_join(covariates %>% select(id, SDQ_total, O2S3_total, IUSC_total, age, household_income),
                    by = "id") %>%
          mutate(thin_sexualised_female_bodies_seconds = thin_sexualised_female_bodies_seconds / 60,
                 id = as.factor(id)) %>%
          arrange(id, date) %>% as.data.frame()

# Missingness overview (manuscript: 300 AM SSES, 16 thin sexualised female bodies exposure, 28 household income)
colSums(is.na(mi_data))


### Imputation model, Methods eq. (2)

# Level 1 targets: AM body satisfaction, thin sexualised female bodies exposure, lagged EMA covariates
# Level 1 predictors: complete baseline covariates (SDQ, O2S3, IUS-C, age)
f.level1 <- as.formula(paste(paste(c("SSES_1_multipleChoice_index", "thin_sexualised_female_bodies_seconds", lag_cols), collapse = " + "),
                             "~ 1 + SDQ_total + O2S3_total + IUSC_total + age + (1 | id)"))
# Level 2 target: household income
f.level2 <- household_income ~ 1 + SDQ_total + O2S3_total + IUSC_total + age

# Print the assembled formula and check it against eq. (2)
f.level1
f.level2


### Imputation

# 10 independent chains x 3 draws = m = 30 imputed data sets (registered m = 30),
# burn-in 20,000, draws 200 iterations apart, seed as used in the reported run
n.chains <- 10 ; draws.per.chain <- 3 ; n.burn <- 20000 ; n.iter <- 200 ; seed <- 8600

# The chains are saved and REUSED: if the file below exists, the imputations are loaded from
# it and only the models are refitted (minutes instead of an hour). The saved chains must
# come from exactly this data set and these settings -- otherwise the script stops. To
# impute afresh, delete the file. The same file feeds 04_imputation_trace_plots.R.
chains_file <- file.path(data_dir, "EH1_EH3_MI_chains_clothing_anyposing_thin.rds")
if (file.exists(chains_file)) {
  imputations <- readRDS(chains_file)
  saved_columns <- intersect(names(imputations[[1]]$data), names(mi_data))           # mitml keeps the model variables only
  same_data     <- length(saved_columns) == ncol(imputations[[1]]$data) &&
                   isTRUE(all.equal(imputations[[1]]$data[saved_columns], mi_data[saved_columns], check.attributes = FALSE))
  same_settings <- length(imputations) == n.chains && imputations[[1]]$iter$burn == n.burn &&
                   imputations[[1]]$iter$iter == n.iter && imputations[[1]]$iter$m == draws.per.chain
  if (!same_data)     stop("saved chains were made from different data: delete ", basename(chains_file), " and rerun")
  if (!same_settings) stop("saved chains were made with different settings: delete ", basename(chains_file), " and rerun")
  cat("imputations loaded from", basename(chains_file), "(delete the file to impute afresh)\n")
} else {
  imputations <- mclapply(1:n.chains, function(k)
            jomoImpute(mi_data,
                       formula = list(f.level1, f.level2),
                       n.burn  = n.burn,
                       n.iter  = n.iter,
                       m       = draws.per.chain,
                       seed    = seed + k,
                       silent  = TRUE),
            mc.cores = max(1, min(n.chains, detectCores() - 2)))
  saveRDS(imputations, chains_file)
}

# Trace plots (burn-in check; see deviation note above): made from the saved chains by
# 03_main_analysis/04_imputation_trace_plots.R, not here -- mitml's plot method opens its own
# screen device and pauses before every page, which in an interactive session would swallow
# the following script lines as key presses

# Stack the 30 completed data sets
datasets <- do.call(c, lapply(imputations, mitmlComplete, print = "all"))
length(datasets)   # expect 30

# Person-mean centering WITHIN each imputed data set (Grund et al., 2016, 2018)
datasets <- lapply(datasets, function(d)
              d %>%
              rename(thin_sexualised_female_bodies_exposure = thin_sexualised_female_bodies_seconds) %>%
              group_by(id) %>%
              mutate(SSES_1_multipleChoice_index_between = mean(SSES_1_multipleChoice_index),
                     SSES_1_multipleChoice_index_within  = SSES_1_multipleChoice_index - SSES_1_multipleChoice_index_between) %>%
              ungroup() %>% as.data.frame())


### Registered model on each imputed data set (identical to the complete-case model above)

models <- lapply(datasets, function(d)
            lme(thin_sexualised_female_bodies_exposure ~ SSES_1_multipleChoice_index_within + SSES_1_multipleChoice_index_between,
                data        = d,
                random      = ~ SSES_1_multipleChoice_index_within | id,
                correlation = corAR1(form = ~ day_index | id),
                control     = lmeControl(maxIter = 500, msMaxIter = 500, msMaxEval = 2000,
                                         niterEM = 50, opt = "optim"),
                na.action   = na.fail))

# If ANY of the 30 fits above fails, do not pool a subset and do not mix model variants:
# refit ALL 30 with random = ~ 1 | id (random intercept only), pool those, and record the
# change in Table S5. Never let a fallback happen silently inside the loop.


### Pooling (Rubin's rules)

pooled <- testEstimates(models, extra.pars = TRUE)
pooled
confint(pooled)

# p-values as preregistered (first registration, OSF k7953, "Inference criteria": one-sided
# p-values for its directional hypotheses): within-person ONE-sided and between-person
# ONE-sided, both predicting beta < 0
pooled_estimates <- pooled$estimates
pt(pooled_estimates["SSES_1_multipleChoice_index_within",  "t.value"], df = pooled_estimates["SSES_1_multipleChoice_index_within",  "df"])   # within-person, one-sided
pt(pooled_estimates["SSES_1_multipleChoice_index_between", "t.value"], df = pooled_estimates["SSES_1_multipleChoice_index_between", "df"])   # between-person, one-sided

# Fraction of missing information (manuscript: FMI = .33 for the within-person effect)
pooled_estimates[, "FMI"]

# Save pooled result and the 30 fitted models: read by the figure script (to be added)
saveRDS(list(definition = "clothing_anyposing_thin",
             pooled = pooled, ci = confint(pooled), m = length(datasets), models = models),
        file.path(data_dir, "EH1_EH3_MI_pooled_clothing_anyposing_thin.rds"))

### Plots for the manuscript: separate figure script (to be added), reads the saved models




#######################################################################################################
#######################################################################################################
#######################################################################################################
# SECOND REGISTRATION: EXPOSURE MEASURES FROM THE TIKTOK METADATA
#
# The two blocks above (complete case, multiple imputation) repeated for the two measures of the
# second registration, beauty filters and beauty content, one block pair each.
# Same model, imputation model, m and seeds; only the exposure file differs
# (02_exposure_building/04_build_metadata_exposure_morning.R). In those files the exposure column
# is beauty_filter_seconds / beauty_content_seconds (every watched second of a video with the flag).
# The second registration (OSF chbxw) also registered the ZINB robustness check for beauty filters
# and beauty content -- but conditionally: only "if inspection of the outcome distribution indicates
# substantial non-normality" (right skew, overdispersion, excess zeros), and with a random slope,
# (1 + within-person body satisfaction | id). NOT YET IMPLEMENTED in the blocks below.




#######################################################################################################
#######################################################################################################
#######################################################################################################
# DISTRIBUTION OF THE DAILY EXPOSURE OUTCOMES (for the Supplement, and the ZINB decision)
#
# The second registration (OSF chbxw) makes the zero-inflated negative binomial robustness check
# conditional: it is run only "if inspection of the outcome distribution indicates substantial
# non-normality, specifically pronounced right skew, overdispersion, and an excess of zero-valued
# observations". This block is that inspection, on record: for each morning exposure outcome, the
# share of zero days, the skewness, and the variance against the mean (overdispersion index) of
# the daily seconds on the covered days. The thin, sexualised female bodies measure is included
# for comparison; its ZINB check is registered unconditionally (OSF k7953) and runs above.
# The decision for the metadata measures is written into the Supplement from these numbers.

# Clear workspace
rm(list = ls()); data_dir <- "/Volumes/LaCie/0_IdealBodies_Final/1_Data/Processed_data"

exposure_files <- c("thin, sexualised female bodies" = "daily_exposure_morning_clothing_anyposing_thin.rds",
                    "beauty filter"                  = "daily_exposure_morning_beauty_filter.rds",
                    "beauty content label"           = "daily_exposure_morning_beauty_content_label.rds")
exposure_columns <- c("thin_sexualised_female_bodies_seconds", "beauty_filter_seconds", "beauty_content_seconds")

for (k in seq_along(exposure_files)) {
  daily   <- readRDS(file.path(data_dir, exposure_files[[k]]))
  seconds <- daily[[exposure_columns[[k]]]]
  seconds <- seconds[!is.na(seconds)]                                          # covered days only
  skewness <- mean((seconds - mean(seconds))^3) / sd(seconds)^3                # sample skewness (0 = symmetric)
  cat(sprintf("\n%s (morning window, %d covered days)\n", names(exposure_files)[k], length(seconds)))
  cat(sprintf("  zero days:            %d (%.1f%%)\n", sum(seconds == 0), 100 * mean(seconds == 0)))
  cat(sprintf("  mean / median (min):  %.2f / %.2f\n", mean(seconds) / 60, median(seconds) / 60))
  cat(sprintf("  skewness:             %.2f\n", skewness))
  cat(sprintf("  variance / mean:      %.1f  (1 = Poisson-like; far above 1 = overdispersed)\n", var(seconds) / mean(seconds)))
}



#######################################################################################################
#######################################################################################################
#######################################################################################################
# H2: Start-of-day body satisfaction predicts subsequent content exposure throughout the day.
#     Beauty filters. Lower start-of-day body satisfaction will predict female adolescents spending
#     more daily time (in seconds) watching TikTok videos with a beauty filter on that day.
#     (Confirmatory, secondary.)
# H4: Average start-of-day body satisfaction predicts average content exposure.
#     No corresponding hypothesis was registered for beauty filters: the between-person term is a
#     covariate only and is not tested.
#
# BEAUTY FILTERS -- COMPLETE CASE
# (preregistered sensitivity analysis of the second registration)

# Clear workspace
rm(list = ls()); data_dir <- "/Volumes/LaCie/0_IdealBodies_Final/1_Data/Processed_data"

# Load daily beauty filter exposure on the 924-row participant-day dataset, morning window (66 participants x 14 days;
# window = morning EMA start -> 03:00 UTC next day; days without a morning EMA
# use a substituted start, see window_start_source)
daily <- readRDS(file.path(data_dir, "daily_exposure_morning_beauty_filter.rds"))
daily$id <- as.character(daily$id)

# Body satisfaction under its original m-Path name: SSES item 1 ("I felt satisfied with the way my body looks"),
# morning prompt; the participant-day dataset stores it as sses_am
daily <- rename(daily, SSES_1_multipleChoice_index = sses_am)

daily    <- daily[order(daily$id, daily$date), ]

# Check the grid
stopifnot(nrow(daily) == 924, length(unique(daily$id)) == 66)
table(daily$window_start_source)

# Complete case: keep days with a morning body satisfaction rating AND derivable beauty filter exposure
# (days outside donation coverage have NA beauty filter exposure; non-use days have beauty filter exposure 0, as registered)
complete_case <- subset(daily, !is.na(SSES_1_multipleChoice_index) & !is.na(beauty_filter_seconds))
nrow(complete_case)

# Rescale beauty filter exposure from seconds to minutes (numerical stability; betas are minutes per scale point)
complete_case$beauty_filter_exposure <- complete_case$beauty_filter_seconds / 60

# Person-mean centering of the predictor: between = person mean, within = daily deviation
complete_case <- complete_case %>%
      group_by(id) %>%
      mutate(SSES_1_multipleChoice_index_between = mean(SSES_1_multipleChoice_index),
             SSES_1_multipleChoice_index_within  = SSES_1_multipleChoice_index - SSES_1_multipleChoice_index_between) %>%
      ungroup() %>% as.data.frame()


###########################################################
### Registered model (OSF chbxw), beauty filter exposure as outcome

# Random intercept and random slope for within-person body satisfaction,
# AR(1) residual correlation across study days within participant.
# Registered code: corAR1(form = ~ 1 | id); implemented over day_index so that gaps between
# observed days are respected in the complete-case frame (identical on the full grid) -- S8
# Optimizer: optim (as in the reported run). With beauty filter exposure as the outcome the random-slope
# variance is close to zero and nlminb stops with "singular convergence" on some data sets;
# optim converges. State this in Table S5 next to the nlminb entry for the end-of-day body satisfaction models.
m.h2b.complete_case <- lme(beauty_filter_exposure ~ SSES_1_multipleChoice_index_within + SSES_1_multipleChoice_index_between,
                data        = complete_case,
                random      = ~ SSES_1_multipleChoice_index_within | id,
                correlation = corAR1(form = ~ day_index | id),
                control     = lmeControl(maxIter = 500, msMaxIter = 500, msMaxEval = 2000,
                                         niterEM = 50, opt = "optim"),
                na.action   = na.fail)

summary(m.h2b.complete_case)
intervals(m.h2b.complete_case, which = "fixed")

# p-values as preregistered (second registration, OSF chbxw, "Inference criteria"):
#   within-person  ONE-sided (directional hypothesis, beta < 0)
#   between-person no hypothesis registered (second registration): average start-of-day body
#                  satisfaction is a covariate in this model only, and no test is computed or
#                  reported for it
fixed_effects <- summary(m.h2b.complete_case)$tTable
pt(fixed_effects["SSES_1_multipleChoice_index_within",  "t-value"], df = fixed_effects["SSES_1_multipleChoice_index_within",  "DF"])   # within-person, one-sided

# AR(1) autocorrelation
coef(m.h2b.complete_case$modelStruct$corStruct, unconstrained = FALSE)

# If the model above does not converge on the real data, do NOT silently simplify it:
# refit with random = ~ 1 | id (random intercept only) and record the change in Table S5.




#######################################################################################################
#######################################################################################################
#######################################################################################################
# H2: Start-of-day body satisfaction predicts subsequent content exposure throughout the day.
#     Beauty filters. Lower start-of-day body satisfaction will predict female adolescents spending
#     more daily time (in seconds) watching TikTok videos with a beauty filter on that day.
#     (Confirmatory, secondary.)
# H4: Average start-of-day body satisfaction predicts average content exposure.
#     No corresponding hypothesis was registered for beauty filters: the between-person term is a
#     covariate only and is not tested.
#
# BEAUTY FILTERS -- MULTIPLE IMPUTATION
# (preregistered primary analysis of the second registration)
#
# Deviations from the preregistration (documented in the Supplement):
#   - jomo engine instead of pan: household income (level 2) has 28/66 missing,
#     pan cannot impute level-2 variables
#   - burn-in 20,000 instead of 6,000 after inspection of trace plots
#
# Runtime: roughly 15-45 min on an M3 Ultra with 10 parallel chains.

# Clear workspace
rm(list = ls()); data_dir <- "/Volumes/LaCie/0_IdealBodies_Final/1_Data/Processed_data"

# Load daily beauty filter exposure on the 924-row participant-day dataset, morning window
daily <- readRDS(file.path(data_dir, "daily_exposure_morning_beauty_filter.rds"))
daily$id <- as.character(daily$id)

# Body satisfaction under its original m-Path name: SSES item 1 ("I felt satisfied with the way my body looks"),
# morning prompt; the participant-day dataset stores it as sses_am
daily <- rename(daily, SSES_1_multipleChoice_index = sses_am)

# The second registration's imputation model for start-of-day body satisfaction contains the
# PREVIOUS day's exposure to both metadata measures (OSF chbxw, "Missing data": "+ seconds of beauty
# filter content consumed the previous day + seconds of beauty labeled content consumed the previous
# day"). That registration defines daily exposure as 04:00 -> end-of-day assessment, so the previous
# study day's value comes from the EVENING-window files. Day 1 has no previous study day (NA, imputed).
previous_day_beauty_filter  <- readRDS(file.path(data_dir, "daily_exposure_evening_beauty_filter.rds"))
previous_day_beauty_content <- readRDS(file.path(data_dir, "daily_exposure_evening_beauty_content_label.rds"))
previous_day_beauty_filter$id  <- as.character(previous_day_beauty_filter$id)
previous_day_beauty_content$id <- as.character(previous_day_beauty_content$id)
previous_day_exposure <- previous_day_beauty_filter %>%
                         select(id, day_index, beauty_filter_seconds_previous_day = beauty_filter_seconds) %>%
                         left_join(previous_day_beauty_content %>% select(id, day_index, beauty_content_seconds_previous_day = beauty_content_seconds),
                                   by = c("id", "day_index")) %>%
                         mutate(day_index = day_index + 1L)          # the value of day d is joined to day d + 1
daily <- left_join(daily, previous_day_exposure, by = c("id", "day_index"))
stopifnot(nrow(daily) == 924)
sum(is.na(daily$beauty_filter_seconds_previous_day[daily$day_index == 1]))   # 66: day 1 of every participant

# Load person-level baseline covariates
covariates <- readRDS(file.path(data_dir, "participant_level_covariates.rds"))
covariates$id <- as.character(covariates$id)

# Check: one row per participant; SDQ, O2S3, IUS-C and age complete (they enter the
# imputation model as predictors); household income incomplete (imputed at level 2, see S5)
stopifnot(!any(duplicated(covariates$id)))
colSums(is.na(covariates[, c("SDQ_total", "O2S3_total", "IUSC_total", "age", "household_income")]))
stopifnot(all(complete.cases(covariates[, c("SDQ_total", "O2S3_total", "IUSC_total", "age")])))


### Preprocessing

# Lagged EMA covariates (most recent occasion before the morning prompt, i.e. the previous
# evening) entering the imputation model, Methods eq. (2). Same list as in 01_exposure_predicts_body_satisfaction.R;
# check the printed names against 01_preprocessing/04_build_participant_day_dataset_evening.R (the "prompts" block)
lag_cols <- grep("^lag_", names(daily), value = TRUE)
lag_cols <- setdiff(lag_cols, c("lag_occasion_time", "lag_prompt_type"))
lag_cols <- lag_cols[sapply(daily[lag_cols], is.numeric)]
lag_cols

# Drop lagged covariates without variance (would make the covariance matrix singular)
lag_sd      <- sapply(daily[lag_cols], sd, na.rm = TRUE)
lag_cols <- lag_cols[is.finite(lag_sd) & lag_sd > 1e-8]
lag_cols

# Analysis data set: AM body satisfaction, beauty filter exposure of the day (the analysis outcome), the
# previous day's beauty filter and beauty content exposure (all in minutes), lagged EMA covariates,
# baseline covariates
mi_data <- daily %>%
          select(id, date, day_index, SSES_1_multipleChoice_index, beauty_filter_seconds,
                 beauty_filter_seconds_previous_day, beauty_content_seconds_previous_day, all_of(lag_cols)) %>%
          left_join(covariates %>% select(id, SDQ_total, O2S3_total, IUSC_total, age, household_income),
                    by = "id") %>%
          mutate(beauty_filter_seconds = beauty_filter_seconds / 60,
                 beauty_filter_seconds_previous_day  = beauty_filter_seconds_previous_day / 60,
                 beauty_content_seconds_previous_day = beauty_content_seconds_previous_day / 60,
                 id = as.factor(id)) %>%
          arrange(id, date) %>% as.data.frame()

# Missingness overview (expect 300 AM SSES, 16 beauty filter exposure and 28 household income, as in the thin,
# sexualised female bodies blocks; the previous day's exposure: 66 day-1 rows plus the days after an uncovered day)
colSums(is.na(mi_data))


### Imputation model as registered in the second registration (OSF chbxw, "Missing data")

# Level 1 targets: AM body satisfaction, the previous day's beauty filter and beauty content
#                  exposure (registered), beauty filter exposure of the day (the analysis outcome: kept in
#                  the joint model so the imputed body satisfaction keeps its relation to it), lagged
#                  EMA covariates
# Level 1 predictors: complete baseline covariates (SDQ, O2S3, IUS-C, age)
f.level1 <- as.formula(paste(paste(c("SSES_1_multipleChoice_index", "beauty_filter_seconds", "beauty_filter_seconds_previous_day", "beauty_content_seconds_previous_day", lag_cols), collapse = " + "),
                             "~ 1 + SDQ_total + O2S3_total + IUSC_total + age + (1 | id)"))
# Level 2 target: household income
f.level2 <- household_income ~ 1 + SDQ_total + O2S3_total + IUSC_total + age

# Print the assembled formula and check it against the registered model
f.level1
f.level2


### Imputation

# 10 independent chains x 3 draws = m = 30 imputed data sets (registered m = 30),
# burn-in 20,000, draws 200 iterations apart, seed as used in the reported run
n.chains <- 10 ; draws.per.chain <- 3 ; n.burn <- 20000 ; n.iter <- 200 ; seed <- 8600

# The chains are saved and REUSED: if the file below exists, the imputations are loaded from
# it and only the models are refitted (minutes instead of an hour). The saved chains must
# come from exactly this data set and these settings -- otherwise the script stops. To
# impute afresh, delete the file. The same file feeds 04_imputation_trace_plots.R.
chains_file <- file.path(data_dir, "H2b_H4b_MI_chains_beauty_filter.rds")
if (file.exists(chains_file)) {
  imputations <- readRDS(chains_file)
  saved_columns <- intersect(names(imputations[[1]]$data), names(mi_data))           # mitml keeps the model variables only
  same_data     <- length(saved_columns) == ncol(imputations[[1]]$data) &&
                   isTRUE(all.equal(imputations[[1]]$data[saved_columns], mi_data[saved_columns], check.attributes = FALSE))
  same_settings <- length(imputations) == n.chains && imputations[[1]]$iter$burn == n.burn &&
                   imputations[[1]]$iter$iter == n.iter && imputations[[1]]$iter$m == draws.per.chain
  if (!same_data)     stop("saved chains were made from different data: delete ", basename(chains_file), " and rerun")
  if (!same_settings) stop("saved chains were made with different settings: delete ", basename(chains_file), " and rerun")
  cat("imputations loaded from", basename(chains_file), "(delete the file to impute afresh)\n")
} else {
  imputations <- mclapply(1:n.chains, function(k)
            jomoImpute(mi_data,
                       formula = list(f.level1, f.level2),
                       n.burn  = n.burn,
                       n.iter  = n.iter,
                       m       = draws.per.chain,
                       seed    = seed + k,
                       silent  = TRUE),
            mc.cores = max(1, min(n.chains, detectCores() - 2)))
  saveRDS(imputations, chains_file)
}

# Trace plots (burn-in check; see deviation note above): made from the saved chains by
# 03_main_analysis/04_imputation_trace_plots.R, not here -- mitml's plot method opens its own
# screen device and pauses before every page, which in an interactive session would swallow
# the following script lines as key presses

# Stack the 30 completed data sets
datasets <- do.call(c, lapply(imputations, mitmlComplete, print = "all"))
length(datasets)   # expect 30

# Person-mean centering WITHIN each imputed data set (Grund et al., 2016, 2018)
datasets <- lapply(datasets, function(d)
              d %>%
              rename(beauty_filter_exposure = beauty_filter_seconds) %>%
              group_by(id) %>%
              mutate(SSES_1_multipleChoice_index_between = mean(SSES_1_multipleChoice_index),
                     SSES_1_multipleChoice_index_within  = SSES_1_multipleChoice_index - SSES_1_multipleChoice_index_between) %>%
              ungroup() %>% as.data.frame())


### Registered model on each imputed data set (identical to the complete-case model above)

models <- lapply(datasets, function(d)
            lme(beauty_filter_exposure ~ SSES_1_multipleChoice_index_within + SSES_1_multipleChoice_index_between,
                data        = d,
                random      = ~ SSES_1_multipleChoice_index_within | id,
                correlation = corAR1(form = ~ day_index | id),
                control     = lmeControl(maxIter = 500, msMaxIter = 500, msMaxEval = 2000,
                                         niterEM = 50, opt = "optim"),
                na.action   = na.fail))

# If ANY of the 30 fits above fails, do not pool a subset and do not mix model variants:
# refit ALL 30 with random = ~ 1 | id (random intercept only), pool those, and record the
# change in Table S5. Never let a fallback happen silently inside the loop.


### Pooling (Rubin's rules)

pooled <- testEstimates(models, extra.pars = TRUE)
pooled
confint(pooled)

# p-values as preregistered (second registration, OSF chbxw, "Inference criteria"):
#   within-person  ONE-sided (directional hypothesis, beta < 0)
#   between-person no hypothesis registered (second registration): average start-of-day body
#                  satisfaction is a covariate in this model only, and no test is computed or
#                  reported for it
pooled_estimates <- pooled$estimates
pt(pooled_estimates["SSES_1_multipleChoice_index_within",  "t.value"], df = pooled_estimates["SSES_1_multipleChoice_index_within",  "df"])   # within-person, one-sided

# Fraction of missing information
pooled_estimates[, "FMI"]

# Save pooled result and the 30 fitted models: read by the figure script (to be added)
saveRDS(list(definition = "beauty_filter",
             pooled = pooled, ci = confint(pooled), m = length(datasets), models = models),
        file.path(data_dir, "H2b_H4b_MI_pooled_beauty_filter.rds"))

### Plots for the manuscript: separate figure script (to be added), reads the saved models




#######################################################################################################
#######################################################################################################
#######################################################################################################
# H2: Start-of-day body satisfaction predicts subsequent content exposure throughout the day.
#     Beauty content. Lower start-of-day body satisfaction will predict female adolescents spending
#     more daily time (in seconds) watching TikTok videos labelled as containing beauty content on
#     that day. (Confirmatory, secondary.)
# H4: Average start-of-day body satisfaction predicts average content exposure.
#     No corresponding hypothesis was registered for beauty content: the between-person term is a
#     covariate only and is not tested.
#
# BEAUTY CONTENT -- COMPLETE CASE
# (preregistered sensitivity analysis of the second registration)

# Clear workspace
rm(list = ls()); data_dir <- "/Volumes/LaCie/0_IdealBodies_Final/1_Data/Processed_data"

# Load daily beauty content exposure on the 924-row participant-day dataset, morning window (66 participants x 14 days;
# window = morning EMA start -> 03:00 UTC next day; days without a morning EMA
# use a substituted start, see window_start_source)
daily <- readRDS(file.path(data_dir, "daily_exposure_morning_beauty_content_label.rds"))
daily$id <- as.character(daily$id)

# Body satisfaction under its original m-Path name: SSES item 1 ("I felt satisfied with the way my body looks"),
# morning prompt; the participant-day dataset stores it as sses_am
daily <- rename(daily, SSES_1_multipleChoice_index = sses_am)

daily    <- daily[order(daily$id, daily$date), ]

# Check the grid
stopifnot(nrow(daily) == 924, length(unique(daily$id)) == 66)
table(daily$window_start_source)

# Complete case: keep days with a morning body satisfaction rating AND derivable beauty content exposure
# (days outside donation coverage have NA beauty content exposure; non-use days have beauty content exposure 0, as registered)
complete_case <- subset(daily, !is.na(SSES_1_multipleChoice_index) & !is.na(beauty_content_seconds))
nrow(complete_case)

# Rescale beauty content exposure from seconds to minutes (numerical stability; betas are minutes per scale point)
complete_case$beauty_content_exposure <- complete_case$beauty_content_seconds / 60

# Person-mean centering of the predictor: between = person mean, within = daily deviation
complete_case <- complete_case %>%
      group_by(id) %>%
      mutate(SSES_1_multipleChoice_index_between = mean(SSES_1_multipleChoice_index),
             SSES_1_multipleChoice_index_within  = SSES_1_multipleChoice_index - SSES_1_multipleChoice_index_between) %>%
      ungroup() %>% as.data.frame()


###########################################################
### Registered model (OSF chbxw), beauty content exposure as outcome

# Random intercept and random slope for within-person body satisfaction,
# AR(1) residual correlation across study days within participant.
# Registered code: corAR1(form = ~ 1 | id); implemented over day_index so that gaps between
# observed days are respected in the complete-case frame (identical on the full grid) -- S8
# Optimizer: optim (as in the reported run). With beauty content exposure as the outcome the random-slope
# variance is close to zero and nlminb stops with "singular convergence" on some data sets;
# optim converges. State this in Table S5 next to the nlminb entry for the end-of-day body satisfaction models.
m.h2c.complete_case <- lme(beauty_content_exposure ~ SSES_1_multipleChoice_index_within + SSES_1_multipleChoice_index_between,
                data        = complete_case,
                random      = ~ SSES_1_multipleChoice_index_within | id,
                correlation = corAR1(form = ~ day_index | id),
                control     = lmeControl(maxIter = 500, msMaxIter = 500, msMaxEval = 2000,
                                         niterEM = 50, opt = "optim"),
                na.action   = na.fail)

summary(m.h2c.complete_case)
intervals(m.h2c.complete_case, which = "fixed")

# p-values as preregistered (second registration, OSF chbxw, "Inference criteria"):
#   within-person  ONE-sided (directional hypothesis, beta < 0)
#   between-person no hypothesis registered (second registration): average start-of-day body
#                  satisfaction is a covariate in this model only, and no test is computed or
#                  reported for it
fixed_effects <- summary(m.h2c.complete_case)$tTable
pt(fixed_effects["SSES_1_multipleChoice_index_within",  "t-value"], df = fixed_effects["SSES_1_multipleChoice_index_within",  "DF"])   # within-person, one-sided

# AR(1) autocorrelation
coef(m.h2c.complete_case$modelStruct$corStruct, unconstrained = FALSE)

# If the model above does not converge on the real data, do NOT silently simplify it:
# refit with random = ~ 1 | id (random intercept only) and record the change in Table S5.




#######################################################################################################
#######################################################################################################
#######################################################################################################
# H2: Start-of-day body satisfaction predicts subsequent content exposure throughout the day.
#     Beauty content. Lower start-of-day body satisfaction will predict female adolescents spending
#     more daily time (in seconds) watching TikTok videos labelled as containing beauty content on
#     that day. (Confirmatory, secondary.)
# H4: Average start-of-day body satisfaction predicts average content exposure.
#     No corresponding hypothesis was registered for beauty content: the between-person term is a
#     covariate only and is not tested.
#
# BEAUTY CONTENT -- MULTIPLE IMPUTATION
# (preregistered primary analysis of the second registration)
#
# Deviations from the preregistration (documented in the Supplement):
#   - jomo engine instead of pan: household income (level 2) has 28/66 missing,
#     pan cannot impute level-2 variables
#   - burn-in 20,000 instead of 6,000 after inspection of trace plots
#
# Runtime: roughly 15-45 min on an M3 Ultra with 10 parallel chains.

# Clear workspace
rm(list = ls()); data_dir <- "/Volumes/LaCie/0_IdealBodies_Final/1_Data/Processed_data"

# Load daily beauty content exposure on the 924-row participant-day dataset, morning window
daily <- readRDS(file.path(data_dir, "daily_exposure_morning_beauty_content_label.rds"))
daily$id <- as.character(daily$id)

# Body satisfaction under its original m-Path name: SSES item 1 ("I felt satisfied with the way my body looks"),
# morning prompt; the participant-day dataset stores it as sses_am
daily <- rename(daily, SSES_1_multipleChoice_index = sses_am)

# The second registration's imputation model for start-of-day body satisfaction contains the
# PREVIOUS day's exposure to both metadata measures (OSF chbxw, "Missing data": "+ seconds of beauty
# filter content consumed the previous day + seconds of beauty labeled content consumed the previous
# day"). That registration defines daily exposure as 04:00 -> end-of-day assessment, so the previous
# study day's value comes from the EVENING-window files. Day 1 has no previous study day (NA, imputed).
previous_day_beauty_filter  <- readRDS(file.path(data_dir, "daily_exposure_evening_beauty_filter.rds"))
previous_day_beauty_content <- readRDS(file.path(data_dir, "daily_exposure_evening_beauty_content_label.rds"))
previous_day_beauty_filter$id  <- as.character(previous_day_beauty_filter$id)
previous_day_beauty_content$id <- as.character(previous_day_beauty_content$id)
previous_day_exposure <- previous_day_beauty_filter %>%
                         select(id, day_index, beauty_filter_seconds_previous_day = beauty_filter_seconds) %>%
                         left_join(previous_day_beauty_content %>% select(id, day_index, beauty_content_seconds_previous_day = beauty_content_seconds),
                                   by = c("id", "day_index")) %>%
                         mutate(day_index = day_index + 1L)          # the value of day d is joined to day d + 1
daily <- left_join(daily, previous_day_exposure, by = c("id", "day_index"))
stopifnot(nrow(daily) == 924)
sum(is.na(daily$beauty_filter_seconds_previous_day[daily$day_index == 1]))   # 66: day 1 of every participant

# Load person-level baseline covariates
covariates <- readRDS(file.path(data_dir, "participant_level_covariates.rds"))
covariates$id <- as.character(covariates$id)

# Check: one row per participant; SDQ, O2S3, IUS-C and age complete (they enter the
# imputation model as predictors); household income incomplete (imputed at level 2, see S5)
stopifnot(!any(duplicated(covariates$id)))
colSums(is.na(covariates[, c("SDQ_total", "O2S3_total", "IUSC_total", "age", "household_income")]))
stopifnot(all(complete.cases(covariates[, c("SDQ_total", "O2S3_total", "IUSC_total", "age")])))


### Preprocessing

# Lagged EMA covariates (most recent occasion before the morning prompt, i.e. the previous
# evening) entering the imputation model, Methods eq. (2). Same list as in 01_exposure_predicts_body_satisfaction.R;
# check the printed names against 01_preprocessing/04_build_participant_day_dataset_evening.R (the "prompts" block)
lag_cols <- grep("^lag_", names(daily), value = TRUE)
lag_cols <- setdiff(lag_cols, c("lag_occasion_time", "lag_prompt_type"))
lag_cols <- lag_cols[sapply(daily[lag_cols], is.numeric)]
lag_cols

# Drop lagged covariates without variance (would make the covariance matrix singular)
lag_sd      <- sapply(daily[lag_cols], sd, na.rm = TRUE)
lag_cols <- lag_cols[is.finite(lag_sd) & lag_sd > 1e-8]
lag_cols

# Analysis data set: AM body satisfaction, beauty content exposure of the day (the analysis outcome), the
# previous day's beauty filter and beauty content exposure (all in minutes), lagged EMA covariates,
# baseline covariates
mi_data <- daily %>%
          select(id, date, day_index, SSES_1_multipleChoice_index, beauty_content_seconds,
                 beauty_filter_seconds_previous_day, beauty_content_seconds_previous_day, all_of(lag_cols)) %>%
          left_join(covariates %>% select(id, SDQ_total, O2S3_total, IUSC_total, age, household_income),
                    by = "id") %>%
          mutate(beauty_content_seconds = beauty_content_seconds / 60,
                 beauty_filter_seconds_previous_day  = beauty_filter_seconds_previous_day / 60,
                 beauty_content_seconds_previous_day = beauty_content_seconds_previous_day / 60,
                 id = as.factor(id)) %>%
          arrange(id, date) %>% as.data.frame()

# Missingness overview (expect 300 AM SSES, 16 beauty content exposure and 28 household income, as in the thin,
# sexualised female bodies blocks; the previous day's exposure: 66 day-1 rows plus the days after an uncovered day)
colSums(is.na(mi_data))


### Imputation model as registered in the second registration (OSF chbxw, "Missing data")

# Level 1 targets: AM body satisfaction, the previous day's beauty filter and beauty content
#                  exposure (registered), beauty content exposure of the day (the analysis outcome: kept in
#                  the joint model so the imputed body satisfaction keeps its relation to it), lagged
#                  EMA covariates
# Level 1 predictors: complete baseline covariates (SDQ, O2S3, IUS-C, age)
f.level1 <- as.formula(paste(paste(c("SSES_1_multipleChoice_index", "beauty_content_seconds", "beauty_filter_seconds_previous_day", "beauty_content_seconds_previous_day", lag_cols), collapse = " + "),
                             "~ 1 + SDQ_total + O2S3_total + IUSC_total + age + (1 | id)"))
# Level 2 target: household income
f.level2 <- household_income ~ 1 + SDQ_total + O2S3_total + IUSC_total + age

# Print the assembled formula and check it against the registered model
f.level1
f.level2


### Imputation

# 10 independent chains x 3 draws = m = 30 imputed data sets (registered m = 30),
# burn-in 20,000, draws 200 iterations apart, seed as used in the reported run
n.chains <- 10 ; draws.per.chain <- 3 ; n.burn <- 20000 ; n.iter <- 200 ; seed <- 8600

# The chains are saved and REUSED: if the file below exists, the imputations are loaded from
# it and only the models are refitted (minutes instead of an hour). The saved chains must
# come from exactly this data set and these settings -- otherwise the script stops. To
# impute afresh, delete the file. The same file feeds 04_imputation_trace_plots.R.
chains_file <- file.path(data_dir, "H2c_H4c_MI_chains_beauty_content_label.rds")
if (file.exists(chains_file)) {
  imputations <- readRDS(chains_file)
  saved_columns <- intersect(names(imputations[[1]]$data), names(mi_data))           # mitml keeps the model variables only
  same_data     <- length(saved_columns) == ncol(imputations[[1]]$data) &&
                   isTRUE(all.equal(imputations[[1]]$data[saved_columns], mi_data[saved_columns], check.attributes = FALSE))
  same_settings <- length(imputations) == n.chains && imputations[[1]]$iter$burn == n.burn &&
                   imputations[[1]]$iter$iter == n.iter && imputations[[1]]$iter$m == draws.per.chain
  if (!same_data)     stop("saved chains were made from different data: delete ", basename(chains_file), " and rerun")
  if (!same_settings) stop("saved chains were made with different settings: delete ", basename(chains_file), " and rerun")
  cat("imputations loaded from", basename(chains_file), "(delete the file to impute afresh)\n")
} else {
  imputations <- mclapply(1:n.chains, function(k)
            jomoImpute(mi_data,
                       formula = list(f.level1, f.level2),
                       n.burn  = n.burn,
                       n.iter  = n.iter,
                       m       = draws.per.chain,
                       seed    = seed + k,
                       silent  = TRUE),
            mc.cores = max(1, min(n.chains, detectCores() - 2)))
  saveRDS(imputations, chains_file)
}

# Trace plots (burn-in check; see deviation note above): made from the saved chains by
# 03_main_analysis/04_imputation_trace_plots.R, not here -- mitml's plot method opens its own
# screen device and pauses before every page, which in an interactive session would swallow
# the following script lines as key presses

# Stack the 30 completed data sets
datasets <- do.call(c, lapply(imputations, mitmlComplete, print = "all"))
length(datasets)   # expect 30

# Person-mean centering WITHIN each imputed data set (Grund et al., 2016, 2018)
datasets <- lapply(datasets, function(d)
              d %>%
              rename(beauty_content_exposure = beauty_content_seconds) %>%
              group_by(id) %>%
              mutate(SSES_1_multipleChoice_index_between = mean(SSES_1_multipleChoice_index),
                     SSES_1_multipleChoice_index_within  = SSES_1_multipleChoice_index - SSES_1_multipleChoice_index_between) %>%
              ungroup() %>% as.data.frame())


### Registered model on each imputed data set (identical to the complete-case model above)

models <- lapply(datasets, function(d)
            lme(beauty_content_exposure ~ SSES_1_multipleChoice_index_within + SSES_1_multipleChoice_index_between,
                data        = d,
                random      = ~ SSES_1_multipleChoice_index_within | id,
                correlation = corAR1(form = ~ day_index | id),
                control     = lmeControl(maxIter = 500, msMaxIter = 500, msMaxEval = 2000,
                                         niterEM = 50, opt = "optim"),
                na.action   = na.fail))

# If ANY of the 30 fits above fails, do not pool a subset and do not mix model variants:
# refit ALL 30 with random = ~ 1 | id (random intercept only), pool those, and record the
# change in Table S5. Never let a fallback happen silently inside the loop.


### Pooling (Rubin's rules)

pooled <- testEstimates(models, extra.pars = TRUE)
pooled
confint(pooled)

# p-values as preregistered (second registration, OSF chbxw, "Inference criteria"):
#   within-person  ONE-sided (directional hypothesis, beta < 0)
#   between-person no hypothesis registered (second registration): average start-of-day body
#                  satisfaction is a covariate in this model only, and no test is computed or
#                  reported for it
pooled_estimates <- pooled$estimates
pt(pooled_estimates["SSES_1_multipleChoice_index_within",  "t.value"], df = pooled_estimates["SSES_1_multipleChoice_index_within",  "df"])   # within-person, one-sided

# Fraction of missing information
pooled_estimates[, "FMI"]

# Save pooled result and the 30 fitted models: read by the figure script (to be added)
saveRDS(list(definition = "beauty_content_label",
             pooled = pooled, ci = confint(pooled), m = length(datasets), models = models),
        file.path(data_dir, "H2c_H4c_MI_pooled_beauty_content_label.rds"))

### Plots for the manuscript: separate figure script (to be added), reads the saved models
