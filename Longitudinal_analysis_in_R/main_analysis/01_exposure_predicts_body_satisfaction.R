#######################################################################################
#                                                                                     #
#   Exposure to ideal appearance content on TikTok and body satisfaction              #
#   in female adolescents                                                             #
#                                                                                     #
#   Within-person:  H1: Exposure throughout the day predicts end-of-day body          #
#                   satisfaction.                                                     #
#   Between-person: H3: Average content exposure predicts average end-of-day body     #
#                   satisfaction.                                                     #
#   Three exposure measures, one block pair each: thin, sexualised female bodies;     #
#   beauty filters; beauty content. Each block is titled with the hypotheses it tests.#
#   Body satisfaction = SSES_1_multipleChoice_index (evening prompt).                 #
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



#######################################################################################################
#######################################################################################################
#######################################################################################################
# H1: Exposure throughout the day predicts end-of-day body satisfaction.
#     The daily time (in seconds) female adolescents spend watching TikTok videos containing at
#     least one thin and sexualised female body (indexed by slim body stature and minimal clothing)
#     will predict subsequent lower end-of-day body satisfaction. (Confirmatory; primary hypothesis
#     of the first registration.)
# H3: Average content exposure predicts average end-of-day body satisfaction.
#     Female adolescents spending more daily time (in seconds) – on average – watching TikTok videos
#     containing at least one thin (indexed by slim body stature) and sexualised female body
#     (indexed by minimal clothing and sexually suggestive posing of body or face) will report lower
#     average end-of-day body satisfaction. (Exploratory.)
#
# THIN, SEXUALISED FEMALE BODIES -- COMPLETE CASE
# (preregistered sensitivity analysis; manuscript reports beta_within = -0.052, p = .115)

# Clear workspace
rm(list = ls()); data_dir <- "/Volumes/LaCie/0_IdealBodies_Final/1_Data/Processed_data"

# Load daily thin sexualised female bodies exposure on the 924-row grid (66 participants x 14 days)
daily <- readRDS(file.path(data_dir, "daily_exposure_evening_clothing_anyposing_thin.rds"))
daily$id <- as.character(daily$id)

# Body satisfaction under its original m-Path name: SSES item 1 ("I felt satisfied with the way my body looks"),
# evening prompt; the participant-day dataset stores it as sses_pm
daily <- rename(daily, SSES_1_multipleChoice_index = sses_pm)

daily    <- daily[order(daily$id, daily$date), ]

# Check the grid
stopifnot(nrow(daily) == 924, length(unique(daily$id)) == 66)

# Complete case: keep days with an evening body satisfaction rating AND derivable thin sexualised female bodies exposure
# (days outside donation coverage have NA thin sexualised female bodies exposure; non-use days have thin sexualised female bodies exposure 0, as registered)
complete_case <- subset(daily, !is.na(SSES_1_multipleChoice_index) & !is.na(thin_sexualised_female_bodies_seconds))
nrow(complete_case)

# Rescale thin sexualised female bodies exposure from seconds to minutes (numerical stability; betas are per minute)
complete_case$thin_sexualised_female_bodies_exposure <- complete_case$thin_sexualised_female_bodies_seconds / 60

# Person-mean centering: between = person mean, within = daily deviation from it
complete_case <- complete_case %>%
      group_by(id) %>%
      mutate(thin_sexualised_female_bodies_exposure_between = mean(thin_sexualised_female_bodies_exposure),
             thin_sexualised_female_bodies_exposure_within  = thin_sexualised_female_bodies_exposure - thin_sexualised_female_bodies_exposure_between) %>%
      ungroup() %>% as.data.frame()


###########################################################
### Registered model (OSF k7953)

# Random intercept and random slope for within-person thin sexualised female bodies exposure,
# AR(1) residual correlation across study days within participant.
# Registered code: corAR1(form = ~ 1 | id); implemented over day_index so that gaps between
# observed days are respected in the complete-case frame (identical on the full grid) -- S8
m.h1.complete_case <- lme(SSES_1_multipleChoice_index ~ thin_sexualised_female_bodies_exposure_within + thin_sexualised_female_bodies_exposure_between,
               data        = complete_case,
               random      = ~ thin_sexualised_female_bodies_exposure_within | id,
               correlation = corAR1(form = ~ day_index | id),
               control     = lmeControl(maxIter = 500, msMaxIter = 500, msMaxEval = 2000,
                                        niterEM = 50, opt = "nlminb"),
               na.action   = na.fail)

summary(m.h1.complete_case)
intervals(m.h1.complete_case, which = "fixed")

# p-values as preregistered (first registration, OSF k7953, "Inference criteria": one-sided
# p-values for its directional hypotheses): within-person ONE-sided and between-person
# ONE-sided, both predicting beta < 0
fixed_effects <- summary(m.h1.complete_case)$tTable
pt(fixed_effects["thin_sexualised_female_bodies_exposure_within", "t-value"], df = fixed_effects["thin_sexualised_female_bodies_exposure_within", "DF"])   # within-person, one-sided
pt(fixed_effects["thin_sexualised_female_bodies_exposure_between", "t-value"], df = fixed_effects["thin_sexualised_female_bodies_exposure_between", "DF"])   # between-person, one-sided

# AR(1) autocorrelation
coef(m.h1.complete_case$modelStruct$corStruct, unconstrained = FALSE)




#######################################################################################################
#######################################################################################################
#######################################################################################################
# H1: Exposure throughout the day predicts end-of-day body satisfaction.
#     The daily time (in seconds) female adolescents spend watching TikTok videos containing at
#     least one thin and sexualised female body (indexed by slim body stature and minimal clothing)
#     will predict subsequent lower end-of-day body satisfaction. (Confirmatory; primary hypothesis
#     of the first registration.)
# H3: Average content exposure predicts average end-of-day body satisfaction.
#     Female adolescents spending more daily time (in seconds) – on average – watching TikTok videos
#     containing at least one thin (indexed by slim body stature) and sexualised female body
#     (indexed by minimal clothing and sexually suggestive posing of body or face) will report lower
#     average end-of-day body satisfaction. (Exploratory.)
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

# Load daily thin sexualised female bodies exposure on the 924-row grid
daily <- readRDS(file.path(data_dir, "daily_exposure_evening_clothing_anyposing_thin.rds"))
daily$id <- as.character(daily$id)

# Body satisfaction under its original m-Path name: SSES item 1 ("I felt satisfied with the way my body looks"),
# evening prompt; the participant-day dataset stores it as sses_pm
daily <- rename(daily, SSES_1_multipleChoice_index = sses_pm)

# Load person-level baseline covariates
covariates <- readRDS(file.path(data_dir, "participant_level_covariates.rds"))
covariates$id <- as.character(covariates$id)

# Check: one row per participant; SDQ, O2S3, IUS-C and age complete (they enter the
# imputation model as predictors); household income incomplete (imputed at level 2, see S5)
stopifnot(!any(duplicated(covariates$id)))
colSums(is.na(covariates[, c("SDQ_total", "O2S3_total", "IUSC_total", "age", "household_income")]))
stopifnot(all(complete.cases(covariates[, c("SDQ_total", "O2S3_total", "IUSC_total", "age")])))


### Preprocessing

# Lagged EMA covariates (previous occasion) entering the imputation model, Methods eq. (1).
# Written out explicitly so the list can be checked against the grid builder
# (01_preprocessing/04_build_participant_day_dataset_evening.R, the "prompts" block)
lag_cols <- grep("^lag_", names(daily), value = TRUE)
lag_cols <- setdiff(lag_cols, c("lag_occasion_time", "lag_prompt_type"))
lag_cols <- lag_cols[sapply(daily[lag_cols], is.numeric)]
lag_cols

# Drop lagged covariates without variance (would make the covariance matrix singular)
lag_sd      <- sapply(daily[lag_cols], sd, na.rm = TRUE)
lag_cols <- lag_cols[is.finite(lag_sd) & lag_sd > 1e-8]
lag_cols

# Analysis data set: outcome, thin sexualised female bodies exposure (in minutes), lagged EMA covariates, baseline covariates
mi_data <- daily %>%
          select(id, date, day_index, SSES_1_multipleChoice_index, thin_sexualised_female_bodies_seconds, all_of(lag_cols)) %>%
          left_join(covariates %>% select(id, SDQ_total, O2S3_total, IUSC_total, age, household_income),
                    by = "id") %>%
          mutate(thin_sexualised_female_bodies_seconds = thin_sexualised_female_bodies_seconds / 60,
                 id = as.factor(id)) %>%
          arrange(id, date) %>% as.data.frame()

# Missingness overview (manuscript: 170 PM SSES, 16 thin sexualised female bodies exposure, 28 household income)
colSums(is.na(mi_data))


### Imputation model, Methods eq. (1)

# Level 1 targets: PM body satisfaction, thin sexualised female bodies exposure, lagged EMA covariates
# Level 1 predictors: complete baseline covariates (SDQ, O2S3, IUS-C, age)
f.level1 <- as.formula(paste(paste(c("SSES_1_multipleChoice_index", "thin_sexualised_female_bodies_seconds", lag_cols), collapse = " + "),
                             "~ 1 + SDQ_total + O2S3_total + IUSC_total + age + (1 | id)"))
# Level 2 target: household income
f.level2 <- household_income ~ 1 + SDQ_total + O2S3_total + IUSC_total + age

# Print the assembled formula and check it against eq. (1)
f.level1
f.level2


### Imputation

# 10 independent chains x 3 draws = m = 30 imputed data sets (registered m = 30),
# burn-in 20,000, draws 200 iterations apart, seed as registered
n.chains <- 10 ; draws.per.chain <- 3 ; n.burn <- 20000 ; n.iter <- 200 ; seed <- 20260731

# The chains are saved and REUSED: if the file below exists, the imputations are loaded from
# it and only the models are refitted (minutes instead of an hour). The saved chains must
# come from exactly this data set and these settings -- otherwise the script stops. To
# impute afresh, delete the file. The same file feeds 04_imputation_trace_plots.R.
chains_file <- file.path(data_dir, "H1_EH2_MI_chains_clothing_anyposing_thin.rds")
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
              mutate(thin_sexualised_female_bodies_exposure_between = mean(thin_sexualised_female_bodies_exposure),
                     thin_sexualised_female_bodies_exposure_within  = thin_sexualised_female_bodies_exposure - thin_sexualised_female_bodies_exposure_between) %>%
              ungroup() %>% as.data.frame())


### Registered model on each imputed data set (identical to the complete-case model above)

# nlminb first (as in the complete-case model). When the random-slope variance is close to zero,
# nlminb can stop with "singular convergence" on some imputed data sets; then ALL 30 are refitted
# with optim -- the same model, only the optimiser differs (record in Table S5). Never pool a
# subset and never mix optimisers across the 30 fits. If optim fails as well, the script stops.
models <- lapply(datasets, function(d)
            tryCatch(lme(SSES_1_multipleChoice_index ~ thin_sexualised_female_bodies_exposure_within + thin_sexualised_female_bodies_exposure_between,
                         data        = d,
                         random      = ~ thin_sexualised_female_bodies_exposure_within | id,
                         correlation = corAR1(form = ~ day_index | id),
                         control     = lmeControl(maxIter = 500, msMaxIter = 500, msMaxEval = 2000,
                                                  niterEM = 50, opt = "nlminb"),
                         na.action   = na.fail),
                     error = function(e) NULL))
n_failed_nlminb <- sum(sapply(models, is.null))
cat("fits that failed with nlminb:", n_failed_nlminb, "of", length(datasets), "\n")
if (n_failed_nlminb > 0) {
  cat("refitting ALL", length(datasets), "imputed data sets with optim (same model; record the optimiser in Table S5)\n")
  models <- lapply(datasets, function(d)
              lme(SSES_1_multipleChoice_index ~ thin_sexualised_female_bodies_exposure_within + thin_sexualised_female_bodies_exposure_between,
                  data        = d,
                  random      = ~ thin_sexualised_female_bodies_exposure_within | id,
                  correlation = corAR1(form = ~ day_index | id),
                  control     = lmeControl(maxIter = 500, msMaxIter = 500, msMaxEval = 2000,
                                           niterEM = 50, opt = "optim"),
                  na.action   = na.fail))
}
stopifnot(!any(sapply(models, is.null)), length(models) == length(datasets))


### Pooling (Rubin's rules)

pooled <- testEstimates(models, extra.pars = TRUE)
pooled
confint(pooled)

# p-values as preregistered (first registration, OSF k7953, "Inference criteria": one-sided
# p-values for its directional hypotheses): within-person ONE-sided and between-person
# ONE-sided, both predicting beta < 0
pooled_estimates <- pooled$estimates
pt(pooled_estimates["thin_sexualised_female_bodies_exposure_within",  "t.value"], df = pooled_estimates["thin_sexualised_female_bodies_exposure_within",  "df"])   # within-person, one-sided
pt(pooled_estimates["thin_sexualised_female_bodies_exposure_between", "t.value"], df = pooled_estimates["thin_sexualised_female_bodies_exposure_between", "df"])   # between-person, one-sided

# Fraction of missing information (manuscript: FMI = .20 for the within-person effect)
pooled_estimates[, "FMI"]

# Save pooled result and the 30 fitted models: read by the figure script (to be added)
saveRDS(list(definition = "clothing_anyposing_thin",
             pooled = pooled, ci = confint(pooled), m = length(datasets), models = models),
        file.path(data_dir, "H1_EH2_MI_pooled_clothing_anyposing_thin.rds"))

### Plots for the manuscript: separate figure script (to be added), reads the saved models




#######################################################################################################
#######################################################################################################
#######################################################################################################
# SECOND REGISTRATION: EXPOSURE MEASURES FROM THE TIKTOK METADATA
#
# The two blocks above (complete case, multiple imputation) repeated for the two measures of the
# second registration, beauty filters and beauty content, one block pair each.
# Same model, imputation model, m and seeds; only the exposure file differs
# (02_exposure_building/03_build_metadata_exposure_evening.R). In those files the exposure column
# is beauty_filter_seconds / beauty_content_seconds (every watched second of a video with the flag).




#######################################################################################################
#######################################################################################################
#######################################################################################################
# H1: Exposure throughout the day predicts end-of-day body satisfaction.
#     More daily time (in seconds) that female adolescents spend watching TikTok videos containing a
#     beauty filter will predict subsequent lower end-of-day body satisfaction. (Confirmatory;
#     primary hypothesis of the second registration.)
# H3: Average content exposure predicts average end-of-day body satisfaction.
#     Female adolescents who spend more daily time (in seconds) – on average – watching TikTok
#     videos tagged as using a beauty filter will report lower average end-of-day body satisfaction.
#     (Confirmatory, secondary.)
#
# BEAUTY FILTERS -- COMPLETE CASE
# (preregistered sensitivity analysis of the second registration)

# Clear workspace
rm(list = ls()); data_dir <- "/Volumes/LaCie/0_IdealBodies_Final/1_Data/Processed_data"

# Load daily beauty filter exposure on the 924-row grid (66 participants x 14 days)
daily <- readRDS(file.path(data_dir, "daily_exposure_evening_beauty_filter.rds"))
daily$id <- as.character(daily$id)

# Body satisfaction under its original m-Path name: SSES item 1 ("I felt satisfied with the way my body looks"),
# evening prompt; the participant-day dataset stores it as sses_pm
daily <- rename(daily, SSES_1_multipleChoice_index = sses_pm)

daily    <- daily[order(daily$id, daily$date), ]

# Check the grid
stopifnot(nrow(daily) == 924, length(unique(daily$id)) == 66)

# Complete case: keep days with an evening body satisfaction rating AND derivable beauty filter exposure
# (days outside donation coverage have NA beauty filter exposure; non-use days have beauty filter exposure 0, as registered)
complete_case <- subset(daily, !is.na(SSES_1_multipleChoice_index) & !is.na(beauty_filter_seconds))
nrow(complete_case)

# Rescale beauty filter exposure from seconds to minutes (numerical stability; betas are per minute)
complete_case$beauty_filter_exposure <- complete_case$beauty_filter_seconds / 60

# Person-mean centering: between = person mean, within = daily deviation from it
complete_case <- complete_case %>%
      group_by(id) %>%
      mutate(beauty_filter_exposure_between = mean(beauty_filter_exposure),
             beauty_filter_exposure_within  = beauty_filter_exposure - beauty_filter_exposure_between) %>%
      ungroup() %>% as.data.frame()


###########################################################
### Registered model (OSF chbxw)

# Random intercept and random slope for within-person beauty filter exposure,
# AR(1) residual correlation across study days within participant.
# Registered code: corAR1(form = ~ 1 | id); implemented over day_index so that gaps between
# observed days are respected in the complete-case frame (identical on the full grid) -- S8
m.h1b.complete_case <- lme(SSES_1_multipleChoice_index ~ beauty_filter_exposure_within + beauty_filter_exposure_between,
               data        = complete_case,
               random      = ~ beauty_filter_exposure_within | id,
               correlation = corAR1(form = ~ day_index | id),
               control     = lmeControl(maxIter = 500, msMaxIter = 500, msMaxEval = 2000,
                                        niterEM = 50, opt = "nlminb"),
               na.action   = na.fail)

summary(m.h1b.complete_case)
intervals(m.h1b.complete_case, which = "fixed")

# p-values as preregistered (second registration, OSF chbxw, "Inference criteria"):
#   within-person  ONE-sided (directional hypothesis, beta < 0)
#   between-person TWO-sided (registered as non-directional in the inference criteria,
#                  although the hypothesis text predicts lower body satisfaction)
fixed_effects <- summary(m.h1b.complete_case)$tTable
pt(fixed_effects["beauty_filter_exposure_within", "t-value"], df = fixed_effects["beauty_filter_exposure_within", "DF"])   # within-person, one-sided
2 * pt(-abs(fixed_effects["beauty_filter_exposure_between", "t-value"]), df = fixed_effects["beauty_filter_exposure_between", "DF"])   # between-person, two-sided

# AR(1) autocorrelation
coef(m.h1b.complete_case$modelStruct$corStruct, unconstrained = FALSE)




#######################################################################################################
#######################################################################################################
#######################################################################################################
# H1: Exposure throughout the day predicts end-of-day body satisfaction.
#     More daily time (in seconds) that female adolescents spend watching TikTok videos containing a
#     beauty filter will predict subsequent lower end-of-day body satisfaction. (Confirmatory;
#     primary hypothesis of the second registration.)
# H3: Average content exposure predicts average end-of-day body satisfaction.
#     Female adolescents who spend more daily time (in seconds) – on average – watching TikTok
#     videos tagged as using a beauty filter will report lower average end-of-day body satisfaction.
#     (Confirmatory, secondary.)
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

# Load daily beauty filter exposure on the 924-row grid
daily <- readRDS(file.path(data_dir, "daily_exposure_evening_beauty_filter.rds"))
daily$id <- as.character(daily$id)

# Body satisfaction under its original m-Path name: SSES item 1 ("I felt satisfied with the way my body looks"),
# evening prompt; the participant-day dataset stores it as sses_pm
daily <- rename(daily, SSES_1_multipleChoice_index = sses_pm)

# The second registration's imputation model contains both metadata measures of the same day
# (OSF chbxw, "Missing data": "+ seconds of beauty filter content consumed on that day + seconds
# of beauty labeled content consumed on that day"): add beauty content exposure from its own file
beauty_content_seconds_file <- readRDS(file.path(data_dir, "daily_exposure_evening_beauty_content_label.rds"))
beauty_content_seconds_file$id <- as.character(beauty_content_seconds_file$id)
daily <- left_join(daily, beauty_content_seconds_file %>% select(id, date, beauty_content_seconds), by = c("id", "date"))
stopifnot(nrow(daily) == 924)

# Load person-level baseline covariates
covariates <- readRDS(file.path(data_dir, "participant_level_covariates.rds"))
covariates$id <- as.character(covariates$id)

# Check: one row per participant; SDQ, O2S3, IUS-C and age complete (they enter the
# imputation model as predictors); household income incomplete (imputed at level 2, see S5)
stopifnot(!any(duplicated(covariates$id)))
colSums(is.na(covariates[, c("SDQ_total", "O2S3_total", "IUSC_total", "age", "household_income")]))
stopifnot(all(complete.cases(covariates[, c("SDQ_total", "O2S3_total", "IUSC_total", "age")])))


### Preprocessing

# Lagged EMA covariates (previous occasion) entering the imputation model, Methods eq. (1).
# Written out explicitly so the list can be checked against the grid builder
# (01_preprocessing/04_build_participant_day_dataset_evening.R, the "prompts" block)
lag_cols <- grep("^lag_", names(daily), value = TRUE)
lag_cols <- setdiff(lag_cols, c("lag_occasion_time", "lag_prompt_type"))
lag_cols <- lag_cols[sapply(daily[lag_cols], is.numeric)]
lag_cols

# Drop lagged covariates without variance (would make the covariance matrix singular)
lag_sd      <- sapply(daily[lag_cols], sd, na.rm = TRUE)
lag_cols <- lag_cols[is.finite(lag_sd) & lag_sd > 1e-8]
lag_cols

# Analysis data set: outcome, beauty filter exposure and beauty content exposure (both in minutes), lagged EMA
# covariates, baseline covariates
mi_data <- daily %>%
          select(id, date, day_index, SSES_1_multipleChoice_index, beauty_filter_seconds, beauty_content_seconds, all_of(lag_cols)) %>%
          left_join(covariates %>% select(id, SDQ_total, O2S3_total, IUSC_total, age, household_income),
                    by = "id") %>%
          mutate(beauty_filter_seconds = beauty_filter_seconds / 60,
                 beauty_content_seconds = beauty_content_seconds / 60,
                 id = as.factor(id)) %>%
          arrange(id, date) %>% as.data.frame()

# Missingness overview (expect 170 PM SSES, 16 beauty filter exposure, 16 beauty content exposure, 28 household income, as in the thin, sexualised female bodies blocks)
colSums(is.na(mi_data))


### Imputation model as registered in the second registration (OSF chbxw, "Missing data")

# Level 1 targets: PM body satisfaction, beauty filter exposure and beauty content exposure of the same day,
#                  lagged EMA covariates
# Level 1 predictors: complete baseline covariates (SDQ, O2S3, IUS-C, age)
f.level1 <- as.formula(paste(paste(c("SSES_1_multipleChoice_index", "beauty_filter_seconds", "beauty_content_seconds", lag_cols), collapse = " + "),
                             "~ 1 + SDQ_total + O2S3_total + IUSC_total + age + (1 | id)"))
# Level 2 target: household income
f.level2 <- household_income ~ 1 + SDQ_total + O2S3_total + IUSC_total + age

# Print the assembled formula and check it against the registered model
f.level1
f.level2


### Imputation

# 10 independent chains x 3 draws = m = 30 imputed data sets (registered m = 30),
# burn-in 20,000, draws 200 iterations apart, seed as registered
n.chains <- 10 ; draws.per.chain <- 3 ; n.burn <- 20000 ; n.iter <- 200 ; seed <- 20260731

# The chains are saved and REUSED: if the file below exists, the imputations are loaded from
# it and only the models are refitted (minutes instead of an hour). The saved chains must
# come from exactly this data set and these settings -- otherwise the script stops. To
# impute afresh, delete the file. The same file feeds 04_imputation_trace_plots.R.
chains_file <- file.path(data_dir, "H1b_H3b_MI_chains_beauty_filter.rds")
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
              mutate(beauty_filter_exposure_between = mean(beauty_filter_exposure),
                     beauty_filter_exposure_within  = beauty_filter_exposure - beauty_filter_exposure_between) %>%
              ungroup() %>% as.data.frame())


### Registered model on each imputed data set (identical to the complete-case model above)

# nlminb first (as in the complete-case model). When the random-slope variance is close to zero,
# nlminb can stop with "singular convergence" on some imputed data sets; then ALL 30 are refitted
# with optim -- the same model, only the optimiser differs (record in Table S5). Never pool a
# subset and never mix optimisers across the 30 fits. If optim fails as well, the script stops.
models <- lapply(datasets, function(d)
            tryCatch(lme(SSES_1_multipleChoice_index ~ beauty_filter_exposure_within + beauty_filter_exposure_between,
                         data        = d,
                         random      = ~ beauty_filter_exposure_within | id,
                         correlation = corAR1(form = ~ day_index | id),
                         control     = lmeControl(maxIter = 500, msMaxIter = 500, msMaxEval = 2000,
                                                  niterEM = 50, opt = "nlminb"),
                         na.action   = na.fail),
                     error = function(e) NULL))
n_failed_nlminb <- sum(sapply(models, is.null))
cat("fits that failed with nlminb:", n_failed_nlminb, "of", length(datasets), "\n")
if (n_failed_nlminb > 0) {
  cat("refitting ALL", length(datasets), "imputed data sets with optim (same model; record the optimiser in Table S5)\n")
  models <- lapply(datasets, function(d)
              lme(SSES_1_multipleChoice_index ~ beauty_filter_exposure_within + beauty_filter_exposure_between,
                  data        = d,
                  random      = ~ beauty_filter_exposure_within | id,
                  correlation = corAR1(form = ~ day_index | id),
                  control     = lmeControl(maxIter = 500, msMaxIter = 500, msMaxEval = 2000,
                                           niterEM = 50, opt = "optim"),
                  na.action   = na.fail))
}
stopifnot(!any(sapply(models, is.null)), length(models) == length(datasets))


### Pooling (Rubin's rules)

pooled <- testEstimates(models, extra.pars = TRUE)
pooled
confint(pooled)

# p-values as preregistered (second registration, OSF chbxw, "Inference criteria"):
#   within-person  ONE-sided (directional hypothesis, beta < 0)
#   between-person TWO-sided (registered as non-directional in the inference criteria,
#                  although the hypothesis text predicts lower body satisfaction)
pooled_estimates <- pooled$estimates
pt(pooled_estimates["beauty_filter_exposure_within",  "t.value"], df = pooled_estimates["beauty_filter_exposure_within",  "df"])   # within-person, one-sided
2 * pt(-abs(pooled_estimates["beauty_filter_exposure_between", "t.value"]), df = pooled_estimates["beauty_filter_exposure_between", "df"])   # between-person, two-sided

# Fraction of missing information
pooled_estimates[, "FMI"]

# Save pooled result and the 30 fitted models: read by the figure script (to be added)
saveRDS(list(definition = "beauty_filter",
             pooled = pooled, ci = confint(pooled), m = length(datasets), models = models),
        file.path(data_dir, "H1b_H3b_MI_pooled_beauty_filter.rds"))

### Plots for the manuscript: separate figure script (to be added), reads the saved models




#######################################################################################################
#######################################################################################################
#######################################################################################################
# H1: Exposure throughout the day predicts end-of-day body satisfaction.
#     More daily time (in seconds) that female adolescents spend viewing TikTok videos labeled as
#     beauty content will predict subsequent lower end-of-day body satisfaction. (Confirmatory.)
# H3: Average content exposure predicts average end-of-day body satisfaction.
#     Female adolescents who spend more daily time (in seconds) – on average – watching TikTok
#     videos containing beauty content labels will report lower average end-of-day body
#     satisfaction. (Confirmatory, secondary.)
#
# BEAUTY CONTENT -- COMPLETE CASE
# (preregistered sensitivity analysis of the second registration)

# Clear workspace
rm(list = ls()); data_dir <- "/Volumes/LaCie/0_IdealBodies_Final/1_Data/Processed_data"

# Load daily beauty content exposure on the 924-row grid (66 participants x 14 days)
daily <- readRDS(file.path(data_dir, "daily_exposure_evening_beauty_content_label.rds"))
daily$id <- as.character(daily$id)

# Body satisfaction under its original m-Path name: SSES item 1 ("I felt satisfied with the way my body looks"),
# evening prompt; the participant-day dataset stores it as sses_pm
daily <- rename(daily, SSES_1_multipleChoice_index = sses_pm)

daily    <- daily[order(daily$id, daily$date), ]

# Check the grid
stopifnot(nrow(daily) == 924, length(unique(daily$id)) == 66)

# Complete case: keep days with an evening body satisfaction rating AND derivable beauty content exposure
# (days outside donation coverage have NA beauty content exposure; non-use days have beauty content exposure 0, as registered)
complete_case <- subset(daily, !is.na(SSES_1_multipleChoice_index) & !is.na(beauty_content_seconds))
nrow(complete_case)

# Rescale beauty content exposure from seconds to minutes (numerical stability; betas are per minute)
complete_case$beauty_content_exposure <- complete_case$beauty_content_seconds / 60

# Person-mean centering: between = person mean, within = daily deviation from it
complete_case <- complete_case %>%
      group_by(id) %>%
      mutate(beauty_content_exposure_between = mean(beauty_content_exposure),
             beauty_content_exposure_within  = beauty_content_exposure - beauty_content_exposure_between) %>%
      ungroup() %>% as.data.frame()


###########################################################
### Registered model (OSF chbxw)

# Random intercept and random slope for within-person beauty content exposure,
# AR(1) residual correlation across study days within participant.
# Registered code: corAR1(form = ~ 1 | id); implemented over day_index so that gaps between
# observed days are respected in the complete-case frame (identical on the full grid) -- S8
m.h1c.complete_case <- lme(SSES_1_multipleChoice_index ~ beauty_content_exposure_within + beauty_content_exposure_between,
               data        = complete_case,
               random      = ~ beauty_content_exposure_within | id,
               correlation = corAR1(form = ~ day_index | id),
               control     = lmeControl(maxIter = 500, msMaxIter = 500, msMaxEval = 2000,
                                        niterEM = 50, opt = "nlminb"),
               na.action   = na.fail)

summary(m.h1c.complete_case)
intervals(m.h1c.complete_case, which = "fixed")

# p-values as preregistered (second registration, OSF chbxw, "Inference criteria"):
#   within-person  ONE-sided (directional hypothesis, beta < 0)
#   between-person TWO-sided (registered as non-directional in the inference criteria,
#                  although the hypothesis text predicts lower body satisfaction)
fixed_effects <- summary(m.h1c.complete_case)$tTable
pt(fixed_effects["beauty_content_exposure_within", "t-value"], df = fixed_effects["beauty_content_exposure_within", "DF"])   # within-person, one-sided
2 * pt(-abs(fixed_effects["beauty_content_exposure_between", "t-value"]), df = fixed_effects["beauty_content_exposure_between", "DF"])   # between-person, two-sided

# AR(1) autocorrelation
coef(m.h1c.complete_case$modelStruct$corStruct, unconstrained = FALSE)




#######################################################################################################
#######################################################################################################
#######################################################################################################
# H1: Exposure throughout the day predicts end-of-day body satisfaction.
#     More daily time (in seconds) that female adolescents spend viewing TikTok videos labeled as
#     beauty content will predict subsequent lower end-of-day body satisfaction. (Confirmatory.)
# H3: Average content exposure predicts average end-of-day body satisfaction.
#     Female adolescents who spend more daily time (in seconds) – on average – watching TikTok
#     videos containing beauty content labels will report lower average end-of-day body
#     satisfaction. (Confirmatory, secondary.)
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

# Load daily beauty content exposure on the 924-row grid
daily <- readRDS(file.path(data_dir, "daily_exposure_evening_beauty_content_label.rds"))
daily$id <- as.character(daily$id)

# Body satisfaction under its original m-Path name: SSES item 1 ("I felt satisfied with the way my body looks"),
# evening prompt; the participant-day dataset stores it as sses_pm
daily <- rename(daily, SSES_1_multipleChoice_index = sses_pm)

# The second registration's imputation model contains both metadata measures of the same day
# (OSF chbxw, "Missing data": "+ seconds of beauty filter content consumed on that day + seconds
# of beauty labeled content consumed on that day"): add beauty filter exposure from its own file
beauty_filter_seconds_file <- readRDS(file.path(data_dir, "daily_exposure_evening_beauty_filter.rds"))
beauty_filter_seconds_file$id <- as.character(beauty_filter_seconds_file$id)
daily <- left_join(daily, beauty_filter_seconds_file %>% select(id, date, beauty_filter_seconds), by = c("id", "date"))
stopifnot(nrow(daily) == 924)

# Load person-level baseline covariates
covariates <- readRDS(file.path(data_dir, "participant_level_covariates.rds"))
covariates$id <- as.character(covariates$id)

# Check: one row per participant; SDQ, O2S3, IUS-C and age complete (they enter the
# imputation model as predictors); household income incomplete (imputed at level 2, see S5)
stopifnot(!any(duplicated(covariates$id)))
colSums(is.na(covariates[, c("SDQ_total", "O2S3_total", "IUSC_total", "age", "household_income")]))
stopifnot(all(complete.cases(covariates[, c("SDQ_total", "O2S3_total", "IUSC_total", "age")])))


### Preprocessing

# Lagged EMA covariates (previous occasion) entering the imputation model, Methods eq. (1).
# Written out explicitly so the list can be checked against the grid builder
# (01_preprocessing/04_build_participant_day_dataset_evening.R, the "prompts" block)
lag_cols <- grep("^lag_", names(daily), value = TRUE)
lag_cols <- setdiff(lag_cols, c("lag_occasion_time", "lag_prompt_type"))
lag_cols <- lag_cols[sapply(daily[lag_cols], is.numeric)]
lag_cols

# Drop lagged covariates without variance (would make the covariance matrix singular)
lag_sd      <- sapply(daily[lag_cols], sd, na.rm = TRUE)
lag_cols <- lag_cols[is.finite(lag_sd) & lag_sd > 1e-8]
lag_cols

# Analysis data set: outcome, beauty content exposure and beauty filter exposure (both in minutes), lagged EMA
# covariates, baseline covariates
mi_data <- daily %>%
          select(id, date, day_index, SSES_1_multipleChoice_index, beauty_content_seconds, beauty_filter_seconds, all_of(lag_cols)) %>%
          left_join(covariates %>% select(id, SDQ_total, O2S3_total, IUSC_total, age, household_income),
                    by = "id") %>%
          mutate(beauty_content_seconds = beauty_content_seconds / 60,
                 beauty_filter_seconds = beauty_filter_seconds / 60,
                 id = as.factor(id)) %>%
          arrange(id, date) %>% as.data.frame()

# Missingness overview (expect 170 PM SSES, 16 beauty content exposure, 16 beauty filter exposure, 28 household income, as in the thin, sexualised female bodies blocks)
colSums(is.na(mi_data))


### Imputation model as registered in the second registration (OSF chbxw, "Missing data")

# Level 1 targets: PM body satisfaction, beauty content exposure and beauty filter exposure of the same day,
#                  lagged EMA covariates
# Level 1 predictors: complete baseline covariates (SDQ, O2S3, IUS-C, age)
f.level1 <- as.formula(paste(paste(c("SSES_1_multipleChoice_index", "beauty_content_seconds", "beauty_filter_seconds", lag_cols), collapse = " + "),
                             "~ 1 + SDQ_total + O2S3_total + IUSC_total + age + (1 | id)"))
# Level 2 target: household income
f.level2 <- household_income ~ 1 + SDQ_total + O2S3_total + IUSC_total + age

# Print the assembled formula and check it against the registered model
f.level1
f.level2


### Imputation

# 10 independent chains x 3 draws = m = 30 imputed data sets (registered m = 30),
# burn-in 20,000, draws 200 iterations apart, seed as registered
n.chains <- 10 ; draws.per.chain <- 3 ; n.burn <- 20000 ; n.iter <- 200 ; seed <- 20260731

# The chains are saved and REUSED: if the file below exists, the imputations are loaded from
# it and only the models are refitted (minutes instead of an hour). The saved chains must
# come from exactly this data set and these settings -- otherwise the script stops. To
# impute afresh, delete the file. The same file feeds 04_imputation_trace_plots.R.
chains_file <- file.path(data_dir, "H1c_H3c_MI_chains_beauty_content_label.rds")
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
              mutate(beauty_content_exposure_between = mean(beauty_content_exposure),
                     beauty_content_exposure_within  = beauty_content_exposure - beauty_content_exposure_between) %>%
              ungroup() %>% as.data.frame())


### Registered model on each imputed data set (identical to the complete-case model above)

# nlminb first (as in the complete-case model). When the random-slope variance is close to zero,
# nlminb can stop with "singular convergence" on some imputed data sets; then ALL 30 are refitted
# with optim -- the same model, only the optimiser differs (record in Table S5). Never pool a
# subset and never mix optimisers across the 30 fits. If optim fails as well, the script stops.
models <- lapply(datasets, function(d)
            tryCatch(lme(SSES_1_multipleChoice_index ~ beauty_content_exposure_within + beauty_content_exposure_between,
                         data        = d,
                         random      = ~ beauty_content_exposure_within | id,
                         correlation = corAR1(form = ~ day_index | id),
                         control     = lmeControl(maxIter = 500, msMaxIter = 500, msMaxEval = 2000,
                                                  niterEM = 50, opt = "nlminb"),
                         na.action   = na.fail),
                     error = function(e) NULL))
n_failed_nlminb <- sum(sapply(models, is.null))
cat("fits that failed with nlminb:", n_failed_nlminb, "of", length(datasets), "\n")
if (n_failed_nlminb > 0) {
  cat("refitting ALL", length(datasets), "imputed data sets with optim (same model; record the optimiser in Table S5)\n")
  models <- lapply(datasets, function(d)
              lme(SSES_1_multipleChoice_index ~ beauty_content_exposure_within + beauty_content_exposure_between,
                  data        = d,
                  random      = ~ beauty_content_exposure_within | id,
                  correlation = corAR1(form = ~ day_index | id),
                  control     = lmeControl(maxIter = 500, msMaxIter = 500, msMaxEval = 2000,
                                           niterEM = 50, opt = "optim"),
                  na.action   = na.fail))
}
stopifnot(!any(sapply(models, is.null)), length(models) == length(datasets))


### Pooling (Rubin's rules)

pooled <- testEstimates(models, extra.pars = TRUE)
pooled
confint(pooled)

# p-values as preregistered (second registration, OSF chbxw, "Inference criteria"):
#   within-person  ONE-sided (directional hypothesis, beta < 0)
#   between-person TWO-sided (registered as non-directional in the inference criteria,
#                  although the hypothesis text predicts lower body satisfaction)
pooled_estimates <- pooled$estimates
pt(pooled_estimates["beauty_content_exposure_within",  "t.value"], df = pooled_estimates["beauty_content_exposure_within",  "df"])   # within-person, one-sided
2 * pt(-abs(pooled_estimates["beauty_content_exposure_between", "t.value"]), df = pooled_estimates["beauty_content_exposure_between", "df"])   # between-person, two-sided

# Fraction of missing information
pooled_estimates[, "FMI"]

# Save pooled result and the 30 fitted models: read by the figure script (to be added)
saveRDS(list(definition = "beauty_content_label",
             pooled = pooled, ci = confint(pooled), m = length(datasets), models = models),
        file.path(data_dir, "H1c_H3c_MI_pooled_beauty_content_label.rds"))

### Plots for the manuscript: separate figure script (to be added), reads the saved models
