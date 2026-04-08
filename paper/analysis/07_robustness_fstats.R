###############################################################################
## 07_robustness_fstats.R
##
## Two additions to the analysis pipeline:
##   Task 1: Extract first-stage F-statistics from all 2SLS models
##   Task 2: SUTVA contamination robustness check (exclude control MCCs
##           where a milk analyzer is actually in use)
##
## Must be run with working directory set to paper/analysis/.
## Requires 01_prep_main.R and 02_analysis_main.R to have been run first
## (prepped data and Anderson indices must exist in paper/results/).
##
## Outputs (saved to paper/results/):
##   first_stage_F.rds            -- first-stage F-stats for all 2SLS models
##   robustness_contamination.rds -- ITT re-estimated dropping contaminated MCCs
###############################################################################

source("00_utilities.R")

library(clubSandwich)
library(andersonTools)
library(car)
library(AER)

## ---------------------------------------------------------------------------
## Detect project root and load prepped data
## ---------------------------------------------------------------------------
path <- strsplit(getwd(), "/paper/analysis")[[1]]

farmers_end     <- readRDS(paste(path, "paper/results/prepped_farmers.rds", sep = "/"))
farmers_base    <- readRDS(paste(path, "paper/results/prepped_farmers_base.rds", sep = "/"))
MCCs_end        <- readRDS(paste(path, "paper/results/prepped_MCCs.rds", sep = "/"))


###############################################################################
## RECONSTRUCT VARIABLES NEEDED (same as 02_analysis_main.R)
## NOTE: prepped data already has treat (logical), treat_TOT, vid, catch_ID,
##       and baseline outcome columns merged. We only need to construct
##       Anderson indices and demeaned variables.
###############################################################################

## LATE instrument at MCC level (needed for MCC-level regressions)
MCCs_end$treat_TOT <- MCCs_end$machine_in_use %in% 1:2

## Demeaned treatment for orthogonalized models
farmers_end$vid_demeaned   <- farmers_end$vid   - mean(farmers_end$vid, na.rm = TRUE)
farmers_end$treat_demeaned <- farmers_end$treat  - mean(farmers_end$treat, na.rm = TRUE)

## Primary outcomes and Anderson index
outcomes_primary   <- c("improve_index", "check_MA", "avg_sales_p", "gets_q_bonus")
b_outcomes_primary <- c("b_improve_index", "b_check_MA", "b_avg_sales_p", "b_gets_q_bonus")

farmers_end$primary_farmer_index   <- anderson_index(farmers_end[outcomes_primary])$index
farmers_end$b_primary_farmer_index <- anderson_index(farmers_end[b_outcomes_primary])$index

outcomes_primary   <- c(outcomes_primary, "primary_farmer_index")
b_outcomes_primary <- c(b_outcomes_primary, "b_primary_farmer_index")


###############################################################################
## TASK 1: FIRST-STAGE F-STATISTICS FROM ALL 2SLS MODELS
###############################################################################

cat("\n--- Task 1: Extracting first-stage F-statistics ---\n")

first_stage_F <- list()

## -------------------------------------------------------------------------
## 1a. Primary farmer-level LATE (Section C in 02_analysis_main.R)
## -------------------------------------------------------------------------

## C1. Additive 2SLS: outcome ~ treat_TOT + vid + b_outcome | treat + vid + b_outcome
farmer_F_additive <- numeric(length(outcomes_primary))
names(farmer_F_additive) <- outcomes_primary

for (i in seq_along(outcomes_primary)) {
  f_2sls <- as.formula(paste(outcomes_primary[i],
    "~ treat_TOT + vid +", b_outcomes_primary[i],
    "| treat + vid +", b_outcomes_primary[i]))
  ivmod <- ivreg(f_2sls, data = farmers_end)
  diag  <- summary(ivmod, diagnostics = TRUE)$diagnostics
  farmer_F_additive[i] <- diag["Weak instruments", "statistic"]
}

first_stage_F$farmer_LATE_additive <- farmer_F_additive
cat("  Farmer LATE (additive):", round(farmer_F_additive, 2), "\n")

## C2. Orthogonalized 2SLS: outcome ~ treat_TOT*vid_demeaned + b_outcome
##     | treat*vid_demeaned + b_outcome
## Note: interaction creates two endogenous vars, so diagnostics has two
## "Weak instruments" rows. We extract both: treat_TOT and the interaction.
farmer_F_ortho_main <- numeric(length(outcomes_primary))
farmer_F_ortho_interact <- numeric(length(outcomes_primary))
names(farmer_F_ortho_main) <- outcomes_primary
names(farmer_F_ortho_interact) <- outcomes_primary

for (i in seq_along(outcomes_primary)) {
  f_2sls3 <- as.formula(paste(outcomes_primary[i],
    "~ treat_TOT*vid_demeaned +", b_outcomes_primary[i],
    "| treat*vid_demeaned +", b_outcomes_primary[i]))
  ivmod3 <- ivreg(f_2sls3, data = farmers_end)
  diag3  <- summary(ivmod3, diagnostics = TRUE)$diagnostics
  ## Extract F-stats for both endogenous variables
  wi_rows <- grep("^Weak instruments", rownames(diag3))
  farmer_F_ortho_main[i]     <- diag3[wi_rows[1], "statistic"]
  farmer_F_ortho_interact[i] <- diag3[wi_rows[2], "statistic"]
}

first_stage_F$farmer_LATE_ortho_treat_TOT   <- farmer_F_ortho_main
first_stage_F$farmer_LATE_ortho_interaction  <- farmer_F_ortho_interact
cat("  Farmer LATE (orthogonalized, treat_TOT):", round(farmer_F_ortho_main, 2), "\n")
cat("  Farmer LATE (orthogonalized, interaction):", round(farmer_F_ortho_interact, 2), "\n")

## -------------------------------------------------------------------------
## 1b. MCC-level 2SLS (Section E in 02_analysis_main.R)
## -------------------------------------------------------------------------

outcomes_mcc_e   <- c("test_MA_in", "test_MA_out", "price_bought", "avg_sales_p",
                       "gives_q_bonus", "gets_q_bonus")
b_outcomes_mcc_e <- c("b_test_MA_in", "b_test_MA_out", "b_price_bought",
                       "b_avg_sales_p", "b_gives_q_bonus", "b_gets_q_bonus")

MCCs_end$primary_MCC_index   <- anderson_index(MCCs_end[outcomes_mcc_e])$index
MCCs_end$b_primary_MCC_index <- anderson_index(MCCs_end[b_outcomes_mcc_e])$index

outcomes_mcc_e   <- c(outcomes_mcc_e, "primary_MCC_index")
b_outcomes_mcc_e <- c(b_outcomes_mcc_e, "b_primary_MCC_index")

mcc_F <- numeric(length(outcomes_mcc_e))
names(mcc_F) <- outcomes_mcc_e

for (i in seq_along(outcomes_mcc_e)) {
  b_var <- MCCs_end[[b_outcomes_mcc_e[i]]]
  has_baseline <- length(unique(b_var[!is.na(b_var)])) > 1
  if (has_baseline) {
    iv_formula <- as.formula(paste0(
      outcomes_mcc_e[i], " ~ treat_TOT + ", b_outcomes_mcc_e[i],
      " | treat + ", b_outcomes_mcc_e[i]))
  } else {
    iv_formula <- as.formula(paste0(
      outcomes_mcc_e[i], " ~ treat_TOT | treat"))
  }
  iv_mod <- ivreg(iv_formula, data = MCCs_end)
  diag   <- summary(iv_mod, diagnostics = TRUE)$diagnostics
  mcc_F[i] <- diag["Weak instruments", "statistic"]
}

first_stage_F$MCC_LATE <- mcc_F
cat("  MCC LATE:", round(mcc_F, 2), "\n")

## -------------------------------------------------------------------------
## 1c. Milk samples 2SLS (Section F in 02_analysis_main.R)
## -------------------------------------------------------------------------

samples <- read.csv(paste(path, "endline/data/public/samples.csv", sep = "/"))
samples$today <- as.Date(samples$today)

## Calibration fix: Ntungamo samples before Dec 5 are excluded
samples <- samples[
  (samples$today >= as.Date("2024-12-04") &
   (samples$district == "Kazo" | samples$district == "Mbarara")) |
  samples$today >= as.Date("2024-12-05"), ]

samples <- merge(samples, MCCs_end[c("MCC_ID", "treat_TOT")], all.x = TRUE)

sample_outcomes <- c("Fat", "SNF", "Added.Water", "Protein",
                     "Corrected.Lactometer.Reading")
samples$samples_index <- anderson_index(samples[sample_outcomes], revcols = 3)$index
sample_outcomes <- c(sample_outcomes, "samples_index")

samples_F <- numeric(length(sample_outcomes))
names(samples_F) <- sample_outcomes

for (i in seq_along(sample_outcomes)) {
  iv_mod <- ivreg(as.formula(paste0(sample_outcomes[i], " ~ treat_TOT | treat")),
                  data = samples, weights = samples$Qty)
  diag   <- summary(iv_mod, diagnostics = TRUE)$diagnostics
  samples_F[i] <- diag["Weak instruments", "statistic"]
}

first_stage_F$samples_LATE <- samples_F
cat("  Samples LATE:", round(samples_F, 2), "\n")

## Save
saveRDS(first_stage_F,
        file = paste(path, "paper/results/first_stage_F.rds", sep = "/"))
cat("  Saved: paper/results/first_stage_F.rds\n")


###############################################################################
## TASK 2: SUTVA CONTAMINATION ROBUSTNESS CHECK
###############################################################################
## Identify control MCCs where a milk analyzer is actually in use
## (treat == "C" but treat_TOT == TRUE) and re-run main ITT specifications
## excluding these MCCs and their linked farmers.

cat("\n--- Task 2: SUTVA contamination robustness check ---\n")

## Use already-loaded MCCs_end (with treat_TOT reconstructed above)
## and farmers_end (with treat as logical, baseline cols already merged)

## Identify contaminated control MCCs: assigned to control but machine in use
contaminated_mccs <- MCCs_end$MCC_ID[MCCs_end$treat == "C" & MCCs_end$treat_TOT == TRUE]
cat("  Contaminated control MCCs (treat=C, machine in use):",
    length(contaminated_mccs), "\n")
if (length(contaminated_mccs) > 0) {
  cat("  MCC IDs:", paste(contaminated_mccs, collapse = ", "), "\n")
}

## Create clean datasets excluding contaminated MCCs
MCCs_clean    <- MCCs_end[!(MCCs_end$MCC_ID %in% contaminated_mccs), ]
farmers_clean <- farmers_end[!(farmers_end$MCC_ID_linked %in% contaminated_mccs), ]

## Reconstruct Anderson index on clean sample
outcomes_rob   <- c("improve_index", "check_MA", "avg_sales_p", "gets_q_bonus")
b_outcomes_rob <- c("b_improve_index", "b_check_MA", "b_avg_sales_p", "b_gets_q_bonus")

farmers_clean$primary_farmer_index   <- anderson_index(farmers_clean[outcomes_rob])$index
farmers_clean$b_primary_farmer_index <- anderson_index(farmers_clean[b_outcomes_rob])$index

outcomes_rob   <- c(outcomes_rob, "primary_farmer_index")
b_outcomes_rob <- c(b_outcomes_rob, "b_primary_farmer_index")

robustness <- list()
robustness$contaminated_mccs <- contaminated_mccs
robustness$n_contaminated    <- length(contaminated_mccs)
robustness$n_MCCs_original   <- nrow(MCCs_end)
robustness$n_MCCs_clean      <- nrow(MCCs_clean)
robustness$n_farmers_original <- nrow(farmers_end)
robustness$n_farmers_clean    <- nrow(farmers_clean)

## -------------------------------------------------------------------------
## 2a. Table 3: Primary farmer ITT (dropping contaminated control MCCs)
## -------------------------------------------------------------------------
## Same layout as res_farmers: 15 columns
res_farmers_clean <- array(NA, dim = c(length(outcomes_rob), 15),
  dimnames = list(outcomes_rob,
    c("ctrl_mean", "ctrl_sd", "treat_coef", "treat_se", "treat_p",
      "vid_coef", "vid_se", "vid_p", "N",
      "q_treat", "q_vid",
      "pooled_coef", "pooled_se", "pooled_p", "q_pooled")))

## Demeaned vid for orthogonalized models
farmers_clean$vid_demeaned <- farmers_clean$vid - mean(farmers_clean$vid, na.rm = TRUE)

for (i in seq_along(outcomes_rob)) {
  ols <- lm(as.formula(paste(outcomes_rob[i], "~ treat + vid +", b_outcomes_rob[i])),
            data = farmers_clean)
  vcov_cluster <- vcovCR(ols, cluster = farmers_clean$catch_ID, type = "CR2")
  res <- coef_test(ols, vcov_cluster)

  ctrl <- farmers_clean[farmers_clean$vid == FALSE & farmers_clean$treat == FALSE,
                        outcomes_rob[i]]
  res_farmers_clean[i, 1] <- mean(ctrl, na.rm = TRUE)
  res_farmers_clean[i, 2] <- sd(ctrl, na.rm = TRUE)
  res_farmers_clean[i, 3:5] <- c(res[2, 2], res[2, 3], res[2, 7])
  res_farmers_clean[i, 6:8] <- c(res[3, 2], res[3, 3], res[3, 7])
  res_farmers_clean[i, 9]   <- nobs(ols)
}

idx <- 1:(length(outcomes_rob) - 1)
res_farmers_clean[idx, 10] <- anderson_sharp_q(res_farmers_clean[idx, 5])
res_farmers_clean[idx, 11] <- anderson_sharp_q(res_farmers_clean[idx, 8])

## Orthogonalized (pooled treat effect)
for (i in seq_along(outcomes_rob)) {
  ols <- lm(as.formula(paste(outcomes_rob[i], "~ treat*vid_demeaned +", b_outcomes_rob[i])),
            data = farmers_clean)
  vcov_cluster <- vcovCR(ols, cluster = farmers_clean$catch_ID, type = "CR2")
  res <- coef_test(ols, vcov_cluster)
  res_farmers_clean[i, 12:14] <- c(res[2, 2], res[2, 3], res[2, 7])
}

res_farmers_clean[idx, 15] <- anderson_sharp_q(res_farmers_clean[idx, 14])
res_farmers_clean <- round(res_farmers_clean, digits = 3)

robustness$res_farmers <- res_farmers_clean

## -------------------------------------------------------------------------
## 2b. Table 4: MCC-level ITT (dropping contaminated control MCCs)
## -------------------------------------------------------------------------

outcomes_mcc_rob   <- c("test_MA_in", "test_MA_out", "price_bought", "avg_sales_p",
                         "gives_q_bonus", "gets_q_bonus")
b_outcomes_mcc_rob <- c("b_test_MA_in", "b_test_MA_out", "b_price_bought",
                         "b_avg_sales_p", "b_gives_q_bonus", "b_gets_q_bonus")

MCCs_clean$primary_MCC_index   <- anderson_index(MCCs_clean[outcomes_mcc_rob])$index
MCCs_clean$b_primary_MCC_index <- anderson_index(MCCs_clean[b_outcomes_mcc_rob])$index

outcomes_mcc_rob   <- c(outcomes_mcc_rob, "primary_MCC_index")
b_outcomes_mcc_rob <- c(b_outcomes_mcc_rob, "b_primary_MCC_index")

## MCC result array: 7 outcomes x 7 columns (OLS ITT only)
res_MCCs_clean <- array(NA, dim = c(length(outcomes_mcc_rob), 7),
  dimnames = list(outcomes_mcc_rob,
    c("ctrl_mean", "ctrl_sd", "coef", "se", "p", "N", "q")))

for (i in seq_along(outcomes_mcc_rob)) {
  b_var <- MCCs_clean[[b_outcomes_mcc_rob[i]]]
  has_baseline <- length(unique(b_var[!is.na(b_var)])) > 1
  if (has_baseline) {
    fmla <- paste(outcomes_mcc_rob[i], "~ treat +", b_outcomes_mcc_rob[i])
  } else {
    fmla <- paste(outcomes_mcc_rob[i], "~ treat")
  }
  ols <- lm(as.formula(fmla), data = MCCs_clean)

  y_ctrl <- MCCs_clean[MCCs_clean$treat == "C", outcomes_mcc_rob[i]]
  res_MCCs_clean[i, 1] <- mean(y_ctrl, na.rm = TRUE)
  res_MCCs_clean[i, 2] <- sd(y_ctrl, na.rm = TRUE)

  use_rows <- !is.na(MCCs_clean[[outcomes_mcc_rob[i]]])
  if (has_baseline) use_rows <- use_rows & !is.na(MCCs_clean[[b_outcomes_mcc_rob[i]]])
  vcov_cr2 <- vcovCR(ols,
    cluster = MCCs_clean[use_rows, "catchment_ID"],
    type = "CR2")
  cr2_test <- coef_test(ols, vcov = vcov_cr2)[2, ]
  res_MCCs_clean[i, 3] <- cr2_test[["beta"]]
  res_MCCs_clean[i, 4] <- cr2_test[["SE"]]
  res_MCCs_clean[i, 5] <- cr2_test[["p_Satt"]]
  res_MCCs_clean[i, 6] <- nobs(ols)
}

idx_mcc <- 1:(length(outcomes_mcc_rob) - 1)
res_MCCs_clean[idx_mcc, 7] <- anderson_sharp_q(res_MCCs_clean[idx_mcc, 5])
res_MCCs_clean <- round(res_MCCs_clean, digits = 3)

robustness$res_MCCs <- res_MCCs_clean

## -------------------------------------------------------------------------
## 2c. Table 5: Milk samples ITT (dropping contaminated control MCCs)
## -------------------------------------------------------------------------

samples2 <- read.csv(paste(path, "endline/data/public/samples.csv", sep = "/"))
samples2$today <- as.Date(samples2$today)

## Calibration fix
samples2 <- samples2[
  (samples2$today >= as.Date("2024-12-04") &
   (samples2$district == "Kazo" | samples2$district == "Mbarara")) |
  samples2$today >= as.Date("2024-12-05"), ]

## Drop samples from contaminated MCCs
samples2 <- samples2[!(samples2$MCC_ID %in% contaminated_mccs), ]

sample_outcomes_rob <- c("Fat", "SNF", "Added.Water", "Protein",
                          "Corrected.Lactometer.Reading")
samples2$samples_index <- anderson_index(samples2[sample_outcomes_rob], revcols = 3)$index
sample_outcomes_rob <- c(sample_outcomes_rob, "samples_index")

res_samples_clean <- array(NA, dim = c(length(sample_outcomes_rob), 5),
  dimnames = list(sample_outcomes_rob,
    c("ctrl_mean", "ctrl_sd", "coef", "se", "p")))

for (i in seq_along(sample_outcomes_rob)) {
  ols <- lm(as.formula(paste(sample_outcomes_rob[i], "~ treat")),
            data = samples2, weights = samples2$Qty)

  y_ctrl <- samples2[samples2$treat == "C", sample_outcomes_rob[i]]
  res_samples_clean[i, 1] <- mean(y_ctrl, na.rm = TRUE)
  res_samples_clean[i, 2] <- sd(y_ctrl, na.rm = TRUE)

  vcov_ols <- vcovCR(ols, cluster = samples2$catch_ID, type = "CR2")
  res <- coef_test(ols, vcov_ols)
  res_samples_clean[i, 3:5] <- c(res[2, 2], res[2, 3], res[2, 7])
}

## Add N and q-values
res_samples_full <- array(NA, dim = c(length(sample_outcomes_rob), 7),
  dimnames = list(sample_outcomes_rob,
    c("ctrl_mean", "ctrl_sd", "coef", "se", "p", "N", "q")))
res_samples_full[, 1:5] <- res_samples_clean

for (i in seq_along(sample_outcomes_rob)) {
  ols <- lm(as.formula(paste(sample_outcomes_rob[i], "~ treat")),
            data = samples2, weights = samples2$Qty)
  res_samples_full[i, 6] <- nobs(ols)
}

idx_s <- 1:(length(sample_outcomes_rob) - 1)
res_samples_full[idx_s, 7] <- anderson_sharp_q(res_samples_full[idx_s, 5])
res_samples_full <- round(res_samples_full, digits = 3)

robustness$res_samples <- res_samples_full

## Save
saveRDS(robustness,
        file = paste(path, "paper/results/robustness_contamination.rds", sep = "/"))

cat("  Saved: paper/results/robustness_contamination.rds\n")
cat("\n07_robustness_fstats.R completed successfully.\n")
