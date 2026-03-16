###############################################################################
## 06_randomization_inference.R
##
## Fisher Randomization Inference P-Values + Omnibus Count Test
##
## Computes RI p-values by permuting treatment assignment according to the
## experimental design (5,000 permutations). The test statistic is the OLS
## coefficient (NOT the t-statistic), avoiding the need to compute CR2 SEs
## 5,000 times.
##
## Applied to:
##   (A) Farmer-level primary outcomes (ANCOVA: outcome ~ treat + vid + baseline)
##   (B) MCC-level primary outcomes (ANCOVA: outcome ~ treat + baseline)
##   (C) Milk sample outcomes (WLS: outcome ~ treat, weighted by Qty)
##
## Includes an omnibus count test (EGAP approach) for each outcome family.
##
## Must be run with working directory set to paper/analysis/.
## Sources 00_utilities.R and loads prepped data from paper/results/.
##
## Output: paper/results/randomization_inference.rds
###############################################################################

source("00_utilities.R")

library(andersonTools)

## ---------------------------------------------------------------------------
## Detect project root and load prepped data
## ---------------------------------------------------------------------------
path <- strsplit(getwd(), "/paper/analysis")[[1]]

farmers_end  <- readRDS(paste(path, "paper/results/prepped_farmers.rds", sep = "/"))
farmers_base <- readRDS(paste(path, "paper/results/prepped_farmers_base.rds", sep = "/"))
MCCs_end     <- readRDS(paste(path, "paper/results/prepped_MCCs.rds", sep = "/"))
MCCs_base    <- readRDS(paste(path, "paper/results/prepped_MCCs_base.rds", sep = "/"))

## ---------------------------------------------------------------------------
## RI parameters
## ---------------------------------------------------------------------------
set.seed(12345)
n_perms <- 5000


# ===========================================================================
# HELPER: Omnibus count test (EGAP approach)
# ===========================================================================
# Under the sharp null of no treatment effect, what is the probability of
# observing as many individually significant results as we actually observe?
#
# For each permutation:
#   1. Compute the individual RI p-value for each outcome
#   2. Count how many have p < alpha
# Compare the observed count to this permutation distribution of counts.

omnibus_count_test <- function(obs_coefs, perm_matrix, alpha = 0.05) {
  K <- length(obs_coefs)
  n_perms <- nrow(perm_matrix)

  ## For each outcome, compute its individual RI p-value
  obs_p <- sapply(1:K, function(k) {
    mean(abs(perm_matrix[, k]) >= abs(obs_coefs[k]))
  })
  obs_count <- sum(obs_p < alpha)

  ## For each permutation, count how many outcomes would appear "significant"
  ## if that permutation were the "observed" data
  perm_counts <- rep(NA, n_perms)
  abs_perm <- abs(perm_matrix)

  for (p in 1:n_perms) {
    p_values_this_perm <- sapply(1:K, function(k) {
      mean(abs_perm[, k] >= abs_perm[p, k])
    })
    perm_counts[p] <- sum(p_values_this_perm < alpha)
  }

  omnibus_p <- mean(perm_counts >= obs_count)

  return(list(obs_count = obs_count, obs_indiv_p = obs_p,
              omnibus_p = omnibus_p, perm_counts = perm_counts))
}


# ===========================================================================
# SECTION 1: FARMER-LEVEL RANDOMIZATION INFERENCE
# ===========================================================================
# ANCOVA: outcome ~ treat + vid + baseline_outcome
# T1 (treat): MCC-level permutation
# T2 (vid): farmer-level within-MCC permutation

cat("\n===========================================================\n")
cat("  Fisher Randomization Inference: Farmer-Level Outcomes\n")
cat("===========================================================\n\n")

## Primary outcomes
farmer_outcomes   <- c("improve_index", "check_MA", "avg_sales_p", "gets_q_bonus",
                        "primary_farmer_index")
farmer_b_outcomes <- c("b_improve_index", "b_check_MA", "b_avg_sales_p", "b_gets_q_bonus",
                        "b_primary_farmer_index")

## Construct Anderson index if not already present
if (!"primary_farmer_index" %in% names(farmers_end)) {
  farmers_end$primary_farmer_index   <- anderson_index(
    farmers_end[c("improve_index", "check_MA", "avg_sales_p", "gets_q_bonus")])$index
  farmers_end$b_primary_farmer_index <- anderson_index(
    farmers_end[c("b_improve_index", "b_check_MA", "b_avg_sales_p", "b_gets_q_bonus")])$index
}

## Build MCC-level treatment mapping for permutation
mcc_treat_map <- unique(farmers_end[, c("MCC_ID_linked", "treat")])
names(mcc_treat_map) <- c("MCC_ID_linked", "treat_orig")
n_treat_mccs  <- sum(mcc_treat_map$treat_orig == TRUE)
n_total_mccs  <- nrow(mcc_treat_map)

cat(sprintf("MCC-level permutation: %d treated out of %d MCCs\n", n_treat_mccs, n_total_mccs))

## Build within-MCC vid counts for permutation
vid_counts <- tapply(farmers_end$vid, farmers_end$MCC_ID_linked, sum)

cat(sprintf("Farmer-level permutation: vid within %d MCCs\n", length(vid_counts)))
cat(sprintf("Number of permutations: %d\n\n", n_perms))

## Storage for permuted coefficients
ri_farmer_treat <- matrix(NA, nrow = n_perms, ncol = length(farmer_outcomes))
ri_farmer_vid   <- matrix(NA, nrow = n_perms, ncol = length(farmer_outcomes))

## Observed coefficients
obs_farmer_treat <- numeric(length(farmer_outcomes))
obs_farmer_vid   <- numeric(length(farmer_outcomes))
obs_farmer_N     <- integer(length(farmer_outcomes))

for (i in seq_along(farmer_outcomes)) {
  outcome   <- farmer_outcomes[i]
  b_outcome <- farmer_b_outcomes[i]
  fml <- as.formula(paste(outcome, "~ treat + vid +", b_outcome))
  ols <- lm(fml, data = farmers_end)
  obs_farmer_treat[i] <- coef(ols)["treatTRUE"]
  obs_farmer_vid[i]   <- coef(ols)["vidTRUE"]
  obs_farmer_N[i]     <- nobs(ols)
}

cat("Observed coefficients (treat):\n")
for (i in seq_along(farmer_outcomes)) {
  cat(sprintf("  %-25s  coef = %9.5f  (N = %d)\n",
              farmer_outcomes[i], obs_farmer_treat[i], obs_farmer_N[i]))
}

## Permutation loop
cat("\nRunning farmer-level RI permutations...\n")

for (p in 1:n_perms) {
  if (p %% 1000 == 0) cat(sprintf("  Permutation %d / %d\n", p, n_perms))

  ## (a) Permute treat at MCC level
  perm_treat_idx <- sample(n_total_mccs, n_treat_mccs, replace = FALSE)
  perm_treat_map <- mcc_treat_map
  perm_treat_map$treat_perm <- FALSE
  perm_treat_map$treat_perm[perm_treat_idx] <- TRUE

  farmers_end$treat_perm <- perm_treat_map$treat_perm[
    match(farmers_end$MCC_ID_linked, perm_treat_map$MCC_ID_linked)]

  ## (b) Permute vid within each MCC
  farmers_end$vid_perm <- FALSE
  for (mcc in names(vid_counts)) {
    idx <- which(farmers_end$MCC_ID_linked == mcc)
    n_vid <- vid_counts[mcc]
    if (n_vid > 0 && n_vid < length(idx)) {
      vid_idx <- sample(idx, n_vid, replace = FALSE)
      farmers_end$vid_perm[vid_idx] <- TRUE
    } else if (n_vid == length(idx)) {
      farmers_end$vid_perm[idx] <- TRUE
    }
  }

  ## (c) Estimate ANCOVA with permuted treatments
  for (i in seq_along(farmer_outcomes)) {
    outcome   <- farmer_outcomes[i]
    b_outcome <- farmer_b_outcomes[i]
    fml <- as.formula(paste(outcome, "~ treat_perm + vid_perm +", b_outcome))
    ols <- lm(fml, data = farmers_end)
    coefs <- coef(ols)
    ri_farmer_treat[p, i] <- coefs["treat_permTRUE"]
    ri_farmer_vid[p, i]   <- coefs["vid_permTRUE"]
  }
}

## Compute two-sided RI p-values
ri_p_farmer_treat <- numeric(length(farmer_outcomes))
ri_p_farmer_vid   <- numeric(length(farmer_outcomes))

for (i in seq_along(farmer_outcomes)) {
  ri_p_farmer_treat[i] <- mean(abs(ri_farmer_treat[, i]) >= abs(obs_farmer_treat[i]),
                                na.rm = TRUE)
  ri_p_farmer_vid[i]   <- mean(abs(ri_farmer_vid[, i]) >= abs(obs_farmer_vid[i]),
                                na.rm = TRUE)
}

cat("\nFarmer-level RI p-values (treat):\n")
for (i in seq_along(farmer_outcomes)) {
  cat(sprintf("  %-25s  RI p = %7.4f\n", farmer_outcomes[i], ri_p_farmer_treat[i]))
}


# ===========================================================================
# SECTION 2: MCC-LEVEL RANDOMIZATION INFERENCE
# ===========================================================================
# ANCOVA: outcome ~ treat + baseline_outcome
# Only T1 (treat) is relevant. Permute which MCCs are treated.
# Note: MCCs_end$treat is character "T"/"C". Coefficient name is "treatT".

cat("\n===========================================================\n")
cat("  Fisher Randomization Inference: MCC-Level Outcomes\n")
cat("===========================================================\n\n")

MCC_outcomes   <- c("test_MA_in", "test_MA_out", "price_bought", "avg_sales_p",
                     "gives_q_bonus", "gets_q_bonus")
MCC_b_outcomes <- c("b_test_MA_in", "b_test_MA_out", "b_price_bought", "b_avg_sales_p",
                     "b_gives_q_bonus", "b_gets_q_bonus")

## Construct Anderson index if not present
if (!"primary_MCC_index" %in% names(MCCs_end)) {
  MCCs_end$primary_MCC_index   <- anderson_index(MCCs_end[MCC_outcomes])$index
  MCCs_end$b_primary_MCC_index <- anderson_index(MCCs_end[MCC_b_outcomes])$index
}

MCC_outcomes   <- c(MCC_outcomes, "primary_MCC_index")
MCC_b_outcomes <- c(MCC_b_outcomes, "b_primary_MCC_index")

n_treat_mcc_level <- sum(MCCs_end$treat == "T")
n_total_mcc_level <- nrow(MCCs_end)

cat(sprintf("MCC-level permutation: %d treated out of %d MCCs\n",
            n_treat_mcc_level, n_total_mcc_level))
cat(sprintf("Number of permutations: %d\n\n", n_perms))

## Storage
ri_mcc <- matrix(NA, nrow = n_perms, ncol = length(MCC_outcomes))

## Observed coefficients
obs_mcc   <- numeric(length(MCC_outcomes))
obs_mcc_N <- integer(length(MCC_outcomes))

for (i in seq_along(MCC_outcomes)) {
  outcome   <- MCC_outcomes[i]
  b_outcome <- MCC_b_outcomes[i]
  fml <- as.formula(paste(outcome, "~ treat +", b_outcome))
  ols <- lm(fml, data = MCCs_end)
  obs_mcc[i]   <- coef(ols)["treatT"]
  obs_mcc_N[i] <- nobs(ols)
}

cat("Observed coefficients (treat):\n")
for (i in seq_along(MCC_outcomes)) {
  cat(sprintf("  %-25s  coef = %9.5f  (N = %d)\n",
              MCC_outcomes[i], obs_mcc[i], obs_mcc_N[i]))
}

## Permutation loop
cat("\nRunning MCC-level RI permutations...\n")

for (p in 1:n_perms) {
  if (p %% 1000 == 0) cat(sprintf("  Permutation %d / %d\n", p, n_perms))

  perm_idx <- sample(n_total_mcc_level, n_treat_mcc_level, replace = FALSE)
  MCCs_end$treat_perm <- "C"
  MCCs_end$treat_perm[perm_idx] <- "T"

  for (i in seq_along(MCC_outcomes)) {
    outcome   <- MCC_outcomes[i]
    b_outcome <- MCC_b_outcomes[i]
    fml <- as.formula(paste(outcome, "~ treat_perm +", b_outcome))
    ols <- lm(fml, data = MCCs_end)
    ri_mcc[p, i] <- coef(ols)["treat_permT"]
  }
}

## Compute two-sided RI p-values
ri_p_mcc <- numeric(length(MCC_outcomes))
for (i in seq_along(MCC_outcomes)) {
  ri_p_mcc[i] <- mean(abs(ri_mcc[, i]) >= abs(obs_mcc[i]), na.rm = TRUE)
}

cat("\nMCC-level RI p-values:\n")
for (i in seq_along(MCC_outcomes)) {
  cat(sprintf("  %-25s  RI p = %7.4f\n", MCC_outcomes[i], ri_p_mcc[i]))
}


# ===========================================================================
# SECTION 3: MILK SAMPLE RANDOMIZATION INFERENCE
# ===========================================================================
# WLS: outcome ~ treat (weighted by Qty)
# Treatment is at MCC level. Permute which MCCs are treated.
# Note: samples$treat is character "T"/"C". Coefficient name is "treatT".

cat("\n===========================================================\n")
cat("  Fisher Randomization Inference: Milk Sample Outcomes\n")
cat("===========================================================\n\n")

## Load samples from raw CSV (not in prepped data)
samples <- read.csv(paste(path, "endline/data/public/samples.csv", sep = "/"))
samples$today <- as.Date(samples$today)

## Calibration fix: exclude Ntungamo samples before Dec 5
samples <- samples[
  (samples$today >= as.Date("2024-12-04") &
   (samples$district == "Kazo" | samples$district == "Mbarara")) |
  samples$today >= as.Date("2024-12-05"), ]

sample_outcomes <- c("Fat", "SNF", "Added.Water", "Protein",
                      "Corrected.Lactometer.Reading")
samples$samples_index <- anderson_index(samples[sample_outcomes], revcols = 3)$index
sample_outcomes <- c(sample_outcomes, "samples_index")

## Build MCC-level treatment mapping for samples
sample_mcc_map <- unique(samples[, c("MCC_ID", "treat")])
names(sample_mcc_map) <- c("MCC_ID", "treat_orig")
n_treat_sample_mccs <- sum(sample_mcc_map$treat_orig == "T")
n_total_sample_mccs <- nrow(sample_mcc_map)

cat(sprintf("MCC-level permutation (samples): %d treated out of %d MCCs\n",
            n_treat_sample_mccs, n_total_sample_mccs))
cat(sprintf("Number of permutations: %d\n\n", n_perms))

## Storage
ri_samples <- matrix(NA, nrow = n_perms, ncol = length(sample_outcomes))

## Observed coefficients
obs_samples   <- numeric(length(sample_outcomes))
obs_samples_N <- integer(length(sample_outcomes))

for (i in seq_along(sample_outcomes)) {
  outcome <- sample_outcomes[i]
  fml <- as.formula(paste(outcome, "~ treat"))
  ols <- lm(fml, data = samples, weights = samples$Qty)
  obs_samples[i]   <- coef(ols)["treatT"]
  obs_samples_N[i] <- nobs(ols)
}

cat("Observed coefficients (treat):\n")
for (i in seq_along(sample_outcomes)) {
  cat(sprintf("  %-35s  coef = %9.5f  (N = %d)\n",
              sample_outcomes[i], obs_samples[i], obs_samples_N[i]))
}

## Permutation loop
cat("\nRunning sample-level RI permutations...\n")

for (p in 1:n_perms) {
  if (p %% 1000 == 0) cat(sprintf("  Permutation %d / %d\n", p, n_perms))

  perm_idx <- sample(n_total_sample_mccs, n_treat_sample_mccs, replace = FALSE)
  perm_map <- sample_mcc_map
  perm_map$treat_perm <- "C"
  perm_map$treat_perm[perm_idx] <- "T"

  samples$treat_perm <- perm_map$treat_perm[
    match(samples$MCC_ID, perm_map$MCC_ID)]

  for (i in seq_along(sample_outcomes)) {
    outcome <- sample_outcomes[i]
    fml <- as.formula(paste(outcome, "~ treat_perm"))
    ols <- lm(fml, data = samples, weights = samples$Qty)
    ri_samples[p, i] <- coef(ols)["treat_permT"]
  }
}

## Compute two-sided RI p-values
ri_p_samples <- numeric(length(sample_outcomes))
for (i in seq_along(sample_outcomes)) {
  ri_p_samples[i] <- mean(abs(ri_samples[, i]) >= abs(obs_samples[i]), na.rm = TRUE)
}

cat("\nSample-level RI p-values:\n")
for (i in seq_along(sample_outcomes)) {
  cat(sprintf("  %-35s  RI p = %7.4f\n", sample_outcomes[i], ri_p_samples[i]))
}


# ===========================================================================
# SECTION 4: OMNIBUS COUNT TESTS (EGAP approach)
# ===========================================================================

cat("\n===========================================================\n")
cat("  OMNIBUS COUNT TEST (EGAP approach)\n")
cat("===========================================================\n\n")

## Farmer-level primary outcomes, treat arm (exclude Anderson index)
farmer_idx <- 1:(length(farmer_outcomes) - 1)
omni_farmer_treat <- omnibus_count_test(obs_farmer_treat[farmer_idx],
                                         ri_farmer_treat[, farmer_idx])

cat(sprintf("Farmer-level (treat): %d of %d outcomes significant at 5%% level\n",
            omni_farmer_treat$obs_count, length(farmer_idx)))
cat(sprintf("  Omnibus RI p-value: %.4f\n\n", omni_farmer_treat$omnibus_p))

## Farmer-level primary outcomes, vid arm (exclude index)
omni_farmer_vid <- omnibus_count_test(obs_farmer_vid[farmer_idx],
                                       ri_farmer_vid[, farmer_idx])

cat(sprintf("Farmer-level (vid): %d of %d outcomes significant at 5%% level\n",
            omni_farmer_vid$obs_count, length(farmer_idx)))
cat(sprintf("  Omnibus RI p-value: %.4f\n\n", omni_farmer_vid$omnibus_p))

## MCC-level primary outcomes (exclude Anderson index)
mcc_idx <- 1:(length(MCC_outcomes) - 1)
omni_mcc <- omnibus_count_test(obs_mcc[mcc_idx], ri_mcc[, mcc_idx])

cat(sprintf("MCC-level: %d of %d outcomes significant at 5%% level\n",
            omni_mcc$obs_count, length(mcc_idx)))
cat(sprintf("  Omnibus RI p-value: %.4f\n\n", omni_mcc$omnibus_p))

## Milk sample outcomes (exclude index)
sample_idx <- 1:(length(sample_outcomes) - 1)
omni_samples <- omnibus_count_test(obs_samples[sample_idx],
                                    ri_samples[, sample_idx])

cat(sprintf("Milk samples: %d of %d outcomes significant at 5%% level\n",
            omni_samples$obs_count, length(sample_idx)))
cat(sprintf("  Omnibus RI p-value: %.4f\n\n", omni_samples$omnibus_p))


# ===========================================================================
# SECTION 5: EXPERIMENT 2 — TRADER-LEVEL QUALITY PREMIUM RCT
# ===========================================================================
# Treatment randomized at trader level within MCC blocks.
# Permutation: within each MCC, permute which traders are treated
# (maintaining the count of treated traders per MCC).
#
# Three outcome levels:
#   (A) Daily submissions (trader-day panel): outcome ~ treatment
#       Test statistic: OLS coefficient on treatment (no FE in RI, FE absorbed)
#   (B) Trader endline: outcome ~ treat
#   (C) Farmer endline: outcome ~ treat (farmers inherit trader treatment)
# ---------------------------------------------------------------------------

cat("\n===========================================================\n")
cat("  Fisher Randomization Inference: Experiment 2\n")
cat("===========================================================\n\n")

## Load Experiment 2 prepped data
submissions_list <- readRDS(paste(path, "paper/results/prepped_submissions.rds", sep = "/"))
trader_day <- submissions_list$trader_day
traders    <- readRDS(paste(path, "paper/results/prepped_traders.rds", sep = "/"))
farmers_fu <- readRDS(paste(path, "paper/results/prepped_farmers_fu.rds", sep = "/"))

## Build within-MCC treatment count for permutation
## Each MCC has a fixed number of treated traders; we permute which ones
trader_mcc_map <- unique(trader_day[, c("trader_ID", "MCC_ID", "treatment")])
treat_counts_by_mcc <- tapply(trader_mcc_map$treatment, trader_mcc_map$MCC_ID, sum)

cat(sprintf("Experiment 2: %d traders across %d MCCs\n",
            nrow(trader_mcc_map), length(treat_counts_by_mcc)))
cat(sprintf("Number of permutations: %d\n\n", n_perms))

## --- 5A. Daily submissions: full period, stage 1, stage 2 ---
## Outcomes: avg_fat, avg_snf, total_qty, bonus
## Periods: full (after Nov 3), stage1 (Nov 3-14), stage2 (Nov 15+)

post_treat <- trader_day[trader_day$date >= as.Date("2025-11-03"), ]
stage1     <- trader_day[trader_day$date >= as.Date("2025-11-03") &
                           trader_day$date <= as.Date("2025-11-14"), ]
stage2     <- trader_day[trader_day$date >= as.Date("2025-11-15"), ]

sub_outcomes <- c("avg_fat", "avg_snf", "total_qty", "bonus")
periods <- list(full = post_treat, s1 = stage1, s2 = stage2)

## Demean outcomes within MCC to match the feols FE specification
## This ensures the RI test statistic corresponds to the within-MCC estimator
for (y in sub_outcomes) {
  for (period_name in names(periods)) {
    dat <- periods[[period_name]]
    mcc_means <- tapply(dat[[y]], dat$MCC_ID, mean, na.rm = TRUE)
    dat[[paste0(y, "_dm")]] <- dat[[y]] - as.numeric(mcc_means[as.character(dat$MCC_ID)])
    periods[[period_name]] <- dat
  }
  mcc_m <- tapply(trader_day[[y]], trader_day$MCC_ID, mean, na.rm = TRUE)
  trader_day[[paste0(y, "_dm")]] <- trader_day[[y]] - as.numeric(mcc_m[as.character(trader_day$MCC_ID)])
}
## Also demean the full subsets
post_treat <- periods$full
stage1 <- periods$s1
stage2 <- periods$s2

sub_outcomes_dm <- paste0(sub_outcomes, "_dm")

## Observed coefficients: 4 outcomes x 3 periods (using demeaned outcomes)
obs_sub <- matrix(NA, nrow = length(sub_outcomes), ncol = length(periods))
for (j in seq_along(periods)) {
  dat <- periods[[j]]
  for (i in seq_along(sub_outcomes)) {
    ols <- lm(as.formula(paste(sub_outcomes_dm[i], "~ treatment")), data = dat)
    obs_sub[i, j] <- coef(ols)["treatment"]
  }
}

## Permutation loop for submissions
ri_sub <- array(NA, dim = c(n_perms, length(sub_outcomes), length(periods)))

cat("Running Experiment 2 submission RI permutations...\n")
for (p in 1:n_perms) {
  if (p %% 1000 == 0) cat(sprintf("  Permutation %d / %d\n", p, n_perms))

  ## Permute treatment within each MCC
  perm_map <- trader_mcc_map
  perm_map$treat_perm <- 0
  for (mcc in names(treat_counts_by_mcc)) {
    idx <- which(perm_map$MCC_ID == mcc)
    n_treat <- treat_counts_by_mcc[mcc]
    if (n_treat > 0 && n_treat < length(idx)) {
      treat_idx <- sample(idx, n_treat, replace = FALSE)
      perm_map$treat_perm[treat_idx] <- 1
    } else if (n_treat == length(idx)) {
      perm_map$treat_perm[idx] <- 1
    }
  }

  ## Map permuted treatment back to trader_day
  trader_day$treat_perm <- perm_map$treat_perm[
    match(trader_day$trader_ID, perm_map$trader_ID)]

  for (j in seq_along(periods)) {
    dat <- if (j == 1) trader_day[trader_day$date >= as.Date("2025-11-03"), ]
           else if (j == 2) trader_day[trader_day$date >= as.Date("2025-11-03") &
                                         trader_day$date <= as.Date("2025-11-14"), ]
           else trader_day[trader_day$date >= as.Date("2025-11-15"), ]

    for (i in seq_along(sub_outcomes)) {
      ols <- lm(as.formula(paste(sub_outcomes_dm[i], "~ treat_perm")), data = dat)
      ri_sub[p, i, j] <- coef(ols)["treat_perm"]
    }
  }
}

## RI p-values for submissions: 4 outcomes x 3 periods
ri_p_sub <- matrix(NA, nrow = length(sub_outcomes), ncol = length(periods),
                   dimnames = list(sub_outcomes, c("full", "s1", "s2")))
for (j in seq_along(periods)) {
  for (i in seq_along(sub_outcomes)) {
    ri_p_sub[i, j] <- mean(abs(ri_sub[, i, j]) >= abs(obs_sub[i, j]), na.rm = TRUE)
  }
}

cat("\nExperiment 2 submission RI p-values:\n")
for (j in seq_along(periods)) {
  cat(sprintf("  %s period:\n", names(periods)[j]))
  for (i in seq_along(sub_outcomes)) {
    cat(sprintf("    %-15s  RI p = %.4f\n", sub_outcomes[i], ri_p_sub[i, j]))
  }
}

## --- 5B. Trader endline ---
traders$any_rejected  <- as.numeric(traders$rejected_month > 0)
traders$pays_premium  <- as.numeric(traders$pay_premim == 1)
trader_outcomes <- c("Fat_supervised", "delivered_quantity", "any_rejected",
                     "pays_premium", "avg_purchase_price")

## Build trader-level MCC map for permutation
trader_map_endline <- traders[, c("trader_ID", "MCC_ID", "treat")]
treat_counts_endline <- tapply(trader_map_endline$treat, trader_map_endline$MCC_ID, sum)

## Demean trader outcomes within MCC to match feols FE specification
for (y in trader_outcomes) {
  mcc_means <- tapply(traders[[y]], traders$MCC_ID, mean, na.rm = TRUE)
  traders[[paste0(y, "_dm")]] <- traders[[y]] - as.numeric(mcc_means[as.character(traders$MCC_ID)])
}
trader_outcomes_dm <- paste0(trader_outcomes, "_dm")

obs_trader <- numeric(length(trader_outcomes))
for (i in seq_along(trader_outcomes)) {
  ols <- lm(as.formula(paste(trader_outcomes_dm[i], "~ treat")), data = traders)
  obs_trader[i] <- coef(ols)["treat"]
}

ri_trader <- matrix(NA, nrow = n_perms, ncol = length(trader_outcomes))

cat("\nRunning Experiment 2 trader endline RI permutations...\n")
for (p in 1:n_perms) {
  if (p %% 1000 == 0) cat(sprintf("  Permutation %d / %d\n", p, n_perms))

  perm_map <- trader_map_endline
  perm_map$treat_perm <- 0
  for (mcc in names(treat_counts_endline)) {
    idx <- which(perm_map$MCC_ID == mcc)
    n_treat <- treat_counts_endline[mcc]
    if (n_treat > 0 && n_treat < length(idx)) {
      treat_idx <- sample(idx, n_treat, replace = FALSE)
      perm_map$treat_perm[treat_idx] <- 1
    } else if (n_treat == length(idx)) {
      perm_map$treat_perm[idx] <- 1
    }
  }

  for (i in seq_along(trader_outcomes)) {
    traders$treat_perm <- perm_map$treat_perm[match(traders$trader_ID, perm_map$trader_ID)]
    ols <- lm(as.formula(paste(trader_outcomes_dm[i], "~ treat_perm")), data = traders)
    ri_trader[p, i] <- coef(ols)["treat_perm"]
  }
}

ri_p_trader <- numeric(length(trader_outcomes))
names(ri_p_trader) <- trader_outcomes
for (i in seq_along(trader_outcomes)) {
  ri_p_trader[i] <- mean(abs(ri_trader[, i]) >= abs(obs_trader[i]), na.rm = TRUE)
}

cat("\nTrader endline RI p-values:\n")
for (i in seq_along(trader_outcomes)) {
  cat(sprintf("  %-25s  RI p = %.4f\n", trader_outcomes[i], ri_p_trader[i]))
}

## --- 5C. Farmer endline ---
farmer_fu_outcomes <- c("avg_price", "quality_checked", "feeding_index",
                        "used_bran", "used_residu", "used_lick", "used_cgrazing")

## Farmers inherit treatment from their trader
## Demean farmer outcomes within MCC to match feols FE specification
for (y in farmer_fu_outcomes) {
  mcc_means <- tapply(farmers_fu[[y]], farmers_fu$MCC_ID, mean, na.rm = TRUE)
  farmers_fu[[paste0(y, "_dm")]] <- farmers_fu[[y]] - as.numeric(mcc_means[as.character(farmers_fu$MCC_ID)])
}
farmer_fu_outcomes_dm <- paste0(farmer_fu_outcomes, "_dm")

obs_farmer_fu <- numeric(length(farmer_fu_outcomes))
for (i in seq_along(farmer_fu_outcomes)) {
  ols <- lm(as.formula(paste(farmer_fu_outcomes_dm[i], "~ treat")), data = farmers_fu)
  obs_farmer_fu[i] <- coef(ols)["treat"]
}

ri_farmer_fu <- matrix(NA, nrow = n_perms, ncol = length(farmer_fu_outcomes))

cat("\nRunning Experiment 2 farmer endline RI permutations...\n")
for (p in 1:n_perms) {
  if (p %% 1000 == 0) cat(sprintf("  Permutation %d / %d\n", p, n_perms))

  ## Same trader-level permutation, propagated to farmers
  perm_map <- trader_map_endline
  perm_map$treat_perm <- 0
  for (mcc in names(treat_counts_endline)) {
    idx <- which(perm_map$MCC_ID == mcc)
    n_treat <- treat_counts_endline[mcc]
    if (n_treat > 0 && n_treat < length(idx)) {
      treat_idx <- sample(idx, n_treat, replace = FALSE)
      perm_map$treat_perm[treat_idx] <- 1
    } else if (n_treat == length(idx)) {
      perm_map$treat_perm[idx] <- 1
    }
  }

  farmers_fu$treat_perm <- perm_map$treat_perm[
    match(farmers_fu$trader_ID, perm_map$trader_ID)]

  for (i in seq_along(farmer_fu_outcomes)) {
    ols <- lm(as.formula(paste(farmer_fu_outcomes_dm[i], "~ treat_perm")), data = farmers_fu)
    ri_farmer_fu[p, i] <- coef(ols)["treat_perm"]
  }
}

ri_p_farmer_fu <- numeric(length(farmer_fu_outcomes))
names(ri_p_farmer_fu) <- farmer_fu_outcomes
for (i in seq_along(farmer_fu_outcomes)) {
  ri_p_farmer_fu[i] <- mean(abs(ri_farmer_fu[, i]) >= abs(obs_farmer_fu[i]), na.rm = TRUE)
}

cat("\nFarmer endline RI p-values:\n")
for (i in seq_along(farmer_fu_outcomes)) {
  cat(sprintf("  %-25s  RI p = %.4f\n", farmer_fu_outcomes[i], ri_p_farmer_fu[i]))
}

## --- 5D. Omnibus tests for Experiment 2 ---
## Submissions: omnibus across 4 outcomes for each period
omni_sub_full <- omnibus_count_test(obs_sub[, 1], ri_sub[, , 1])
omni_sub_s1   <- omnibus_count_test(obs_sub[, 2], ri_sub[, , 2])
omni_sub_s2   <- omnibus_count_test(obs_sub[, 3], ri_sub[, , 3])

cat(sprintf("\nOmnibus — Submissions full: %d/%d sig, p=%.4f\n",
            omni_sub_full$obs_count, length(sub_outcomes), omni_sub_full$omnibus_p))
cat(sprintf("Omnibus — Submissions S1: %d/%d sig, p=%.4f\n",
            omni_sub_s1$obs_count, length(sub_outcomes), omni_sub_s1$omnibus_p))
cat(sprintf("Omnibus — Submissions S2: %d/%d sig, p=%.4f\n",
            omni_sub_s2$obs_count, length(sub_outcomes), omni_sub_s2$omnibus_p))

## Trader endline omnibus
omni_trader <- omnibus_count_test(obs_trader, ri_trader)
cat(sprintf("Omnibus — Trader endline: %d/%d sig, p=%.4f\n",
            omni_trader$obs_count, length(trader_outcomes), omni_trader$omnibus_p))

## Farmer endline omnibus (exclude individual feeding practices, keep avg_price, quality_checked, feeding_index)
farmer_fu_primary_idx <- 1:3
omni_farmer_fu <- omnibus_count_test(obs_farmer_fu[farmer_fu_primary_idx],
                                      ri_farmer_fu[, farmer_fu_primary_idx])
cat(sprintf("Omnibus — Farmer FU (primary): %d/%d sig, p=%.4f\n",
            omni_farmer_fu$obs_count, length(farmer_fu_primary_idx), omni_farmer_fu$omnibus_p))


# ===========================================================================
# SECTION 6: SAVE ALL RESULTS
# ===========================================================================

ri_results <- list(
  ## Farmer-level results
  farmer = list(
    outcomes     = farmer_outcomes,
    b_outcomes   = farmer_b_outcomes,
    obs_treat    = obs_farmer_treat,
    obs_vid      = obs_farmer_vid,
    ri_p_treat   = ri_p_farmer_treat,
    ri_p_vid     = ri_p_farmer_vid,
    N            = obs_farmer_N,
    perm_coefs_treat = ri_farmer_treat,
    perm_coefs_vid   = ri_farmer_vid
  ),
  ## MCC-level results
  MCC = list(
    outcomes     = MCC_outcomes,
    b_outcomes   = MCC_b_outcomes,
    obs_treat    = obs_mcc,
    ri_p         = ri_p_mcc,
    N            = obs_mcc_N,
    perm_coefs   = ri_mcc
  ),
  ## Sample-level results
  samples = list(
    outcomes     = sample_outcomes,
    obs_treat    = obs_samples,
    ri_p         = ri_p_samples,
    N            = obs_samples_N,
    perm_coefs   = ri_samples
  ),
  ## Experiment 2 results
  exp2_submissions = list(
    outcomes   = sub_outcomes,
    obs_coefs  = obs_sub,
    ri_p       = ri_p_sub,
    perm_coefs = ri_sub
  ),
  exp2_traders = list(
    outcomes   = trader_outcomes,
    obs_coefs  = obs_trader,
    ri_p       = ri_p_trader,
    perm_coefs = ri_trader
  ),
  exp2_farmers = list(
    outcomes   = farmer_fu_outcomes,
    obs_coefs  = obs_farmer_fu,
    ri_p       = ri_p_farmer_fu,
    perm_coefs = ri_farmer_fu
  ),
  ## Omnibus count tests
  omnibus = list(
    farmer_treat = omni_farmer_treat,
    farmer_vid   = omni_farmer_vid,
    mcc          = omni_mcc,
    samples      = omni_samples,
    exp2_sub_full = omni_sub_full,
    exp2_sub_s1   = omni_sub_s1,
    exp2_sub_s2   = omni_sub_s2,
    exp2_traders  = omni_trader,
    exp2_farmers  = omni_farmer_fu,
    alpha        = 0.05
  ),
  ## Metadata
  n_perms = n_perms,
  seed    = 12345
)

saveRDS(ri_results, file = paste(path, "paper/results/randomization_inference.rds", sep = "/"))

cat("\n===== Results saved to paper/results/randomization_inference.rds =====\n")
cat("06_randomization_inference.R completed successfully.\n")
