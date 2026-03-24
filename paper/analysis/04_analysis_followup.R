###############################################################################
## 04_analysis_followup.R
##
## Experiment 2 regressions: trader-level randomized quality premium intervention
## (leveraging the digital monitoring system from Experiment 1).
## Uses fixest for FE regressions on daily submission data, trader endline,
## and farmer follow-up endline.
##
## Must be run with working directory set to paper/analysis/.
## Sources 00_utilities.R for helper functions.
##
## Outputs (saved to paper/results/):
##   balance_followup.rds  — pre-treatment balance (4 x 5)
##   res_submissions.rds   — daily submission regressions (4 x 15)
##   res_traders.rds       — trader endline regressions (5 x 5)
##   res_farmers_fu.rds    — farmer follow-up endline regressions (7 x 5)
###############################################################################

source("00_utilities.R")

library(fixest)
library(dplyr)

## ---------------------------------------------------------------------------
## Detect project root and load prepped data
## ---------------------------------------------------------------------------
path <- strsplit(getwd(), "/paper/analysis")[[1]]

submissions_list <- readRDS(paste(path, "paper/results/prepped_submissions.rds", sep = "/"))
trader_day <- submissions_list$trader_day
daily_avg  <- submissions_list$daily_avg
traders    <- readRDS(paste(path, "paper/results/prepped_traders.rds", sep = "/"))
farmers_fu <- readRDS(paste(path, "paper/results/prepped_farmers_fu.rds", sep = "/"))


###############################################################################
## A. BALANCE CHECK (pre-treatment period <= Nov 1, 2025)
###############################################################################
## For each quality/quantity outcome, test whether treatment predicts
## pre-treatment levels using MCC fixed effects and trader-level clustering.

pre_treat <- trader_day[trader_day$date <= as.Date("2025-11-01"), ]

balance_outcomes <- c("avg_fat", "avg_snf", "total_qty", "bonus")

balance_followup <- array(NA, dim = c(length(balance_outcomes), 5),
                          dimnames = list(balance_outcomes,
                                          c("coef", "se", "p", "n", "ctrl_mean")))

for (i in seq_along(balance_outcomes)) {
  y <- balance_outcomes[i]

  ## Control mean
  ctrl_vals <- pre_treat[[y]][pre_treat$treatment == 0]
  balance_followup[i, 5] <- mean(ctrl_vals, na.rm = TRUE)

  ## FE regression with trader-level clustering
  fml <- as.formula(paste(y, "~ treatment | MCC_ID"))
  fit <- feols(fml, data = pre_treat, vcov = ~trader_ID)

  balance_followup[i, 1] <- coef(fit)["treatment"]
  balance_followup[i, 2] <- se(fit)["treatment"]
  balance_followup[i, 3] <- pvalue(fit)["treatment"]
  balance_followup[i, 4] <- fit$nobs
}

balance_followup <- round(balance_followup, digits = 4)

saveRDS(balance_followup,
        file = paste(path, "paper/results/balance_followup.rds", sep = "/"))


###############################################################################
## B. DAILY SUBMISSION REGRESSIONS
###############################################################################
## For each outcome (avg_fat, avg_snf, total_qty, bonus):
##   - Full period (after Nov 3)
##   - Stage 1 only (Nov 3 to Nov 14)
##   - Stage 2 only (Nov 15+)
##
## Result array: 4 outcomes x 15 columns (5 per period: coef, se, p, n, ctrl_mean)

submission_outcomes <- c("avg_fat", "avg_snf", "total_qty", "bonus")

## Define period subsets
post_treat <- trader_day[trader_day$date >= as.Date("2025-11-03"), ]
stage1     <- trader_day[trader_day$date >= as.Date("2025-11-03") &
                           trader_day$date <= as.Date("2025-11-14"), ]
stage2     <- trader_day[trader_day$date >= as.Date("2025-11-15"), ]

res_submissions <- array(NA, dim = c(length(submission_outcomes), 15),
                         dimnames = list(submission_outcomes,
                           c("coef_full", "se_full", "p_full", "n_full", "ctrl_mean_full",
                             "coef_s1", "se_s1", "p_s1", "n_s1", "ctrl_mean_s1",
                             "coef_s2", "se_s2", "p_s2", "n_s2", "ctrl_mean_s2")))

periods <- list(
  full = post_treat,
  s1   = stage1,
  s2   = stage2
)

for (i in seq_along(submission_outcomes)) {
  y <- submission_outcomes[i]
  col_offset <- 0

  for (p in seq_along(periods)) {
    dat <- periods[[p]]
    col_start <- (p - 1) * 5 + 1

    ## Control mean for this period
    ctrl_vals <- dat[[y]][dat$treatment == 0]
    res_submissions[i, col_start + 4] <- mean(ctrl_vals, na.rm = TRUE)

    ## FE regression with trader-level clustering
    fml <- as.formula(paste(y, "~ treatment | MCC_ID"))
    fit <- feols(fml, data = dat, vcov = ~trader_ID)

    res_submissions[i, col_start]     <- coef(fit)["treatment"]
    res_submissions[i, col_start + 1] <- se(fit)["treatment"]
    res_submissions[i, col_start + 2] <- pvalue(fit)["treatment"]
    res_submissions[i, col_start + 3] <- fit$nobs
  }
}

res_submissions <- round(res_submissions, digits = 4)

saveRDS(res_submissions,
        file = paste(path, "paper/results/res_submissions.rds", sep = "/"))


###############################################################################
## C. TRADER ENDLINE REGRESSIONS
###############################################################################
## Outcomes: Fat_supervised, delivered_quantity, (rejected_month > 0),
##           (pay_premim == 1), avg_price_paid
## No clustering needed — traders are the unit of randomization.
## MCC fixed effects included.

## Construct binary outcomes
traders$any_rejected  <- as.numeric(traders$rejected_month > 0)
traders$pays_premium  <- as.numeric(traders$pay_premim == 1)

trader_outcomes <- c("Fat_supervised", "delivered_quantity", "any_rejected",
                     "pays_premium", "avg_purchase_price")

res_traders <- array(NA, dim = c(length(trader_outcomes), 5),
                     dimnames = list(trader_outcomes,
                                     c("coef", "se", "p", "n", "ctrl_mean")))

for (i in seq_along(trader_outcomes)) {
  y <- trader_outcomes[i]

  ## Control mean
  ctrl_vals <- traders[[y]][traders$treat == 0]
  res_traders[i, 5] <- mean(ctrl_vals, na.rm = TRUE)

  ## FE regression (no clustering — trader is unit of randomization)
  fml <- as.formula(paste(y, "~ treat | MCC_ID"))
  fit <- feols(fml, data = traders)

  res_traders[i, 1] <- coef(fit)["treat"]
  res_traders[i, 2] <- se(fit)["treat"]
  res_traders[i, 3] <- pvalue(fit)["treat"]
  res_traders[i, 4] <- fit$nobs
}

res_traders <- round(res_traders, digits = 4)

saveRDS(res_traders,
        file = paste(path, "paper/results/res_traders.rds", sep = "/"))


###############################################################################
## D. FARMER ENDLINE REGRESSIONS
###############################################################################
## Outcomes: avg_price (volume-weighted p_sold), quality_checked,
##           feeding_index (Anderson), plus individual feeding practices
## MCC FE, clustered at trader level.

## Feeding practice indicators and Anderson index already constructed in 03_prep_followup.R
## Columns: used_bran, used_residu, used_lick, used_cgrazing, feeding_index

farmer_fu_outcomes <- c("avg_price", "quality_checked", "feeding_index",
                        "used_bran", "used_residu", "used_lick", "used_cgrazing")

res_farmers_fu <- array(NA, dim = c(length(farmer_fu_outcomes), 5),
                        dimnames = list(farmer_fu_outcomes,
                                        c("coef", "se", "p", "n", "ctrl_mean")))

for (i in seq_along(farmer_fu_outcomes)) {
  y <- farmer_fu_outcomes[i]

  ## Control mean
  ctrl_vals <- farmers_fu[[y]][farmers_fu$treat == 0]
  res_farmers_fu[i, 5] <- mean(ctrl_vals, na.rm = TRUE)

  ## FE regression with trader-level clustering
  fml <- as.formula(paste(y, "~ treat | MCC_ID"))
  fit <- feols(fml, data = farmers_fu, vcov = ~trader_ID)

  res_farmers_fu[i, 1] <- coef(fit)["treat"]
  res_farmers_fu[i, 2] <- se(fit)["treat"]
  res_farmers_fu[i, 3] <- pvalue(fit)["treat"]
  res_farmers_fu[i, 4] <- fit$nobs
}

res_farmers_fu <- round(res_farmers_fu, digits = 4)

saveRDS(res_farmers_fu,
        file = paste(path, "paper/results/res_farmers_fu.rds", sep = "/"))

###############################################################################
## E. TRADER ENDLINE — SECONDARY OUTCOMES
###############################################################################
## Additional pre-registered trader outcomes not included in the primary table.
## Same specification: treat with MCC FE, no clustering (trader = unit).

## ---------------------------------------------------------------------------
## E1. Clean additional variables (999 -> NA, recode 1/2/3 survey codes)
## ---------------------------------------------------------------------------

## self: 1 = tests milk themselves, 2 = no, 3 = don't know -> NA
traders$self_tests <- ifelse(traders$self == 3, NA,
                             ifelse(traders$self == 1, 1, 0))

## use_lacto: 1 = yes, 2 = no, 3 = don't know -> NA
traders$uses_lactometer <- ifelse(traders$use_lacto == 3, NA,
                                  ifelse(traders$use_lacto == 1, 1, 0))

## use_alcohol: 1 = yes, 2 = no, 3 = don't know -> NA
traders$uses_alcohol <- ifelse(traders$use_alcohol == 3, NA,
                               ifelse(traders$use_alcohol == 1, 1, 0))

## ---------------------------------------------------------------------------
## E2. Seller concentration (Herfindahl-style)
##     Sum of (farmer_share)^2 where farmer_share = purchase_q_i / total
##     Higher = more concentrated (fewer suppliers matter)
## ---------------------------------------------------------------------------
qty_cols <- c("purchase_q_1", "purchase_q_2", "purchase_q_3",
              "purchase_q_4", "purchase_q_5", "purchase_q_6")

## Build matrix of purchase quantities (already cleaned in 03_prep_followup.R)
q_mat <- as.matrix(traders[qty_cols])

## Total purchased from all farmers
row_totals <- rowSums(q_mat, na.rm = TRUE)

## Shares per farmer
share_mat <- q_mat / row_totals

## HHI = sum of squared shares (ignoring NAs = farmers not listed)
traders$seller_hhi <- rowSums(share_mat^2, na.rm = TRUE)
traders$seller_hhi[row_totals == 0] <- NA

## ---------------------------------------------------------------------------
## E3. Run regressions and store results
## ---------------------------------------------------------------------------
secondary_outcomes <- c("SNF_supervised", "self_tests", "uses_lactometer",
                        "uses_alcohol", "seller_hhi")

res_traders_secondary <- array(NA, dim = c(length(secondary_outcomes), 6),
                               dimnames = list(secondary_outcomes,
                                               c("coef", "se", "p",
                                                 "ctrl_mean", "ctrl_sd", "n")))

for (i in seq_along(secondary_outcomes)) {
  y <- secondary_outcomes[i]

  ## Control mean and SD
  ctrl_vals <- traders[[y]][traders$treat == 0]
  res_traders_secondary[i, "ctrl_mean"] <- mean(ctrl_vals, na.rm = TRUE)
  res_traders_secondary[i, "ctrl_sd"]   <- sd(ctrl_vals, na.rm = TRUE)

  ## FE regression (no clustering — trader is unit of randomization)
  fml <- as.formula(paste(y, "~ treat | MCC_ID"))
  fit <- feols(fml, data = traders)

  res_traders_secondary[i, "coef"] <- coef(fit)["treat"]
  res_traders_secondary[i, "se"]   <- se(fit)["treat"]
  res_traders_secondary[i, "p"]    <- pvalue(fit)["treat"]
  res_traders_secondary[i, "n"]    <- fit$nobs
}

res_traders_secondary <- round(res_traders_secondary, digits = 4)

saveRDS(res_traders_secondary,
        file = paste(path, "paper/results/res_traders_secondary.rds", sep = "/"))

cat("\n04_analysis_followup.R completed successfully.\n")
