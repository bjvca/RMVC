###############################################################################
## 02_analysis_main.R
##
## Experiment 1 regressions: 2x2 factorial RCT
## (quality measurement & digital monitoring system x information video)
## Ported from audited PAP/analysis/analysis.R and balance_tables.R
##
## Must be run with working directory set to paper/analysis/.
## Sources 00_utilities.R for helper functions (ihs, cr2_ols, etc.)
##
## NOTE: Trader heterogeneity has been removed from all regressions.
## The trader variable is kept in the data as context but is not used
## as a heterogeneity dimension in any analysis.
##
## Outputs (saved to paper/results/):
##   balance.rds               — MCC-level balance array (10 x 8)
##   balance_farmer.rds        — farmer-level balance array (10 x 15)
##   F_test.rds                — joint F-test array (2 x 8)
##   res_farmers.rds           — primary farmer ITT (5 x 15)
##   res_farmers_TOT.rds       — primary farmer LATE (5 x 15)
##   res_farmers_sec_quant.rds — secondary: sales quantities (5 x 11)
##   res_farmers_sec_uptake.rds— secondary: treatment uptake (5 x 11)
##   res_farmers_sec_sold.rds  — secondary: sales/market channel (6 x 11)
##   res_farmers_sec_switching.rds — secondary: switching (4 x 11)
##   res_MCCs.rds              — MCC-level primary (7 x 12)
##   attrition.rds             — attrition test results
###############################################################################

source("00_utilities.R")

library(clubSandwich)   # CR2 cluster-robust variance estimation
library(andersonTools)  # Anderson-Sharp sharpened q-values, Anderson indices
library(car)            # linearHypothesis, Wald_test
library(AER)            # ivreg for 2SLS

## ---------------------------------------------------------------------------
## Detect project root and load prepped data
## ---------------------------------------------------------------------------
path <- strsplit(getwd(), "/paper/analysis")[[1]]

farmers_end     <- readRDS(paste(path, "paper/results/prepped_farmers.rds", sep = "/"))
farmers_base    <- readRDS(paste(path, "paper/results/prepped_farmers_base.rds", sep = "/"))
MCCs_end        <- readRDS(paste(path, "paper/results/prepped_MCCs.rds", sep = "/"))
MCCs_base       <- readRDS(paste(path, "paper/results/prepped_MCCs_base.rds", sep = "/"))
MCC_balance     <- readRDS(paste(path, "paper/results/MCC_balance.rds", sep = "/"))
farmers_balance <- readRDS(paste(path, "paper/results/farmers_balance.rds", sep = "/"))


###############################################################################
## A. BALANCE TABLES
###############################################################################

## ---- A1. MCC-level balance (10 outcomes x 8 columns) ----------------------
## Columns: ctrl_mean | ctrl_sd | coef | se | pval | ci_lo | ci_hi | nobs

outcomes_mcc <- c("is_coop", "full_timers", "clients_rainy", "capacity",
                   "capacity_use_dry", "quality_premium", "years_in_operation",
                   "nr_milk_cans", "supply_credit", "supply_accaracides")

balance <- array(NA, dim = c(length(outcomes_mcc), 8),
                 dimnames = list(outcomes_mcc,
                                 c("ctrl_mean", "ctrl_sd", "coef", "se",
                                   "pval", "ci_lo", "ci_hi", "nobs")))

for (i in seq_along(outcomes_mcc)) {
  ols  <- lm(as.formula(paste(outcomes_mcc[i], "~ lactoscan")), data = MCC_balance)
  vcov_cluster <- vcovCR(ols, cluster = MCC_balance$catchment_ID, type = "CR2")
  res  <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)

  ctrl <- MCC_balance[MCC_balance$lactoscan == "C", outcomes_mcc[i]]
  balance[i, ] <- c(mean(ctrl, na.rm = TRUE),
                     sd(ctrl, na.rm = TRUE),
                     res[2, 2], res[2, 3], res[2, 7],   # coef, se, p
                     conf[2, 5], conf[2, 6],             # CI bounds
                     nobs(ols))
}

## ---- A2. Joint F-tests (Wald via CR2) -------------------------------------
## F_test: row 1 = F-stat, row 2 = p-value
## Columns 1-2: MCC main/appendix | 3-5: farmer main | 6-8: farmer appendix
F_test <- array(NA, dim = c(2, 8),
                dimnames = list(c("Fstat", "pval"),
                                c("MCC_main", "MCC_appx",
                                  "farmer_T1", "farmer_T2", "farmer_T1xT2",
                                  "farmer_appx_T1", "farmer_appx_T2",
                                  "farmer_appx_T1xT2")))

## F-test: main table variables predict lactoscan assignment
mod <- lm((lactoscan == "T") ~ is_coop + capacity + quality_premium +
            years_in_operation + supply_accaracides, data = MCC_balance)
W <- Wald_test(mod,
               constraints = constrain_zero(c("is_coopTRUE", "capacity",
                                              "quality_premiumTRUE",
                                              "years_in_operation",
                                              "supply_accaracidesTRUE")),
               vcov = "CR2", cluster = MCC_balance$catchment_ID, test = "HTZ")
F_test[1, 1] <- W$Fstat
F_test[2, 1] <- W$p_val

## F-test: appendix table variables predict lactoscan assignment
mod <- lm((lactoscan == "T") ~ full_timers + clients_rainy + capacity_use_dry +
            nr_milk_cans + supply_credit, data = MCC_balance)
W <- Wald_test(mod,
               constraints = constrain_zero(c("full_timers", "clients_rainy",
                                              "capacity_use_dry", "nr_milk_cans",
                                              "supply_creditTRUE")),
               vcov = "CR2", cluster = MCC_balance$catchment_ID, test = "HTZ")
F_test[1, 2] <- W$Fstat
F_test[2, 2] <- W$p_val


## ---- A3. Farmer-level balance (10 outcomes x 15 columns) ------------------
##
## New column layout (same as primary farmer results):
##  1-2:   ctrl_mean, ctrl_sd (pure control: lactoscan=C & video=FALSE)
##  3-5:   T1 (measurement & monitoring system) coef, se, p
##  6-8:   T2 (video) coef, se, p
##  9:     N
##  10-11: Anderson-Sharp q-values for cols 5, 8
##  12-14: pooled T1 coef, se, p (orthogonalized: lactoscan * vid_demeaned)
##  15:    pooled T1 q-value for col 14

outcomes_farmer <- c("hh_size", "age_head", "herd_size", "improved_share",
                     "liter_day_wet", "liter_sold_day_wet", "sell_MCC_wet",
                     "use_steel", "coop_member", "acaracide_exp")

balance_farmer <- array(NA, dim = c(length(outcomes_farmer), 15))

for (i in seq_along(outcomes_farmer)) {

  ## --- Factorial model: outcome ~ lactoscan * video_shown ---
  ols <- lm(as.formula(paste(outcomes_farmer[i], "~ lactoscan * video_shown")),
            data = farmers_balance)
  vcov_cluster <- vcovCR(ols, cluster = farmers_balance$catchment_ID, type = "CR2")
  res <- coef_test(ols, vcov_cluster)

  ctrl <- farmers_balance[farmers_balance$lactoscan == "C" &
                            farmers_balance$video_shown == FALSE,
                          outcomes_farmer[i]]
  balance_farmer[i, 1]    <- mean(ctrl, na.rm = TRUE)
  balance_farmer[i, 2]    <- sd(ctrl, na.rm = TRUE)
  balance_farmer[i, 3:5]  <- c(res[2, 2], res[2, 3], res[2, 7])   # T1
  balance_farmer[i, 6:8]  <- c(res[3, 2], res[3, 3], res[3, 7])   # T2
  balance_farmer[i, 9]    <- nobs(ols)
}

## Anderson-Sharp q-values
balance_farmer[, 10] <- anderson_sharp_q(balance_farmer[, 5])    # T1
balance_farmer[, 11] <- anderson_sharp_q(balance_farmer[, 8])    # T2

## --- Pooled models with demeaned orthogonal treatment (Muralidharan et al.) ---
farmers_balance$vid_demeaned <- farmers_balance$video_shown -
  mean(farmers_balance$video_shown, na.rm = TRUE)
farmers_balance$lactoscan_demeaned <- (farmers_balance$lactoscan == "T") -
  mean(farmers_balance$lactoscan == "T", na.rm = TRUE)

for (i in seq_along(outcomes_farmer)) {
  ## Pooled T1 effect (demeaning video)
  ols <- lm(as.formula(paste(outcomes_farmer[i], "~ lactoscan * vid_demeaned")),
            data = farmers_balance)
  vcov_cluster <- vcovCR(ols, cluster = farmers_balance$catchment_ID, type = "CR2")
  res <- coef_test(ols, vcov_cluster)
  balance_farmer[i, 12:14] <- c(res[2, 2], res[2, 3], res[2, 7])
}

## Anderson-Sharp q-values for pooled p-values
balance_farmer[, 15] <- anderson_sharp_q(balance_farmer[, 14])   # pooled T1

## ---- A4. Farmer-level joint F-tests ----------------------------------------

## Helper: run a Wald joint-significance test with CR2 clustering
run_farmer_ftest <- function(lhs_formula, rhs_vars, data) {
  mod <- lm(lhs_formula, data = data)
  W <- Wald_test(mod,
                 constraints = constrain_zero(rhs_vars),
                 vcov = "CR2", cluster = data$catchment_ID, test = "HTZ")
  c(W$Fstat, W$p_val)
}

## Main-table variables predict treatment assignment
main_rhs <- c("age_head", "herd_size", "improved_share",
              "liter_sold_day_wet", "acaracide_exp")

F_test[, 3] <- run_farmer_ftest(
  (lactoscan == "T") ~ age_head + herd_size + improved_share +
    liter_sold_day_wet + acaracide_exp,
  main_rhs, farmers_balance)
F_test[, 4] <- run_farmer_ftest(
  video_shown ~ age_head + herd_size + improved_share +
    liter_sold_day_wet + acaracide_exp,
  main_rhs, farmers_balance)
F_test[, 5] <- run_farmer_ftest(
  (video_shown * (lactoscan == "T")) ~ age_head + herd_size + improved_share +
    liter_sold_day_wet + acaracide_exp,
  main_rhs, farmers_balance)

## Appendix-table variables predict treatment assignment
appx_rhs <- c("hh_size", "liter_day_wet", "sell_MCC_wetTRUE",
               "use_steelTRUE", "coop_memberTRUE")

F_test[, 6] <- run_farmer_ftest(
  (lactoscan == "T") ~ hh_size + liter_day_wet + sell_MCC_wet + use_steel +
    coop_member,
  appx_rhs, farmers_balance)
F_test[, 7] <- run_farmer_ftest(
  video_shown ~ hh_size + liter_day_wet + sell_MCC_wet + use_steel + coop_member,
  appx_rhs, farmers_balance)
F_test[, 8] <- run_farmer_ftest(
  (video_shown * (lactoscan == "T")) ~ hh_size + liter_day_wet + sell_MCC_wet +
    use_steel + coop_member,
  appx_rhs, farmers_balance)

## Round and save balance tables
balance[, 1:7]  <- round(balance[, 1:7], digits = 3)
balance_farmer  <- round(balance_farmer, digits = 3)
F_test          <- round(F_test, digits = 3)

saveRDS(balance,        file = paste(path, "paper/results/balance.rds", sep = "/"))
saveRDS(balance_farmer, file = paste(path, "paper/results/balance_farmer.rds", sep = "/"))
saveRDS(F_test,         file = paste(path, "paper/results/F_test.rds", sep = "/"))


###############################################################################
## B. PRIMARY FARMER-LEVEL ITT (Section 4)
###############################################################################
## Outcome family: improve_index, check_MA, avg_sales_p, gets_q_bonus
## Plus Anderson index combining these four
##
## Result array layout (15 columns):
##  1-2:   control mean, control SD
##  3-5:   treat coef, SE, p  (from outcome ~ treat + vid + b_outcome)
##  6-8:   vid coef, SE, p    (same model)
##  9:     N observations
##  10-11: Anderson-Sharp q-values for cols 5, 8
##  12-14: pooled treat coef, SE, p (orthogonalized: treat * vid_demeaned + b_outcome)
##  15:    pooled treat q-value for col 14

outcomes   <- c("improve_index", "check_MA", "avg_sales_p", "gets_q_bonus")
b_outcomes <- c("b_improve_index", "b_check_MA", "b_avg_sales_p", "b_gets_q_bonus")

## Anderson index combining primary outcomes
farmers_end$primary_farmer_index   <- anderson_index(farmers_end[outcomes])$index
farmers_end$b_primary_farmer_index <- anderson_index(farmers_end[b_outcomes])$index

outcomes   <- c(outcomes, "primary_farmer_index")
b_outcomes <- c(b_outcomes, "b_primary_farmer_index")

## ---------------------------------------------------------------------------
## B1. Additive ITT: outcome ~ treat + vid + b_outcome
## ---------------------------------------------------------------------------
## Coefficient ordering: 1=(Intercept), 2=treat, 3=vid, 4=b_outcome

res_farmers <- array(NA, dim = c(length(outcomes), 15))

for (i in seq_along(outcomes)) {
  ols <- lm(as.formula(paste(outcomes[i], "~ treat + vid +", b_outcomes[i])),
            data = farmers_end)
  vcov_cluster <- vcovCR(ols, cluster = farmers_end$catch_ID, type = "CR2")
  res <- coef_test(ols, vcov_cluster)

  ## Control group mean and SD
  ctrl <- farmers_end[farmers_end$vid == FALSE & farmers_end$treat == FALSE,
                      outcomes[i]]
  res_farmers[i, 1] <- mean(ctrl, na.rm = TRUE)
  res_farmers[i, 2] <- sd(ctrl, na.rm = TRUE)

  ## Treatment effects
  res_farmers[i, 3:5] <- c(res[2, 2], res[2, 3], res[2, 7])   # treat
  res_farmers[i, 6:8] <- c(res[3, 2], res[3, 3], res[3, 7])   # vid
  res_farmers[i, 9]   <- nobs(ols)
}

## Anderson-Sharp q-values (exclude Anderson index row = last row)
idx <- 1:(length(outcomes) - 1)
res_farmers[idx, 10] <- anderson_sharp_q(res_farmers[idx, 5])
res_farmers[idx, 11] <- anderson_sharp_q(res_farmers[idx, 8])

## ---------------------------------------------------------------------------
## B2. Orthogonalized (demeaned) treatment models (Muralidharan et al.)
## ---------------------------------------------------------------------------
farmers_end$vid_demeaned    <- farmers_end$vid    - mean(farmers_end$vid, na.rm = TRUE)
farmers_end$treat_demeaned  <- farmers_end$treat  - mean(farmers_end$treat, na.rm = TRUE)

## Pooled measurement & monitoring system effect (demeaning vid)
for (i in seq_along(outcomes)) {
  ols <- lm(as.formula(paste(outcomes[i], "~ treat*vid_demeaned +", b_outcomes[i])),
            data = farmers_end)
  vcov_cluster <- vcovCR(ols, cluster = farmers_end$catch_ID, type = "CR2")
  res <- coef_test(ols, vcov_cluster)
  res_farmers[i, 12:14] <- c(res[2, 2], res[2, 3], res[2, 7])
}

res_farmers[idx, 15] <- anderson_sharp_q(res_farmers[idx, 14])

res_farmers <- round(res_farmers, digits = 3)

saveRDS(res_farmers, file = paste(path, "paper/results/res_farmers.rds", sep = "/"))


###############################################################################
## C. PRIMARY FARMER-LEVEL LATE (2SLS)
###############################################################################
## Same outcome family as ITT, but instrumenting actual use of the
## measurement & monitoring system (treat_TOT) with random assignment (treat).
##
## Result array layout (15 columns) — same as ITT:
##  1-2:   control mean, control SD
##  3-5:   treat_TOT coef, SE, p  (from 2SLS: outcome ~ treat_TOT + vid + b_outcome)
##  6-8:   vid coef, SE, p        (same model)
##  9:     N observations
##  10-11: Anderson-Sharp q-values for cols 5, 8
##  12-14: pooled treat_TOT coef, SE, p (orthogonalized 2SLS)
##  15:    pooled treat_TOT q-value for col 14

res_farmers_TOT <- array(NA, dim = c(length(outcomes), 15))

## ---------------------------------------------------------------------------
## C1. Additive 2SLS: outcome ~ treat_TOT + vid + b_outcome
##     Instruments: treat + vid + b_outcome
## ---------------------------------------------------------------------------
for (i in seq_along(outcomes)) {
  f_2sls <- as.formula(paste(outcomes[i],
    "~ treat_TOT + vid +", b_outcomes[i],
    "| treat + vid +", b_outcomes[i]))

  ivmod <- ivreg(f_2sls, data = farmers_end)
  vcov_cluster <- vcovCR(ivmod, cluster = farmers_end$catch_ID, type = "CR2")
  res <- coef_test(ivmod, vcov = vcov_cluster)

  ctrl <- farmers_end[farmers_end$vid == FALSE & farmers_end$treat == FALSE,
                      outcomes[i]]
  res_farmers_TOT[i, 1] <- mean(ctrl, na.rm = TRUE)
  res_farmers_TOT[i, 2] <- sd(ctrl, na.rm = TRUE)

  res_farmers_TOT[i, 3:5] <- c(res[2, 2], res[2, 3], res[2, 7])   # treat_TOT
  res_farmers_TOT[i, 6:8] <- c(res[3, 2], res[3, 3], res[3, 7])   # vid
  res_farmers_TOT[i, 9]   <- nobs(ivmod)
}

## Anderson-Sharp q-values (exclude index row)
idx <- 1:(length(outcomes) - 1)
res_farmers_TOT[idx, 10] <- anderson_sharp_q(res_farmers_TOT[idx, 5])
res_farmers_TOT[idx, 11] <- anderson_sharp_q(res_farmers_TOT[idx, 8])

## ---------------------------------------------------------------------------
## C2. Orthogonalized 2SLS models
## ---------------------------------------------------------------------------
## Pooled measurement & monitoring system LATE (demeaning vid)
for (i in seq_along(outcomes)) {
  f_2sls3 <- as.formula(paste(outcomes[i],
    "~ treat_TOT*vid_demeaned +", b_outcomes[i],
    "| treat*vid_demeaned +", b_outcomes[i]))

  ivmod3 <- ivreg(f_2sls3, data = farmers_end)
  vcov3  <- vcovCR(ivmod3, cluster = farmers_end$catch_ID, type = "CR2")
  res3   <- coef_test(ivmod3, vcov = vcov3)
  res_farmers_TOT[i, 12:14] <- c(res3[2, 2], res3[2, 3], res3[2, 7])
}

res_farmers_TOT[idx, 15] <- anderson_sharp_q(res_farmers_TOT[idx, 14])

res_farmers_TOT <- round(res_farmers_TOT, digits = 3)

saveRDS(res_farmers_TOT, file = paste(path, "paper/results/res_farmers_TOT.rds", sep = "/"))


###############################################################################
## D. SECONDARY FARMER OUTCOMES
###############################################################################
## Inline loops using simplified 11-column layout (no trader heterogeneity):
##
##  Secondary layout (11 columns):
##   1-2:   control mean, SD
##   3-5:   treat coef, SE, p
##   6-8:   vid coef, SE, p
##   9:     N
##   10-11: q-values for cols 5, 8
##
##  For families with baseline: outcome ~ treat + vid + b_outcome
##  For families without baseline: outcome ~ treat + vid

## Helper: run secondary outcome regressions
run_secondary <- function(outcomes, b_outcomes, data, has_baseline) {
  n_out <- length(outcomes)
  res_arr <- array(NA, dim = c(n_out, 11))

  for (i in seq_along(outcomes)) {
    if (has_baseline) {
      fml <- as.formula(paste(outcomes[i], "~ treat + vid +", b_outcomes[i]))
    } else {
      fml <- as.formula(paste(outcomes[i], "~ treat + vid"))
    }
    ols <- lm(fml, data = data)
    vcov_cl <- vcovCR(ols, cluster = data$catch_ID, type = "CR2")
    res <- coef_test(ols, vcov_cl)

    ctrl <- data[data$vid == FALSE & data$treat == FALSE, outcomes[i]]
    res_arr[i, 1]   <- mean(ctrl, na.rm = TRUE)
    res_arr[i, 2]   <- sd(ctrl, na.rm = TRUE)
    res_arr[i, 3:5] <- c(res[2, 2], res[2, 3], res[2, 7])   # treat
    res_arr[i, 6:8] <- c(res[3, 2], res[3, 3], res[3, 7])   # vid
    res_arr[i, 9]   <- nobs(ols)
  }

  ## Sharpened q-values (exclude index row = last row)
  idx <- 1:(n_out - 1)
  res_arr[idx, 10] <- anderson_sharp_q(res_arr[idx, 5])
  res_arr[idx, 11] <- anderson_sharp_q(res_arr[idx, 8])

  round(res_arr, digits = 3)
}

## ---- D1. Sales quantities (has_baseline=TRUE) -------------------
outcomes_sq   <- c("ihs_q_sold_dry", "ihs_q_sold_wet", "sold_last_week",
                    "ihs_avg_sales_q")
b_outcomes_sq <- c("ihs_b_q_sold_dry", "ihs_b_q_sold_wet", "b_sold_last_week",
                    "ihs_b_avg_sales_q")

farmers_end$secondary_farmer_quant_index  <- anderson_index(farmers_end[outcomes_sq])$index
farmers_end$b_secondary_farmer_quant_index <- anderson_index(farmers_end[b_outcomes_sq])$index

outcomes_sq   <- c(outcomes_sq, "secondary_farmer_quant_index")
b_outcomes_sq <- c(b_outcomes_sq, "b_secondary_farmer_quant_index")

res_farmers_sec_quant <- run_secondary(outcomes_sq, b_outcomes_sq, farmers_end,
                                        has_baseline = TRUE)

saveRDS(res_farmers_sec_quant,
        file = paste(path, "paper/results/res_farmers_sec_quant.rds", sep = "/"))

## ---- D2. Treatment uptake (has_baseline=FALSE) ------------------
outcomes_up <- c("recalls_video", "recalls_grass", "used_grass", "knows_comp")

farmers_end$secondary_farmer_uptake_index <- anderson_index(farmers_end[outcomes_up])$index

outcomes_up <- c(outcomes_up, "secondary_farmer_uptake_index")

res_farmers_sec_uptake <- run_secondary(outcomes_up, b_outcomes = NULL, farmers_end,
                                         has_baseline = FALSE)

saveRDS(res_farmers_sec_uptake,
        file = paste(path, "paper/results/res_farmers_sec_uptake.rds", sep = "/"))

## ---- D3. Sales/market channel (has_baseline=TRUE) ---------------
outcomes_sold   <- c("mcc_wet", "mcc_dry", "mcc_last_seek", "price_wet", "price_dry")
b_outcomes_sold <- c("b_mcc_wet", "b_mcc_dry", "b_mcc_last_seek",
                      "b_price_wet", "b_price_dry")

farmers_end$secondary_farmer_sold_index  <- anderson_index(farmers_end[outcomes_sold])$index
farmers_end$b_secondary_farmer_sold_index <- anderson_index(farmers_end[b_outcomes_sold])$index

outcomes_sold   <- c(outcomes_sold, "secondary_farmer_sold_index")
b_outcomes_sold <- c(b_outcomes_sold, "b_secondary_farmer_sold_index")

res_farmers_sec_sold <- run_secondary(outcomes_sold, b_outcomes_sold, farmers_end,
                                       has_baseline = TRUE)

saveRDS(res_farmers_sec_sold,
        file = paste(path, "paper/results/res_farmers_sec_sold.rds", sep = "/"))

## ---- D4. Switching (has_baseline=FALSE) -------------------------
outcomes_sw <- c("still_connected_yes", "still_supplying_wet", "still_supplying_dry")

farmers_end$secondary_farmer_switch_index <- anderson_index(farmers_end[outcomes_sw])$index

outcomes_sw <- c(outcomes_sw, "secondary_farmer_switch_index")

res_farmers_sec_switching <- run_secondary(outcomes_sw, b_outcomes = NULL, farmers_end,
                                            has_baseline = FALSE)

saveRDS(res_farmers_sec_switching,
        file = paste(path, "paper/results/res_farmers_sec_switching.rds", sep = "/"))


###############################################################################
## E. MCC-LEVEL ANALYSIS (Section 11)
###############################################################################
## MCC-level treatment is binary (quality measurement & monitoring system provided or not).
## CR2 cluster-robust SEs clustered at catchment level.
## No change from previous version (already single-treatment, no trader).

outcomes_mcc_e   <- c("test_MA_in", "test_MA_out", "price_bought", "avg_sales_p",
                       "gives_q_bonus", "gets_q_bonus")
b_outcomes_mcc_e <- c("b_test_MA_in", "b_test_MA_out", "b_price_bought",
                       "b_avg_sales_p", "b_gives_q_bonus", "b_gets_q_bonus")

MCCs_end$primary_MCC_index   <- anderson_index(MCCs_end[outcomes_mcc_e])$index
MCCs_end$b_primary_MCC_index <- anderson_index(MCCs_end[b_outcomes_mcc_e])$index

outcomes_mcc_e   <- c(outcomes_mcc_e, "primary_MCC_index")
b_outcomes_mcc_e <- c(b_outcomes_mcc_e, "b_primary_MCC_index")

## MCC result array: 7 outcomes x 12 columns
##  1-2:  control mean, SD
##  3-5:  OLS coef, SE, p
##  6-8:  2SLS coef, SE, p
##  9:    N (OLS)
##  10:   N (2SLS)
##  11:   q-value for OLS p
##  12:   q-value for 2SLS p

res_MCCs <- array(NA, dim = c(length(outcomes_mcc_e), 12))

for (i in seq_along(outcomes_mcc_e)) {
  ## OLS: outcome ~ treat + baseline
  ols <- lm(as.formula(paste(outcomes_mcc_e[i], "~ treat +", b_outcomes_mcc_e[i])),
            data = MCCs_end)

  y_ctrl <- MCCs_end[MCCs_end$treat == "C", outcomes_mcc_e[i]]
  res_MCCs[i, 1] <- mean(y_ctrl, na.rm = TRUE)
  res_MCCs[i, 2] <- sd(y_ctrl, na.rm = TRUE)

  ## CR2 cluster-robust SEs
  vcov_cr2 <- vcovCR(ols,
    cluster = MCCs_end[!is.na(MCCs_end[[outcomes_mcc_e[i]]]) &
                         !is.na(MCCs_end[[b_outcomes_mcc_e[i]]]), "catchment_ID"],
    type = "CR2")
  cr2_test <- coef_test(ols, vcov = vcov_cr2)[2, ]
  res_MCCs[i, 3] <- cr2_test[["beta"]]
  res_MCCs[i, 4] <- cr2_test[["SE"]]
  res_MCCs[i, 5] <- cr2_test[["p_Satt"]]
  res_MCCs[i, 9] <- nobs(ols)

  ## 2SLS: outcome ~ treat_TOT + baseline | treat + baseline
  iv_formula <- as.formula(paste0(
    outcomes_mcc_e[i], " ~ treat_TOT + ", b_outcomes_mcc_e[i],
    " | treat + ", b_outcomes_mcc_e[i]
  ))
  iv_mod <- ivreg(iv_formula, data = MCCs_end)
  vcov_iv_cr2 <- vcovCR(iv_mod,
    cluster = MCCs_end[!is.na(MCCs_end[[outcomes_mcc_e[i]]]) &
                         !is.na(MCCs_end[[b_outcomes_mcc_e[i]]]), "catchment_ID"],
    type = "CR2")
  cr2_iv_test <- coef_test(iv_mod, vcov = vcov_iv_cr2)[2, ]
  res_MCCs[i, 6]  <- cr2_iv_test[["beta"]]
  res_MCCs[i, 7]  <- cr2_iv_test[["SE"]]
  res_MCCs[i, 8]  <- cr2_iv_test[["p_Satt"]]
  res_MCCs[i, 10] <- nobs(iv_mod)
}

## Q-values computed BEFORE rounding (exclude Anderson index row)
idx_mcc <- 1:(length(outcomes_mcc_e) - 1)
res_MCCs[idx_mcc, 11] <- anderson_sharp_q(res_MCCs[idx_mcc, 5])
res_MCCs[idx_mcc, 12] <- anderson_sharp_q(res_MCCs[idx_mcc, 8])
res_MCCs <- round(res_MCCs, digits = 3)

saveRDS(res_MCCs, file = paste(path, "paper/results/res_MCCs.rds", sep = "/"))


###############################################################################
## F. MILK SAMPLE ANALYSIS (supervised quality testing)
###############################################################################
## Supervised milk quality testing at MCCs using project Lactoscans.
## Samples collected at endline with calibration correction for Ntungamo district.
##
## Result array: 6 outcomes x 12 columns
##  1-2:   control mean, SD
##  3-5:   OLS coef, SE, p (WLS weighted by Qty, CR2 clustered by catch_ID)
##  6-8:   2SLS coef, SE, p (instrument: treat for treat_TOT)
##  9:     N (OLS)
##  10:    N (2SLS)
##  11:    q-value for OLS p
##  12:    q-value for 2SLS p

samples <- read.csv(paste(path, "endline/data/public/samples.csv", sep = "/"))

## Calibration fix: Ntungamo samples before Dec 5 are excluded
samples <- samples[
  (samples$today >= as.Date("2024-12-04") &
   (samples$district == "Kazo" | samples$district == "Mbarara")) |
  samples$today >= as.Date("2024-12-05"), ]

## Merge treatment indicators (actual machine use for LATE)
samples <- merge(samples, MCCs_end[c("MCC_ID", "treat_TOT")], all.x = TRUE)

sample_outcomes <- c("Fat", "SNF", "Added.Water", "Protein",
                     "Corrected.Lactometer.Reading")
samples$samples_index <- anderson_index(samples[sample_outcomes], revcols = 3)$index
sample_outcomes <- c(sample_outcomes, "samples_index")

res_samples <- array(NA, dim = c(length(sample_outcomes), 12),
                     dimnames = list(sample_outcomes,
                       c("ctrl_mean", "ctrl_sd", "ols_coef", "ols_se", "ols_p",
                         "iv_coef", "iv_se", "iv_p", "n_ols", "n_iv",
                         "q_ols", "q_iv")))

for (i in seq_along(sample_outcomes)) {
  ## OLS: outcome ~ treat (weighted by quantity)
  ols <- lm(as.formula(paste(sample_outcomes[i], "~ treat")),
            data = samples, weights = samples$Qty)

  y_ctrl <- samples[samples$treat == "C", sample_outcomes[i]]
  res_samples[i, 1] <- mean(y_ctrl, na.rm = TRUE)
  res_samples[i, 2] <- sd(y_ctrl, na.rm = TRUE)

  vcov_ols <- vcovCR(ols, cluster = samples$catch_ID, type = "CR2")
  res <- coef_test(ols, vcov_ols)
  res_samples[i, 3:5] <- c(res[2, 2], res[2, 3], res[2, 7])
  res_samples[i, 9]   <- nobs(ols)

  ## 2SLS: outcome ~ treat_TOT | treat (weighted)
  iv_mod <- ivreg(as.formula(paste0(sample_outcomes[i], " ~ treat_TOT | treat")),
                  data = samples, weights = samples$Qty)
  vcov_iv <- vcovCR(iv_mod, cluster = samples$catch_ID, type = "CR2")
  res_iv  <- coef_test(iv_mod, vcov_iv)
  res_samples[i, 6:8] <- c(res_iv[2, 2], res_iv[2, 3], res_iv[2, 7])
  res_samples[i, 10]  <- nobs(iv_mod)
}

## Anderson-Sharp q-values (exclude index row)
idx <- 1:(length(sample_outcomes) - 1)
res_samples[idx, 11] <- anderson_sharp_q(res_samples[idx, 5])
res_samples[idx, 12] <- anderson_sharp_q(res_samples[idx, 8])
res_samples <- round(res_samples, digits = 3)

saveRDS(res_samples, file = paste(path, "paper/results/res_samples.rds", sep = "/"))


###############################################################################
## G. ATTRITION TESTS
###############################################################################

attrition <- list()

## MCC-level attrition: chi-squared test of treatment vs attrition
attrit <- merge(MCCs_base, MCCs_end[, c("MCC_ID", "treat")],
                by = "MCC_ID", all.x = TRUE)
chi_mcc <- chisq.test(table(attrit$lactoscan, is.na(attrit$treat)))
attrition$mcc_chisq_stat <- chi_mcc$statistic
attrition$mcc_chisq_p    <- chi_mcc$p.value

## Farmer-level attrition
attrit_farmers <- merge(farmers_base, farmers_end[, c("farmer_ID", "treat")],
                        by = "farmer_ID", all.x = TRUE)
attrit_farmers$attrited <- is.na(attrit_farmers$treat)

## Simple attrition ~ lactoscan
ols <- lm(attrited ~ lactoscan, data = attrit_farmers)
vcov_cluster <- vcovCR(ols, cluster = attrit_farmers$catchment_ID, type = "CR2")
res <- coef_test(ols, vcov_cluster)
attrition$farmer_simple <- res

## Factorial attrition model (for appendix): attrition ~ lactoscan * video_shown
ols <- lm(attrited ~ lactoscan * video_shown, data = attrit_farmers)
vcov_cluster <- vcovCR(ols, cluster = attrit_farmers$catchment_ID, type = "CR2")
res <- coef_test(ols, vcov_cluster)
attrition$farmer_interacted <- res

saveRDS(attrition, file = paste(path, "paper/results/attrition.rds", sep = "/"))

cat("\n02_analysis_main.R completed successfully.\n")
