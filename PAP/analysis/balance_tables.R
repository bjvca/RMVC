###############################################################################
## balance_tables.R
##
## Computes randomisation balance tables for the 2x2 factorial dairy RCT.
## Must be run with working directory set to PAP/analysis/.
## Run this BEFORE analysis.R — it saves results that the paper reads.
##
## Outputs (saved to PAP/results/):
##   balance.RData        — MCC-level balance array  (10 x 8)
##   balance_farmer.RData — farmer-level balance array (10 x 32)
##   F_test.RData         — joint F-test array (2 x 8)
###############################################################################

rm(list = ls())

library(clubSandwich)   # CR2 cluster-robust variance estimation
library(andersonTools)  # Anderson-Sharp sharpened q-values
library(car)            # linearHypothesis, Wald_test

## Auto-detect project root from working directory
path <- strsplit(getwd(), "/PAP/analysis")[[1]]

###############################################################################
## 1. MCC-LEVEL BALANCE
###############################################################################

MCC <- read.csv(paste(path, "baseline/data/public/MCCs.csv", sep = "/"))

## --- Construct balance variables from survey responses ---
## Main-table variables (5)
MCC$is_coop          <- MCC$secC_group.q8 == 2          # cooperative (vs private)
MCC$capacity         <- as.numeric(as.character(MCC$qual.q26))  # cooling capacity (litres)
MCC$quality_premium  <- { MCC$q29[MCC$q29 == "98"] <- NA; as.numeric(as.character(MCC$q29)) == 1 }
MCC$years_in_operation <- { x <- as.numeric(as.character(MCC$secC_group.q6a)); x[x > 50] <- NA; x }
MCC$supply_accaracides <- as.numeric(as.character(MCC$q18)) %in% c(1, 2)  # supplies acaricides to farmers

## Appendix-table variables (5)
MCC$full_timers      <- as.numeric(as.character(MCC$q14m))  # full-time employees
MCC$clients_rainy    <- { x <- as.numeric(as.character(MCC$Q23)); x[x > 1000 | x < 5] <- NA; x }
MCC$amount_dry       <- as.numeric(as.character(MCC$qual.q27))
MCC$capacity_use_dry <- { x <- MCC$amount_dry / MCC$capacity * 100; x[x > 100] <- NA; x }
MCC$nr_milk_cans     <- as.numeric(as.character(MCC$q14i))
MCC$supply_credit    <- as.numeric(as.character(MCC$q17)) %in% c(1, 2)

## All 10 outcomes: first 5 appear in main balance table, last 5 in appendix
outcomes_mcc <- c("is_coop", "full_timers", "clients_rainy", "capacity",
                   "capacity_use_dry", "quality_premium", "years_in_operation",
                   "nr_milk_cans", "supply_credit", "supply_accaracides")

## --- Balance array ---
## Columns: ctrl_mean | ctrl_sd | coef | se | p-val | CI_lo | CI_hi | nobs
balance <- array(NA, dim = c(length(outcomes_mcc), 8),
                 dimnames = list(outcomes_mcc,
                                 c("ctrl_mean", "ctrl_sd", "coef", "se",
                                   "pval", "ci_lo", "ci_hi", "nobs")))

for (i in seq_along(outcomes_mcc)) {
  ols  <- lm(as.formula(paste(outcomes_mcc[i], "~ lactoscan")), data = MCC)
  vcov_cluster <- vcovCR(ols, cluster = MCC$catchment_ID, type = "CR2")
  res  <- coef_test(ols, vcov_cluster)
  conf <- conf_int(ols, vcov_cluster)

  ctrl <- MCC[MCC$lactoscan == "C", outcomes_mcc[i]]
  balance[i, ] <- c(mean(ctrl, na.rm = TRUE),
                     sd(ctrl, na.rm = TRUE),
                     res[2, 2], res[2, 3], res[2, 7],   # coef, se, p
                     conf[2, 5], conf[2, 6],             # CI bounds
                     nobs(ols))
}

## --- Joint F-tests (Wald via CR2) ---
## F_test: row 1 = F-stat, row 2 = p-value
## Columns 1-2: MCC main/appendix | 3-5: farmer main | 6-8: farmer appendix
F_test <- array(NA, dim = c(2, 8),
                dimnames = list(c("Fstat", "pval"),
                                c("MCC_main", "MCC_appx",
                                  "farmer_T1", "farmer_T2", "farmer_T1xT2",
                                  "farmer_appx_T1", "farmer_appx_T2", "farmer_appx_T1xT2")))

## F-test: main table variables predict lactoscan assignment
mod <- lm((lactoscan == "T") ~ is_coop + capacity + quality_premium +
            years_in_operation + supply_accaracides, data = MCC)
W <- Wald_test(mod,
               constraints = constrain_zero(c("is_coopTRUE", "capacity",
                                              "quality_premiumTRUE",
                                              "years_in_operation",
                                              "supply_accaracidesTRUE")),
               vcov = "CR2", cluster = MCC$catchment_ID, test = "HTZ")
F_test[1, 1] <- W$Fstat
F_test[2, 1] <- W$p_val

## F-test: appendix table variables predict lactoscan assignment
mod <- lm((lactoscan == "T") ~ full_timers + clients_rainy + capacity_use_dry +
            nr_milk_cans + supply_credit, data = MCC)
W <- Wald_test(mod,
               constraints = constrain_zero(c("full_timers", "clients_rainy",
                                              "capacity_use_dry", "nr_milk_cans",
                                              "supply_creditTRUE")),
               vcov = "CR2", cluster = MCC$catchment_ID, test = "HTZ")
F_test[1, 2] <- W$Fstat
F_test[2, 2] <- W$p_val

## Round MCC balance (leave nobs as integer in col 8)
balance[, 1:7] <- round(balance[, 1:7], digits = 3)

saveRDS(balance, file = paste(path, "PAP/results/balance.RData", sep = "/"))


###############################################################################
## 2. FARMER-LEVEL BALANCE
###############################################################################

farmers <- read.csv(paste(path, "baseline/data/public/farmers.csv", sep = "/"))

## --- Construct balance variables ---
farmers$hh_size  <- as.numeric(as.character(farmers$q21))
farmers$age_head <- as.numeric(as.character(farmers$q18))
farmers$age_head[is.na(farmers$age_head)] <-
  as.numeric(as.character(farmers$q14[is.na(farmers$age_head)]))

## Herd composition: q24=local cows, q26=local heifers, q28=local calves,
##                   q30=improved cows, q32=improved heifers, q34=improved calves
## 999 = "don't know" per questionnaire instructions — treat as NA.
## Replace 999 BEFORE applying outlier thresholds so that 999 in a sibling column
## does not trigger a false threshold check on q24.
herd_cols <- c("q24", "q26", "q28", "q30", "q32", "q34")
farmers[herd_cols] <- lapply(farmers[herd_cols], function(x) {
  x <- as.numeric(as.character(x))
  replace(x, x == 999, NA)
})

## Outlier thresholds (applied after 999 removal)
farmers$q24[farmers$q24 > 500] <- NA
farmers$q24[farmers$q26 > 200] <- NA
farmers$q24[farmers$q28 > 200] <- NA
farmers$q24[farmers$q30 > 500] <- NA
farmers$q24[farmers$q32 > 200] <- NA
farmers$q24[farmers$q34 > 200] <- NA

farmers$herd_size      <- rowSums(farmers[herd_cols])
farmers$improved_share <- rowSums(farmers[c("q30", "q32", "q34")]) / farmers$herd_size

farmers$liter_day_wet      <- farmers$q44
farmers$liter_sold_day_wet <- farmers$q50
farmers$sell_MCC_wet       <- (farmers$q51 == 2)

## Uses steel/aluminium container for any delivery trip
farmers$use_steel <- (farmers$q60 %in% c("4", "6") |
                      farmers$qx7 %in% c("4", "6") |
                      farmers$qx19 %in% c("4", "6") |
                      farmers$qx31 %in% c("4", "6") |
                      farmers$qx43 %in% c("4", "6") |
                      farmers$qx55 %in% c("4", "6"))

farmers$coop_member   <- farmers$q22 == "Yes"
farmers$acaracide_exp <- as.numeric(as.character(farmers$Tick3.q74)) / 3700  # USD

## Trader indicator: farmer_ID suffix _T or _T_R means delivery via trader
farmers$trader <- ifelse(sub("^[^0-9]*[0-9]+", "", farmers$farmer_ID) %in% c("_T", "_T_R"), 1, 0)

## All 10 outcomes: first 5 in main table, last 5 in appendix
outcomes_farmer <- c("hh_size", "age_head", "herd_size", "improved_share",
                     "liter_day_wet", "liter_sold_day_wet", "sell_MCC_wet",
                     "use_steel", "coop_member", "acaracide_exp")

## Subset for "pure control vs full bundle" comparison (H5):
## keeps only (control, no video) and (treated, video) farmers
farmers_int <- farmers[(farmers$video_shown == TRUE  & farmers$lactoscan == "T") |
                       (farmers$video_shown == FALSE & farmers$lactoscan == "C"), ]

## --- Farmer balance array (10 outcomes x 32 columns) ---
##
## Column layout (mirrors the res_farmers array in analysis.R):
##  1-2:   ctrl_mean, ctrl_sd  (pure control: lactoscan=C & video=FALSE)
##  3-5:   T1 (lactoscan) coef, se, p     [from fully interacted model]
##  6-8:   T2 (video) coef, se, p         [from fully interacted model]
##  9-12:  T1xT2 interaction coef, se, p, nobs  [from fully interacted model]
##  13-15: p-values for H0: treatment effect = treatment x trader effect
##         (tests whether T1, T2, T1xT2 effects differ for traders)
##  16-18: Anderson-Sharp q-values for cols 5, 8, 11
##  19-21: T1 (pooled) coef, se, p  [demeaned orthogonal: lactoscan * vid_demeaned]
##  22-24: T2 (pooled) coef, se, p  [demeaned orthogonal: video * lactoscan_demeaned]
##  25-26: Anderson-Sharp q-values for cols 21, 24
##  27-29: "bundle" coef, se, p  [pure control vs full bundle, from farmers_int]
##  30:    bundle x trader interaction p-value
##  31:    Anderson-Sharp q-value for col 29
##  32:    (unused — placeholder for layout consistency with analysis.R)

balance_farmer <- array(NA, dim = c(length(outcomes_farmer), 32))

for (i in seq_along(outcomes_farmer)) {

  ## --- Fully interacted model: outcome ~ lactoscan * video_shown * trader ---
  ols <- lm(as.formula(paste(outcomes_farmer[i], "~ lactoscan * video_shown * trader")),
            data = farmers)
  vcov_cluster <- vcovCR(ols, cluster = farmers$catchment_ID, type = "CR2")
  res <- coef_test(ols, vcov_cluster)

  ctrl <- farmers[farmers$lactoscan == "C" & farmers$video_shown == FALSE,
                  outcomes_farmer[i]]
  balance_farmer[i, 1]    <- mean(ctrl, na.rm = TRUE)
  balance_farmer[i, 2]    <- sd(ctrl, na.rm = TRUE)
  balance_farmer[i, 3:5]  <- c(res[2, 2], res[2, 3], res[2, 7])   # T1: lactoscanT
  balance_farmer[i, 6:8]  <- c(res[3, 2], res[3, 3], res[3, 7])   # T2: video_shownTRUE
  balance_farmer[i, 9:12] <- c(res[5, 2], res[5, 3], res[5, 7],   # T1xT2 interaction
                                nobs(ols))

  ## Tests: does the treatment effect differ for traders?
  balance_farmer[i, 13] <- linearHypothesis(
    ols, "lactoscanT = lactoscanT:trader", vcov. = vcov_cluster)[[4]][2]
  balance_farmer[i, 14] <- linearHypothesis(
    ols, "video_shownTRUE = video_shownTRUE:trader", vcov. = vcov_cluster)[[4]][2]
  balance_farmer[i, 15] <- linearHypothesis(
    ols, "lactoscanT:video_shownTRUE = lactoscanT:video_shownTRUE:trader",
    vcov. = vcov_cluster)[[4]][2]

  ## --- Bundle comparison: pure control vs full bundle (farmers_int subset) ---
  ## Model: outcome ~ lactoscan * trader  (lactoscan=T implies video=TRUE here)
  ols_int <- lm(as.formula(paste(outcomes_farmer[i], "~ lactoscan * trader")),
                data = farmers_int)
  vcov_int <- vcovCR(ols_int, cluster = farmers_int$catchment_ID, type = "CR2")
  res_int  <- coef_test(ols_int, vcov_int)

  balance_farmer[i, 27:29] <- c(res_int[2, 2], res_int[2, 3], res_int[2, 7])  # bundle effect
  ## BUG FIX: was res[5,7] which is out-of-bounds for this 4-coefficient model;
  ## row 4 = lactoscanT:trader interaction
  balance_farmer[i, 30]    <- res_int[4, 7]
}

## Anderson-Sharp q-values for the fully interacted model p-values
balance_farmer[, 16] <- anderson_sharp_q(balance_farmer[, 5])    # T1 q-values
balance_farmer[, 17] <- anderson_sharp_q(balance_farmer[, 8])    # T2 q-values
balance_farmer[, 18] <- anderson_sharp_q(balance_farmer[, 11])   # T1xT2 q-values
balance_farmer[, 31] <- anderson_sharp_q(balance_farmer[, 29])   # bundle q-values

## --- Pooled models with demeaned orthogonal treatment ---
## Per Muralidharan et al.: demean the other treatment to get the marginal effect
## of one treatment pooled across levels of the other

farmers$vid_demeaned       <- farmers$video_shown - mean(farmers$video_shown, na.rm = TRUE)
farmers$lactoscan_demeaned <- (farmers$lactoscan == "T") - mean(farmers$lactoscan == "T", na.rm = TRUE)

for (i in seq_along(outcomes_farmer)) {
  ## Pooled T1 effect (demeaning video)
  ols <- lm(as.formula(paste(outcomes_farmer[i], "~ lactoscan * vid_demeaned")),
            data = farmers)
  vcov_cluster <- vcovCR(ols, cluster = farmers$catchment_ID, type = "CR2")
  res <- coef_test(ols, vcov_cluster)
  balance_farmer[i, 19:21] <- c(res[2, 2], res[2, 3], res[2, 7])

  ## Pooled T2 effect (demeaning lactoscan)
  ols <- lm(as.formula(paste(outcomes_farmer[i], "~ video_shown * lactoscan_demeaned")),
            data = farmers)
  vcov_cluster <- vcovCR(ols, cluster = farmers$catchment_ID, type = "CR2")
  res <- coef_test(ols, vcov_cluster)
  balance_farmer[i, 22:24] <- c(res[2, 2], res[2, 3], res[2, 7])
}

## Anderson-Sharp q-values for pooled p-values
balance_farmer[, 25] <- anderson_sharp_q(balance_farmer[, 21])   # pooled T1
balance_farmer[, 26] <- anderson_sharp_q(balance_farmer[, 24])   # pooled T2

balance_farmer <- round(balance_farmer, digits = 3)


###############################################################################
## 3. FARMER-LEVEL JOINT F-TESTS
###############################################################################

## Helper: run a Wald joint-significance test with CR2 clustering
run_farmer_ftest <- function(lhs_formula, rhs_vars, data) {
  mod <- lm(lhs_formula, data = data)
  W <- Wald_test(mod,
                 constraints = constrain_zero(rhs_vars),
                 vcov = "CR2", cluster = data$catchment_ID, test = "HTZ")
  c(W$Fstat, W$p_val)
}

## Main-table variables predict treatment assignment
main_vars <- c("age_head", "herd_size", "improved_share",
               "liter_sold_day_wet", "acaracide_exp")
main_rhs  <- main_vars  # all numeric, no boolean suffix needed

F_test[, 3] <- run_farmer_ftest(
  (lactoscan == "T") ~ age_head + herd_size + improved_share + liter_sold_day_wet + acaracide_exp,
  main_rhs, farmers)
F_test[, 4] <- run_farmer_ftest(
  video_shown ~ age_head + herd_size + improved_share + liter_sold_day_wet + acaracide_exp,
  main_rhs, farmers)
F_test[, 5] <- run_farmer_ftest(
  (video_shown * (lactoscan == "T")) ~ age_head + herd_size + improved_share + liter_sold_day_wet + acaracide_exp,
  main_rhs, farmers)

## Appendix-table variables predict treatment assignment
appx_rhs <- c("hh_size", "liter_day_wet", "sell_MCC_wetTRUE",
               "use_steelTRUE", "coop_memberTRUE")

F_test[, 6] <- run_farmer_ftest(
  (lactoscan == "T") ~ hh_size + liter_day_wet + sell_MCC_wet + use_steel + coop_member,
  appx_rhs, farmers)
F_test[, 7] <- run_farmer_ftest(
  video_shown ~ hh_size + liter_day_wet + sell_MCC_wet + use_steel + coop_member,
  appx_rhs, farmers)
F_test[, 8] <- run_farmer_ftest(
  (video_shown * (lactoscan == "T")) ~ hh_size + liter_day_wet + sell_MCC_wet + use_steel + coop_member,
  appx_rhs, farmers)

F_test <- round(F_test, digits = 3)


###############################################################################
## 4. SAVE
###############################################################################

saveRDS(balance_farmer, file = paste(path, "PAP/results/balance_farmer.RData", sep = "/"))
saveRDS(F_test,         file = paste(path, "PAP/results/F_test.RData", sep = "/"))
