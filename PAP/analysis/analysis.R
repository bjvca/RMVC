rm(list = ls())

# ===========================================================================
# Main Analysis Script
# ---------------------------------------------------------------------------
# Technology, Transparency, and the Emergence of Markets for Quality:
# Evidence from Ugandan Dairy Supply Chains
#
# This script estimates treatment effects for a 2x2 factorial RCT:
#   T1: Lactoscan milk analyzer (MCC-level, cluster-randomized)
#   T2: Quality awareness video (farmer-level, within-MCC)
#
# Statistical methods:
#   - ITT via OLS with CR2 cluster-robust SEs (clubSandwich)
#   - LATE via 2SLS (AER::ivreg), instrumenting actual use with assignment
#   - Orthogonalized factorial models (Muralidharan et al.)
#   - Anderson-Sharp sharpened q-values for multiple testing
#   - Anderson indices as summary measures across outcome families
#
# Output: .RData files in PAP/results/, forest plots as .png/.eps
# ===========================================================================

library(clubSandwich)
library(andersonTools)
library(car)
library(lubridate)
library(ggplot2)

# ---------------------------------------------------------------------------
# Inverse hyperbolic sine transform for skewed outcomes
# ---------------------------------------------------------------------------
ihs <- function(x) {
  log(x + sqrt(x^2 + 1))
}

# ---------------------------------------------------------------------------
# Detect project root from working directory
# ---------------------------------------------------------------------------
path <- strsplit(getwd(), "/PAP/analysis")[[1]]

# ===========================================================================
# SECTION 1: DATA LOADING AND PREPARATION
# ===========================================================================

### Read endline and baseline farmer data
farmers_end  <- read.csv(paste(path, "endline/data/public/farmers.csv", sep = "/"))
farmers_end  <- subset(farmers_end, check.consent == "Yes")
farmers_base <- read.csv(paste(path, "baseline/data/public/farmers.csv", sep = "/"))

### Read endline and baseline MCC data
MCCs_end  <- read.csv(paste(path, "endline/data/public/MCCs.csv", sep = "/"))
MCCs_end  <- subset(MCCs_end, consent == 1)
MCCs_base <- read.csv(paste(path, "baseline/data/public/MCCs.csv", sep = "/"))

### Clustering variable for farmer-level regressions
farmers_end$catch_ID <- as.factor(farmers_end$catchment)

### Treatment indicators (MCC-level assignment and farmer-level video)
farmers_end$treat <- farmers_end$treat == "T"

table(farmers_end$treat, farmers_end$vid)

### Keep only farmers linked to consenting MCCs
farmers_end <- subset(farmers_end, MCC_ID_linked %in% MCCs_end$MCC_ID)

### Trader indicator from farmer_ID suffix (baseline)
farmers_base$trader <- ifelse(
  sub("^[^0-9]*[0-9]+", "", farmers_base$farmer_ID) %in% c("_T", "_T_R"), 1, 0
)

table(farmers_end$trader)

# ===========================================================================
# SECTION 2: ATTRITION CHECKS
# ===========================================================================

### MCC-level attrition: chi-squared test of treatment vs attrition
attrit <- merge(MCCs_base, MCCs_end, by = "MCC_ID", all.x = TRUE)
chisq.test(table(attrit$lactoscan, is.na(attrit$treat)))

### Farmer-level attrition
attrit_farmers <- merge(farmers_base, farmers_end, by = "farmer_ID", all.x = TRUE)
attrit_farmers$attrited <- is.na(attrit_farmers$treat)

## Simple attrition ~ lactoscan
ols <- lm(attrited ~ lactoscan, data = attrit_farmers)
vcov_cluster <- vcovCR(ols, cluster = attrit_farmers$catchment_ID, type = "CR2")
res <- coef_test(ols, vcov_cluster)

## Fully interacted attrition model
ols <- lm(attrited ~ lactoscan * video_shown * trader, data = attrit_farmers)
vcov_cluster <- vcovCR(ols, cluster = attrit_farmers$catchment_ID, type = "CR2")
res <- coef_test(ols, vcov_cluster)

# ===========================================================================
# SECTION 3: CONSTRUCT OUTCOME VARIABLES
# ===========================================================================

## Rename endline farmer_type to avoid collision with baseline
names(farmers_end)[names(farmers_end) == "farmer_type"] <- "farmer_type_end"

## Merge baseline farmer_type for heterogeneity analysis
farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "farmer_type")],
                     by = "farmer_ID", all.x = TRUE)
farmers_end$trader <- farmers_end$farmer_type == 2

## Override with ID-based trader classification (more reliable)
farmers_end$trader <- ifelse(
  sub("^[^0-9]*[0-9]+", "", farmers_end$farmer_ID) %in% c("_T", "_T_R"), 1, 0
)

# ---------------------------------------------------------------------------
# 3a. Improved practices index (primary outcome family)
# ---------------------------------------------------------------------------
# q39 = oversowing, q39c = legume pastures, q39d = trees
# q40 in (1,3) = controlled/zero grazing, q42 = pasture conservation, q43 = supplements

farmers_end$q39c[farmers_end$q39c == "98"] <- NA
farmers_end$q39d[farmers_end$q39d == "98"] <- NA
farmers_end$q42[farmers_end$q42 == "98"]   <- NA
farmers_end$q40[farmers_end$q40 == "96"]   <- NA
farmers_end$q41[farmers_end$q41 == "99" | farmers_end$q41 == "96"] <- NA

farmers_base$b_improve_index <- anderson_index(cbind(
  farmers_base$q39 == "Yes", farmers_base$q39c == "Yes",
  farmers_base$q40 %in% c(1, 3), farmers_base$q42 == "Yes",
  farmers_base$q43 == "Yes"
))$index

farmers_end$improve_index <- anderson_index(cbind(
  farmers_end$q39 == "Yes", farmers_end$q39c == "Yes",
  farmers_end$q40 %in% c(1, 3), farmers_end$q42 == "Yes",
  farmers_end$q43 == "Yes"
))$index

farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_improve_index")],
                     by = "farmer_ID", all.x = TRUE)

# ---------------------------------------------------------------------------
# 3b. Buyer checks using milk analyzer
# ---------------------------------------------------------------------------
# check_MA = TRUE if buyer used milk analyzer for any transaction in last 7 days
farmers_end$check_MA <- farmers_end$q58.4 == "True" | farmers_end$qx5.4 == "True" |
  farmers_end$qx17.4 == "True" | farmers_end$qx29.4 == "True" |
  farmers_end$qx41.4 == "True" | farmers_end$qx53.4 == "True"
farmers_end$check_MA[farmers_end$q52 == "n/a"] <- NA
farmers_end$check_MA[farmers_end$q52 == "No"]  <- NA

farmers_base$b_check_MA <- farmers_base$q58 == "4" | farmers_base$qx5.4 == "True" |
  farmers_base$qx17.4 == "True" | farmers_base$qx29.4 == "True" |
  farmers_base$qx41.4 == "True" | farmers_base$qx53.4 == "True"
farmers_base$b_check_MA[farmers_base$q52 == "No"] <- NA

farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_check_MA")],
                     by = "farmer_ID", all.x = TRUE)

# ---------------------------------------------------------------------------
# 3c. Average sales price (quantity-weighted across buyers in last 7 days)
# ---------------------------------------------------------------------------
# Price columns: q55/qx2/qx14/qx26/qx38/qx50
# Quantity columns: q54/qx1/qx13/qx25/qx37/qx49

columns   <- c("q55", "qx2", "qx14", "qx26", "qx38", "qx50")
columns_q <- c("q54", "qx1", "qx13", "qx25", "qx37", "qx49")

## Clean price columns
farmers_end[columns] <- lapply(farmers_end[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA
  as.numeric(as.character(x))
})
farmers_base[columns] <- lapply(farmers_base[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA
  as.numeric(as.character(x))
})

## Clean quantity columns
farmers_end[columns_q]  <- lapply(farmers_end[columns_q],  function(x) as.numeric(as.character(x)))
farmers_base[columns_q] <- lapply(farmers_base[columns_q], function(x) as.numeric(as.character(x)))

## Quantity-weighted average price
farmers_end$avg_sales_p <- rowSums(farmers_end[columns] * farmers_end[columns_q], na.rm = TRUE) /
  rowSums(farmers_end[columns_q], na.rm = TRUE)
farmers_end$avg_sales_p[is.nan(farmers_end$avg_sales_p)] <- NA

farmers_base$b_avg_sales_p <- rowSums(farmers_base[columns] * farmers_base[columns_q], na.rm = TRUE) /
  rowSums(farmers_base[columns_q], na.rm = TRUE)
farmers_base$b_avg_sales_p[is.nan(farmers_base$b_avg_sales_p)] <- NA

## Remove outliers
farmers_base$b_avg_sales_p[farmers_base$b_avg_sales_p >= 1500] <- NA
farmers_base$b_avg_sales_p[farmers_base$b_avg_sales_p <= 600]  <- NA
farmers_end$avg_sales_p[farmers_end$avg_sales_p >= 1500] <- NA
farmers_end$avg_sales_p[farmers_end$avg_sales_p <= 600]  <- NA

farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_avg_sales_p")],
                     by = "farmer_ID", all.x = TRUE)

# ---------------------------------------------------------------------------
# 3d. Quality bonus from buyer
# ---------------------------------------------------------------------------
# q61/qx8/qx20/qx32/qx44/qx56 = "Does the buyer pay for higher quality milk?"

## BUG FIX: line 186 originally had farmers_end$qx8[farmers_end$qx44 == "98"] <- NA
## which incorrectly set qx8 to NA based on qx44's value. Fixed to clean qx44 directly.
farmers_end$qx8[farmers_end$qx8 == "98"]   <- NA
farmers_end$qx44[farmers_end$qx44 == "98"] <- NA

farmers_end$gets_q_bonus <- farmers_end$q61 == "Yes" | farmers_end$qx8 == "Yes" |
  farmers_end$qx20 == "Yes" | farmers_end$qx32 == "Yes" |
  farmers_end$qx44 == "Yes" | farmers_end$qx56 == "Yes"
farmers_end$gets_q_bonus[farmers_end$q52 == "n/a" | farmers_end$q52 == "No"] <- NA

farmers_base$q61[farmers_base$q61 == "98"]   <- NA
farmers_base$qx8[farmers_base$qx8 == "98"]   <- NA
farmers_base$qx44[farmers_base$qx44 == "98"] <- NA

farmers_base$b_gets_q_bonus <- farmers_base$q61 == "Yes" | farmers_base$qx8 == "Yes" |
  farmers_base$qx20 == "Yes" | farmers_base$qx32 == "Yes" |
  farmers_base$qx44 == "Yes" | farmers_base$qx56 == "Yes"
farmers_base$b_gets_q_bonus[farmers_base$q52 == "No"] <- NA

farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_gets_q_bonus")],
                     by = "farmer_ID", all.x = TRUE)

# ---------------------------------------------------------------------------
# 3e. Bargaining power
# ---------------------------------------------------------------------------
# q65/qx12/qx24/qx36/qx48/qx60: farmer sets price or negotiates (codes 1, 3)
farmers_end$bargain_power <- farmers_end$q65 %in% c(1, 3) |
  farmers_end$qx12 %in% c(1, 3) | farmers_end$qx24 %in% c(1, 3) |
  farmers_end$qx36 %in% c(1, 3) | farmers_end$qx48 %in% c(1, 3) |
  farmers_end$qx60 %in% c(1, 3)
farmers_end$bargain_power[farmers_end$q52 == "n/a" | farmers_end$q52 == "No"] <- NA

farmers_base$b_bargain_power <- farmers_base$q65 %in% c(1, 3) |
  farmers_base$qx12 %in% c(1, 3) | farmers_base$qx24 %in% c(1, 3) |
  farmers_base$qx36 %in% c(1, 3) | farmers_base$qx48 %in% c(1, 3) |
  farmers_base$qx60 %in% c(1, 3)
farmers_base$b_bargain_power[farmers_base$q52 == "No"] <- NA

farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_bargain_power")],
                     by = "farmer_ID", all.x = TRUE)

# ---------------------------------------------------------------------------
# 3f. LATE instrument: actual machine use at MCC
# ---------------------------------------------------------------------------
MCCs_end$treat_TOT <- MCCs_end$machine_in_use %in% 1:2
farmers_end <- merge(farmers_end, MCCs_end[c("MCC_ID", "treat_TOT")],
                     by.x = "MCC_ID_linked", by.y = "MCC_ID", all.x = TRUE)

# ===========================================================================
# SECTION 4: PRIMARY FARMER-LEVEL ITT
# ===========================================================================
# Outcome family: improve_index, check_MA, avg_sales_p, gets_q_bonus
# Plus Anderson index combining these four
#
# Result array layout (33 columns):
#  1-2:   control mean, control SD
#  3-5:   treat coefficient, SE, p-value (full model)
#  6-8:   vid coefficient, SE, p-value (full model)
#  9-11:  treat:vid interaction coefficient, SE, p-value (full model)
#  12:    N observations
#  13:    heterogeneity p-value: treat effect differs by trader
#  14:    heterogeneity p-value: vid effect differs by trader
#  16:    sharpened q-value for treat p (col 5)
#  17:    sharpened q-value for vid p (col 8)
#  18:    sharpened q-value for interaction p (col 11)
#  19-21: orthogonalized treat coefficient, SE, p (demeaned vid)
#  22-24: orthogonalized vid coefficient, SE, p (demeaned treat)
#  25:    sharpened q-value for orthog treat p (col 21)
#  26:    sharpened q-value for orthog vid p (col 24)
#  27-29: bundle (control vs both-treated) coefficient, SE, p
#  33:    heterogeneity p-value: treat:vid interaction differs by trader

outcomes   <- c("improve_index", "check_MA", "avg_sales_p", "gets_q_bonus")
b_outcomes <- c("b_improve_index", "b_check_MA", "b_avg_sales_p", "b_gets_q_bonus")

### Anderson index combining primary outcomes
farmers_end$primary_farmer_index   <- anderson_index(farmers_end[outcomes])$index
farmers_end$b_primary_farmer_index <- anderson_index(farmers_end[b_outcomes])$index

outcomes   <- c(outcomes, "primary_farmer_index")
b_outcomes <- c(b_outcomes, "b_primary_farmer_index")

### Subset for bundle comparison: both treated vs both control
farmers_end_int <- farmers_end[
  (farmers_end$vid == TRUE & farmers_end$treat == TRUE) |
  (farmers_end$vid == FALSE & farmers_end$treat == FALSE), ]

# ---------------------------------------------------------------------------
# 4a. Fully interacted ITT model: outcome ~ treat*vid*trader + baseline
# ---------------------------------------------------------------------------
# With baseline control, coefficient ordering is:
#   1=(Intercept), 2=treat, 3=vid, 4=trader, 5=b_outcome,
#   6=treat:vid, 7=treat:trader, 8=vid:trader, 9=treat:vid:trader

res_farmers <- array(NA, dim = c(length(outcomes), 33))

for (i in seq_along(outcomes)) {
  ## Full interaction model with baseline control
  ols <- lm(as.formula(paste(outcomes[i], "~ treat*vid*trader +", b_outcomes[i])),
            data = farmers_end)
  vcov_cluster <- vcovCR(ols, cluster = farmers_end$catch_ID, type = "CR2")
  res <- coef_test(ols, vcov_cluster)

  ## Control group mean and SD
  ctrl <- farmers_end[farmers_end$vid == FALSE & farmers_end$treat == FALSE, outcomes[i]]
  res_farmers[i, 1] <- mean(ctrl, na.rm = TRUE)
  res_farmers[i, 2] <- sd(ctrl, na.rm = TRUE)

  ## Treatment effects: treat (row 2), vid (row 3), treat:vid (row 6)
  res_farmers[i, 3:5]   <- c(res[2, 2], res[2, 3], res[2, 7])
  res_farmers[i, 6:8]   <- c(res[3, 2], res[3, 3], res[3, 7])
  res_farmers[i, 9:12]  <- c(res[6, 2], res[6, 3], res[6, 7], nobs(ols))

  ## Heterogeneity tests: does effect differ by trader status?
  res_farmers[i, 13] <- linearHypothesis(ols, "treatTRUE = treatTRUE:trader",
                                         vcov. = vcov_cluster)[[4]][2]
  res_farmers[i, 14] <- linearHypothesis(ols, "vidTRUE = vidTRUE:trader",
                                         vcov. = vcov_cluster)[[4]][2]
  res_farmers[i, 33] <- linearHypothesis(ols, "treatTRUE:vidTRUE = treatTRUE:vidTRUE:trader",
                                         vcov. = vcov_cluster)[[4]][2]

  ## Bundle comparison: control vs both-treated
  ## Uses subset where (treat=T, vid=T) or (treat=F, vid=F)
  ols_int <- lm(as.formula(paste(outcomes[i], "~ treat +", b_outcomes[i])),
                data = farmers_end_int)
  vcov_int <- vcovCR(ols_int, cluster = farmers_end_int$catch_ID, type = "CR2")
  res_int  <- coef_test(ols_int, vcov_int)
  res_farmers[i, 27:29] <- c(res_int[2, 2], res_int[2, 3], res_int[2, 7])
}

## Anderson-Sharp sharpened q-values (exclude index row = last row)
## BUG FIX: original used 1:length(outcomes)-1 which is (1:n)-1 = 0:(n-1).
## R silently drops row 0, giving correct result by accident. Fixed for clarity.
idx <- 1:(length(outcomes) - 1)
res_farmers[idx, 16] <- anderson_sharp_q(res_farmers[idx, 5])
res_farmers[idx, 17] <- anderson_sharp_q(res_farmers[idx, 8])
res_farmers[idx, 18] <- anderson_sharp_q(res_farmers[idx, 11])

# ---------------------------------------------------------------------------
# 4b. Orthogonalized (demeaned) treatment models (Muralidharan et al.)
# ---------------------------------------------------------------------------
# Demean the other treatment to get the marginal effect of each treatment
# pooling across levels of the other treatment

farmers_end$trader_demeaned <- farmers_end$trader - mean(farmers_end$trader, na.rm = TRUE)
farmers_end$vid_demeaned    <- farmers_end$vid    - mean(farmers_end$vid, na.rm = TRUE)
farmers_end$treat_demeaned  <- farmers_end$treat  - mean(farmers_end$treat, na.rm = TRUE)

## Pooled analyzer effect (demeaning vid)
for (i in seq_along(outcomes)) {
  ols <- lm(as.formula(paste(outcomes[i], "~ treat*vid_demeaned +", b_outcomes[i])),
            data = farmers_end)
  vcov_cluster <- vcovCR(ols, cluster = farmers_end$catch_ID, type = "CR2")
  res <- coef_test(ols, vcov_cluster)
  res_farmers[i, 19:21] <- c(res[2, 2], res[2, 3], res[2, 7])
}

## Pooled video effect (demeaning treat)
for (i in seq_along(outcomes)) {
  ols <- lm(as.formula(paste(outcomes[i], "~ vid*treat_demeaned +", b_outcomes[i])),
            data = farmers_end)
  vcov_cluster <- vcovCR(ols, cluster = farmers_end$catch_ID, type = "CR2")
  res <- coef_test(ols, vcov_cluster)
  res_farmers[i, 22:24] <- c(res[2, 2], res[2, 3], res[2, 7])
}

res_farmers[idx, 25] <- anderson_sharp_q(res_farmers[idx, 21])
res_farmers[idx, 26] <- anderson_sharp_q(res_farmers[idx, 24])

res_farmers <- round(res_farmers, digits = 3)
res_farmers_prim <- res_farmers  # save for forest plots later

saveRDS(res_farmers, file = paste(path, "PAP/results/res_farmers.RData", sep = "/"))

# ===========================================================================
# SECTION 5: PRIMARY FARMER-LEVEL 2SLS / LATE
# ===========================================================================
# Same outcome family as Section 4, but instrumenting actual machine use
# (treat_TOT) with random assignment (treat).
# Result array layout matches the ITT (33 columns).

library(AER)

res_farmers_TOT <- array(NA, dim = c(length(outcomes), 33))

# ---------------------------------------------------------------------------
# 5a. Fully interacted 2SLS: outcome ~ treat_TOT*vid*trader + baseline
#     Instruments: treat*vid*trader + baseline
# ---------------------------------------------------------------------------
# Coefficient ordering same as OLS: row 2=treat_TOT, 3=vid, 6=treat_TOT:vid

for (i in seq_along(outcomes)) {
  f_rhs  <- paste("treat_TOT*vid*trader", b_outcomes[i], sep = "+")
  f_iv   <- paste("treat*vid*trader", b_outcomes[i], sep = "+")
  f_2sls <- as.formula(paste(outcomes[i], "~", f_rhs, "|", f_iv))

  ivmod <- ivreg(f_2sls, data = farmers_end)
  vcov_cluster <- vcovCR(ivmod, cluster = farmers_end$catch_ID, type = "CR2")
  res <- coef_test(ivmod, vcov = vcov_cluster)

  ctrl <- farmers_end[farmers_end$vid == FALSE & farmers_end$treat == FALSE, outcomes[i]]
  res_farmers_TOT[i, 1] <- mean(ctrl, na.rm = TRUE)
  res_farmers_TOT[i, 2] <- sd(ctrl, na.rm = TRUE)

  res_farmers_TOT[i, 3:5]   <- c(res[2, 2], res[2, 3], res[2, 7])
  res_farmers_TOT[i, 6:8]   <- c(res[3, 2], res[3, 3], res[3, 7])
  res_farmers_TOT[i, 9:12]  <- c(res[6, 2], res[6, 3], res[6, 7], nobs(ivmod))

  res_farmers_TOT[i, 13] <- linearHypothesis(
    ivmod, "treat_TOTTRUE = treat_TOTTRUE:trader", vcov. = vcov_cluster)[[4]][2]
  res_farmers_TOT[i, 14] <- linearHypothesis(
    ivmod, "vidTRUE = vidTRUE:trader", vcov. = vcov_cluster)[[4]][2]
  res_farmers_TOT[i, 33] <- linearHypothesis(
    ivmod, "treat_TOTTRUE:vidTRUE = treat_TOTTRUE:vidTRUE:trader",
    vcov. = vcov_cluster)[[4]][2]

  ## Bundle comparison: control vs both-treated (2SLS)
  f_rhs2  <- paste("treat_TOT", b_outcomes[i], sep = "+")
  f_iv2   <- paste("treat", b_outcomes[i], sep = "+")
  f_2sls2 <- as.formula(paste(outcomes[i], "~", f_rhs2, "|", f_iv2))

  ivmod2 <- ivreg(f_2sls2, data = farmers_end_int)
  vcov2  <- vcovCR(ivmod2, cluster = farmers_end_int$catch_ID, type = "CR2")
  res2   <- coef_test(ivmod2, vcov = vcov2)
  res_farmers_TOT[i, 27:29] <- c(res2[2, 2], res2[2, 3], res2[2, 7])
}

idx <- 1:(length(outcomes) - 1)
res_farmers_TOT[idx, 16] <- anderson_sharp_q(res_farmers_TOT[idx, 5])
res_farmers_TOT[idx, 17] <- anderson_sharp_q(res_farmers_TOT[idx, 8])
res_farmers_TOT[idx, 18] <- anderson_sharp_q(res_farmers_TOT[idx, 11])

# ---------------------------------------------------------------------------
# 5b. Orthogonalized 2SLS models
# ---------------------------------------------------------------------------
## Pooled analyzer LATE (demeaning vid)
for (i in seq_along(outcomes)) {
  f_rhs3  <- paste("treat_TOT*vid_demeaned", b_outcomes[i], sep = "+")
  f_iv3   <- paste("treat*vid_demeaned", b_outcomes[i], sep = "+")
  f_2sls3 <- as.formula(paste(outcomes[i], "~", f_rhs3, "|", f_iv3))

  ivmod3 <- ivreg(f_2sls3, data = farmers_end)
  vcov3  <- vcovCR(ivmod3, cluster = farmers_end$catch_ID, type = "CR2")
  res3   <- coef_test(ivmod3, vcov = vcov3)
  res_farmers_TOT[i, 19:21] <- c(res3[2, 2], res3[2, 3], res3[2, 7])
}

## Pooled video LATE (demeaning treat_TOT)
farmers_end$treat_TOT_demeaned <- farmers_end$treat_TOT - mean(farmers_end$treat_TOT, na.rm = TRUE)

for (i in seq_along(outcomes)) {
  f_rhs4  <- paste("vid*treat_TOT_demeaned", b_outcomes[i], sep = "+")
  f_iv4   <- paste("vid*treat_demeaned", b_outcomes[i], sep = "+")
  f_2sls4 <- as.formula(paste(outcomes[i], "~", f_rhs4, "|", f_iv4))

  ivmod4 <- ivreg(f_2sls4, data = farmers_end)
  vcov4  <- vcovCR(ivmod4, cluster = farmers_end$catch_ID, type = "CR2")
  res4   <- coef_test(ivmod4, vcov = vcov4)
  res_farmers_TOT[i, 22:24] <- c(res4[2, 2], res4[2, 3], res4[2, 7])
}

res_farmers_TOT[idx, 25] <- anderson_sharp_q(res_farmers_TOT[idx, 21])
res_farmers_TOT[idx, 26] <- anderson_sharp_q(res_farmers_TOT[idx, 24])

res_farmers_TOT <- round(res_farmers_TOT, digits = 3)

saveRDS(res_farmers_TOT, file = paste(path, "PAP/results/res_farmers_TOT.RData", sep = "/"))

# ===========================================================================
# HELPER: run_farmer_regressions()
# ---------------------------------------------------------------------------
# Runs the full set of farmer-level regressions for a given outcome family:
#   (a) Fully interacted model: outcome ~ treat*vid*trader [+ baseline]
#   (b) Direct comparison (bundle): control vs both-treated [+ baseline]
#   (c) Demeaned orthogonal models with trader heterogeneity
#
# Arguments:
#   outcomes, b_outcomes : character vectors of outcome / baseline names
#   data_full, data_int  : full sample and both-treated-vs-control subset
#   has_baseline         : whether baseline controls are available
#   ncols                : number of result columns (32 or 33)
#
# Returns: rounded result array
# ---------------------------------------------------------------------------
# Column layout for 32-col arrays (secondary with baseline):
#  1-2:   control mean, SD
#  3-5:   treat (coef, SE, p)
#  6-8:   vid (coef, SE, p)
#  9-11:  treat:vid interaction (coef, SE, p)
#  12:    N
#  13:    het p: treat by trader
#  14:    het p: vid by trader
#  15:    het p: treat:vid by trader
#  16-18: q-values for cols 5, 8, 11
#  19-21: orthog treat (coef, SE, p) — demeaned vid & trader
#  22-24: orthog vid (coef, SE, p) — demeaned treat & trader
#  25-26: q-values for cols 21, 24
#  27-29: bundle (coef, SE, p)
#  30:    bundle het p: treat by trader
#  31:    orthog treat het p: treat by trader (demeaned)
#  32:    orthog vid het p: vid by trader (demeaned)
#
# Column layout for 33-col arrays (secondary without baseline):
#  Same as 32-col except het p for treat:vid goes in col 33 (not 15)
# ===========================================================================

run_farmer_regressions <- function(outcomes, b_outcomes, data_full, data_int,
                                   has_baseline, ncols) {
  n_out <- length(outcomes)
  res_arr <- array(NA, dim = c(n_out, ncols))

  ## Coefficient row for treat:vid depends on whether baseline is in the model
  ## With baseline: row 6 (intercept, treat, vid, trader, b_outcome, treat:vid, ...)
  ## Without baseline: row 5 (intercept, treat, vid, trader, treat:vid, ...)
  int_row <- if (has_baseline) 6 else 5

  ## Column for treat:vid heterogeneity test
  het_int_col <- if (ncols == 33) 33 else 15

  for (i in seq_along(outcomes)) {
    ## --- (a) Fully interacted model ---
    if (has_baseline) {
      fml <- as.formula(paste(outcomes[i], "~ treat*vid*trader +", b_outcomes[i]))
    } else {
      fml <- as.formula(paste(outcomes[i], "~ treat*vid*trader"))
    }
    ols <- lm(fml, data = data_full)
    vcov_cl <- vcovCR(ols, cluster = data_full$catch_ID, type = "CR2")
    res <- coef_test(ols, vcov_cl)

    ctrl <- data_full[data_full$vid == FALSE & data_full$treat == FALSE, outcomes[i]]
    res_arr[i, 1]     <- mean(ctrl, na.rm = TRUE)
    res_arr[i, 2]     <- sd(ctrl, na.rm = TRUE)
    res_arr[i, 3:5]   <- c(res[2, 2], res[2, 3], res[2, 7])         # treat
    res_arr[i, 6:8]   <- c(res[3, 2], res[3, 3], res[3, 7])         # vid
    res_arr[i, 9:12]  <- c(res[int_row, 2], res[int_row, 3],        # treat:vid
                            res[int_row, 7], nobs(ols))

    res_arr[i, 13] <- linearHypothesis(ols, "treatTRUE = treatTRUE:trader",
                                       vcov. = vcov_cl)[[4]][2]
    res_arr[i, 14] <- linearHypothesis(ols, "vidTRUE = vidTRUE:trader",
                                       vcov. = vcov_cl)[[4]][2]
    res_arr[i, het_int_col] <- linearHypothesis(
      ols, "treatTRUE:vidTRUE = treatTRUE:vidTRUE:trader",
      vcov. = vcov_cl)[[4]][2]

    ## --- (b) Bundle comparison: control vs both-treated ---
    ## BUG FIX: switching section originally referenced stale b_outcomes from
    ## the previous (sales) section. Now correctly omits baseline when unavailable.
    if (has_baseline) {
      ## With baseline: row 5 = treat:trader (intercept, treat, trader, b_outcome, treat:trader)
      ols_int <- lm(as.formula(paste(outcomes[i], "~ treat*trader +", b_outcomes[i])),
                    data = data_int)
      trader_row <- 5
    } else {
      ## Without baseline: row 4 = treat:trader (intercept, treat, trader, treat:trader)
      ols_int <- lm(as.formula(paste(outcomes[i], "~ treat*trader")),
                    data = data_int)
      trader_row <- 4
    }
    vcov_int <- vcovCR(ols_int, cluster = data_int$catch_ID, type = "CR2")
    res_int  <- coef_test(ols_int, vcov_int)
    res_arr[i, 27:29] <- c(res_int[2, 2], res_int[2, 3], res_int[2, 7])
    res_arr[i, 30]    <- res_int[trader_row, 7]
  }

  ## Sharpened q-values for full model (exclude index row)
  idx <- 1:(n_out - 1)
  res_arr[idx, 16] <- anderson_sharp_q(res_arr[idx, 5])
  res_arr[idx, 17] <- anderson_sharp_q(res_arr[idx, 8])
  res_arr[idx, 18] <- anderson_sharp_q(res_arr[idx, 11])

  ## --- (c) Demeaned orthogonal models with trader heterogeneity ---
  ## treat*vid_demeaned*trader_demeaned (without baseline for secondary,
  ## keeping consistent with original design)
  ## Row 6 = treat:trader_demeaned interaction p-value (in 8-coef model)
  ## Row 2 = treat coefficient (pooled over demeaned vid & trader)

  for (i in seq_along(outcomes)) {
    ols_dm <- lm(as.formula(paste(outcomes[i],
                                  "~ treat*vid_demeaned*trader_demeaned")),
                 data = data_full)
    vcov_dm <- vcovCR(ols_dm, cluster = data_full$catch_ID, type = "CR2")
    res_dm  <- coef_test(ols_dm, vcov_dm)
    res_arr[i, 19:21] <- c(res_dm[2, 2], res_dm[2, 3], res_dm[2, 7])
    res_arr[i, 31]    <- res_dm[6, 7]  # treat:trader_demeaned het p
  }

  for (i in seq_along(outcomes)) {
    ols_dm <- lm(as.formula(paste(outcomes[i],
                                  "~ vid*treat_demeaned*trader_demeaned")),
                 data = data_full)
    vcov_dm <- vcovCR(ols_dm, cluster = data_full$catch_ID, type = "CR2")
    res_dm  <- coef_test(ols_dm, vcov_dm)
    res_arr[i, 22:24] <- c(res_dm[2, 2], res_dm[2, 3], res_dm[2, 7])
    res_arr[i, 32]    <- res_dm[6, 7]  # vid:treat_demeaned het p
  }

  ## BUG FIX: original secondary sections computed q-values from wrong columns.
  ## Cols 18 (already a q-value) were used instead of cols 21 (raw demeaned p).
  ## Fixed to use cols 21 and 24, matching the primary section's logic.
  res_arr[idx, 25] <- anderson_sharp_q(res_arr[idx, 21])
  res_arr[idx, 26] <- anderson_sharp_q(res_arr[idx, 24])

  round(res_arr, digits = 3)
}

# ===========================================================================
# SECTION 6: SECONDARY OUTCOMES — SALES QUANTITIES
# ===========================================================================

## Sold in last week (q52)
farmers_end$q52[farmers_end$q52 == "n/a"] <- NA
farmers_end$sold_last_week <- farmers_end$q52 == "Yes"
farmers_base$b_sold_last_week <- farmers_base$q52 == "Yes"
farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_sold_last_week")],
                     by = "farmer_ID", all.x = TRUE)

## Volumes sold in dry/rainy season (q50, q50x)
columns <- c("q50", "q50x")
farmers_end[columns] <- lapply(farmers_end[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA
  as.numeric(as.character(x))
})
farmers_base[columns] <- lapply(farmers_base[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA
  as.numeric(as.character(x))
})

farmers_end$q_sold_dry    <- farmers_end$q50
farmers_base$b_q_sold_dry <- farmers_base$q50
farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_q_sold_dry")],
                     by = "farmer_ID", all.x = TRUE)

farmers_end$q_sold_wet    <- farmers_end$q50x
farmers_base$b_q_sold_wet <- farmers_base$q50x
farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_q_sold_wet")],
                     by = "farmer_ID", all.x = TRUE)

## Average quantity sold per transaction (last 7 days)
columns <- c("q54", "qx1", "qx13", "qx25", "qx37", "qx49")
farmers_end[columns] <- lapply(farmers_end[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA
  as.numeric(as.character(x))
})
farmers_base[columns] <- lapply(farmers_base[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA
  as.numeric(as.character(x))
})

farmers_end$avg_sales_q <- rowMeans(farmers_end[columns], na.rm = TRUE)
farmers_end$avg_sales_q[is.nan(farmers_end$avg_sales_q)] <- NA
farmers_end$avg_sales_q[is.na(farmers_end$avg_sales_q)]  <- 0
farmers_end$avg_sales_q[is.na(farmers_end$q52)]          <- NA

farmers_base$b_avg_sales_q <- rowMeans(farmers_base[columns], na.rm = TRUE)
farmers_base$b_avg_sales_q[is.nan(farmers_base$b_avg_sales_q)] <- NA
farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_avg_sales_q")],
                     by = "farmer_ID", all.x = TRUE)

## Remove outliers (> 500 liters)
farmers_end$q_sold_dry[farmers_end$q_sold_dry > 500]     <- NA
farmers_end$q_sold_wet[farmers_end$q_sold_wet > 500]     <- NA
farmers_end$avg_sales_q[farmers_end$avg_sales_q > 500]   <- NA
farmers_end$b_q_sold_dry[farmers_end$b_q_sold_dry > 500] <- NA
farmers_end$b_q_sold_wet[farmers_end$b_q_sold_wet > 500] <- NA
farmers_end$b_avg_sales_q[farmers_end$b_avg_sales_q > 500] <- NA

## IHS transform for skewed volume outcomes
farmers_end$ihs_q_sold_dry   <- ihs(farmers_end$q_sold_dry)
farmers_end$ihs_q_sold_wet   <- ihs(farmers_end$q_sold_wet)
farmers_end$ihs_avg_sales_q  <- ihs(farmers_end$avg_sales_q)
farmers_end$ihs_b_q_sold_dry  <- ihs(farmers_end$b_q_sold_dry)
farmers_end$ihs_b_q_sold_wet  <- ihs(farmers_end$b_q_sold_wet)
farmers_end$ihs_b_avg_sales_q <- ihs(farmers_end$b_avg_sales_q)

outcomes   <- c("ihs_q_sold_dry", "ihs_q_sold_wet", "sold_last_week", "ihs_avg_sales_q")
b_outcomes <- c("ihs_b_q_sold_dry", "ihs_b_q_sold_wet", "b_sold_last_week", "ihs_b_avg_sales_q")

farmers_end$secondary_farmer_quant_index  <- anderson_index(farmers_end[outcomes])$index
farmers_end$b_secondary_farmer_quant_index <- anderson_index(farmers_end[b_outcomes])$index

outcomes   <- c(outcomes, "secondary_farmer_quant_index")
b_outcomes <- c(b_outcomes, "b_secondary_farmer_quant_index")

farmers_end_int <- farmers_end[
  (farmers_end$vid == TRUE & farmers_end$treat == TRUE) |
  (farmers_end$vid == FALSE & farmers_end$treat == FALSE), ]

res_farmers_sec_quant <- run_farmer_regressions(
  outcomes, b_outcomes, farmers_end, farmers_end_int,
  has_baseline = TRUE, ncols = 32
)

saveRDS(res_farmers_sec_quant, file = paste(path, "PAP/results/res_farmers_sec_quant.RData", sep = "/"))

# ===========================================================================
# SECTION 7: SECONDARY OUTCOMES — TREATMENT UPTAKE
# ===========================================================================
# No baseline data for these outcomes (video recall, grass recall, etc.)

farmers_end$recalls_video <- farmers_end$recalls_video == "Yes"
farmers_end$recalls_grass <- farmers_end$recalls_grass == "Yes"
farmers_end$used_grass    <- farmers_end$used_grass == "Yes"
farmers_end$used_video    <- farmers_end$used_video == "Yes"

farmers_end$knows_comp <- anderson_index(cbind(
  farmers_end$test_know_1 == 1,
  farmers_end$test_know_2 == 2,
  farmers_end$test_know_3 == 1
))$index

outcomes <- c("recalls_video", "recalls_grass", "used_grass", "knows_comp")

farmers_end$secondary_farmer_quant_index <- anderson_index(farmers_end[outcomes])$index

outcomes <- c(outcomes, "secondary_farmer_quant_index")
farmers_end_int <- farmers_end[
  (farmers_end$vid == TRUE & farmers_end$treat == TRUE) |
  (farmers_end$vid == FALSE & farmers_end$treat == FALSE), ]

res_farmers_sec_uptake <- run_farmer_regressions(
  outcomes, b_outcomes = NULL, farmers_end, farmers_end_int,
  has_baseline = FALSE, ncols = 33
)

saveRDS(res_farmers_sec_uptake, file = paste(path, "PAP/results/res_farmers_sec_uptake.RData", sep = "/"))

# ===========================================================================
# SECTION 8: SECONDARY OUTCOMES — SALES/MARKET CHANNEL
# ===========================================================================

## Sold to MCC in wet/dry season
farmers_end$mcc_wet    <- farmers_end$q51 == "2"
farmers_base$b_mcc_wet <- farmers_base$q51 == "2"
farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_mcc_wet")],
                     by = "farmer_ID", all.x = TRUE)

farmers_end$mcc_dry    <- farmers_end$q51x == "2"
farmers_base$b_mcc_dry <- farmers_base$q51x == "2"
farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_mcc_dry")],
                     by = "farmer_ID", all.x = TRUE)

## Sold to MCC in last week
farmers_end$mcc_last_seek    <- farmers_end$q53.2 == "True"
farmers_base$b_mcc_last_seek <- farmers_base$q53.2 == "True"
farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_mcc_last_seek")],
                     by = "farmer_ID", all.x = TRUE)

## Wet season price
farmers_end$price_wet <- as.numeric(as.character(farmers_end$q51a))
farmers_end$price_wet[farmers_end$price_wet == 999] <- NA
farmers_base$b_price_wet <- as.numeric(as.character(farmers_base$q51a))
farmers_base$b_price_wet[farmers_base$b_price_wet == 999] <- NA
farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_price_wet")],
                     by = "farmer_ID", all.x = TRUE)

## Dry season price
farmers_end$price_dry <- as.numeric(as.character(farmers_end$q51ax))
farmers_end$price_dry[farmers_end$price_dry == 999] <- NA
farmers_base$b_price_dry <- as.numeric(as.character(farmers_base$q51ax))
farmers_base$b_price_dry[farmers_base$b_price_dry == 999] <- NA
farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_price_dry")],
                     by = "farmer_ID", all.x = TRUE)

outcomes   <- c("mcc_wet", "mcc_dry", "mcc_last_seek", "price_wet", "price_dry")
b_outcomes <- c("b_mcc_wet", "b_mcc_dry", "b_mcc_last_seek", "b_price_wet", "b_price_dry")

farmers_end$secondary_farmer_quant_index   <- anderson_index(farmers_end[outcomes])$index
farmers_end$b_secondary_farmer_quant_index <- anderson_index(farmers_end[b_outcomes])$index

outcomes   <- c(outcomes, "secondary_farmer_quant_index")
b_outcomes <- c(b_outcomes, "b_secondary_farmer_quant_index")

farmers_end_int <- farmers_end[
  (farmers_end$vid == TRUE & farmers_end$treat == TRUE) |
  (farmers_end$vid == FALSE & farmers_end$treat == FALSE), ]

res_farmers_sec_sold <- run_farmer_regressions(
  outcomes, b_outcomes, farmers_end, farmers_end_int,
  has_baseline = TRUE, ncols = 32
)

saveRDS(res_farmers_sec_sold, file = paste(path, "PAP/results/res_farmers_sec_sold.RData", sep = "/"))

# ===========================================================================
# SECTION 9: SECONDARY OUTCOMES — SWITCHING MCCs
# ===========================================================================
# No baseline equivalents for switching outcomes.
# BUG FIX: original code referenced stale b_outcomes from the sales section
# in the direct comparison model, incorrectly controlling for b_mcc_wet etc.
# Fixed by passing has_baseline = FALSE.

farmers_end$still_connected_yes <- farmers_end$still_connected == 1
farmers_end$still_connected_yes[farmers_end$still_connected == "n/a"] <- NA

farmers_end$q51_name[is.na(farmers_end$q51_name)]   <- "NA"
farmers_end$still_supplying_wet <- as.character(farmers_end$q51_name_prev) ==
  as.character(farmers_end$q51_name)

farmers_end$q51_namex[is.na(farmers_end$q51_namex)] <- "NA"
farmers_end$still_supplying_dry <- as.character(farmers_end$q51_name_prevx) ==
  as.character(farmers_end$q51_namex)

outcomes <- c("still_connected_yes", "still_supplying_wet", "still_supplying_dry")

farmers_end$secondary_farmer_switch_index <- anderson_index(farmers_end[outcomes])$index

outcomes <- c(outcomes, "secondary_farmer_switch_index")
farmers_end_int <- farmers_end[
  (farmers_end$vid == TRUE & farmers_end$treat == TRUE) |
  (farmers_end$vid == FALSE & farmers_end$treat == FALSE), ]

res_farmers_sec_switching <- run_farmer_regressions(
  outcomes, b_outcomes = NULL, farmers_end, farmers_end_int,
  has_baseline = FALSE, ncols = 32
)

saveRDS(res_farmers_sec_switching, file = paste(path, "PAP/results/res_farmers_sec_switching.RData", sep = "/"))

# ===========================================================================
# SECTION 10: FOREST PLOTS (for presentations)
# ===========================================================================
library(dplyr)
library(tidyr)

# ---------------------------------------------------------------------------
# Helper: build forest plot data from result array rows
# ---------------------------------------------------------------------------
build_forest_data <- function(res_rows, outcome_names, col_layout = "demeaned") {
  df <- as.data.frame(res_rows)
  colnames(df) <- c(
    "Mean", "SD",
    "Coef1", "SE1", "Pval1",
    "Coef2", "SE2", "Pval2",
    "Coef3", "SE3", "Pval3", "N",
    "LinearHyp1", "LinearHyp2", "LinearHyp3",
    "ASQ5", "ASQ8", "ASQ11",
    "Coef_Demeaned1", "SE_Demeaned1", "Pval_Demeaned1",
    "Coef_Demeaned2", "SE_Demeaned2", "Pval_Demeaned2",
    "ASQ18", "ASQ21",
    "Coef4", "SE4", "Pval4",
    "LinearHyp4", "LinearHyp1_demeaned", "LinearHyp2_demeaned"
  )
  df$Outcome <- outcome_names
  df
}

custom_colors <- c(
  "milk analyzer" = "#1f77b4",
  "video"         = "#ff7f0e",
  "bundle"        = "#2ca02c"
)

# ---------------------------------------------------------------------------
# 10a. Switching forest plot (full model coefficients)
# ---------------------------------------------------------------------------
res_df <- build_forest_data(res_farmers_sec_switching[1:3, ], outcomes[1:3])

forest_long <- res_df %>%
  select(Outcome, Coef1, SE1, LinearHyp1, Coef2, SE2, LinearHyp2,
         Coef4, SE4, LinearHyp4) %>%
  pivot_longer(cols = starts_with("Coef"), names_to = "Coefficient",
               values_to = "Estimate") %>%
  mutate(
    SE = case_when(
      Coefficient == "Coef1" ~ SE1,
      Coefficient == "Coef2" ~ SE2,
      Coefficient == "Coef4" ~ SE4
    ),
    LinearHypothesis = case_when(
      Coefficient == "Coef1" ~ LinearHyp1,
      Coefficient == "Coef2" ~ LinearHyp2,
      Coefficient == "Coef4" ~ LinearHyp4
    ),
    CI_Lower = Estimate - 1.96 * SE,
    CI_Upper = Estimate + 1.96 * SE,
    Coefficient = case_when(
      Coefficient == "Coef1" ~ "milk analyzer",
      Coefficient == "Coef2" ~ "video",
      Coefficient == "Coef4" ~ "bundle"
    )
  ) %>%
  filter(!is.na(Coefficient))

forest_long$Coefficient <- factor(forest_long$Coefficient,
                                  levels = c("milk analyzer", "video", "bundle"))

forest_plot <- ggplot(forest_long, aes(x = Estimate, y = reorder(Outcome, Estimate),
                                       color = Coefficient)) +
  geom_point(size = 3, position = position_dodge(width = 0.7)) +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2,
                 position = position_dodge(width = 0.7)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_text(aes(label = ifelse(!is.na(LinearHypothesis),
                               paste0("p = ", round(LinearHypothesis, 3)), "")),
            position = position_dodge(width = 0.7), vjust = -1, size = 3.5,
            show.legend = FALSE) +
  scale_color_manual(values = custom_colors) +
  labs(title = "Farmer Switching MCCs", x = "Coefficient (95% CI)",
       y = "Outcomes", color = "Model") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.position = "top")

ggsave(file = paste(path, "PAP/results/forest_plot.png", sep = "/"),
       plot = forest_plot, width = 10, height = 6, dpi = 300, device = "png")

# ---------------------------------------------------------------------------
# 10b. Price forest plot (demeaned coefficients)
# ---------------------------------------------------------------------------
res_df <- build_forest_data(rbind(res_farmers_prim[3, ], res_farmers_sec_sold[4:5, ]),
                            c("price last week", "price in wet season", "price in dry season"))

forest_long <- res_df %>%
  select(Outcome, Coef_Demeaned1, SE_Demeaned1, LinearHyp1_demeaned,
         Coef_Demeaned2, SE_Demeaned2, LinearHyp2_demeaned,
         Coef4, SE4, LinearHyp4) %>%
  pivot_longer(cols = starts_with("Coef"), names_to = "Coefficient",
               values_to = "Estimate") %>%
  mutate(
    SE = case_when(
      Coefficient == "Coef_Demeaned1" ~ SE_Demeaned1,
      Coefficient == "Coef_Demeaned2" ~ SE_Demeaned2,
      Coefficient == "Coef4" ~ SE4
    ),
    LinearHypothesis = case_when(
      Coefficient == "Coef_Demeaned1" ~ LinearHyp1_demeaned,
      Coefficient == "Coef_Demeaned2" ~ LinearHyp2_demeaned,
      Coefficient == "Coef4" ~ LinearHyp4
    ),
    CI_Lower = Estimate - 1.96 * SE,
    CI_Upper = Estimate + 1.96 * SE,
    Coefficient = case_when(
      Coefficient == "Coef_Demeaned1" ~ "milk analyzer",
      Coefficient == "Coef_Demeaned2" ~ "video",
      Coefficient == "Coef4" ~ "bundle"
    )
  ) %>%
  filter(!is.na(Coefficient))

forest_long$Coefficient <- factor(forest_long$Coefficient,
                                  levels = c("milk analyzer", "video", "bundle"))

forest_plot <- ggplot(forest_long, aes(x = Estimate, y = reorder(Outcome, Estimate),
                                       color = Coefficient)) +
  geom_point(size = 3, position = position_dodge(width = 0.7)) +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2,
                 position = position_dodge(width = 0.7)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_text(aes(label = ifelse(!is.na(LinearHypothesis),
                               paste0("p = ", round(LinearHypothesis, 3)), "")),
            position = position_dodge(width = 0.7), vjust = -1, size = 3.5,
            show.legend = FALSE) +
  scale_color_manual(values = custom_colors) +
  labs(title = "Milk Prices received by farmers", x = "Coefficient (95% CI)",
       y = "Outcomes", color = "Model") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.position = "top")

ggsave(file = paste(path, "PAP/results/forest_plot_price.png", sep = "/"),
       plot = forest_plot, width = 10, height = 6, dpi = 300)

# ---------------------------------------------------------------------------
# 10c-e. Simple forest plots (farmer use, prices, switching)
# ---------------------------------------------------------------------------
# Helper for simple 3-treatment forest plots
make_simple_forest <- function(outcomes, outcome_labels, data_full, title, filename,
                               cluster_var = "catch_ID") {
  data_int <- data_full[
    (data_full$vid == TRUE & data_full$treat == TRUE) |
    (data_full$vid == FALSE & data_full$treat == FALSE), ]

  results <- data.frame(
    Outcome = character(), Treatment = character(),
    Estimate = numeric(), StdError = numeric(),
    CI_Lower = numeric(), CI_Upper = numeric(),
    stringsAsFactors = FALSE
  )

  for (i in seq_along(outcomes)) {
    ols_MA     <- lm(as.formula(paste(outcomes[i], "~ treat")), data = data_full)
    ols_vid    <- lm(as.formula(paste(outcomes[i], "~ vid")), data = data_full)
    ols_bundle <- lm(as.formula(paste(outcomes[i], "~ vid")), data = data_int)

    datasets <- list(MA = data_full, Video = data_full, Bundle = data_int)
    interventions <- list(MA = ols_MA, Video = ols_vid, Bundle = ols_bundle)
    for (nm in names(interventions)) {
      model <- interventions[[nm]]
      dat   <- datasets[[nm]]
      ## CR2 cluster-robust SEs
      complete <- complete.cases(model$model)
      clust <- dat[[cluster_var]][complete]
      vcov_cr2 <- vcovCR(model, cluster = clust, type = "CR2")
      cr2_test <- coef_test(model, vcov = vcov_cr2)
      est  <- cr2_test[2, "beta"]
      se   <- cr2_test[2, "SE"]
      df   <- cr2_test[2, "df_Satt"]
      results <- rbind(results, data.frame(
        Outcome   = outcomes[i],
        Treatment = nm,
        Estimate  = est,
        StdError  = se,
        CI_Lower  = est - qt(0.975, df) * se,
        CI_Upper  = est + qt(0.975, df) * se
      ))
    }
  }

  results$Outcome   <- factor(results$Outcome, levels = rev(outcomes),
                               labels = rev(outcome_labels))
  results$Treatment <- factor(results$Treatment, levels = c("MA", "Video", "Bundle"))

  p <- ggplot(results, aes(x = Estimate, y = Outcome, color = Treatment)) +
    geom_point(position = position_dodge(width = 0.6), size = 4) +
    geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper),
                   position = position_dodge(width = 0.6), height = 0.3, linewidth = 1) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 1.2) +
    theme_minimal(base_size = 16) +
    labs(title = title, x = "Treatment Effect Estimate", y = "", color = "Treatment") +
    scale_color_manual(values = c("MA" = "#1b9e77", "Video" = "#d95f02", "Bundle" = "#7570b3")) +
    theme(axis.text.y = element_text(size = 14, face = "bold"),
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

  ggsave(file = paste(path, "PAP/results", filename, sep = "/"),
         plot = p, width = 10, height = 6, dpi = 300)
}

## Farmer use (bundle outcomes)
farmers_end$any_true <- with(farmers_end,
  q39 == "Yes" | q39c == "Yes" | q40 %in% c(1, 3) |
  q41 %in% c(1, 3) | q42 == "Yes" | q43 == "Yes"
)

make_simple_forest(
  c("check_MA", "knows_comp", "any_true"),
  c("milk was checked at MCC", "knows importance of quality", "invested in quality"),
  farmers_end, "Farmer level use", "forest_plot_farmer_use_simple.png"
)

## Prices
make_simple_forest(
  c("avg_sales_p", "price_wet", "price_dry"),
  c("price in last week", "price in wet season", "price in dry season"),
  farmers_end, "Farmer level impact on prices", "forest_plot_price_simple.png"
)

## Switching
make_simple_forest(
  c("still_connected_yes", "still_supplying_wet", "still_supplying_dry"),
  c("still connected to the same MCC", "still supplying to same in wet season",
    "still supplying to same in dry season"),
  farmers_end, "Impact on switching", "forest_plot_swithing_simple.png"
)

# ===========================================================================
# SECTION 11: MCC-LEVEL ANALYSIS
# ===========================================================================
# MCC-level treatment is binary (lactoscan provided or not).
# Only treat is relevant (no vid at MCC level).
# CR2 cluster-robust SEs are computed alongside OLS SEs for comparison.
# With ~120 MCCs in ~92 catchments (70 singletons), the two are near-identical.

### Merge catchment_ID from baseline for CR2 clustering
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "catchment_ID")],
                  by = "MCC_ID", all.x = TRUE)
MCCs_end$catchment_ID <- as.factor(MCCs_end$catchment_ID)

### ITT vs TOT compliance tables
table(MCCs_end$machine, MCCs_end$treat)
table(MCCs_end$machine_project, MCCs_end$treat)
table(MCCs_end$machine_in_use, MCCs_end$treat)

MCCs_end$treat_TOT <- MCCs_end$machine_in_use %in% 1:2

# ---------------------------------------------------------------------------
# 11a. MCC primary outcomes: quality testing and pricing
# ---------------------------------------------------------------------------

## Testing of incoming milk quality using milk analyzer
MCCs_end$test_MA_in <- MCCs_end$q25x3 != 3
MCCs_end$test_MA_in[MCCs_end$q25x3 == "n/a"] <- NA
MCCs_base$b_test_MA_in <- MCCs_base$q25x3 != 1
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_test_MA_in")],
                  by = "MCC_ID", all.x = TRUE)

## Testing of outgoing milk quality (SNF & fat tested for any sale in last 7 days)
MCCs_end$test_MA_out <- (MCCs_end$q39a == 2 & MCCs_end$q39c == 2) |
  (MCCs_end$q52a == 2 & MCCs_end$q52c == 2) |
  (MCCs_end$q62a == 2 & MCCs_end$q62c == 2) |
  (MCCs_end$q72a == 2 & MCCs_end$q72c == 2) |
  (MCCs_end$q82a == 2 & MCCs_end$q82c == 2)

MCCs_base$b_test_MA_out <- (MCCs_base$q39a == 1 | MCCs_base$q39c == 1) |
  (MCCs_base$q52a == 1 | MCCs_base$q52c == 1) |
  (MCCs_base$q62a == 1 | MCCs_base$q62c == 1) |
  (MCCs_base$q72a == 1 | MCCs_base$q72c == 1) |
  (MCCs_base$q82a == 1 | MCCs_base$q82c == 1)
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_test_MA_out")],
                  by = "MCC_ID", all.x = TRUE)

## Average price at which milk was bought from farmers (last 7 days)
MCCs_end$q25b[MCCs_end$q25b %in% c("n/a", "999")] <- NA
MCCs_end$q25b <- as.numeric(as.character(MCCs_end$q25b))
names(MCCs_end)[names(MCCs_end) == "q25b"] <- "price_bought"
MCCs_end$price_bought[MCCs_end$price_bought <= 600] <- NA

MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "q25b")],
                  by = "MCC_ID", all.x = TRUE)
names(MCCs_end)[names(MCCs_end) == "q25b"] <- "b_price_bought"
MCCs_end$b_price_bought[MCCs_end$b_price_bought < 500]  <- NA
MCCs_end$b_price_bought[MCCs_end$b_price_bought > 1250] <- NA

## Average sales price (quantity-weighted across buyers in last 7 days)
columns   <- c("q36", "q49", "q59", "q69", "q79")
columns_2 <- c("q35", "q48", "q58", "q68", "q78")

MCCs_end[columns] <- lapply(MCCs_end[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA; as.numeric(as.character(x))
})
MCCs_base[columns] <- lapply(MCCs_base[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA; x <- as.numeric(as.character(x)); x[x > 10000] <- NA; x
})
MCCs_end[columns_2] <- lapply(MCCs_end[columns_2], function(x) {
  x[x %in% c("n/a", "999")] <- NA; x <- as.numeric(as.character(x)); x[is.na(x)] <- 0; x
})
MCCs_base[columns_2] <- lapply(MCCs_base[columns_2], function(x) {
  x[x %in% c("n/a", "999")] <- NA; x <- as.numeric(as.character(x)); x[is.na(x)] <- 0; x
})

MCCs_end$avg_sales_p <- rowSums(MCCs_end[columns] * MCCs_end[columns_2], na.rm = TRUE) /
  rowSums(MCCs_end[columns_2], na.rm = TRUE)
MCCs_end$avg_sales_p[is.nan(MCCs_end$avg_sales_p)] <- NA
MCCs_end$avg_sales_p[MCCs_end$avg_sales_p == 0]    <- NA

MCCs_base$b_avg_sales_p <- rowSums(MCCs_base[columns] * MCCs_base[columns_2], na.rm = TRUE) /
  rowSums(MCCs_base[columns_2], na.rm = TRUE)
MCCs_base$b_avg_sales_p[is.nan(MCCs_base$b_avg_sales_p)] <- NA
MCCs_base$b_avg_sales_p[MCCs_base$b_avg_sales_p == 0]    <- NA
MCCs_base$b_avg_sales_p[MCCs_base$b_avg_sales_p > 2000]  <- NA
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_avg_sales_p")],
                  by = "MCC_ID", all.x = TRUE)

## Quality bonus: MCC gives premium to farmers
MCCs_end$q29[MCCs_end$q29 == "n/a"] <- NA
MCCs_end$gives_q_bonus <- MCCs_end$q29 == 1
MCCs_base$q29[MCCs_base$q29 == 98] <- NA
MCCs_base$b_gives_q_bonus <- MCCs_base$q29 == 1
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_gives_q_bonus")],
                  by = "MCC_ID", all.x = TRUE)

## Quality bonus: MCC receives premium from buyer
MCCs_end$gets_q_bonus <- ifelse(
  rowSums(MCCs_end[, c("q44", "q54", "q64", "q74", "q84")] == "n/a") == 5,
  NA,
  MCCs_end$q44 == 1 | MCCs_end$q54 == 1 | MCCs_end$q64 == 1 |
    MCCs_end$q74 == 1 | MCCs_end$q84 == 1
)
MCCs_base$b_gets_q_bonus <- ifelse(
  rowSums(MCCs_base[, c("q44", "q54", "q64", "q74", "q84")] == "n/a") ==
    ncol(MCCs_base[, c("q44", "q54", "q64", "q74", "q84")]),
  NA,
  MCCs_base$q44 == 1 | MCCs_base$q54 == 1 | MCCs_base$q64 == 1 |
    MCCs_base$q74 == 1 | MCCs_base$q84 == 1
)
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_gets_q_bonus")],
                  by = "MCC_ID", all.x = TRUE)

### Run MCC primary regressions (OLS + 2SLS)
outcomes   <- c("test_MA_in", "test_MA_out", "price_bought", "avg_sales_p",
                "gives_q_bonus", "gets_q_bonus")
b_outcomes <- c("b_test_MA_in", "b_test_MA_out", "b_price_bought", "b_avg_sales_p",
                "b_gives_q_bonus", "b_gets_q_bonus")

MCCs_end$primary_MCC_index   <- anderson_index(MCCs_end[outcomes])$index
MCCs_end$b_primary_MCC_index <- anderson_index(MCCs_end[b_outcomes])$index

outcomes   <- c(outcomes, "primary_MCC_index")
b_outcomes <- c(b_outcomes, "b_primary_MCC_index")

# MCC result array: 12 columns
#  1-2:  control mean, SD
#  3-5:  OLS coef, SE, p
#  6-8:  2SLS coef, SE, p
#  9:    N (OLS)
#  10:   N (2SLS)
#  11:   q-value for OLS p
#  12:   q-value for 2SLS p

res_MCCs <- array(NA, dim = c(length(outcomes), 12))

for (i in seq_along(outcomes)) {
  ## OLS: outcome ~ treat + baseline
  ols <- lm(as.formula(paste(outcomes[i], "~ treat +", b_outcomes[i])),
            data = MCCs_end)

  y_ctrl <- MCCs_end[MCCs_end$treat == "C", outcomes[i]]
  res_MCCs[i, 1] <- mean(y_ctrl, na.rm = TRUE)
  res_MCCs[i, 2] <- sd(y_ctrl, na.rm = TRUE)

  ## CR2 cluster-robust SEs (clustered at catchment level)
  vcov_cr2 <- vcovCR(ols, cluster = MCCs_end[!is.na(MCCs_end[[outcomes[i]]]) &
    !is.na(MCCs_end[[b_outcomes[i]]]), "catchment_ID"], type = "CR2")
  cr2_test <- coef_test(ols, vcov = vcov_cr2)[2, ]
  res_MCCs[i, 3] <- cr2_test[["beta"]]
  res_MCCs[i, 4] <- cr2_test[["SE"]]
  res_MCCs[i, 5] <- cr2_test[["p_Satt"]]
  res_MCCs[i, 9] <- nobs(ols)

  ## 2SLS: outcome ~ treat_TOT + baseline | treat + baseline
  iv_formula <- as.formula(paste0(
    outcomes[i], " ~ treat_TOT + ", b_outcomes[i],
    " | treat + ", b_outcomes[i]
  ))
  iv_mod  <- ivreg(iv_formula, data = MCCs_end)
  vcov_iv_cr2 <- vcovCR(iv_mod, cluster = MCCs_end[!is.na(MCCs_end[[outcomes[i]]]) &
    !is.na(MCCs_end[[b_outcomes[i]]]), "catchment_ID"], type = "CR2")
  cr2_iv_test <- coef_test(iv_mod, vcov = vcov_iv_cr2)[2, ]
  res_MCCs[i, 6]  <- cr2_iv_test[["beta"]]
  res_MCCs[i, 7]  <- cr2_iv_test[["SE"]]
  res_MCCs[i, 8]  <- cr2_iv_test[["p_Satt"]]
  res_MCCs[i, 10] <- nobs(iv_mod)
}

## BUG FIX: compute q-values BEFORE rounding (original rounded first,
## feeding imprecise p-values to anderson_sharp_q)
idx <- 1:(length(outcomes) - 1)
res_MCCs[idx, 11] <- anderson_sharp_q(res_MCCs[idx, 5])
res_MCCs[idx, 12] <- anderson_sharp_q(res_MCCs[idx, 8])
res_MCCs <- round(res_MCCs, digits = 3)

saveRDS(res_MCCs, file = paste(path, "PAP/results/res_MCCs.RData", sep = "/"))

# ---------------------------------------------------------------------------
# 11b. MCC secondary outcomes: volumes and farmer counts
# ---------------------------------------------------------------------------

MCCs_end$nr_farmers_wet <- as.numeric(MCCs_end$Q23)
MCCs_base$b_nr_farmers_wet <- as.numeric(MCCs_base$Q23)
MCCs_base$b_nr_farmers_wet[MCCs_base$b_nr_farmers_wet >= 1000] <- NA
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_nr_farmers_wet")],
                  by = "MCC_ID", all.x = TRUE)

MCCs_end$nr_farmers_dry <- as.numeric(MCCs_end$Q24)
MCCs_base$b_nr_farmers_dry <- as.numeric(MCCs_base$Q24)
MCCs_base$b_nr_farmers_dry[MCCs_base$b_nr_farmers_dry >= 1000] <- NA
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_nr_farmers_dry")],
                  by = "MCC_ID", all.x = TRUE)

MCCs_end$nr_farmers_last_week <- as.numeric(MCCs_end$q25a)
MCCs_base$b_nr_farmers_last_week <- as.numeric(MCCs_base$q25a)
MCCs_base$b_nr_farmers_last_week[MCCs_base$b_nr_farmers_last_week >= 1000] <- NA
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_nr_farmers_last_week")],
                  by = "MCC_ID", all.x = TRUE)

MCCs_end$vol_dry <- as.numeric(MCCs_end$q27)
MCCs_base$b_vol_dry <- as.numeric(MCCs_base$qual.q27)
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_vol_dry")],
                  by = "MCC_ID", all.x = TRUE)

MCCs_end$vol_wet <- as.numeric(MCCs_end$q28)
MCCs_base$b_vol_wet <- as.numeric(MCCs_base$qual.q28)
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_vol_wet")],
                  by = "MCC_ID", all.x = TRUE)

MCCs_end$vol_last_week <- as.numeric(MCCs_end$q25c)
MCCs_base$b_vol_last_week <- as.numeric(MCCs_base$q25c)
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_vol_last_week")],
                  by = "MCC_ID", all.x = TRUE)

outcomes   <- c("nr_farmers_wet", "nr_farmers_dry", "nr_farmers_last_week",
                "vol_dry", "vol_wet", "vol_last_week")
b_outcomes <- c("b_nr_farmers_wet", "b_nr_farmers_dry", "b_nr_farmers_last_week",
                "b_vol_dry", "b_vol_wet", "b_vol_last_week")

MCCs_end$primary_MCC_index   <- anderson_index(MCCs_end[outcomes])$index
MCCs_end$b_primary_MCC_index <- anderson_index(MCCs_end[b_outcomes])$index
outcomes   <- c(outcomes, "primary_MCC_index")
b_outcomes <- c(b_outcomes, "b_primary_MCC_index")

res_MCCs <- array(NA, dim = c(length(outcomes), 12))

for (i in seq_along(outcomes)) {
  ols <- lm(as.formula(paste(outcomes[i], "~ treat +", b_outcomes[i])),
            data = MCCs_end)
  y_ctrl <- MCCs_end[MCCs_end$treat == "C", outcomes[i]]
  res_MCCs[i, 1]   <- mean(y_ctrl, na.rm = TRUE)
  res_MCCs[i, 2]   <- sd(y_ctrl, na.rm = TRUE)

  vcov_cr2 <- vcovCR(ols, cluster = MCCs_end[!is.na(MCCs_end[[outcomes[i]]]) &
    !is.na(MCCs_end[[b_outcomes[i]]]), "catchment_ID"], type = "CR2")
  cr2_test <- coef_test(ols, vcov = vcov_cr2)[2, ]
  res_MCCs[i, 3] <- cr2_test[["beta"]]
  res_MCCs[i, 4] <- cr2_test[["SE"]]
  res_MCCs[i, 5] <- cr2_test[["p_Satt"]]
  res_MCCs[i, 9] <- nobs(ols)

  iv_formula <- as.formula(paste0(
    outcomes[i], " ~ treat_TOT + ", b_outcomes[i],
    " | treat + ", b_outcomes[i]
  ))
  iv_mod  <- ivreg(iv_formula, data = MCCs_end)
  vcov_iv_cr2 <- vcovCR(iv_mod, cluster = MCCs_end[!is.na(MCCs_end[[outcomes[i]]]) &
    !is.na(MCCs_end[[b_outcomes[i]]]), "catchment_ID"], type = "CR2")
  cr2_iv_test <- coef_test(iv_mod, vcov = vcov_iv_cr2)[2, ]
  res_MCCs[i, 6]  <- cr2_iv_test[["beta"]]
  res_MCCs[i, 7]  <- cr2_iv_test[["SE"]]
  res_MCCs[i, 8]  <- cr2_iv_test[["p_Satt"]]
  res_MCCs[i, 10] <- nobs(iv_mod)
}

idx <- 1:(length(outcomes) - 1)
res_MCCs[idx, 11] <- anderson_sharp_q(res_MCCs[idx, 5])
res_MCCs[idx, 12] <- anderson_sharp_q(res_MCCs[idx, 8])
res_MCCs <- round(res_MCCs, digits = 3)

res_MCCs_sec_quant <- res_MCCs
saveRDS(res_MCCs_sec_quant, file = paste(path, "PAP/results/res_MCCs_sec_quant.RData", sep = "/"))

# ---------------------------------------------------------------------------
# 11c. MCC secondary outcomes: treatment uptake (no baseline, no 2SLS)
# ---------------------------------------------------------------------------
# Result array: 7 columns (mean, SD, coef, SE, p, q-val, N)

MCCs_end$poster          <- MCCs_end$poster == 1
MCCs_end$machine         <- MCCs_end$machine == 1
MCCs_end$machine_project <- MCCs_end$machine_project == 1
MCCs_end$machine_in_use  <- MCCs_end$machine_in_use == 1 | MCCs_end$machine_in_use == 2
MCCs_end$test_samples    <- MCCs_end$q16c == "1"
MCCs_end$uses_app        <- MCCs_end$record_keeping.4 == "True" |
                            MCCs_end$record_keeping.5 == "True"

outcomes <- c("poster", "machine", "machine_project", "machine_in_use", "uses_app")
MCCs_end$secondary_MCC_index <- anderson_index(MCCs_end[outcomes])$index
outcomes <- c(outcomes, "secondary_MCC_index")

res_MCCs <- array(NA, dim = c(length(outcomes), 7))

for (i in seq_along(outcomes)) {
  ols <- lm(as.formula(paste(outcomes[i], "~ treat")), data = MCCs_end)
  y_ctrl <- MCCs_end[MCCs_end$treat == "C", outcomes[i]]
  res_MCCs[i, 1]   <- mean(y_ctrl, na.rm = TRUE)
  res_MCCs[i, 2]   <- sd(y_ctrl, na.rm = TRUE)

  vcov_cr2 <- vcovCR(ols, cluster = MCCs_end[!is.na(MCCs_end[[outcomes[i]]]),
    "catchment_ID"], type = "CR2")
  cr2_test <- coef_test(ols, vcov = vcov_cr2)[2, ]
  res_MCCs[i, 3] <- cr2_test[["beta"]]
  res_MCCs[i, 4] <- cr2_test[["SE"]]
  res_MCCs[i, 5] <- cr2_test[["p_Satt"]]
  res_MCCs[i, 7] <- nobs(ols)
}

idx <- 1:(length(outcomes) - 1)
res_MCCs[idx, 6] <- anderson_sharp_q(res_MCCs[idx, 5])
res_MCCs <- round(res_MCCs, digits = 3)

res_MCCs_sec_uptake <- res_MCCs
saveRDS(res_MCCs_sec_uptake, file = paste(path, "PAP/results/res_MCCs_sec_uptake.RData", sep = "/"))

# ---------------------------------------------------------------------------
# 11d. MCC secondary outcomes: sales and bargaining
# ---------------------------------------------------------------------------

## Total volumes sold (last 7 days)
columns <- c("q35", "q48", "q58", "q68", "q78")
MCCs_end[columns] <- lapply(MCCs_end[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA; as.numeric(as.character(x))
})
MCCs_base[columns] <- lapply(MCCs_base[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA; as.numeric(as.character(x))
})
MCCs_end$tot_sales_q     <- rowSums(MCCs_end[columns], na.rm = TRUE)
MCCs_base$b_tot_sales_q  <- rowSums(MCCs_base[columns], na.rm = TRUE)
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_tot_sales_q")],
                  by = "MCC_ID", all.x = TRUE)

## Used milk analyzer for outgoing sales
MCCs_end$test_MA <- (MCCs_end$q39a == 2 | MCCs_end$q39c == 2) |
  (MCCs_end$q52a == 2 | MCCs_end$q52c == 2) |
  (MCCs_end$q62a == 2 | MCCs_end$q62c == 2) |
  (MCCs_end$q72a == 2 | MCCs_end$q72c == 2) |
  (MCCs_end$q82a == 2 | MCCs_end$q82c == 2)

MCCs_base$b_test_MA <- (MCCs_base$q39a == 2 | MCCs_base$q39c == 2) |
  (MCCs_base$q52a == 2 | MCCs_base$q52c == 2) |
  (MCCs_base$q62a == 2 | MCCs_base$q62c == 2) |
  (MCCs_base$q72a == 2 | MCCs_base$q72c == 2) |
  (MCCs_base$q82a == 2 | MCCs_base$q82c == 2)
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_test_MA")],
                  by = "MCC_ID", all.x = TRUE)

## MCC decides on price (vs buyer or negotiation)
MCCs_end$MCC_decides <- MCCs_end$q40 == "2" | MCCs_end$q53 == "2" |
  MCCs_end$q63 == "2" | MCCs_end$q73 == "2" | MCCs_end$q83 == "2"
MCCs_base$b_MCC_decides <- MCCs_base$q40 == "2" | MCCs_base$q53 == "2" |
  MCCs_base$q63 == "2" | MCCs_base$q73 == "2" | MCCs_base$q83 == "2"
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_MCC_decides")],
                  by = "MCC_ID", all.x = TRUE)

## Buyer paid quality premium
MCCs_end$MCC_got_premium <- MCCs_end$q44 == "1" | MCCs_end$q54 == "1" |
  MCCs_end$q64 == "1" | MCCs_end$q74 == "1" | MCCs_end$q84 == "1"
MCCs_base$b_MCC_got_premium <- MCCs_base$q44 == "1" | MCCs_base$q54 == "1" |
  MCCs_base$q64 == "1" | MCCs_base$q74 == "1" | MCCs_base$q84 == "1"
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_MCC_got_premium")],
                  by = "MCC_ID", all.x = TRUE)

## Average quality premium amount (UGX per liter)
columns <- c("q46", "q56", "q66", "q76", "q86")
MCCs_end[columns] <- lapply(MCCs_end[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA; as.numeric(as.character(x))
})
MCCs_base[columns] <- lapply(MCCs_base[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA; as.numeric(as.character(x))
})
MCCs_end$avg_prem_received <- rowMeans(MCCs_end[columns], na.rm = TRUE)
MCCs_end$avg_prem_received[is.nan(MCCs_end$avg_prem_received)] <- NA
MCCs_base$b_avg_prem_received <- rowMeans(MCCs_base[columns], na.rm = TRUE)
MCCs_base$b_avg_prem_received[is.nan(MCCs_base$b_avg_prem_received)] <- NA
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_avg_prem_received")],
                  by = "MCC_ID", all.x = TRUE)

outcomes   <- c("tot_sales_q", "test_MA", "MCC_decides", "MCC_got_premium", "avg_prem_received")
b_outcomes <- c("b_tot_sales_q", "b_test_MA", "b_MCC_decides", "b_MCC_got_premium", "b_avg_prem_received")

MCCs_end$primary_MCC_index   <- anderson_index(MCCs_end[outcomes])$index
MCCs_end$b_primary_MCC_index <- anderson_index(MCCs_end[b_outcomes])$index
outcomes   <- c(outcomes, "primary_MCC_index")
b_outcomes <- c(b_outcomes, "b_primary_MCC_index")

res_MCCs <- array(NA, dim = c(length(outcomes), 7))

for (i in seq_along(outcomes)) {
  ols <- lm(as.formula(paste(outcomes[i], "~ treat +", b_outcomes[i])),
            data = MCCs_end)
  y_ctrl <- MCCs_end[MCCs_end$treat == "C", outcomes[i]]
  res_MCCs[i, 1]   <- mean(y_ctrl, na.rm = TRUE)
  res_MCCs[i, 2]   <- sd(y_ctrl, na.rm = TRUE)

  vcov_cr2 <- vcovCR(ols, cluster = MCCs_end[!is.na(MCCs_end[[outcomes[i]]]) &
    !is.na(MCCs_end[[b_outcomes[i]]]), "catchment_ID"], type = "CR2")
  cr2_test <- coef_test(ols, vcov = vcov_cr2)[2, ]
  res_MCCs[i, 3] <- cr2_test[["beta"]]
  res_MCCs[i, 4] <- cr2_test[["SE"]]
  res_MCCs[i, 5] <- cr2_test[["p_Satt"]]
  res_MCCs[i, 7] <- nobs(ols)
}

idx <- 1:(length(outcomes) - 1)
res_MCCs[idx, 6] <- anderson_sharp_q(res_MCCs[idx, 5])
res_MCCs <- round(res_MCCs, digits = 3)

res_MCCs_sec_sales <- res_MCCs
saveRDS(res_MCCs_sec_sales, file = paste(path, "PAP/results/res_MCCs_sec_sales.RData", sep = "/"))

## Sold to top 5 processors (used in paper but not in a result array)
MCCs_end$top_proc <- MCCs_end$q32.2 == "True" & MCCs_end$q33.6 != "True"
MCCs_base$b_top_proc <- MCCs_base$q32.2 == "True" & MCCs_base$q33.6 != "True"
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_top_proc")],
                  by = "MCC_ID", all.x = TRUE)

# ---------------------------------------------------------------------------
# 11e. Simple MCC forest plots (for presentations)
# ---------------------------------------------------------------------------

## MCC use outcomes
make_single_treatment_forest <- function(outcomes, outcome_labels, data, title, filename,
                                         cluster_var = "catchment_ID") {
  results <- data.frame(
    Outcome = character(), Estimate = numeric(), StdError = numeric(),
    CI_Lower = numeric(), CI_Upper = numeric(), stringsAsFactors = FALSE
  )
  for (i in seq_along(outcomes)) {
    ols <- lm(as.formula(paste(outcomes[i], "~ treat")), data = data)
    ## CR2 cluster-robust SEs
    complete <- complete.cases(ols$model)
    clust <- data[[cluster_var]][complete]
    vcov_cr2 <- vcovCR(ols, cluster = clust, type = "CR2")
    cr2_test <- coef_test(ols, vcov = vcov_cr2)
    est  <- cr2_test[2, "beta"]
    se   <- cr2_test[2, "SE"]
    df   <- cr2_test[2, "df_Satt"]
    results <- rbind(results, data.frame(
      Outcome  = outcomes[i],
      Estimate = est,
      StdError = se,
      CI_Lower = est - qt(0.975, df) * se,
      CI_Upper = est + qt(0.975, df) * se
    ))
  }
  results$Outcome   <- factor(results$Outcome, levels = outcomes, labels = outcome_labels)
  results$Treatment <- "MA"

  p <- ggplot(results, aes(x = Estimate, y = Outcome, color = Treatment)) +
    geom_point(size = 4) +
    geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.3, linewidth = 1) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 1.2) +
    theme_minimal(base_size = 16) +
    labs(title = title, x = "Treatment Effect Estimate", y = "", color = "Treatment") +
    scale_color_manual(values = c("MA" = "#1b9e77")) +
    theme(axis.text.y = element_text(size = 14, face = "bold"),
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

  ggsave(file = paste(path, "PAP/results", filename, sep = "/"),
         plot = p, width = 10, height = 6, dpi = 300)
}

make_single_treatment_forest(
  c("test_MA_in", "test_MA_out", "uses_app"),
  c("used MA to test incoming samples", "used MA to test outgoing samples",
    "digital quality tracking"),
  MCCs_end, "Innovation bundle use", "forest_plot_MCC_use_simple.png"
)

make_single_treatment_forest(
  c("price_bought", "avg_sales_p"),
  c("price at which milk was bought", "price at which milk was sold"),
  MCCs_end, "Impact on prices", "forest_plot_MCC_prices_simple.png"
)

# ===========================================================================
# SECTION 12: MILK SAMPLE ANALYSIS
# ===========================================================================
# Supervised milk quality testing at MCCs using project Lactoscans.
# Samples collected at endline with calibration correction for Ntungamo district.

samples <- read.csv(paste(path, "endline/data/public/samples.csv", sep = "/"))

## Calibration fix: Ntungamo samples before Dec 5 are excluded
samples <- samples[
  (samples$today >= as.Date("2024-12-04") &
   (samples$district == "Kazo" | samples$district == "Mbarara")) |
  samples$today >= as.Date("2024-12-05"), ]

## Merge treatment indicators
samples <- merge(samples, MCCs_end[c("MCC_ID", "treat_TOT")], all.x = TRUE)

outcomes <- c("Fat", "SNF", "Added.Water", "Protein", "Corrected.Lactometer.Reading")
samples$samples_index <- anderson_index(samples[outcomes], revcols = 3)$index
outcomes <- c(outcomes, "samples_index")

## Sample result array: 12 columns (same layout as MCC)
res_samples <- array(NA, dim = c(length(outcomes), 12))

for (i in seq_along(outcomes)) {
  ## OLS: outcome ~ treat (weighted by quantity)
  ols <- lm(as.formula(paste(outcomes[i], "~ treat")),
            data = samples, weights = samples$Qty)

  y_ctrl <- samples[samples$treat == "C", outcomes[i]]
  res_samples[i, 1] <- mean(y_ctrl, na.rm = TRUE)
  res_samples[i, 2] <- sd(y_ctrl, na.rm = TRUE)

  vcov_ols <- vcovCR(ols, cluster = samples$catch_ID, type = "CR2")
  res <- coef_test(ols, vcov_ols)
  res_samples[i, 3:5] <- c(res[2, 2], res[2, 3], res[2, 7])
  res_samples[i, 9]   <- nobs(ols)

  ## 2SLS: outcome ~ treat_TOT | treat (weighted)
  iv_mod <- ivreg(as.formula(paste0(outcomes[i], " ~ treat_TOT | treat")),
                  data = samples, weights = samples$Qty)
  vcov_iv <- vcovCR(iv_mod, cluster = samples$catch_ID, type = "CR2")
  res_iv  <- coef_test(iv_mod, vcov_iv)
  res_samples[i, 6:8] <- c(res_iv[2, 2], res_iv[2, 3], res_iv[2, 7])
  res_samples[i, 10]  <- nobs(iv_mod)
}

idx <- 1:(length(outcomes) - 1)
res_samples[idx, 11] <- anderson_sharp_q(res_samples[idx, 5])
res_samples[idx, 12] <- anderson_sharp_q(res_samples[idx, 8])
res_samples <- round(res_samples, digits = 3)

saveRDS(res_samples, file = paste(path, "PAP/results/res_samples.RData", sep = "/"))

# ---------------------------------------------------------------------------
# 12a. Sample quality forest plot (comparing RCT treatment vs android app data)
# ---------------------------------------------------------------------------
samples <- read.csv(paste(path, "endline/data/public/samples.csv", sep = "/"))
samples <- samples[
  (samples$today >= as.Date("2024-12-04") &
   (samples$district == "Kazo" | samples$district == "Mbarara")) |
  samples$today >= as.Date("2024-12-05"), ]

## Load android app data and stack
android_file <- paste(path, "sample_submissions/dta_reports.csv", sep = "/")
if (file.exists(android_file)) {
  android_dta <- read.csv(android_file)
  android_dta <- android_dta[android_dta$date >= as.Date("2024-12-01"), ]
  android_dta$treat <- "TA"
  common_cols <- intersect(names(samples), names(android_dta))
  samples <- rbind(samples[common_cols], android_dta[common_cols])
}

samples$Rejected <- samples$Rejected == 1
samples$Price    <- as.numeric(samples$Price)
samples$Qty      <- as.numeric(samples$Qty)
samples$Analogue.Lactometer.Test <- as.numeric(samples$Analogue.Lactometer.Test)

outcomes       <- c("Fat", "Added.Water", "Rejected", "SNF", "Corrected.Lactometer.Reading")
outcome_labels <- c("Fat", "Added Water", "Rejected", "SNF", "Corrected.Lactometer.Reading")

## Standardize outcomes
for (outcome in outcomes) {
  samples[[paste0(outcome, "_standardized")]] <- scale(samples[[outcome]])
}

## Only run presentation plots if android data was stacked (has "TA" group)
if ("TA" %in% samples$treat) {
  samples_clean <- samples %>% filter(!(treat == "TA" & is.na(Fat)))

  ## Assign unique cluster IDs to android (TA) rows that lack catch_ID
  if ("catch_ID" %in% names(samples)) {
    samples$cluster_id <- samples$catch_ID
    na_idx <- is.na(samples$cluster_id)
    if (any(na_idx)) {
      samples$cluster_id[na_idx] <- paste0("singleton_", seq_len(sum(na_idx)))
    }
    samples$cluster_id <- as.factor(samples$cluster_id)
  }

  results <- data.frame(
    Outcome = character(), Estimate_T = numeric(), Estimate_TA = numeric(),
    StdError_T = numeric(), StdError_TA = numeric(),
    CI_Lower_T = numeric(), CI_Upper_T = numeric(),
    CI_Lower_TA = numeric(), CI_Upper_TA = numeric(),
    p_value = numeric(), stringsAsFactors = FALSE
  )

  for (i in seq_along(outcomes)) {
    std_outcome <- paste0(outcomes[i], "_standardized")
    ols <- lm(as.formula(paste(std_outcome, "~ treat")),
              data = samples, weights = samples$Qty)

    ## CR2 cluster-robust SEs
    complete <- complete.cases(ols$model)
    clust <- samples$cluster_id[complete]
    vcov_cr2 <- vcovCR(ols, cluster = clust, type = "CR2")
    cr2_test <- coef_test(ols, vcov = vcov_cr2)

    coef_T  <- cr2_test["treatT", "beta"]
    coef_TA <- cr2_test["treatTA", "beta"]
    se_T    <- cr2_test["treatT", "SE"]
    se_TA   <- cr2_test["treatTA", "SE"]
    df_T    <- cr2_test["treatT", "df_Satt"]
    df_TA   <- cr2_test["treatTA", "df_Satt"]
    ci_T    <- c(coef_T - qt(0.975, df_T) * se_T, coef_T + qt(0.975, df_T) * se_T)
    ci_TA   <- c(coef_TA - qt(0.975, df_TA) * se_TA, coef_TA + qt(0.975, df_TA) * se_TA)
    p_val   <- linearHypothesis(ols, "treatT = treatTA", vcov. = vcov_cr2)$`Pr(>F)`[2]

    results <- rbind(results, data.frame(
      Outcome = outcomes[i], Estimate_T = coef_T, Estimate_TA = coef_TA,
      StdError_T = se_T, StdError_TA = se_TA,
      CI_Lower_T = ci_T[1], CI_Upper_T = ci_T[2],
      CI_Lower_TA = ci_TA[1], CI_Upper_TA = ci_TA[2],
      p_value = p_val
    ))
  }

  results$Outcome <- factor(results$Outcome, levels = rev(outcomes),
                             labels = rev(outcome_labels))

  forest_plot <- ggplot(results, aes(x = Estimate_T, y = Outcome)) +
    geom_point(aes(color = "T"), size = 4, position = position_nudge(y = 0.2)) +
    geom_point(aes(x = Estimate_TA, color = "TA"), size = 4,
               position = position_nudge(y = -0.2)) +
    geom_errorbarh(aes(xmin = CI_Lower_T, xmax = CI_Upper_T), height = 0.3,
                   linewidth = 1, color = "#1b9e77", position = position_nudge(y = 0.2)) +
    geom_errorbarh(aes(xmin = CI_Lower_TA, xmax = CI_Upper_TA), height = 0.3,
                   linewidth = 1, color = "#85d1b0", position = position_nudge(y = -0.2)) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 1.2) +
    theme_minimal(base_size = 16) +
    labs(title = "Impact on Quality by Treatment",
         x = "Treatment Effect Estimate (Standardized)", y = "", color = "Treatment") +
    scale_color_manual(values = c("T" = "#1b9e77", "TA" = "#85d1b0")) +
    geom_text(aes(x = Inf, label = sprintf("p = %.3f", p_value), hjust = 1), size = 4)

  ggsave(file = paste(path, "PAP/results/forest_plot_samples_simple.png", sep = "/"),
         plot = forest_plot, width = 10, height = 6, dpi = 300)
}

# ---------------------------------------------------------------------------
# 12b. Cumulative distribution of milk delivery times
# ---------------------------------------------------------------------------
samples <- read.csv(paste(path, "endline/data/public/samples.csv", sep = "/"))
samples <- samples[
  (samples$today >= as.Date("2024-12-04") &
   (samples$district == "Kazo" | samples$district == "Mbarara")) |
  samples$today >= as.Date("2024-12-05"), ]

samples$timestamps <- ymd_hms(samples$start, tz = "Africa/Kampala")
samples$timestamps <- as.POSIXct(format(samples$timestamps, format = "2024-01-01 %H:%M:%S"),
                                 tz = "Africa/Kampala")

## Keep only deliveries between 7:00 and 14:00
samples <- samples %>%
  filter(hour(timestamps) >= 7 & hour(timestamps) < 14)

samples <- samples %>%
  arrange(timestamps) %>%
  group_by(treat) %>%
  mutate(cumulative_count = row_number(),
         cumulative_percentage = (cumulative_count / n()) * 100) %>%
  ungroup()

cum_dist <- ggplot(samples, aes(x = timestamps, y = cumulative_percentage, color = treat)) +
  geom_line(linewidth = 1.5) +
  labs(title = "Cumulative Distribution of Milk Deliveries",
       x = "Time of Delivery", y = "Cumulative Percentage of Events") +
  scale_color_manual(values = c("T" = "#1b9e77", "C" = "red")) +
  scale_x_datetime(labels = scales::date_format("%I:%M %p"),
                   breaks = scales::date_breaks("2 hours")) +
  theme_minimal(base_size = 16) +
  theme(legend.title = element_blank(),
        legend.position = c(0.85, 0.2))

ggsave(paste(path, "PAP/results/distribution_of_delivery.png", sep = "/"))

## Statistical tests for treatment vs control delivery times
assorted_tests <- array(NA, dim = c(4, 2))

group_T <- samples$cumulative_percentage[samples$treat == "T"]
group_C <- samples$cumulative_percentage[samples$treat == "C"]

ks_result <- ks.test(group_T, group_C)
assorted_tests[1, 1:2] <- as.numeric(ks_result[1:2])

group_T_times <- as.numeric(samples$timestamps[samples$treat == "T"])
group_C_times <- as.numeric(samples$timestamps[samples$treat == "C"])

t_result <- t.test(group_T_times, group_C_times)
assorted_tests[2, 1:2] <- as.numeric(t_result[c(1, 3)])

wilcox_result <- wilcox.test(group_T_times, group_C_times)
assorted_tests[3, 1:2] <- as.numeric(wilcox_result[c(1, 3)])

## First-order stochastic dominance check
sorted_T <- sort(samples$timestamps[samples$treat == "T"])
sorted_C <- sort(samples$timestamps[samples$treat == "C"])
cdf_T <- cumsum(rep(1, length(sorted_T))) / length(sorted_T)
cdf_C <- cumsum(rep(1, length(sorted_C))) / length(sorted_C)
## NOTE: FOSD check requires equal-length CDFs — this is approximate
cdf_diff <- cdf_T - cdf_C

if (all(cdf_diff <= 0) && any(cdf_diff < 0)) {
  cat("Group T first-order stochastically dominates Group C.\n")
} else if (all(cdf_diff >= 0) && any(cdf_diff > 0)) {
  cat("Group C first-order stochastically dominates Group T.\n")
} else {
  cat("No first-order stochastic dominance between the two groups.\n")
}

## Second-order stochastic dominance check
cdf_diff_area <- cumsum(cdf_T) - cumsum(cdf_C)

if (all(cdf_diff_area <= 0) && any(cdf_diff_area < 0)) {
  cat("Group T second-order stochastically dominates Group C.\n")
} else if (all(cdf_diff_area >= 0) && any(cdf_diff_area > 0)) {
  cat("Group C second-order stochastically dominates Group T.\n")
} else {
  cat("No second-order stochastic dominance between the two groups.\n")
}

saveRDS(assorted_tests, file = paste(path, "PAP/results/assorted_tests.RData", sep = "/"))
