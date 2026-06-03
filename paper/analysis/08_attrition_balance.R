###############################################################################
## 08_attrition_balance.R
##
## Attrition correlates and analysis-sample balance tables.
## Addresses coauthor request: (i) is attrition correlated with baseline
## characteristics (external validity)? (ii) does balance hold on the
## analysis sample, not just on baseline (internal validity on survivors)?
##
## Must be run with working directory set to paper/analysis/.
## Sources 00_utilities.R for helper functions.
##
## Outputs (saved to paper/results/):
##   attrition_correlates_farmer.rds — Exp 1 farmer: attrition ~ baseline X
##   balance_farmer_analysis.rds     — Exp 1 farmer: balance on analysis sample
##   balance_followup_ext.rds        — Exp 2 trader: extended balance
##   balance_farmer_fu.rds           — Exp 2 farmer: balance on observable chars
###############################################################################

source("00_utilities.R")

library(clubSandwich)
library(car)
library(fixest)

path <- strsplit(getwd(), "/paper/analysis")[[1]]

## ---------------------------------------------------------------------------
## Load prepped data
## ---------------------------------------------------------------------------
farmers_balance <- readRDS(paste(path, "paper/results/farmers_balance.rds", sep = "/"))
farmers_end     <- readRDS(paste(path, "paper/results/prepped_farmers.rds", sep = "/"))
traders         <- readRDS(paste(path, "paper/results/prepped_traders.rds", sep = "/"))
farmers_fu      <- readRDS(paste(path, "paper/results/prepped_farmers_fu.rds", sep = "/"))
submissions     <- readRDS(paste(path, "paper/results/prepped_submissions.rds", sep = "/"))


###############################################################################
## TABLE A. EXP 1 FARMER ATTRITION CORRELATES
###############################################################################
## Define attrited = baseline farmer not in endline analysis sample.

farmers_balance$attrited <- !(farmers_balance$farmer_ID %in% farmers_end$farmer_ID)

cat("Exp 1 farmer attrition:",
    sum(farmers_balance$attrited), "of", nrow(farmers_balance),
    "(", round(100 * mean(farmers_balance$attrited), 1), "%)\n")

X_vars <- c("hh_size", "age_head", "herd_size", "improved_share",
            "liter_day_wet", "liter_sold_day_wet", "sell_MCC_wet",
            "use_steel", "coop_member", "acaracide_exp")

## Coerce logicals to numeric so means/regressions behave consistently
for (v in X_vars) {
  if (is.logical(farmers_balance[[v]])) {
    farmers_balance[[v]] <- as.numeric(farmers_balance[[v]])
  }
}

## Columns: retained_mean | attrited_mean | diff | se | p | n
attrition_correlates_farmer <- array(NA, dim = c(length(X_vars), 6),
                                     dimnames = list(X_vars,
                                                     c("retained_mean", "attrited_mean",
                                                       "diff", "se", "p", "n")))

for (i in seq_along(X_vars)) {
  v <- X_vars[i]
  ret <- farmers_balance[[v]][!farmers_balance$attrited]
  att <- farmers_balance[[v]][ farmers_balance$attrited]

  ## Regression: X ~ attrited, CR2 by catchment
  ols <- lm(as.formula(paste(v, "~ attrited")), data = farmers_balance)
  vcv <- vcovCR(ols, cluster = farmers_balance$catchment_ID, type = "CR2")
  res <- coef_test(ols, vcv)

  attrition_correlates_farmer[i, ] <- c(
    mean(ret, na.rm = TRUE),
    mean(att, na.rm = TRUE),
    res[2, 2], res[2, 3], res[2, 7],
    nobs(ols)
  )
}

## Joint F-test: do baseline X jointly predict attrition?
rhs  <- paste(X_vars, collapse = " + ")
keep <- complete.cases(farmers_balance[, c("attrited", X_vars, "catchment_ID")])
mod  <- lm(as.formula(paste("attrited ~", rhs)),
           data = farmers_balance[keep, ])
W <- Wald_test(mod,
               constraints = constrain_zero(X_vars),
               vcov = "CR2",
               cluster = farmers_balance$catchment_ID[keep],
               test = "HTZ")

attrition_F_farmer <- c(Fstat = W$Fstat, pval = W$p_val,
                        n = nobs(mod))

attrition_correlates_farmer <- round(attrition_correlates_farmer, 3)
attrition_F_farmer          <- round(attrition_F_farmer, 3)

saveRDS(list(table = attrition_correlates_farmer, joint = attrition_F_farmer),
        file = paste(path, "paper/results/attrition_correlates_farmer.rds", sep = "/"))


###############################################################################
## TABLE B. EXP 1 FARMER BALANCE ON ANALYSIS SAMPLE
###############################################################################
## Re-run the existing factorial balance regression restricted to the
## endline analysis sample (farmers actually used in primary regressions).
## Layout mirrors balance_farmer.rds: 10 outcomes x 8 cols.
##  1-2: ctrl_mean, ctrl_sd   (pure control: lactoscan=C & video=FALSE)
##  3-5: T1 (lactoscan) coef, se, p
##  6-8: T2 (video)    coef, se, p
##  9:   N

ana <- farmers_balance[!farmers_balance$attrited, ]

## Pooled (orthogonalized) lactoscan coefficient, matching the main balance
## tables: outcome ~ lactoscan + vid_demeaned, where vid_demeaned is the
## video indicator demeaned. (Video analysis is not reported in the paper.)
ana$vid_demeaned <- ana$video_shown - mean(ana$video_shown, na.rm = TRUE)

balance_farmer_analysis <- array(NA, dim = c(length(X_vars), 6),
                                 dimnames = list(X_vars,
                                                 c("ctrl_mean", "ctrl_sd",
                                                   "coef", "se", "p", "n")))

for (i in seq_along(X_vars)) {
  v <- X_vars[i]
  ols <- lm(as.formula(paste(v, "~ lactoscan + vid_demeaned")), data = ana)
  vcv <- vcovCR(ols, cluster = ana$catchment_ID, type = "CR2")
  res <- coef_test(ols, vcv)

  ctrl <- ana[ana$lactoscan == "C", v]
  balance_farmer_analysis[i, 1] <- mean(ctrl, na.rm = TRUE)
  balance_farmer_analysis[i, 2] <- sd(ctrl,   na.rm = TRUE)
  balance_farmer_analysis[i, 3:5] <- c(res[2, 2], res[2, 3], res[2, 7])
  balance_farmer_analysis[i, 6]   <- nobs(ols)
}

## Joint F-test: lactoscan predicted by all baseline X (on the analysis sample)
keep <- complete.cases(ana[, c("lactoscan", X_vars, "catchment_ID")])
mod_F <- lm(as.formula(paste("(lactoscan == 'T') ~", rhs)), data = ana[keep, ])
W_F   <- Wald_test(mod_F, constraints = constrain_zero(X_vars),
                   vcov = "CR2", cluster = ana$catchment_ID[keep], test = "HTZ")

balance_F_analysis <- c(F = W_F$Fstat, p = W_F$p_val,
                        n = nobs(mod_F))

balance_farmer_analysis <- round(balance_farmer_analysis, 3)
balance_F_analysis      <- round(balance_F_analysis,      3)

saveRDS(list(table = balance_farmer_analysis, joint = balance_F_analysis),
        file = paste(path, "paper/results/balance_farmer_analysis.rds", sep = "/"))


###############################################################################
## TABLE C. EXP 2 TRADER BALANCE (EXTENDED)
###############################################################################
## Extends the existing balance_followup (pre-treatment fat, snf, qty, bonus)
## with trader characteristics plausibly pre-determined at recruitment:
##   - self (own trader vs employed)
##   - nr_employed
##   - transport modes (bodaboda, car) — proxy for scale
##   - delivered_quantity (endline-reported but reflects pre-existing scale)
## Each tested with MCC fixed effects, trader-clustered SEs (HC1).

## Note: transport_car has zero variance (no traders use cars);
## transport_bodaboda is near-constant (~99% use bodaboda). Both dropped.
balance_t_vars <- c("self", "nr_employed", "delivered_quantity",
                    "delivered_quantity_othermcc")
## Coerce logicals
for (v in balance_t_vars) {
  if (is.logical(traders[[v]])) traders[[v]] <- as.numeric(traders[[v]])
}

balance_trader_ext <- array(NA, dim = c(length(balance_t_vars), 5),
                            dimnames = list(balance_t_vars,
                                            c("ctrl_mean", "coef", "se", "p", "n")))

for (i in seq_along(balance_t_vars)) {
  v <- balance_t_vars[i]
  ctrl <- traders[[v]][traders$treat == 0]
  fit  <- feols(as.formula(paste(v, "~ treat | MCC_ID")),
                data = traders, vcov = "hetero")
  balance_trader_ext[i, 1] <- mean(ctrl, na.rm = TRUE)
  balance_trader_ext[i, 2] <- coef(fit)["treat"]
  balance_trader_ext[i, 3] <- se(fit)["treat"]
  balance_trader_ext[i, 4] <- pvalue(fit)["treat"]
  balance_trader_ext[i, 5] <- fit$nobs
}

balance_trader_ext <- round(balance_trader_ext, 4)
saveRDS(balance_trader_ext,
        file = paste(path, "paper/results/balance_followup_ext.rds", sep = "/"))


###############################################################################
## TABLE D. EXP 2 FARMER BALANCE
###############################################################################
## Exp 2 farmers (N=422) recruited at endline through traders; no separate
## baseline survey exists. Test balance across trader-treatment status on
## plausibly pre-determined characteristics measured at endline.
## Cluster SEs at trader level. Include trader-randomization fixed effect
## (MCC) since randomization was stratified by MCC.

balance_ffu_vars <- c("farmer.tot_prod", "farmer.tot_sold",
                      "farmer.tot_sold_trader", "farmer.trader_link")
for (v in balance_ffu_vars) {
  if (is.logical(farmers_fu[[v]])) farmers_fu[[v]] <- as.numeric(farmers_fu[[v]])
}

balance_farmer_fu <- array(NA, dim = c(length(balance_ffu_vars), 5),
                           dimnames = list(balance_ffu_vars,
                                           c("ctrl_mean", "coef", "se", "p", "n")))

for (i in seq_along(balance_ffu_vars)) {
  v <- balance_ffu_vars[i]
  ctrl <- farmers_fu[[v]][farmers_fu$treat == 0]
  fit  <- feols(as.formula(paste(v, "~ treat | MCC_ID")),
                data = farmers_fu, cluster = ~trader_ID)
  balance_farmer_fu[i, 1] <- mean(ctrl, na.rm = TRUE)
  balance_farmer_fu[i, 2] <- coef(fit)["treat"]
  balance_farmer_fu[i, 3] <- se(fit)["treat"]
  balance_farmer_fu[i, 4] <- pvalue(fit)["treat"]
  balance_farmer_fu[i, 5] <- fit$nobs
}

balance_farmer_fu <- round(balance_farmer_fu, 4)
saveRDS(balance_farmer_fu,
        file = paste(path, "paper/results/balance_farmer_fu.rds", sep = "/"))


###############################################################################
## RENDER LATEX TABULAR FILES (consumed by paper.lyx via \input)
###############################################################################

fmt <- function(x) {
  if (is.na(x)) return("")
  ax <- abs(x)
  if (ax < 1)   return(sprintf("%.3f", x))
  if (ax < 100) return(sprintf("%.2f", x))
  sprintf("%.1f", x)
}
stars <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.01) return("$^{**}$")
  if (p < 0.05) return("$^{*}$")
  if (p < 0.1)  return("$^{+}$")
  ""
}

tex_dir <- paste(path, "paper/results", sep = "/")

## Load balance_followup (from 04_analysis_followup.R) for Table C
balance_followup <- readRDS(paste(tex_dir, "balance_followup.rds", sep = "/"))

## --- Table A: attrition correlates (Exp 1 farmers) -------------------------
labs_X <- c("Household size", "Household head age", "Herd size",
            "Share improved breed", "Liters/day (wet)",
            "Liters sold/day (wet)", "Sells to MCC (wet)",
            "Uses steel container", "Cooperative member",
            "Acaricide expenditure")
A <- attrition_correlates_farmer
rows <- character(0)
for (i in seq_len(nrow(A))) {
  rows <- c(rows, sprintf(
    "%s & %s & %s & %s%s & (%s) & %s \\\\",
    labs_X[i], fmt(A[i, 1]), fmt(A[i, 2]),
    fmt(A[i, 3]), stars(A[i, 5]), fmt(A[i, 4]), formatC(A[i, 6], format = "d")))
}
tab_A <- paste(c(
  "\\begin{tabular}{lccccc}", "\\hline\\hline",
  " & retained & attrited & diff. & SE & N \\\\",
  "\\hline",
  rows,
  "\\hline",
  sprintf("Joint $F$ on all covariates (HTZ) & \\multicolumn{4}{c}{$F = %s$, $p = %s$} & %s \\\\",
          fmt(attrition_F_farmer["Fstat"]), fmt(attrition_F_farmer["pval"]),
          formatC(attrition_F_farmer["n"], format = "d")),
  "\\hline\\hline",
  "\\end{tabular}"
), collapse = "\n")
writeLines(tab_A, paste(tex_dir, "tab_attrition_correlates_farmer.tex", sep = "/"))


## --- Table B: balance on analysis sample (Exp 1 farmers) -------------------
B <- balance_farmer_analysis
rows <- character(0)
for (i in seq_len(nrow(B))) {
  rows <- c(rows, sprintf(
    "%s & %s & %s%s & %s \\\\",
    labs_X[i],
    fmt(B[i, 1]),
    fmt(B[i, 3]), stars(B[i, 5]),
    formatC(B[i, 6], format = "d")))
  rows <- c(rows, sprintf(
    "(SD) & (%s) & (%s) & \\\\",
    fmt(B[i, 2]), fmt(B[i, 4])))
}
tab_B <- paste(c(
  "\\begin{tabular}{lccc}", "\\hline\\hline",
  " & ctrl & treat & N \\\\",
  "\\hline",
  rows,
  "\\hline",
  sprintf("Joint $F$ on all covariates (HTZ) & & $F=%s$, $p=%s$ & %s \\\\",
          fmt(balance_F_analysis["F"]), fmt(balance_F_analysis["p"]),
          formatC(balance_F_analysis["n"], format = "d")),
  "\\hline\\hline",
  "\\end{tabular}"
), collapse = "\n")
writeLines(tab_B, paste(tex_dir, "tab_balance_farmer_analysis.tex", sep = "/"))


## --- Table C: Exp 2 trader balance (extended) ------------------------------
C_mat <- balance_trader_ext
C_labs_pre <- c("Pre-treatment fat (\\%)", "Pre-treatment SNF (\\%)",
                "Pre-treatment quantity (litres/day)", "Pre-treatment bonus (UGX/day)")
C_labs_ext <- c("Self (owner-trader, $\\in\\{1,2\\}$)", "Number of employees",
                "Delivered quantity (litres)", "Delivered to other MCC (litres)")
rows <- character(0)
## Block 1: existing pre-treatment outcomes
for (i in seq_len(nrow(balance_followup))) {
  rows <- c(rows, sprintf(
    "%s & %s & %s%s & (%s) & %s \\\\",
    C_labs_pre[i],
    fmt(balance_followup[i, "ctrl_mean"]),
    fmt(balance_followup[i, "coef"]), stars(balance_followup[i, "p"]),
    fmt(balance_followup[i, "se"]),
    formatC(balance_followup[i, "n"], format = "d")))
}
rows <- c(rows, "\\hline", "\\multicolumn{5}{l}{\\textit{Trader characteristics}} \\\\")
## Block 2: extended trader characteristics
for (i in seq_len(nrow(C_mat))) {
  rows <- c(rows, sprintf(
    "%s & %s & %s%s & (%s) & %s \\\\",
    C_labs_ext[i],
    fmt(C_mat[i, "ctrl_mean"]),
    fmt(C_mat[i, "coef"]), stars(C_mat[i, "p"]),
    fmt(C_mat[i, "se"]),
    formatC(C_mat[i, "n"], format = "d")))
}
tab_C <- paste(c(
  "\\begin{tabular}{lcccc}", "\\hline\\hline",
  " & ctrl & treat & SE & N \\\\",
  "\\hline",
  rows,
  "\\hline\\hline",
  "\\end{tabular}"
), collapse = "\n")
writeLines(tab_C, paste(tex_dir, "tab_balance_trader.tex", sep = "/"))


## --- Table D: Exp 2 farmer balance -----------------------------------------
D <- balance_farmer_fu
D_labs <- c("Total production (litres/day)", "Total sold (litres/day)",
            "Sold to recruiting trader (litres/day)",
            "Years linked to trader")
rows <- character(0)
for (i in seq_len(nrow(D))) {
  rows <- c(rows, sprintf(
    "%s & %s & %s%s & (%s) & %s \\\\",
    D_labs[i],
    fmt(D[i, "ctrl_mean"]),
    fmt(D[i, "coef"]), stars(D[i, "p"]),
    fmt(D[i, "se"]),
    formatC(D[i, "n"], format = "d")))
}
tab_D <- paste(c(
  "\\begin{tabular}{lcccc}", "\\hline\\hline",
  " & ctrl & treat & SE & N \\\\",
  "\\hline",
  rows,
  "\\hline\\hline",
  "\\end{tabular}"
), collapse = "\n")
writeLines(tab_D, paste(tex_dir, "tab_balance_farmer_fu.tex", sep = "/"))


## --- Table E: sale-channel selection diagnostic (sec_sold) -----------------
## Renders the existing res_farmers_sec_sold object so that selection into the
## price/sale-conditional analysis sample is visible to readers. Pre-empts the
## reviewer concern that treatment may differentially affect whether farmers
## sell milk in the survey reference period.

sec_sold <- readRDS(paste(tex_dir, "res_farmers_sec_sold.rds", sep = "/"))
E_labs <- c("Sold to MCC (wet season)",
            "Sold to MCC (dry season)",
            "Sold to MCC (last 7 days)",
            "Price received (wet season, UGX/L)",
            "Price received (dry season, UGX/L)",
            "Anderson index of sales-channel outcomes")
rows <- character(0)
for (i in seq_len(nrow(sec_sold))) {
  rows <- c(rows, sprintf(
    "%s & %s & %s%s & (%s) & %s \\\\",
    E_labs[i],
    fmt(sec_sold[i, 1]),
    fmt(sec_sold[i, 3]), stars(sec_sold[i, 5]),
    fmt(sec_sold[i, 4]),
    formatC(sec_sold[i, 9], format = "d")))
}
tab_E <- paste(c(
  "\\begin{tabular}{lcccc}", "\\hline\\hline",
  " & ctrl & treat & SE & N \\\\",
  "\\hline",
  rows,
  "\\hline\\hline",
  "\\end{tabular}"
), collapse = "\n")
writeLines(tab_E, paste(tex_dir, "tab_sec_sold.tex", sep = "/"))


## --- Table F: contamination robustness side-by-side ------------------------
## Compares full-sample point estimates with clean-subsample estimates for the
## three result levels (milk samples, MCCs, farmers). Renders the previously
## computed but undisplayed robustness_contamination object.

rc       <- readRDS(paste(tex_dir, "robustness_contamination.rds", sep = "/"))
rs_full  <- readRDS(paste(tex_dir, "res_samples.rds", sep = "/"))
rm_full  <- readRDS(paste(tex_dir, "res_MCCs.rds", sep = "/"))
rf_full  <- readRDS(paste(tex_dir, "res_farmers.rds", sep = "/"))

sample_labs <- c("Butterfat (%)", "SNF (%)", "Added water (%)",
                 "Protein (%)", "CLR", "Quality index")
mcc_labs    <- c("Tests incoming milk", "Tests outgoing milk",
                 "Farm-gate price", "Buyer price",
                 "Pays quality premium", "Receives quality premium",
                 "Uptake index")
farmer_labs <- c("Improved practices index", "Buyer checks quality",
                 "Average price received", "Receives quality bonus",
                 "Primary farmer index")

panel_rows <- function(full, clean, labs, full_coef_col, full_se_col, full_p_col,
                       full_n_col, clean_coef_col, clean_se_col, clean_p_col,
                       clean_n_col) {
  out <- character(0)
  for (i in seq_len(nrow(full))) {
    out <- c(out, sprintf(
      "%s & %s%s & (%s) & %s & %s%s & (%s) & %s \\\\",
      labs[i],
      fmt(full[i,  full_coef_col]),  stars(full[i,  full_p_col]),
      fmt(full[i,  full_se_col]),
      formatC(full[i, full_n_col], format = "d"),
      fmt(clean[i, clean_coef_col]), stars(clean[i, clean_p_col]),
      fmt(clean[i, clean_se_col]),
      formatC(clean[i, clean_n_col], format = "d")))
  }
  out
}

## Milk samples: full uses res_samples (cols 3,4,5,9); clean uses rc$res_samples
rows_s <- panel_rows(rs_full, rc$res_samples, sample_labs,
                     full_coef_col = 3, full_se_col = 4, full_p_col = 5, full_n_col = 9,
                     clean_coef_col = 3, clean_se_col = 4, clean_p_col = 5, clean_n_col = 6)

## MCCs: full res_MCCs has cols 3 coef, 4 se, 5 p, 9 N; clean rc$res_MCCs cols 3,4,5,6
rows_m <- panel_rows(rm_full, rc$res_MCCs, mcc_labs,
                     full_coef_col = 3, full_se_col = 4, full_p_col = 5, full_n_col = 9,
                     clean_coef_col = 3, clean_se_col = 4, clean_p_col = 5, clean_n_col = 6)

## Farmers: full res_farmers cols 3 coef, 4 se, 5 p, 9 N; clean rc$res_farmers same layout
rows_f <- panel_rows(rf_full, rc$res_farmers, farmer_labs,
                     full_coef_col = 3, full_se_col = 4, full_p_col = 5, full_n_col = 9,
                     clean_coef_col = 3, clean_se_col = 4, clean_p_col = 5, clean_n_col = 9)

tab_F <- paste(c(
  "\\begin{tabular}{lcccccc}", "\\hline\\hline",
  " & \\multicolumn{3}{c}{Full sample} & \\multicolumn{3}{c}{Clean sample} \\\\",
  "\\cline{2-4} \\cline{5-7}",
  " & treat & SE & N & treat & SE & N \\\\",
  "\\hline",
  "\\multicolumn{7}{l}{\\textit{Panel A: Milk samples (supervised endline testing)}} \\\\",
  rows_s,
  "\\hline",
  "\\multicolumn{7}{l}{\\textit{Panel B: MCC-level outcomes}} \\\\",
  rows_m,
  "\\hline",
  "\\multicolumn{7}{l}{\\textit{Panel C: Farmer-level primary outcomes}} \\\\",
  rows_f,
  "\\hline\\hline",
  "\\end{tabular}"
), collapse = "\n")
writeLines(tab_F, paste(tex_dir, "tab_robustness_contamination.tex", sep = "/"))


cat("\n08_attrition_balance.R completed successfully.\n")
cat("Wrote 6 .tex tabular files to paper/results/\n")
