###############################################################################
## 10_decomposition_app.R
##
## Exploratory decomposition of the Experiment 1 quality improvement using
## transaction-level data from the digital monitoring system (app) installed
## at treated MCCs. Because the app exists only where a milk analyzer was
## installed, these data cover TREATED MCCs during the experimental period
## (control MCCs received machines only after the December 2024 endline and
## appear in the export with first submissions from December 2024 onward).
## The analysis is therefore descriptive of dynamics within the treated arm,
## not a treatment-control contrast.
##
## Question (raised by the NOVAFRICA discussant): does the reduction in
## adulteration reflect the same suppliers improving (within-supplier
## discipline) or supplier turnover (new suppliers replacing old ones)?
##
## Design:
##  - Sample: MCCs whose first submission predates 2024-01-01 (the
##    second-half-2023 installation wave), records through 2024-12-08
##    (last day of endline supervised sampling)
##  - Buy/Sale record types are pooled: within the same period they have
##    identical price distributions; the label shifted over time as a
##    recording convention on the platform
##  - Early window: each MCC's first 90 days of operation (Oct 2023-Feb 2024)
##  - Late window: 2024-09-10 to 2024-12-08 (90 days before endline),
##    seasonally aligned with the early window
##  - Within-supplier change: farmer (within-MCC) fixed effects, identified
##    off suppliers observed in both windows
##  - Composition: supplier retention/entry rates and quality gaps between
##    entrants, stayers, and exiters
##
## Clustering is at the MCC level (the app data contain no catchment
## identifier; the MCC is the unit at which the machine was allocated).
##
## Must be run with working directory set to paper/analysis/.
##
## Outputs (saved to paper/results/):
##   res_app_decomp.rds -- list: reg (regression array), comp (composition
##                         stats), diag (testing-intensity diagnostics)
###############################################################################

source("00_utilities.R")

library(fixest)

path <- strsplit(getwd(), "/paper/analysis")[[1]]

app <- read.csv(paste(path, "endline/data/public/dta_reports_masked.csv", sep = "/"))
app$date <- as.Date(app$date)

## ---------------------------------------------------------------------------
## Sample restriction: experimental-wave MCCs, experimental period
## ---------------------------------------------------------------------------
first_sub <- tapply(app$date, app$MCC_ID, min)
exp_mccs  <- names(first_sub)[as.Date(first_sub, origin = "1970-01-01") <
                                as.Date("2024-01-01")]

app <- app[app$MCC_ID %in% exp_mccs & app$date <= as.Date("2024-12-08"), ]
app$mcc_start <- as.Date(first_sub[app$MCC_ID], origin = "1970-01-01")

## Supplier key: farmer_ID is renumbered within MCC in the public release,
## so the panel identifier is the MCC x farmer combination
app$fid <- paste(app$MCC_ID, app$farmer_ID, sep = "::")

## Outcomes
app$aw_pos  <- as.numeric(app$Added.Water > 0)
app$aw_gt5  <- as.numeric(app$Added.Water > 5)
app$days_since   <- as.numeric(app$date - app$mcc_start)
app$months_since <- app$days_since / 30.44

cat(sprintf("Experimental-wave MCCs: %d | submissions: %d | suppliers (MCC x farmer): %d\n",
            length(exp_mccs), nrow(app), length(unique(app$fid))))
cat(sprintf("Share of submissions with non-missing Added.Water: %.3f\n\n",
            mean(!is.na(app$Added.Water))))

## ---------------------------------------------------------------------------
## Early / late windows
## ---------------------------------------------------------------------------
app$window <- NA
app$window[app$days_since <= 90] <- "early"
app$window[app$date >= as.Date("2024-09-10")] <- "late"

el <- app[!is.na(app$window) & !is.na(app$Added.Water), ]
el$late <- as.numeric(el$window == "late")

## ---------------------------------------------------------------------------
## A. Regressions: total vs within-supplier change in adulteration
## ---------------------------------------------------------------------------
## Rows: for each outcome, (1) late-window coefficient with MCC FE = total
## change, (2) late-window coefficient with supplier FE = within-supplier
## change among stayers, (3) linear trend in months since installation.

outcomes <- c("Added.Water", "aw_pos", "aw_gt5", "Fat", "SNF")

reg <- array(NA, dim = c(length(outcomes), 11),
             dimnames = list(outcomes,
               c("early_mean", "total_coef", "total_se", "total_p",
                 "within_coef", "within_se", "within_p", "n_within",
                 "trend_coef", "trend_se", "trend_p")))

for (i in seq_along(outcomes)) {
  y <- outcomes[i]
  d <- el[!is.na(el[[y]]), ]

  reg[i, "early_mean"] <- mean(d[[y]][d$late == 0], na.rm = TRUE)

  fit_tot <- feols(as.formula(paste(y, "~ late | MCC_ID")),
                   data = d, vcov = ~MCC_ID)
  reg[i, 2:4] <- c(coef(fit_tot)["late"], se(fit_tot)["late"],
                   pvalue(fit_tot)["late"])

  fit_win <- feols(as.formula(paste(y, "~ late | fid")),
                   data = d, vcov = ~MCC_ID)
  reg[i, 5:7] <- c(coef(fit_win)["late"], se(fit_win)["late"],
                   pvalue(fit_win)["late"])
  reg[i, "n_within"] <- fit_win$nobs

  dt <- app[!is.na(app[[y]]), ]
  fit_tr <- feols(as.formula(paste(y, "~ months_since | MCC_ID")),
                  data = dt, vcov = ~MCC_ID)
  reg[i, 9:11] <- c(coef(fit_tr)["months_since"], se(fit_tr)["months_since"],
                    pvalue(fit_tr)["months_since"])
}

## ---------------------------------------------------------------------------
## B. Composition: supplier turnover and quality gaps
## ---------------------------------------------------------------------------
sup_early <- unique(el$fid[el$late == 0])
sup_late  <- unique(el$fid[el$late == 1])
stayers   <- intersect(sup_early, sup_late)
exiters   <- setdiff(sup_early, sup_late)
entrants  <- setdiff(sup_late, sup_early)

## Delivery-weighted share of late-window submissions from entrants
late_sub <- el[el$late == 1, ]
early_sub <- el[el$late == 0, ]

comp <- c(
  n_early          = length(sup_early),
  n_late           = length(sup_late),
  n_stayers        = length(stayers),
  retention        = length(stayers) / length(sup_early),
  entrant_share    = length(entrants) / length(sup_late),
  entrant_sub_share = mean(late_sub$fid %in% entrants),
  aw_stayer_early  = mean(early_sub$Added.Water[early_sub$fid %in% stayers]),
  aw_exiter_early  = mean(early_sub$Added.Water[early_sub$fid %in% exiters]),
  aw_stayer_late   = mean(late_sub$Added.Water[late_sub$fid %in% stayers]),
  aw_entrant_late  = mean(late_sub$Added.Water[late_sub$fid %in% entrants])
)

## ---------------------------------------------------------------------------
## C. Diagnostics: testing intensity over time (selective testing check)
## ---------------------------------------------------------------------------
diag <- c(
  test_share_early = mean(app$Milk.Analyzer.Test.Done[
    !is.na(app$window) & app$window == "early"] == "Yes"),
  test_share_late  = mean(app$Milk.Analyzer.Test.Done[
    !is.na(app$window) & app$window == "late"] == "Yes")
)

## ---------------------------------------------------------------------------
## Print and save
## ---------------------------------------------------------------------------
cat("=====================================================================\n")
cat("A. Early (first 90 days) vs late (90 days pre-endline), treated MCCs\n")
cat("=====================================================================\n")
print(round(reg, 4))

cat("\nB. Supplier turnover and quality gaps (Added.Water, percent)\n")
print(round(comp, 4))

cat("\nC. Analyzer-testing intensity (share of submissions tested)\n")
print(round(diag, 4))

meta <- c(n_mcc = length(exp_mccs),
          n_sub = nrow(app),
          n_sup = length(unique(app$fid)))

res_app_decomp <- list(reg = round(reg, 4), comp = round(comp, 4),
                       diag = round(diag, 4), meta = meta)
saveRDS(res_app_decomp,
        file = paste(path, "paper/results/res_app_decomp.rds", sep = "/"))

cat("\n10_decomposition_app.R completed. Results saved to paper/results/res_app_decomp.rds\n")
