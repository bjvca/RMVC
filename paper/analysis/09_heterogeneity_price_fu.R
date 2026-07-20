###############################################################################
## 09_heterogeneity_price_fu.R
##
## Exploratory heterogeneity analysis of the Experiment 2 (trader-level
## quality premium) treatment effect on farm-gate prices (avg_price from
## the farmer follow-up endline), motivated by the three pass-through
## hypotheses discussed in the paper: composition of sampled farmers,
## bargaining leverage, and selection on outside options.
##
## Moderators (all measured at the follow-up endline; treatment could in
## principle affect them, so results are exploratory):
##   - farmer.tot_sold  : liters sold yesterday (above/below median)
##   - farmer.tot_prod  : liters produced yesterday (above/below median)
##   - district         : Kazo vs Kiruhura (no GPS/distance data exists)
##   - farmer.trader_link: relationship duration; codes 1-3 = started
##     within the last month, i.e. during the experiment window
##
## Also estimates the treatment effect on being a newly recruited supplier
## (trader_link <= 3), a direct test of the composition hypothesis, and
## reports the share of farmers who say the trader paid a better price for
## better quality (farmer.bett_prc), a descriptive measure of pass-through.
##
## Specification mirrors 04_analysis_followup.R section D:
## feols(y ~ treat | MCC_ID, vcov = ~trader_ID).
##
## Must be run with working directory set to paper/analysis/.
##
## Outputs (saved to paper/results/):
##   res_price_het_fu.rds  -- subgroup and interaction estimates
###############################################################################

source("00_utilities.R")

library(fixest)

path <- strsplit(getwd(), "/paper/analysis")[[1]]

farmers_fu <- readRDS(paste(path, "paper/results/prepped_farmers_fu.rds", sep = "/"))

## ---------------------------------------------------------------------------
## Construct moderators
## ---------------------------------------------------------------------------

## 99 = "Other/Don't know" in trader_link per codebook -> NA
farmers_fu$farmer.trader_link[farmers_fu$farmer.trader_link == 99] <- NA

## New supplier: relationship started within the last month (codes 1-3),
## i.e. during the ~31-day experiment
farmers_fu$new_supplier <- as.numeric(farmers_fu$farmer.trader_link <= 3)

## Median splits on quantity sold and produced yesterday
med_sold <- median(farmers_fu$farmer.tot_sold, na.rm = TRUE)
med_prod <- median(farmers_fu$farmer.tot_prod, na.rm = TRUE)
farmers_fu$high_sold <- as.numeric(farmers_fu$farmer.tot_sold > med_sold)
farmers_fu$high_prod <- as.numeric(farmers_fu$farmer.tot_prod > med_prod)

## Trader paid better price for better quality last week (1=Yes, 2=No)
farmers_fu$bett_prc <- as.numeric(farmers_fu$farmer.bett_prc == 1)

cat(sprintf("Median liters sold yesterday: %.1f | produced: %.1f\n",
            med_sold, med_prod))
cat(sprintf("Share reporting quality-contingent price (bett_prc): %.3f (%d of %d)\n",
            mean(farmers_fu$bett_prc, na.rm = TRUE),
            sum(farmers_fu$bett_prc, na.rm = TRUE),
            sum(!is.na(farmers_fu$bett_prc))))
cat(sprintf("Share new suppliers (trader_link <= 3): %.3f\n\n",
            mean(farmers_fu$new_supplier, na.rm = TRUE)))

## ---------------------------------------------------------------------------
## Helper: treatment effect on avg_price in a subsample
## ---------------------------------------------------------------------------
run_fe <- function(y, data) {
  fml <- as.formula(paste(y, "~ treat | MCC_ID"))
  fit <- feols(fml, data = data, vcov = ~trader_ID)
  ctrl_vals <- data[[y]][data$treat == 0]
  c(coef = unname(coef(fit)["treat"]),
    se   = unname(se(fit)["treat"]),
    p    = unname(pvalue(fit)["treat"]),
    n    = fit$nobs,
    ctrl_mean = mean(ctrl_vals, na.rm = TRUE))
}

## Helper: interaction model treat x moderator (binary), MCC FE,
## returns interaction coefficient
run_interaction <- function(mod, data) {
  fml <- as.formula(paste("avg_price ~ treat *", mod, "| MCC_ID"))
  fit <- feols(fml, data = data, vcov = ~trader_ID)
  int_name <- grep("^treat:", names(coef(fit)), value = TRUE)
  c(coef = unname(coef(fit)[int_name]),
    se   = unname(se(fit)[int_name]),
    p    = unname(pvalue(fit)[int_name]),
    n    = fit$nobs,
    ctrl_mean = NA)
}

## ---------------------------------------------------------------------------
## A. Subgroup and interaction estimates for avg_price
## ---------------------------------------------------------------------------
res <- list()

res[["Full sample"]] <- run_fe("avg_price", farmers_fu)

## Quantity sold yesterday
res[["Sold <= median"]] <- run_fe("avg_price",
  farmers_fu[!is.na(farmers_fu$high_sold) & farmers_fu$high_sold == 0, ])
res[["Sold > median"]]  <- run_fe("avg_price",
  farmers_fu[!is.na(farmers_fu$high_sold) & farmers_fu$high_sold == 1, ])
res[["Interaction: treat x high_sold"]] <- run_interaction("high_sold", farmers_fu)

## Quantity produced yesterday
res[["Produced <= median"]] <- run_fe("avg_price",
  farmers_fu[!is.na(farmers_fu$high_prod) & farmers_fu$high_prod == 0, ])
res[["Produced > median"]]  <- run_fe("avg_price",
  farmers_fu[!is.na(farmers_fu$high_prod) & farmers_fu$high_prod == 1, ])
res[["Interaction: treat x high_prod"]] <- run_interaction("high_prod", farmers_fu)

## District (MCCs nest within districts, so district main effect is
## absorbed by the MCC FE; subgroup regressions keep MCC FE within district)
res[["District: Kazo"]]     <- run_fe("avg_price",
  farmers_fu[farmers_fu$district == "Kazo", ])
res[["District: Kiruhura"]] <- run_fe("avg_price",
  farmers_fu[farmers_fu$district == "Kiruhura", ])

## Relationship duration: only ~4% of farmers are new suppliers, too few
## for subgroup price regressions with MCC FE; the composition test below
## uses new_supplier as an outcome instead

## ---------------------------------------------------------------------------
## B. Composition test: treatment effect on being a new supplier
## ---------------------------------------------------------------------------
res[["Outcome: new_supplier"]] <- run_fe("new_supplier", farmers_fu)

## Pass-through descriptive: treatment effect on quality-contingent pricing
res[["Outcome: bett_prc"]] <- run_fe("bett_prc", farmers_fu)

## ---------------------------------------------------------------------------
## Assemble, print, save
## ---------------------------------------------------------------------------
res_price_het_fu <- do.call(rbind, res)
res_price_het_fu <- round(res_price_het_fu, digits = 4)

cat("=====================================================================\n")
cat("Exploratory heterogeneity: Experiment 2 effect on farm-gate price\n")
cat("(avg_price ~ treat | MCC_ID, SEs clustered on trader_ID)\n")
cat("=====================================================================\n")
cat(sprintf("%-38s %9s %8s %7s %5s %10s %s\n",
            "", "Coef", "SE", "p", "N", "Ctrl mean", ""))
for (nm in rownames(res_price_het_fu)) {
  r <- res_price_het_fu[nm, ]
  cat(sprintf("%-38s %9.2f %8.2f %7.3f %5d %10.2f %s\n",
              nm, r["coef"], r["se"], r["p"], r["n"],
              r["ctrl_mean"], format_stars(r["p"])))
}

saveRDS(res_price_het_fu,
        file = paste(path, "paper/results/res_price_het_fu.rds", sep = "/"))

cat("\n09_heterogeneity_price_fu.R completed. Results saved to paper/results/res_price_het_fu.rds\n")
