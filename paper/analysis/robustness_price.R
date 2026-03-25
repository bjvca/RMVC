###############################################################################
## robustness_price.R
##
## Robustness checks on price outcomes (avg_sales_p) for the dairy RCT.
## Tests whether null result on prices holds across subgroups, alternative
## measures, alternative specifications, and interaction models.
##
## Must be run with working directory set to paper/analysis/.
###############################################################################

source("00_utilities.R")

library(clubSandwich)
library(AER)

path <- strsplit(getwd(), "/paper/analysis")[[1]]

## Load prepped data
farmers_end     <- readRDS(paste(path, "paper/results/prepped_farmers.rds", sep = "/"))
farmers_base    <- readRDS(paste(path, "paper/results/prepped_farmers_base.rds", sep = "/"))
farmers_balance <- readRDS(paste(path, "paper/results/farmers_balance.rds", sep = "/"))
MCC_balance     <- readRDS(paste(path, "paper/results/MCC_balance.rds", sep = "/"))
MCCs_end        <- readRDS(paste(path, "paper/results/prepped_MCCs.rds", sep = "/"))

## ---- Merge extra variables needed for subgroups ----

# District (from endline data - check what's available)
# Herd size from baseline balance data
farmers_end <- merge(farmers_end,
                     farmers_balance[, c("farmer_ID", "herd_size", "coop_member")],
                     by = "farmer_ID", all.x = TRUE)

# Cooperative status of the MCC (from MCC balance data)
farmers_end <- merge(farmers_end,
                     MCC_balance[, c("MCC_ID", "is_coop")],
                     by.x = "MCC_ID_linked", by.y = "MCC_ID", all.x = TRUE)

## ---- Helper: safe CR2 regression returning a one-row summary ----
safe_cr2 <- function(formula, data, cluster_var = "catch_ID", treat_row = 2) {
  tryCatch({
    d <- model.frame(formula, data = data, na.action = na.omit)
    if (nrow(d) < 10) return(c(coef = NA, se = NA, p = NA, n = NA, sig = ""))
    ols <- lm(formula, data = data)
    clust <- data[[cluster_var]][complete.cases(model.frame(formula, data))]
    vcov_cl <- vcovCR(ols, cluster = clust, type = "CR2")
    res <- coef_test(ols, vcov_cl)
    coef_val <- res[treat_row, "beta"]
    se_val   <- res[treat_row, "SE"]
    p_val    <- res[treat_row, "p_Satt"]
    n_val    <- nobs(ols)
    sig <- ifelse(p_val < 0.01, "***",
           ifelse(p_val < 0.05, "**",
           ifelse(p_val < 0.1, "*", "")))
    c(coef = round(coef_val, 3), se = round(se_val, 3),
      p = round(p_val, 3), n = n_val, sig = sig)
  }, error = function(e) {
    c(coef = NA, se = NA, p = NA, n = NA, sig = paste("ERROR:", e$message))
  })
}

## ---- Collect results ----
results <- list()

cat("\n===================================================================\n")
cat("PRICE ROBUSTNESS CHECKS: Treatment effect of Lactoscan on avg_sales_p\n")
cat("===================================================================\n\n")

###############################################################################
## 0. BASELINE: Main specification (ANCOVA)
###############################################################################
cat("--- 0. BASELINE MAIN SPECIFICATION ---\n")
r <- safe_cr2(avg_sales_p ~ treat + vid + b_avg_sales_p, farmers_end)
results[["Main spec (ANCOVA)"]] <- r
cat(sprintf("  Main ANCOVA: coef=%.3f  SE=%.3f  p=%.3f  N=%s %s\n",
            as.numeric(r["coef"]), as.numeric(r["se"]),
            as.numeric(r["p"]), r["n"], r["sig"]))

###############################################################################
## 1. SUBGROUP ANALYSIS
###############################################################################
cat("\n--- 1. SUBGROUP ANALYSIS ---\n")

## 1a. By trader status
for (tv in c(0, 1)) {
  label <- ifelse(tv == 0, "MCC sellers", "Trader sellers")
  sub <- farmers_end[farmers_end$trader == tv, ]
  r <- safe_cr2(avg_sales_p ~ treat + vid + b_avg_sales_p, sub)
  results[[paste0("Subgroup: ", label)]] <- r
  cat(sprintf("  %s: coef=%.3f  SE=%.3f  p=%.3f  N=%s %s\n",
              label, as.numeric(r["coef"]), as.numeric(r["se"]),
              as.numeric(r["p"]), r["n"], r["sig"]))
}

## 1b. By district
if ("district" %in% names(farmers_end)) {
  districts <- unique(farmers_end$district)
  districts <- districts[!is.na(districts)]
  for (d in districts) {
    sub <- farmers_end[farmers_end$district == d, ]
    r <- safe_cr2(avg_sales_p ~ treat + vid + b_avg_sales_p, sub)
    results[[paste0("District: ", d)]] <- r
    cat(sprintf("  District %s: coef=%.3f  SE=%.3f  p=%.3f  N=%s %s\n",
                d, as.numeric(r["coef"]), as.numeric(r["se"]),
                as.numeric(r["p"]), r["n"], r["sig"]))
  }
} else {
  cat("  [district variable not found in farmers_end]\n")
}

## 1c. By herd size (above/below median)
med_herd <- median(farmers_end$herd_size, na.rm = TRUE)
cat(sprintf("  Median herd size: %.1f\n", med_herd))
for (above in c(FALSE, TRUE)) {
  label <- ifelse(above, "Herd > median", "Herd <= median")
  sub <- if (above) farmers_end[!is.na(farmers_end$herd_size) & farmers_end$herd_size > med_herd, ]
         else farmers_end[!is.na(farmers_end$herd_size) & farmers_end$herd_size <= med_herd, ]
  r <- safe_cr2(avg_sales_p ~ treat + vid + b_avg_sales_p, sub)
  results[[paste0("Subgroup: ", label)]] <- r
  cat(sprintf("  %s: coef=%.3f  SE=%.3f  p=%.3f  N=%s %s\n",
              label, as.numeric(r["coef"]), as.numeric(r["se"]),
              as.numeric(r["p"]), r["n"], r["sig"]))
}

## 1d. By baseline price (above/below median)
med_price <- median(farmers_end$b_avg_sales_p, na.rm = TRUE)
cat(sprintf("  Median baseline price: %.1f\n", med_price))
for (above in c(FALSE, TRUE)) {
  label <- ifelse(above, "Baseline price > median", "Baseline price <= median")
  sub <- if (above) farmers_end[!is.na(farmers_end$b_avg_sales_p) & farmers_end$b_avg_sales_p > med_price, ]
         else farmers_end[!is.na(farmers_end$b_avg_sales_p) & farmers_end$b_avg_sales_p <= med_price, ]
  r <- safe_cr2(avg_sales_p ~ treat + vid + b_avg_sales_p, sub)
  results[[paste0("Subgroup: ", label)]] <- r
  cat(sprintf("  %s: coef=%.3f  SE=%.3f  p=%.3f  N=%s %s\n",
              label, as.numeric(r["coef"]), as.numeric(r["se"]),
              as.numeric(r["p"]), r["n"], r["sig"]))
}

## 1e. By cooperative vs non-cooperative MCC
for (coop in c(TRUE, FALSE)) {
  label <- ifelse(coop, "Cooperative MCC", "Non-cooperative MCC")
  sub <- farmers_end[!is.na(farmers_end$is_coop) & farmers_end$is_coop == coop, ]
  r <- safe_cr2(avg_sales_p ~ treat + vid + b_avg_sales_p, sub)
  results[[paste0("Subgroup: ", label)]] <- r
  cat(sprintf("  %s: coef=%.3f  SE=%.3f  p=%.3f  N=%s %s\n",
              label, as.numeric(r["coef"]), as.numeric(r["se"]),
              as.numeric(r["p"]), r["n"], r["sig"]))
}

###############################################################################
## 2. ALTERNATIVE PRICE MEASURES
###############################################################################
cat("\n--- 2. ALTERNATIVE PRICE MEASURES ---\n")

## 2a. Wet season price
r <- safe_cr2(price_wet ~ treat + vid + b_price_wet, farmers_end)
results[["Alt measure: price_wet"]] <- r
cat(sprintf("  Wet season price: coef=%.3f  SE=%.3f  p=%.3f  N=%s %s\n",
            as.numeric(r["coef"]), as.numeric(r["se"]),
            as.numeric(r["p"]), r["n"], r["sig"]))

## 2b. Dry season price
r <- safe_cr2(price_dry ~ treat + vid + b_price_dry, farmers_end)
results[["Alt measure: price_dry"]] <- r
cat(sprintf("  Dry season price: coef=%.3f  SE=%.3f  p=%.3f  N=%s %s\n",
            as.numeric(r["coef"]), as.numeric(r["se"]),
            as.numeric(r["p"]), r["n"], r["sig"]))

## 2c. Maximum price across buyer types (instead of weighted average)
price_cols <- c("q55", "qx2", "qx14", "qx26", "qx38", "qx50")
farmers_end$max_price <- apply(farmers_end[price_cols], 1, function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA else max(x)
})
# Apply same bounds as avg_sales_p
farmers_end$max_price[farmers_end$max_price >= 1500] <- NA
farmers_end$max_price[farmers_end$max_price <= 600]  <- NA

# Baseline max price
farmers_base$b_max_price <- apply(farmers_base[price_cols], 1, function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA else max(x)
})
farmers_base$b_max_price[farmers_base$b_max_price >= 1500] <- NA
farmers_base$b_max_price[farmers_base$b_max_price <= 600]  <- NA
farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_max_price")],
                     by = "farmer_ID", all.x = TRUE)

r <- safe_cr2(max_price ~ treat + vid + b_max_price, farmers_end)
results[["Alt measure: max_price"]] <- r
cat(sprintf("  Max price across buyers: coef=%.3f  SE=%.3f  p=%.3f  N=%s %s\n",
            as.numeric(r["coef"]), as.numeric(r["se"]),
            as.numeric(r["p"]), r["n"], r["sig"]))

## 2d. Unweighted average across buyer types
farmers_end$unwt_avg_price <- rowMeans(farmers_end[price_cols], na.rm = TRUE)
farmers_end$unwt_avg_price[is.nan(farmers_end$unwt_avg_price)] <- NA
farmers_end$unwt_avg_price[farmers_end$unwt_avg_price >= 1500] <- NA
farmers_end$unwt_avg_price[farmers_end$unwt_avg_price <= 600]  <- NA

farmers_base$b_unwt_avg_price <- rowMeans(farmers_base[price_cols], na.rm = TRUE)
farmers_base$b_unwt_avg_price[is.nan(farmers_base$b_unwt_avg_price)] <- NA
farmers_base$b_unwt_avg_price[farmers_base$b_unwt_avg_price >= 1500] <- NA
farmers_base$b_unwt_avg_price[farmers_base$b_unwt_avg_price <= 600]  <- NA
farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_unwt_avg_price")],
                     by = "farmer_ID", all.x = TRUE)

r <- safe_cr2(unwt_avg_price ~ treat + vid + b_unwt_avg_price, farmers_end)
results[["Alt measure: unwt_avg_price"]] <- r
cat(sprintf("  Unweighted avg price: coef=%.3f  SE=%.3f  p=%.3f  N=%s %s\n",
            as.numeric(r["coef"]), as.numeric(r["se"]),
            as.numeric(r["p"]), r["n"], r["sig"]))

###############################################################################
## 3. ALTERNATIVE SPECIFICATIONS
###############################################################################
cat("\n--- 3. ALTERNATIVE SPECIFICATIONS ---\n")

## 3a. Without baseline control (simple difference)
r <- safe_cr2(avg_sales_p ~ treat + vid, farmers_end)
results[["Spec: No baseline control"]] <- r
cat(sprintf("  No baseline control: coef=%.3f  SE=%.3f  p=%.3f  N=%s %s\n",
            as.numeric(r["coef"]), as.numeric(r["se"]),
            as.numeric(r["p"]), r["n"], r["sig"]))

## 3b. With additional controls (herd size + cooperative)
r <- safe_cr2(avg_sales_p ~ treat + vid + b_avg_sales_p + herd_size + is_coop, farmers_end)
results[["Spec: + herd_size + coop"]] <- r
cat(sprintf("  + herd_size + coop: coef=%.3f  SE=%.3f  p=%.3f  N=%s %s\n",
            as.numeric(r["coef"]), as.numeric(r["se"]),
            as.numeric(r["p"]), r["n"], r["sig"]))

## 3c. With district FE
if ("district" %in% names(farmers_end)) {
  r <- safe_cr2(avg_sales_p ~ treat + vid + b_avg_sales_p + factor(district), farmers_end)
  results[["Spec: + district FE"]] <- r
  cat(sprintf("  + district FE: coef=%.3f  SE=%.3f  p=%.3f  N=%s %s\n",
              as.numeric(r["coef"]), as.numeric(r["se"]),
              as.numeric(r["p"]), r["n"], r["sig"]))
}

## 3d. IHS transformation
farmers_end$ihs_avg_sales_p   <- ihs(farmers_end$avg_sales_p)
farmers_end$ihs_b_avg_sales_p <- ihs(farmers_end$b_avg_sales_p)
r <- safe_cr2(ihs_avg_sales_p ~ treat + vid + ihs_b_avg_sales_p, farmers_end)
results[["Spec: IHS(price)"]] <- r
cat(sprintf("  IHS(price): coef=%.3f  SE=%.3f  p=%.3f  N=%s %s\n",
            as.numeric(r["coef"]), as.numeric(r["se"]),
            as.numeric(r["p"]), r["n"], r["sig"]))

## 3e. Log transformation (prices are always positive in valid range)
farmers_end$log_avg_sales_p   <- log(farmers_end$avg_sales_p)
farmers_end$log_b_avg_sales_p <- log(farmers_end$b_avg_sales_p)
r <- safe_cr2(log_avg_sales_p ~ treat + vid + log_b_avg_sales_p, farmers_end)
results[["Spec: log(price)"]] <- r
cat(sprintf("  log(price): coef=%.3f  SE=%.3f  p=%.3f  N=%s %s\n",
            as.numeric(r["coef"]), as.numeric(r["se"]),
            as.numeric(r["p"]), r["n"], r["sig"]))

## 3f. Trimming outliers (1% and 5%)
for (pct in c(0.01, 0.05)) {
  label <- sprintf("Trim %.0f%%", pct * 100)
  lo <- quantile(farmers_end$avg_sales_p, pct, na.rm = TRUE)
  hi <- quantile(farmers_end$avg_sales_p, 1 - pct, na.rm = TRUE)
  sub <- farmers_end[!is.na(farmers_end$avg_sales_p) &
                       farmers_end$avg_sales_p >= lo &
                       farmers_end$avg_sales_p <= hi, ]
  r <- safe_cr2(avg_sales_p ~ treat + vid + b_avg_sales_p, sub)
  results[[paste0("Spec: ", label)]] <- r
  cat(sprintf("  %s (%.0f-%.0f UGX): coef=%.3f  SE=%.3f  p=%.3f  N=%s %s\n",
              label, lo, hi, as.numeric(r["coef"]), as.numeric(r["se"]),
              as.numeric(r["p"]), r["n"], r["sig"]))
}

###############################################################################
## 4. INTERACTION EFFECTS
###############################################################################
cat("\n--- 4. INTERACTION EFFECTS ---\n")

## 4a. treat x video interaction
ols <- lm(avg_sales_p ~ treat * vid + b_avg_sales_p, data = farmers_end)
clust <- farmers_end$catch_ID[complete.cases(
  model.frame(avg_sales_p ~ treat * vid + b_avg_sales_p, farmers_end))]
vcov_cl <- vcovCR(ols, cluster = clust, type = "CR2")
res <- coef_test(ols, vcov_cl)
cat("  treat x vid interaction model:\n")
for (row_i in 1:nrow(res)) {
  cat(sprintf("    %-25s coef=%.3f  SE=%.3f  p=%.3f %s\n",
              rownames(res)[row_i],
              res[row_i, "beta"], res[row_i, "SE"], res[row_i, "p_Satt"],
              format_stars(res[row_i, "p_Satt"])))
}
results[["Interaction: treat"]] <- c(
  coef = round(res["treatTRUE", "beta"], 3),
  se = round(res["treatTRUE", "SE"], 3),
  p = round(res["treatTRUE", "p_Satt"], 3),
  n = nobs(ols),
  sig = format_stars(res["treatTRUE", "p_Satt"]))
results[["Interaction: treat x vid"]] <- c(
  coef = round(res["treatTRUE:vidTRUE", "beta"], 3),
  se = round(res["treatTRUE:vidTRUE", "SE"], 3),
  p = round(res["treatTRUE:vidTRUE", "p_Satt"], 3),
  n = nobs(ols),
  sig = format_stars(res["treatTRUE:vidTRUE", "p_Satt"]))

## 4b. treat x baseline price interaction (centered)
farmers_end$b_price_centered <- farmers_end$b_avg_sales_p -
  mean(farmers_end$b_avg_sales_p, na.rm = TRUE)

ols2 <- lm(avg_sales_p ~ treat * b_price_centered + vid, data = farmers_end)
clust2 <- farmers_end$catch_ID[complete.cases(
  model.frame(avg_sales_p ~ treat * b_price_centered + vid, farmers_end))]
vcov_cl2 <- vcovCR(ols2, cluster = clust2, type = "CR2")
res2 <- coef_test(ols2, vcov_cl2)
cat("\n  treat x baseline_price interaction model:\n")
for (row_i in 1:nrow(res2)) {
  cat(sprintf("    %-35s coef=%.3f  SE=%.3f  p=%.3f %s\n",
              rownames(res2)[row_i],
              res2[row_i, "beta"], res2[row_i, "SE"], res2[row_i, "p_Satt"],
              format_stars(res2[row_i, "p_Satt"])))
}
results[["Interaction: treat x b_price"]] <- c(
  coef = round(res2["treatTRUE:b_price_centered", "beta"], 3),
  se = round(res2["treatTRUE:b_price_centered", "SE"], 3),
  p = round(res2["treatTRUE:b_price_centered", "p_Satt"], 3),
  n = nobs(ols2),
  sig = format_stars(res2["treatTRUE:b_price_centered", "p_Satt"]))

###############################################################################
## SUMMARY TABLE
###############################################################################
cat("\n\n===================================================================\n")
cat("SUMMARY TABLE: All Price Robustness Checks\n")
cat("===================================================================\n")
cat(sprintf("%-40s %8s %8s %8s %6s %4s\n",
            "Specification", "Coef", "SE", "p-val", "N", "Sig"))
cat(paste(rep("-", 75), collapse = ""), "\n")

for (nm in names(results)) {
  r <- results[[nm]]
  cat(sprintf("%-40s %8s %8s %8s %6s %4s\n",
              nm, r["coef"], r["se"], r["p"], r["n"], r["sig"]))
}

## Count significant results
n_sig <- sum(sapply(results, function(r) r["sig"] != "" & r["sig"] != "NA"),
             na.rm = TRUE)
cat(sprintf("\nSignificant results (p < 0.10): %d out of %d checks\n",
            n_sig, length(results)))

cat("\nrobustness_price.R completed.\n")
