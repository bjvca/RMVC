###############################################################################
## 05_figures.R
##
## Generates all figures for the combined paper:
##   - Time series plots for Experiment 2 daily submission data
##     (quality premium intervention using digital monitoring system)
##   - Forest plots for Experiment 1 and Experiment 2 treatment effects
##
## Must be run with working directory set to paper/analysis/.
## Sources 00_utilities.R for helper functions.
##
## Outputs (saved to paper/figures/):
##   fig_daily_fat.png / .eps       — daily fat % by treatment
##   fig_daily_quantity.png / .eps  — daily quantity by treatment
##   fig_daily_snf.png / .eps      — daily SNF % by treatment
##   fig_daily_bonus.png / .eps    — daily bonus by treatment
##   fig_forest_farmer_primary.png / .eps — Exp 1 farmer primary outcomes
##   fig_forest_mcc_primary.png / .eps    — Exp 1 MCC primary outcomes
##   fig_forest_prices_exp1.png / .eps    — Exp 1 price outcomes (all null)
##   fig_forest_trader_exp2.png / .eps    — Exp 2 trader outcomes
##   fig_forest_farmer_exp2.png / .eps    — Exp 2 farmer outcomes
###############################################################################

source("00_utilities.R")

library(ggplot2)
library(dplyr)
library(tidyr)

## ---------------------------------------------------------------------------
## Detect project root and set up paths
## ---------------------------------------------------------------------------
path <- strsplit(getwd(), "/paper/analysis")[[1]]
fig_dir <- paste(path, "paper/figures", sep = "/")
if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

## ---------------------------------------------------------------------------
## Load results and prepped data
## ---------------------------------------------------------------------------
submissions_list   <- readRDS(paste(path, "paper/results/prepped_submissions.rds", sep = "/"))
trader_day         <- submissions_list$trader_day
daily_avg          <- submissions_list$daily_avg
res_farmers        <- readRDS(paste(path, "paper/results/res_farmers.rds", sep = "/"))
res_MCCs           <- readRDS(paste(path, "paper/results/res_MCCs.rds", sep = "/"))
res_farmers_sec_sold <- readRDS(paste(path, "paper/results/res_farmers_sec_sold.rds", sep = "/"))
res_traders        <- readRDS(paste(path, "paper/results/res_traders.rds", sep = "/"))
res_farmers_fu     <- readRDS(paste(path, "paper/results/res_farmers_fu.rds", sep = "/"))

## ---------------------------------------------------------------------------
## Consistent styling
## ---------------------------------------------------------------------------
custom_colors_treat <- c("Treatment" = "#d95f02", "Control" = "#1b9e77")
custom_colors_3arm  <- c("Lactoscan" = "#1f77b4", "Video" = "#ff7f0e",
                          "Bundle" = "#2ca02c")

## Helper: save plot in both PNG and EPS
save_plot <- function(plot, filename, width = 10, height = 6, dpi = 300) {
  ggsave(file = paste(fig_dir, paste0(filename, ".png"), sep = "/"),
         plot = plot, width = width, height = height, dpi = dpi, device = "png")
  ggsave(file = paste(fig_dir, paste0(filename, ".eps"), sep = "/"),
         plot = plot, width = width, height = height, device = "eps")
}


###############################################################################
## 1-4. TIME SERIES PLOTS: Daily outcomes by treatment group
###############################################################################
## Compute daily means and SEs by date and treatment group

daily_avg <- trader_day %>%
  mutate(group = ifelse(treatment == 1, "Treatment", "Control")) %>%
  group_by(date, group) %>%
  summarise(
    mean_fat  = mean(avg_fat, na.rm = TRUE),
    se_fat    = sd(avg_fat, na.rm = TRUE) / sqrt(n()),
    mean_snf  = mean(avg_snf, na.rm = TRUE),
    se_snf    = sd(avg_snf, na.rm = TRUE) / sqrt(n()),
    mean_qty  = mean(total_qty, na.rm = TRUE),
    se_qty    = sd(total_qty, na.rm = TRUE) / sqrt(n()),
    mean_bonus = mean(bonus, na.rm = TRUE),
    se_bonus   = sd(bonus, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

## Stage transition dates
stage1_date <- as.Date("2025-11-01")
stage2_date <- as.Date("2025-11-15")

## Helper: create a time series plot
make_timeseries <- function(data, y_mean, y_se, y_label, title, filename) {
  data$y_mean <- data[[y_mean]]
  data$y_se   <- data[[y_se]]

  p <- ggplot(data, aes(x = date, y = y_mean, color = group, fill = group)) +
    geom_ribbon(aes(ymin = y_mean - y_se, ymax = y_mean + y_se),
                alpha = 0.2, color = NA) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.5, alpha = 0.7) +
    geom_vline(xintercept = stage1_date, linetype = "dashed",
               color = "grey40", linewidth = 0.6) +
    geom_vline(xintercept = stage2_date, linetype = "dashed",
               color = "grey40", linewidth = 0.6) +
    annotate("text", x = stage1_date, y = Inf, label = "Stage 1",
             hjust = -0.1, vjust = 1.5, size = 4, color = "grey30") +
    annotate("text", x = stage2_date, y = Inf, label = "Stage 2",
             hjust = -0.1, vjust = 1.5, size = 4, color = "grey30") +
    scale_color_manual(values = custom_colors_treat) +
    scale_fill_manual(values = custom_colors_treat) +
    labs(title = title, x = "Date", y = y_label,
         color = "Group", fill = "Group") +
    theme_minimal(base_size = 16) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "top")

  save_plot(p, filename)
  p
}

## Figure 1: Daily fat by treatment
make_timeseries(daily_avg, "mean_fat", "se_fat",
                "Average Fat (%)", "Daily Fat Content by Treatment Group",
                "fig_daily_fat")

## Figure 2: Daily quantity by treatment
make_timeseries(daily_avg, "mean_qty", "se_qty",
                "Total Quantity (liters)", "Daily Milk Quantity by Treatment Group",
                "fig_daily_quantity")

## Figure 3: Daily SNF by treatment
make_timeseries(daily_avg, "mean_snf", "se_snf",
                "Average SNF (%)", "Daily SNF Content by Treatment Group",
                "fig_daily_snf")

## Figure 4: Daily bonus by treatment
make_timeseries(daily_avg, "mean_bonus", "se_bonus",
                "Bonus (UGX)", "Daily Bonus Payments by Treatment Group",
                "fig_daily_bonus")


###############################################################################
## 5. FOREST PLOT: Experiment 1 farmer primary outcomes
###############################################################################
## Uses orthogonalized (demeaned) coefficients for the measurement & monitoring
## system (T1): pooled marginal effect. Cols 12-14 of res_farmers (new layout).

outcome_names_farmer <- c("Improved Practices Index", "Buyer Checks w/ MA",
                          "Average Sales Price", "Gets Quality Bonus")
## Exclude Anderson index row (row 5)
idx_primary <- 1:4

forest_farmer_primary <- data.frame(
  outcome = outcome_names_farmer,
  coef = res_farmers[idx_primary, 12],
  se   = res_farmers[idx_primary, 13],
  stringsAsFactors = FALSE
)

forest_farmer_primary <- forest_farmer_primary %>%
  mutate(
    ci_lo = coef - 1.96 * se,
    ci_hi = coef + 1.96 * se,
    outcome = factor(outcome, levels = rev(outcome_names_farmer))
  )

p5 <- ggplot(forest_farmer_primary, aes(x = coef, y = outcome)) +
  geom_point(size = 4, color = "#1f77b4") +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0.25,
                 linewidth = 0.8, color = "#1f77b4") +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 1) +
  labs(title = "Experiment 1: Farmer Primary Outcomes",
       x = "Treatment Effect (95% CI)", y = "") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 14, face = "bold"))

save_plot(p5, "fig_forest_farmer_primary")


###############################################################################
## 6. FOREST PLOT: Experiment 1 MCC primary outcomes
###############################################################################
## Uses OLS treatment effect (cols 3-5) with 95% CI.

outcome_names_mcc <- c("Tests Incoming (MA)", "Tests Outgoing (MA)",
                       "Price Bought", "Avg Sales Price",
                       "Gives Quality Bonus", "Gets Quality Bonus")
idx_mcc <- 1:6

forest_mcc <- data.frame(
  outcome = outcome_names_mcc,
  coef    = res_MCCs[idx_mcc, 3],
  se      = res_MCCs[idx_mcc, 4],
  stringsAsFactors = FALSE
)

forest_mcc <- forest_mcc %>%
  mutate(
    ci_lo   = coef - 1.96 * se,
    ci_hi   = coef + 1.96 * se,
    outcome = factor(outcome, levels = rev(outcome_names_mcc))
  )

p6 <- ggplot(forest_mcc, aes(x = coef, y = outcome)) +
  geom_point(size = 4, color = "#1f77b4") +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0.25,
                 linewidth = 0.8, color = "#1f77b4") +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 1) +
  labs(title = "Experiment 1: MCC Primary Outcomes (OLS)",
       x = "Treatment Effect (95% CI)", y = "") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 14, face = "bold"))

save_plot(p6, "fig_forest_mcc_primary")


###############################################################################
## 7. FOREST PLOT: Experiment 1 prices (all null)
###############################################################################
## Shows the treatment effect on prices.
## Primary array (res_farmers): orthog treat = cols 12-13 (new 15-col layout).
## Secondary array (res_farmers_sec_sold): treat = cols 3-4 (new 11-col layout).

price_labels <- c("Price Last Week", "Price Wet Season", "Price Dry Season")

forest_prices <- data.frame(
  outcome = price_labels,
  coef = c(
    res_farmers[3, 12], res_farmers_sec_sold[4, 3], res_farmers_sec_sold[5, 3]
  ),
  se = c(
    res_farmers[3, 13], res_farmers_sec_sold[4, 4], res_farmers_sec_sold[5, 4]
  ),
  stringsAsFactors = FALSE
)

forest_prices <- forest_prices %>%
  mutate(
    ci_lo = coef - 1.96 * se,
    ci_hi = coef + 1.96 * se,
    outcome = factor(outcome, levels = rev(price_labels))
  )

p7 <- ggplot(forest_prices, aes(x = coef, y = outcome)) +
  geom_point(size = 4, color = "#1f77b4") +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0.25,
                 linewidth = 0.8, color = "#1f77b4") +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 1) +
  labs(title = "Experiment 1: Price Outcomes",
       x = "Treatment Effect (95% CI)", y = "") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 14, face = "bold"))

save_plot(p7, "fig_forest_prices_exp1")


###############################################################################
## 8. FOREST PLOT: Experiment 2 trader outcomes
###############################################################################

trader_outcome_labels <- c("Fat (Supervised)", "Delivered Quantity",
                           "Any Rejected", "Pays Premium", "Avg Price Paid")

forest_traders <- data.frame(
  outcome = trader_outcome_labels,
  coef    = res_traders[, 1],
  se      = res_traders[, 2],
  stringsAsFactors = FALSE
)

forest_traders <- forest_traders %>%
  mutate(
    ci_lo   = coef - 1.96 * se,
    ci_hi   = coef + 1.96 * se,
    outcome = factor(outcome, levels = rev(trader_outcome_labels))
  )

p8 <- ggplot(forest_traders, aes(x = coef, y = outcome)) +
  geom_point(size = 4, color = "#d95f02") +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0.25,
                 linewidth = 0.8, color = "#d95f02") +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 1) +
  labs(title = "Experiment 2: Trader Endline Outcomes",
       x = "Treatment Effect (95% CI)", y = "") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 14, face = "bold"))

save_plot(p8, "fig_forest_trader_exp2")


###############################################################################
## 9. FOREST PLOT: Experiment 2 farmer outcomes
###############################################################################

farmer_fu_labels <- c("Avg Price Received", "Quality Checked", "Feeding Index",
                      "Used Bran", "Used Crop Residue", "Used Salt Lick",
                      "Controlled Grazing")

forest_farmers_fu <- data.frame(
  outcome = farmer_fu_labels,
  coef    = res_farmers_fu[, 1],
  se      = res_farmers_fu[, 2],
  stringsAsFactors = FALSE
)

forest_farmers_fu <- forest_farmers_fu %>%
  mutate(
    ci_lo   = coef - 1.96 * se,
    ci_hi   = coef + 1.96 * se,
    outcome = factor(outcome, levels = rev(farmer_fu_labels))
  )

p9 <- ggplot(forest_farmers_fu, aes(x = coef, y = outcome)) +
  geom_point(size = 4, color = "#7570b3") +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi), height = 0.25,
                 linewidth = 0.8, color = "#7570b3") +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 1) +
  labs(title = "Experiment 2: Farmer Follow-up Outcomes",
       x = "Treatment Effect (95% CI)", y = "") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 14, face = "bold"))

save_plot(p9, "fig_forest_farmer_exp2")


###############################################################################
## 10. CDF PLOT: Added Water by Treatment Group (Experiment 1)
###############################################################################
## Shows the empirical CDF of added water (%) for treatment vs control samples.
## Key pattern: treatment effect concentrated in the right tail (heavy adulterators).

samples_cdf <- readRDS(paste(path, "paper/results/samples_for_cdf.rds", sep = "/"))
samples_cdf <- samples_cdf[!is.na(samples_cdf$Added.Water), ]
samples_cdf$group <- ifelse(samples_cdf$treat == "T", "Treatment", "Control")

p10 <- ggplot(samples_cdf, aes(x = Added.Water, color = group, linetype = group)) +
  stat_ecdf(geom = "step", linewidth = 1.2, pad = FALSE) +
  scale_color_manual(values = c("Treatment" = "#1b9e77", "Control" = "#d62728")) +
  scale_linetype_manual(values = c("Treatment" = "solid", "Control" = "dashed")) +
  coord_cartesian(xlim = c(0, 15), ylim = c(0.7, 1.0)) +
  labs(x = "Added Water (%)", y = "Cumulative Proportion",
       color = NULL, linetype = NULL) +
  theme_minimal(base_size = 16) +
  theme(legend.position = c(0.85, 0.25),
        legend.text = element_text(size = 14))

save_plot(p10, "fig_cdf_added_water")


cat("\n05_figures.R completed successfully.\n")
cat("Figures saved to:", fig_dir, "\n")
