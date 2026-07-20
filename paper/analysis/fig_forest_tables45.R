## ---------------------------------------------------------------------------
## fig_forest_tables45.R
## Forest plots of Table 4 (farmer primary outcomes) and Table 5 (MCC primary
## outcomes) for slides, in the same style as fig_forest_quality.R:
##   - non-index outcomes standardised by their control-group SD; Anderson
##     indices are left in their native (control-SD) units
##   - thick 90% CI inside a thin 95% CI
##   - green = significant at 5% (per each table's reported p-value basis),
##     grey otherwise; the summary index is drawn as a diamond
## ---------------------------------------------------------------------------

library(ggplot2)
library(dplyr)

path    <- "/home/claude/workspace/RMVC"
fig_dir <- file.path(path, "paper/figures")

res_farmers <- readRDS(file.path(path, "paper/results/res_farmers.rds"))
res_MCCs    <- readRDS(file.path(path, "paper/results/res_MCCs.rds"))
ri          <- readRDS(file.path(path, "paper/results/randomization_inference.rds"))

sig_colors <- c("p < 0.05" = "#1b9e77", "Not significant" = "#9aa3ab")

## Generic builder -----------------------------------------------------------
## d needs columns: outcome, ctrl_sd, coef_raw, se_raw, p, is_index, is_summary
make_forest <- function(d, order_keys, title, subtitle, file) {
  d <- d %>%
    mutate(
      sd_unit = ifelse(is_index, 1, ctrl_sd),   # indices already in SD units
      coef    = coef_raw / sd_unit,
      se      = se_raw   / sd_unit,
      ci95_lo = coef - 1.96  * se,
      ci95_hi = coef + 1.96  * se,
      ci90_lo = coef - 1.645 * se,
      ci90_hi = coef + 1.645 * se,
      sig     = ifelse(p < 0.05, "p < 0.05", "Not significant"),
      sig     = factor(sig, levels = names(sig_colors))
    )
  d$outcome <- factor(d$outcome, levels = rev(d$outcome[match(order_keys, d$key)]))

  p <- ggplot(d, aes(x = coef, y = outcome, color = sig)) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.9) +
    geom_errorbarh(aes(xmin = ci95_lo, xmax = ci95_hi), height = 0.18, linewidth = 0.7) +
    geom_errorbarh(aes(xmin = ci90_lo, xmax = ci90_hi), height = 0, linewidth = 2.4) +
    geom_point(aes(shape = is_summary, size = is_summary), color = "white") +
    geom_point(aes(shape = is_summary, size = is_summary)) +
    scale_color_manual(values = sig_colors, name = "", drop = FALSE) +
    scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 18), guide = "none") +
    scale_size_manual(values = c(`FALSE` = 3.2, `TRUE` = 4.8), guide = "none") +
    labs(title = title, subtitle = subtitle,
         x = "Treatment effect (standard deviations)", y = "",
         caption = "Standardised by control-group SD. Whiskers: thick = 90% CI, thin = 95% CI.") +
    theme_minimal(base_size = 16) +
    theme(
      plot.title    = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 13, color = "#44546A"),
      plot.caption  = element_text(hjust = 0.5, size = 11, color = "#6b7178"),
      axis.text.y   = element_text(size = 14, face = "bold"),
      axis.title.x  = element_text(size = 12),
      legend.position = "top",
      plot.margin   = margin(8, 16, 8, 8)
    )

  ggsave(file.path(fig_dir, paste0(file, ".png")), p, width = 10, height = 6,
         dpi = 300, device = "png")
  ggsave(file.path(fig_dir, paste0(file, ".eps")), p, width = 10, height = 6,
         device = "eps")
  cat("Saved", file, "\n")
  print(d[, c("outcome", "coef", "ci90_lo", "ci90_hi", "p", "sig")])
}

## --- Table 4: farmers (significance from randomization-inference p-values) ---
farmers <- data.frame(
  key       = c("improve_index", "check_MA", "avg_sales_p", "gets_q_bonus",
                "primary_farmer_index"),
  outcome   = c("Production & management (index)", "Buyer checked quality",
                "Price received (UGX/L)", "Gets quality premium",
                "Primary outcomes index"),
  ctrl_sd   = res_farmers[, 2],
  coef_raw  = res_farmers[, 3],
  se_raw    = res_farmers[, 4],
  p         = ri$farmer$ri_p_treat,
  stringsAsFactors = FALSE
)
farmers$is_index   <- farmers$key %in% c("improve_index", "primary_farmer_index")
farmers$is_summary <- farmers$key == "primary_farmer_index"

make_forest(
  farmers,
  order_keys = c("primary_farmer_index", "improve_index", "check_MA",
                 "avg_sales_p", "gets_q_bonus"),
  title    = "Experiment 1: Farmer Primary Outcomes",
  subtitle = "Effect of providing a milk analyzer",
  file     = "fig_forest_farmer_table4"
)

## --- Table 5: MCCs (significance from analytic CR2 p-values, as in the table) -
mcc <- data.frame(
  key       = c("test_MA_in", "test_MA_out", "price_bought", "avg_sales_p",
                "gives_q_bonus", "gets_q_bonus", "primary_MCC_index"),
  outcome   = c("Tests incoming milk", "Tests outgoing milk",
                "Price paid to suppliers (UGX/L)", "Price received (UGX/L)",
                "Pays quality premium", "Buyer pays premium",
                "Primary outcomes index"),
  ctrl_sd   = res_MCCs[, 2],
  coef_raw  = res_MCCs[, 3],
  se_raw    = res_MCCs[, 4],
  p         = res_MCCs[, 5],
  stringsAsFactors = FALSE
)
mcc$is_index   <- mcc$key == "primary_MCC_index"
mcc$is_summary <- mcc$key == "primary_MCC_index"

make_forest(
  mcc,
  order_keys = c("primary_MCC_index", "test_MA_in", "test_MA_out",
                 "price_bought", "avg_sales_p", "gives_q_bonus", "gets_q_bonus"),
  title    = "Experiment 1: MCC Primary Outcomes",
  subtitle = "Effect of providing a milk analyzer",
  file     = "fig_forest_mcc_table5"
)
