## ---------------------------------------------------------------------------
## fig_forest_quality.R
## Standalone forest plot of Table 3 (Milk quality, Experiment 1) for slides.
## Coefficients standardised by the control-group SD so the five physical
## components share a common axis with the Anderson index (already in SD units).
## Each estimate shows a thick 90% CI inside a thin 95% CI.
## ---------------------------------------------------------------------------

library(ggplot2)
library(dplyr)

path    <- "/home/claude/workspace/RMVC"
fig_dir <- file.path(path, "paper/figures")

x <- readRDS(file.path(path, "paper/results/res_samples.rds"))

labels <- c(
  Fat                          = "Butter Fat",
  SNF                          = "Solids-not-fat (SNF)",
  Added.Water                  = "Added Water",
  Protein                      = "Protein",
  Corrected.Lactometer.Reading = "Density (CLR)",
  samples_index                = "Quality Index (all five)"
)

df <- data.frame(
  key       = rownames(x),
  outcome   = labels[rownames(x)],
  ctrl_sd   = x[, "ctrl_sd"],
  coef_raw  = x[, "ols_coef"],
  se_raw    = x[, "ols_se"],
  p         = x[, "ols_p"],
  stringsAsFactors = FALSE
)

## Standardise components by control SD; the index coefficient is already in
## control-SD units (paper reports it directly as "0.21 SD").
df <- df %>%
  mutate(
    sd_unit = ifelse(key == "samples_index", 1, ctrl_sd),
    coef    = coef_raw / sd_unit,
    se      = se_raw   / sd_unit,
    ci95_lo = coef - 1.96  * se,
    ci95_hi = coef + 1.96  * se,
    ci90_lo = coef - 1.645 * se,
    ci90_hi = coef + 1.645 * se,
    sig     = ifelse(p < 0.05, "p < 0.05", "Not significant")
  )

## Display order: index at top (headline), then components
ord <- c("samples_index", "Fat", "Corrected.Lactometer.Reading",
         "Added.Water", "SNF", "Protein")
df$outcome <- factor(df$outcome, levels = rev(labels[ord]))

## Mark the summary index with a bold face by appending a flag column
df$is_index <- df$key == "samples_index"

sig_colors <- c("p < 0.05" = "#1b9e77", "Not significant" = "#9aa3ab")

p <- ggplot(df, aes(x = coef, y = outcome, color = sig)) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.9) +
  ## Thin outer whisker = 95% CI
  geom_errorbarh(aes(xmin = ci95_lo, xmax = ci95_hi), height = 0.18, linewidth = 0.7) +
  ## Thick inner whisker = 90% CI
  geom_errorbarh(aes(xmin = ci90_lo, xmax = ci90_hi), height = 0, linewidth = 2.4) +
  geom_point(aes(shape = is_index, size = is_index), color = "white") +
  geom_point(aes(shape = is_index, size = is_index)) +
  scale_color_manual(values = sig_colors, name = "") +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 18), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 3.2, `TRUE` = 4.8), guide = "none") +
  labs(
    title    = "Experiment 1: Milk Quality at the MCC",
    subtitle = "Effect of providing a milk analyzer (control-SD units; thick = 90% CI, thin = 95% CI)",
    x        = "Treatment effect (standard deviations)",
    y        = ""
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 13, color = "#44546A"),
    axis.text.y   = element_text(size = 14, face = "bold"),
    axis.title.x  = element_text(size = 12),
    legend.position = "top"
  )

ggsave(file.path(fig_dir, "fig_forest_quality_exp1.png"),
       p, width = 10, height = 6, dpi = 300, device = "png")
ggsave(file.path(fig_dir, "fig_forest_quality_exp1.eps"),
       p, width = 10, height = 6, device = "eps")

cat("Saved fig_forest_quality_exp1 (.png/.eps)\n")
print(df[, c("outcome", "coef", "ci90_lo", "ci90_hi", "ci95_lo", "ci95_hi", "p", "sig")])
