## ---------------------------------------------------------------------------
## fig_forest_table9.R
## Forest plot of Table 9 (follow-up experiment: farmer-level outcomes) for
## slides, in the same style as the other forest plots:
##   - non-index outcomes standardised by their control-group SD (computed from
##     the prepped data); the two Anderson indices stay in native SD units
##   - thick 90% CI inside a thin 95% CI
##   - green = significant at 5% (randomization-inference p, as in the table),
##     grey otherwise; the summary index is drawn as a diamond
## ---------------------------------------------------------------------------

library(ggplot2)
library(dplyr)

path    <- "/home/claude/workspace/RMVC"
fig_dir <- file.path(path, "paper/figures")

res <- readRDS(file.path(path, "paper/results/res_farmers_fu.rds"))   # coef se p n ctrl_mean
ri  <- readRDS(file.path(path, "paper/results/randomization_inference.rds"))
dat <- readRDS(file.path(path, "paper/results/prepped_farmers_fu.rds"))

sig_colors <- c("p < 0.05" = "#1b9e77", "Not significant" = "#9aa3ab")

keys <- c("avg_price", "quality_checked", "feeding_index", "used_bran",
          "used_residu", "used_lick", "used_cgrazing", "farmer_fu_index")
labels <- c(
  avg_price       = "Average price received (UGX/L)",
  quality_checked = "Quality checked by trader",
  feeding_index   = "Feeding practices index",
  used_bran       = "Used bran/concentrates",
  used_residu     = "Used crop residues",
  used_lick       = "Used salt/mineral lick",
  used_cgrazing   = "Controlled grazing",
  farmer_fu_index = "Summary index (Anderson)"
)
is_index   <- c("feeding_index", "farmer_fu_index")   # native SD units
is_summary <- "farmer_fu_index"

## Control-group SD for non-index outcomes (exact, from prepped data)
ctrl <- dat[dat$treat == 0, ]
ctrl_sd <- sapply(keys, function(k) {
  if (k %in% is_index) return(1)        # index already in SD-like units
  sd(ctrl[[k]], na.rm = TRUE)
})

d <- data.frame(
  key     = keys,
  outcome = labels[keys],
  coef_raw = res[, "coef"],
  se_raw   = res[, "se"],
  sd_unit  = ctrl_sd,
  p        = ri$exp2_farmers$ri_p[keys],
  stringsAsFactors = FALSE
) %>%
  mutate(
    coef    = coef_raw / sd_unit,
    se      = se_raw   / sd_unit,
    ci95_lo = coef - 1.96  * se,
    ci95_hi = coef + 1.96  * se,
    ci90_lo = coef - 1.645 * se,
    ci90_hi = coef + 1.645 * se,
    sig     = factor(ifelse(p < 0.05, "p < 0.05", "Not significant"),
                     levels = names(sig_colors)),
    is_summary = key %in% is_summary
  )

ord <- c("farmer_fu_index", "avg_price", "quality_checked", "feeding_index",
         "used_bran", "used_residu", "used_lick", "used_cgrazing")
d$outcome <- factor(d$outcome, levels = rev(labels[ord]))

p <- ggplot(d, aes(x = coef, y = outcome, color = sig)) +
  geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.9) +
  geom_errorbarh(aes(xmin = ci95_lo, xmax = ci95_hi), height = 0.18, linewidth = 0.7) +
  geom_errorbarh(aes(xmin = ci90_lo, xmax = ci90_hi), height = 0, linewidth = 2.4) +
  geom_point(aes(shape = is_summary, size = is_summary), color = "white") +
  geom_point(aes(shape = is_summary, size = is_summary)) +
  scale_color_manual(values = sig_colors, name = "", drop = FALSE) +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 18), guide = "none") +
  scale_size_manual(values = c(`FALSE` = 3.2, `TRUE` = 4.8), guide = "none") +
  labs(title = "Follow-up Experiment: Farmer-Level Outcomes",
       subtitle = "Effect of sourcing from a bonus-receiving trader",
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

ggsave(file.path(fig_dir, "fig_forest_farmer_table9.png"), p, width = 10, height = 6,
       dpi = 300, device = "png")
ggsave(file.path(fig_dir, "fig_forest_farmer_table9.eps"), p, width = 10, height = 6,
       device = "eps")
cat("Saved fig_forest_farmer_table9 (.png/.eps)\n")
print(d[, c("outcome", "coef", "ci90_lo", "ci90_hi", "p", "sig")])
