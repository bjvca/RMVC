###############################################################################
## 00_utilities.R
##
## Shared utility functions for the dairy RCT analysis
## (quality measurement and digital monitoring system experiment).
## Sourced by all other analysis scripts.
##
## Must be run with working directory set to paper/analysis/.
###############################################################################

# ===========================================================================
# Inverse hyperbolic sine transform for skewed outcomes
# ===========================================================================
ihs <- function(x) {
  log(x + sqrt(x^2 + 1))
}

# ===========================================================================
# Significance stars for Sexpr in LyX
# ===========================================================================
format_stars <- function(p) {
  ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.1, "*", "")))
}

# ===========================================================================
# CR2 cluster-robust regression wrapper
# Returns named list: model, coef_table, vcov, n
# ===========================================================================
cr2_ols <- function(formula, data, cluster_var) {
  ols <- lm(formula, data = data)
  vcov_cl <- clubSandwich::vcovCR(ols, cluster = data[[cluster_var]], type = "CR2")
  res <- clubSandwich::coef_test(ols, vcov_cl)
  list(model = ols, coef_table = res, vcov = vcov_cl, n = nobs(ols))
}

# ===========================================================================
# Forest plot builder for ggplot2
# Takes a data frame with columns: Outcome, Treatment, Estimate, SE,
# CI_Lower, CI_Upper
# ===========================================================================
make_forest_plot <- function(forest_data, title, filename, path,
                             colors = c("Lactoscan" = "#1b9e77",
                                        "Video" = "#d95f02",
                                        "Bundle" = "#7570b3")) {
  library(ggplot2)
  p <- ggplot(forest_data, aes(x = Estimate, y = Outcome, color = Treatment)) +
    geom_point(position = position_dodge(width = 0.6), size = 4) +
    geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper),
                   position = position_dodge(width = 0.6),
                   height = 0.3, linewidth = 1) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black",
               linewidth = 1.2) +
    theme_minimal(base_size = 16) +
    labs(title = title, x = "Treatment Effect Estimate", y = "",
         color = "Treatment") +
    scale_color_manual(values = colors) +
    theme(axis.text.y = element_text(size = 14, face = "bold"),
          plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

  ggsave(file = paste(path, "paper/figures", filename, sep = "/"),
         plot = p, width = 10, height = 6, dpi = 300)
  invisible(p)
}

# ===========================================================================
# run_farmer_regressions() -- DEPRECATED
# ---------------------------------------------------------------------------
# This function was built around the old 32/33-column layout with trader
# heterogeneity interactions. It is no longer used after the simplification
# to remove the trader heterogeneity dimension. Inline loops in
# 02_analysis_main.R now use the simplified 15-column (primary) or
# 11-column (secondary) layouts instead.
# ===========================================================================
