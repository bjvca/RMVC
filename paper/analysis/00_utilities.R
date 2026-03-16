###############################################################################
## 00_utilities.R
##
## Shared utility functions for the dairy RCT analysis.
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
# run_farmer_regressions()
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
#  19-21: orthog treat (coef, SE, p) -- demeaned vid & trader
#  22-24: orthog vid (coef, SE, p) -- demeaned treat & trader
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
    vcov_cl <- clubSandwich::vcovCR(ols, cluster = data_full$catch_ID, type = "CR2")
    res <- clubSandwich::coef_test(ols, vcov_cl)

    ctrl <- data_full[data_full$vid == FALSE & data_full$treat == FALSE, outcomes[i]]
    res_arr[i, 1]     <- mean(ctrl, na.rm = TRUE)
    res_arr[i, 2]     <- sd(ctrl, na.rm = TRUE)
    res_arr[i, 3:5]   <- c(res[2, 2], res[2, 3], res[2, 7])         # treat
    res_arr[i, 6:8]   <- c(res[3, 2], res[3, 3], res[3, 7])         # vid
    res_arr[i, 9:12]  <- c(res[int_row, 2], res[int_row, 3],        # treat:vid
                            res[int_row, 7], nobs(ols))

    res_arr[i, 13] <- car::linearHypothesis(ols, "treatTRUE = treatTRUE:trader",
                                             vcov. = vcov_cl)[[4]][2]
    res_arr[i, 14] <- car::linearHypothesis(ols, "vidTRUE = vidTRUE:trader",
                                             vcov. = vcov_cl)[[4]][2]
    res_arr[i, het_int_col] <- car::linearHypothesis(
      ols, "treatTRUE:vidTRUE = treatTRUE:vidTRUE:trader",
      vcov. = vcov_cl)[[4]][2]

    ## --- (b) Bundle comparison: control vs both-treated ---
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
    vcov_int <- clubSandwich::vcovCR(ols_int, cluster = data_int$catch_ID, type = "CR2")
    res_int  <- clubSandwich::coef_test(ols_int, vcov_int)
    res_arr[i, 27:29] <- c(res_int[2, 2], res_int[2, 3], res_int[2, 7])
    res_arr[i, 30]    <- res_int[trader_row, 7]
  }

  ## Sharpened q-values for full model (exclude index row)
  idx <- 1:(n_out - 1)
  res_arr[idx, 16] <- andersonTools::anderson_sharp_q(res_arr[idx, 5])
  res_arr[idx, 17] <- andersonTools::anderson_sharp_q(res_arr[idx, 8])
  res_arr[idx, 18] <- andersonTools::anderson_sharp_q(res_arr[idx, 11])

  ## --- (c) Demeaned orthogonal models with trader heterogeneity ---
  ## treat*vid_demeaned*trader_demeaned
  ## Row 6 = treat:trader_demeaned interaction p-value (in 8-coef model)
  ## Row 2 = treat coefficient (pooled over demeaned vid & trader)

  for (i in seq_along(outcomes)) {
    ols_dm <- lm(as.formula(paste(outcomes[i],
                                  "~ treat*vid_demeaned*trader_demeaned")),
                 data = data_full)
    vcov_dm <- clubSandwich::vcovCR(ols_dm, cluster = data_full$catch_ID, type = "CR2")
    res_dm  <- clubSandwich::coef_test(ols_dm, vcov_dm)
    res_arr[i, 19:21] <- c(res_dm[2, 2], res_dm[2, 3], res_dm[2, 7])
    res_arr[i, 31]    <- res_dm[6, 7]  # treat:trader_demeaned het p
  }

  for (i in seq_along(outcomes)) {
    ols_dm <- lm(as.formula(paste(outcomes[i],
                                  "~ vid*treat_demeaned*trader_demeaned")),
                 data = data_full)
    vcov_dm <- clubSandwich::vcovCR(ols_dm, cluster = data_full$catch_ID, type = "CR2")
    res_dm  <- clubSandwich::coef_test(ols_dm, vcov_dm)
    res_arr[i, 22:24] <- c(res_dm[2, 2], res_dm[2, 3], res_dm[2, 7])
    res_arr[i, 32]    <- res_dm[6, 7]  # vid:treat_demeaned het p
  }

  ## Sharpened q-values for demeaned p-values
  res_arr[idx, 25] <- andersonTools::anderson_sharp_q(res_arr[idx, 21])
  res_arr[idx, 26] <- andersonTools::anderson_sharp_q(res_arr[idx, 24])

  round(res_arr, digits = 3)
}
