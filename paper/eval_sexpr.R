# eval_sexpr.R
# Reads paper_raw.tex, evaluates all \Sexpr{...} expressions using saved RDS results,
# and writes paper_resolved.tex with the values substituted.

# Set working directory to paper/
setwd("/home/claude/workspace/RMVC/paper")
path <- getwd()

# Load all results (matching the knitr setup chunk in the LyX file)
balance <- readRDS(paste0(path, "/results/balance.rds"))
balance_farmer <- readRDS(paste0(path, "/results/balance_farmer.rds"))
F_test <- readRDS(paste0(path, "/results/F_test.rds"))
res_farmers <- readRDS(paste0(path, "/results/res_farmers.rds"))
res_farmers_TOT <- readRDS(paste0(path, "/results/res_farmers_TOT.rds"))
res_farmers_sec_quant <- readRDS(paste0(path, "/results/res_farmers_sec_quant.rds"))
res_farmers_sec_uptake <- readRDS(paste0(path, "/results/res_farmers_sec_uptake.rds"))
res_farmers_sec_sold <- readRDS(paste0(path, "/results/res_farmers_sec_sold.rds"))
res_farmers_sec_switching <- readRDS(paste0(path, "/results/res_farmers_sec_switching.rds"))
res_MCCs <- readRDS(paste0(path, "/results/res_MCCs.rds"))
res_MCCs_sec_uptake <- readRDS(paste0(path, "/results/res_MCCs_sec_uptake.rds"))
res_samples <- readRDS(paste0(path, "/results/res_samples.rds"))
attrition <- readRDS(paste0(path, "/results/attrition.rds"))
ri <- readRDS(paste0(path, "/results/randomization_inference.rds"))
balance_fu <- readRDS(paste0(path, "/results/balance_followup.rds"))
res_submissions <- readRDS(paste0(path, "/results/res_submissions.rds"))
res_traders <- readRDS(paste0(path, "/results/res_traders.rds"))
res_traders_secondary <- readRDS(paste0(path, "/results/res_traders_secondary.rds"))
res_farmers_fu <- readRDS(paste0(path, "/results/res_farmers_fu.rds"))
res_price_het_fu <- readRDS(paste0(path, "/results/res_price_het_fu.rds"))
res_app_decomp <- readRDS(paste0(path, "/results/res_app_decomp.rds"))

# Helper functions (matching those defined in the knitr setup chunk)
stars <- function(p) ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", ifelse(p < 0.1, "+", "")))

fmt <- function(x) {
  ax <- abs(x)
  if (is.na(x)) return("NA")
  if (ax < 1) return(sprintf("%.3f", x))
  if (ax < 100) return(sprintf("%.2f", x))
  return(sprintf("%.1f", x))
}

# Read the raw tex file
tex <- readLines("paper_raw.tex", warn = FALSE)
tex_str <- paste(tex, collapse = "\n")

# Function to find matching closing brace for \Sexpr{...}
# Handles nested braces
find_sexpr_end <- function(s, start) {
  # start points to the '{' after \Sexpr
  depth <- 0
  i <- start
  n <- nchar(s)
  while (i <= n) {
    ch <- substr(s, i, i)
    if (ch == '{') {
      depth <- depth + 1
    } else if (ch == '}') {
      depth <- depth - 1
      if (depth == 0) return(i)
    }
    i <- i + 1
  }
  return(-1)  # not found
}

# Find and replace all \Sexpr{...} expressions
# Work from end to start to preserve positions
sexpr_pattern <- "\\\\Sexpr\\{"
matches <- gregexpr(sexpr_pattern, tex_str, perl = TRUE)[[1]]

if (matches[1] != -1) {
  cat("Found", length(matches), "\\Sexpr expressions\n")

  # Process from last to first to maintain string positions
  for (idx in rev(seq_along(matches))) {
    match_start <- matches[idx]
    # Find the opening brace position
    brace_start <- match_start + 6  # length of \Sexpr is 6, then {
    # Find matching closing brace
    brace_end <- find_sexpr_end(tex_str, brace_start)

    if (brace_end == -1) {
      cat("Warning: unmatched brace at position", match_start, "\n")
      next
    }

    # Extract the R expression
    expr_text <- substr(tex_str, brace_start + 1, brace_end - 1)

    # Evaluate the expression
    result <- tryCatch({
      eval(parse(text = expr_text))
    }, error = function(e) {
      cat("Error evaluating:", expr_text, "\n  ", e$message, "\n")
      paste0("[EVAL ERROR: ", substr(expr_text, 1, 50), "]")
    })

    # Convert result to string
    result_str <- as.character(result)

    # Replace in the string
    full_match <- substr(tex_str, match_start, brace_end)
    tex_str <- paste0(
      substr(tex_str, 1, match_start - 1),
      result_str,
      substr(tex_str, brace_end + 1, nchar(tex_str))
    )
  }
}

# Write the resolved tex file
writeLines(tex_str, "paper_resolved.tex")
cat("Wrote paper_resolved.tex\n")
