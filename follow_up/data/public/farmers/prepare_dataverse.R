# prepare_dataverse.R
# Prepares farmer endline data for Dataverse distribution
# Creates: farmers_endline.dta, farmers_endline.rds, codebook.md

library(readxl)
library(haven)
library(labelled)

# === Load data ===
df <- read.csv("anonymized_farmer_endline_data.csv", stringsAsFactors = FALSE)

# === Rename columns: replace dots with underscores (Stata compatibility) ===
names(df) <- gsub("\\.", "_", names(df))

# === Load XLSForm metadata ===
survey <- read_excel("Farmers_Endline_used.xlsx", sheet = "survey")
choices <- read_excel("Farmers_Endline_used.xlsx", sheet = "choices")

# === Define variable labels ===
var_labels <- list(
  farmer_ID = "Farmer ID (anonymized)",
  trader_ID = "Trader ID (anonymized)",
  MCC_ID = "Milk Collection Center ID (anonymized)",
  today = "Date of survey",
  district = "District",
  farmer_tot_prod = "What was total milk production yesterday? (liters)",
  farmer_tot_sold = "How much of yesterday's production was sold? (liters)",
  farmer_tot_sold_trader = "How much of yesterday's production was sold to the trader? (liters)",
  farmer_trader_link = "How long have you been selling to this trader?",
  farmer_rejected_week = "Times milk rejected by trader in the last week",
  farmer_rejected_month = "Times milk rejected by trader in the last month",
  farmer_bett_prc = "Did the trader pay a better price for better quality milk in the last week?",
  farmer_used_bran = "Did you use feed supplements such as maize bran in the last week?",
  farmer_used_residu = "Did you use crop residues as feeding in the last week?",
  farmer_used_lick = "Did you use mineral licks in the last week?",
  farmer_used_cgrazing = "Did you use controlled grazing in the last week?",
  treat = "Treatment assignment (RCT)",
  q_sold_1 = "Quantity sold to trader in transaction 1 (liters)",
  q_sold_2 = "Quantity sold to trader in transaction 2 (liters)",
  q_sold_3 = "Quantity sold to trader in transaction 3 (liters)",
  q_sold_4 = "Quantity sold to trader in transaction 4 (liters)",
  q_sold_5 = "Quantity sold to trader in transaction 5 (liters)",
  q_sold_6 = "Quantity sold to trader in transaction 6 (liters)",
  checked_1 = "Was milk quality checked in transaction 1?",
  checked_2 = "Was milk quality checked in transaction 2?",
  checked_3 = "Was milk quality checked in transaction 3?",
  checked_4 = "Was milk quality checked in transaction 4?",
  checked_5 = "Was milk quality checked in transaction 5?",
  checked_6 = "Was milk quality checked in transaction 6?",
  checked_how_1 = "How was milk quality checked in transaction 1?",
  checked_how_2 = "How was milk quality checked in transaction 2?",
  checked_how_3 = "How was milk quality checked in transaction 3?",
  checked_how_4 = "How was milk quality checked in transaction 4?",
  checked_how_5 = "How was milk quality checked in transaction 5?",
  checked_how_6 = "How was milk quality checked in transaction 6?",
  p_sold_1 = "Price received per liter in transaction 1 (UGX)",
  p_sold_2 = "Price received per liter in transaction 2 (UGX)",
  p_sold_3 = "Price received per liter in transaction 3 (UGX)",
  p_sold_4 = "Price received per liter in transaction 4 (UGX)",
  p_sold_5 = "Price received per liter in transaction 5 (UGX)",
  p_sold_6 = "Price received per liter in transaction 6 (UGX)"
)

# === Define value labels ===

# Yes/No (yex)
yex_labels <- c("Yes" = 1, "No" = 2)

# Trader relationship duration (trader_long)
trader_link_labels <- c(
  "Only started selling yesterday" = 1,
  "Started selling in the last two weeks" = 2,
  "Started selling in the last month" = 3,
  "Started selling in the last year" = 4,
  "Started selling in the last five years" = 5,
  "Started selling more than five years ago" = 6,
  "Other/Don't know" = 99
)

# Treatment
treat_labels <- c("Control" = 0, "Treatment" = 1)

# === Apply value labels ===

# Yes/No variables
yex_vars <- c("farmer_bett_prc", "farmer_used_bran", "farmer_used_residu",
              "farmer_used_lick", "farmer_used_cgrazing")
for (varname in yex_vars) {
  if (varname %in% names(df)) {
    df[[varname]] <- labelled(df[[varname]], yex_labels)
  }
}

# Checked variables (also yes/no)
for (i in 1:6) {
  varname <- paste0("checked_", i)
  if (varname %in% names(df)) {
    df[[varname]] <- labelled(df[[varname]], yex_labels)
  }
}

# Trader link
df$farmer_trader_link <- labelled(df$farmer_trader_link, trader_link_labels)

# Treatment
df$treat <- labelled(df$treat, treat_labels)

# === Apply variable labels (after value labels to preserve them) ===
for (varname in names(var_labels)) {
  if (varname %in% names(df)) {
    var_label(df[[varname]]) <- var_labels[[varname]]
  }
}

# === Export data files ===
write_dta(df, "farmers_endline.dta", version = 14)
saveRDS(df, "farmers_endline.rds")

cat("Created: farmers_endline.dta\n")
cat("Created: farmers_endline.rds\n")

# === Generate codebook ===
codebook_lines <- c(
  "# Codebook: RMVC Farmer Endline Survey",
  "",
  "## Study Information",
  "",
  "**Title:** When milk quality pays: Evidence from an incentive experiment in Uganda",
  "",
  "**Authors:** Bjorn Van Campenhout, Sarah Kariuki, Richard Ariong, Jordan Chamberlin, Benon Byarugaba, and Dennis Atuha",
  "",
  "**Organization:** International Food Policy Research Institute (IFPRI)",
  "",
  "**Location:** Kazo region, Uganda",
  "",
  "**Study Description:** This dataset comes from the endline survey of dairy farmers in a randomized controlled trial examining how quality measurement and price incentives affect milk production standards in smallholder dairy value chains. The intervention made milk quality visible to market participants through milk analyzers at Milk Collection Centers (MCCs) and implemented price incentive mechanisms. Farmers were interviewed about their milk production, sales to traders, quality testing experiences, and feeding practices.",
  "",
  "**Reference:** https://www.ifpri.org/blog/when-milk-quality-pays-evidence-from-an-incentive-experiment-in-uganda/",
  "",
  paste0("**Sample:** ", nrow(df), " dairy farmers associated with milk traders and Milk Collection Centers"),
  "",
  "---",
  "",
  "## Variable Dictionary",
  "",
  "| Variable | Label | Type | Values |",
  "|----------|-------|------|--------|"
)

# Generate variable rows
for (varname in names(df)) {
  label <- var_labels[[varname]]
  if (is.null(label)) label <- varname

  # Determine type
  if (is.labelled(df[[varname]])) {
    type <- "Categorical"
    labs <- val_labels(df[[varname]])
    if (!is.null(labs)) {
      vals <- paste(paste0(labs, "=", names(labs)), collapse = "; ")
    } else {
      vals <- paste(range(df[[varname]], na.rm = TRUE), collapse = " to ")
    }
  } else if (is.numeric(df[[varname]])) {
    type <- "Numeric"
    rng <- range(df[[varname]], na.rm = TRUE)
    if (all(is.finite(rng))) {
      vals <- paste(rng, collapse = " to ")
    } else {
      vals <- "NA"
    }
  } else if (is.character(df[[varname]])) {
    type <- "String"
    unique_vals <- unique(na.omit(df[[varname]]))
    if (length(unique_vals) <= 5) {
      vals <- paste(unique_vals, collapse = ", ")
    } else {
      vals <- paste0(length(unique_vals), " unique values")
    }
  } else {
    type <- class(df[[varname]])[1]
    vals <- "-"
  }

  # Escape pipe characters in label
  label <- gsub("\\|", "/", label)
  vals <- gsub("\\|", "/", vals)

  row <- paste0("| `", varname, "` | ", label, " | ", type, " | ", vals, " |")
  codebook_lines <- c(codebook_lines, row)
}

# Add value labels section
codebook_lines <- c(codebook_lines,
  "",
  "---",
  "",
  "## Value Labels Reference",
  "",
  "### Treatment Status (`treat`)",
  "| Code | Label |",
  "|------|-------|",
  "| 0 | Control |",
  "| 1 | Treatment |",
  "",
  "### Yes/No Variables (`farmer_bett_prc`, `farmer_used_bran`, `farmer_used_residu`, `farmer_used_lick`, `farmer_used_cgrazing`, `checked_1` to `checked_6`)",
  "| Code | Label |",
  "|------|-------|",
  "| 1 | Yes |",
  "| 2 | No |",
  "",
  "### Trader Relationship Duration (`farmer_trader_link`)",
  "| Code | Label |",
  "|------|-------|",
  "| 1 | Only started selling to this trader yesterday |",
  "| 2 | Started selling in the last two weeks |",
  "| 3 | Started selling in the last month |",
  "| 4 | Started selling in the last year |",
  "| 5 | Started selling in the last five years |",
  "| 6 | Started selling more than five years ago |",
  "| 99 | Other/Don't know |",
  "",
  "### Quality Testing Method (`checked_how_1` to `checked_how_6`)",
  "These are select-multiple variables stored as space-separated labels:",
  "",
  "| Value | Description |",
  "|-------|-------------|",
  "| Lactometer | Tested using lactometer |",
  "| Alcohol | Tested using alcohol test |",
  "| Lactoscan | Tested using Lactoscan milk analyzer |",
  "| Other | Other testing method |",
  "",
  "---",
  "",
  "## Notes",
  "",
  "- `NA` values indicate missing data (question not asked due to skip logic, or respondent did not answer)",
  "- Transactions 1-6 refer to the last 3-6 sales the farmer made to the trader, starting with the most recent",
  "- Prices are in Ugandan Shillings (UGX) per liter",
  "- Quantities are in liters",
  "- `checked_how` variables contain space-separated values when multiple testing methods were used (e.g., \"Lactometer Alcohol Lactoscan\")",
  ""
)

writeLines(codebook_lines, "codebook.md")
cat("Created: codebook.md\n")

cat("\nDataverse preparation complete!\n")
