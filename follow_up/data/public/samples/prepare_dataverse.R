# prepare_dataverse.R
# Prepares milk quality submission data for Dataverse distribution
# Creates: submissions.dta, submissions.rds, codebook.md

library(readxl)
library(haven)
library(labelled)

# === Load data ===
df <- read.csv("anonymized_submission_data.csv", stringsAsFactors = FALSE)

# === Load XLSForm metadata ===
survey <- read_excel("Quality_Tracking_used.xlsx", sheet = "survey")
choices <- read_excel("Quality_Tracking_used.xlsx", sheet = "choices")

# === Define variable labels ===
var_labels <- list(
  trader_ID = "Trader ID (anonymized)",
  MCC_ID = "Milk Collection Center ID (anonymized)",
  district = "District",
  treatment = "Treatment assignment (RCT)",
  Fat = "Fat content reading (%)",
  SNF = "Solids-Not-Fat reading (%)",
  Qty = "Quantity of milk delivered (liters)",
  bonus = "Quality bonus paid (UGX)",
  submission_date = "Date and time of submission"
)

# === Define value labels ===

# Treatment
treat_labels <- c("Control" = 0, "Treatment" = 1)

# === Apply value labels ===
df$treatment <- labelled(df$treatment, treat_labels)

# === Apply variable labels (after value labels to preserve them) ===
for (varname in names(var_labels)) {
  if (varname %in% names(df)) {
    var_label(df[[varname]]) <- var_labels[[varname]]
  }
}

# === Export data files ===
write_dta(df, "submissions.dta", version = 14)
saveRDS(df, "submissions.rds")

cat("Created: submissions.dta\n")
cat("Created: submissions.rds\n")

# === Generate codebook ===
codebook_lines <- c(
  "# Codebook: RMVC Milk Quality Submission Data",
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
  "**Study Description:** This dataset contains milk quality submission records from a randomized controlled trial examining how quality measurement and price incentives affect milk production standards in smallholder dairy value chains. The intervention introduced milk analyzers at Milk Collection Centers (MCCs) to make milk quality visible and implemented price incentive mechanisms. Each observation represents a milk delivery by a trader to an MCC, with quality readings (fat and solids-not-fat content) recorded by the milk analyzer and the corresponding quality bonus calculated.",
  "",
  "**Reference:** https://www.ifpri.org/blog/when-milk-quality-pays-evidence-from-an-incentive-experiment-in-uganda/",
  "",
  paste0("**Sample:** ", nrow(df), " milk quality submissions from traders at Milk Collection Centers"),
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
  "### Treatment Status (`treatment`)",
  "| Code | Label |",
  "|------|-------|",
  "| 0 | Control |",
  "| 1 | Treatment |",
  "",
  "---",
  "",
  "## Notes",
  "",
  "- Each observation represents a single milk delivery by a trader to a Milk Collection Center (MCC)",
  "- Traders may appear multiple times as they make repeated deliveries",
  "- `Fat` is the fat content percentage as measured by the milk analyzer",
  "- `SNF` is the solids-not-fat content percentage as measured by the milk analyzer",
  "- `Qty` is the total quantity of milk delivered in liters (all cans, not just the one tested)",
  "- `bonus` is the quality bonus in Ugandan Shillings (UGX) calculated based on quality readings and quantity",
  "- `submission_date` is in ISO 8601 format with timezone (East Africa Time, UTC+03:00)",
  "- Treatment MCCs had milk analyzers installed and price incentives implemented; Control MCCs did not",
  ""
)

writeLines(codebook_lines, "codebook.md")
cat("Created: codebook.md\n")

cat("\nDataverse preparation complete!\n")
