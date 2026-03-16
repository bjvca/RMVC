# prepare_dataverse.R
# Prepares trader endline data for Dataverse distribution
# Creates: traders_endline.dta, traders_endline.rds, codebook.md

library(readxl)
library(haven)
library(labelled)

# === Load data ===
df <- read.csv("anonymized_trader_endline_data.csv", stringsAsFactors = FALSE)

# === Load XLSForm metadata ===
survey <- read_excel("Traders_Endline_used.xlsx", sheet = "survey")
choices <- read_excel("Traders_Endline_used.xlsx", sheet = "choices")

# === Define variable labels ===
# Map from XLSForm names to CSV column names where they differ
var_labels <- list(
  trader_ID = "Trader ID (anonymized)",
  MCC_ID = "Milk Collection Center ID (anonymized)",
  trader_transporter = "Are you a trader or a transporter?",
  self = "Who usually collects milk from farmers?",
  nr_employed = "How many transporters work for you?",
  transport_bodaboda = "Transport used: Boda boda (motorcycle)",
  transport_tuktuk = "Transport used: Tuk tuk/tri-cycle",
  transport_bicycle = "Transport used: Bicycle",
  transport_car = "Transport used: Car/pickup/truck",
  transport_other = "Transport used: Other",
  delivered_quantity = "How much milk in liters did you deliver yesterday?",
  deliver_source = "From how many farmers did you collect milk yesterday?",
  deliver_other_mcc = "Do you deliver to other MCCs besides this one?",
  delivered_quantity_othermcc = "How much milk was delivered to other MCCs?",
  supervised_testing = "Did enumerator observe milk analyzer testing?",
  Fat_supervised = "Fat percentage (supervised test)",
  SNF_supervised = "SNF percentage (supervised test)",
  quantity_supervised = "Quantity tested (supervised test, liters)",
  rejected_week = "Liters of milk rejected in the last week",
  rejected_month = "Liters of milk rejected in the last month",
  use_lacto = "Did you use a lactometer to test milk freshness?",
  lact_min = "Minimum lactometer reading you accept",
  use_alcohol = "Did you test freshness using alcohol test?",
  pay_premim = "Did you pay farmers a premium for better quality?",
  premium = "Premium amount paid (UGX per liter)",
  note_thks = "Survey completion notes",
  treat = "Treatment assignment (RCT)",
  purchase_q_1 = "Quantity purchased from farmer 1 (liters)",
  purchase_q_2 = "Quantity purchased from farmer 2 (liters)",
  purchase_q_3 = "Quantity purchased from farmer 3 (liters)",
  purchase_q_4 = "Quantity purchased from farmer 4 (liters)",
  purchase_q_5 = "Quantity purchased from farmer 5 (liters)",
  purchase_q_6 = "Quantity purchased from farmer 6 (liters)",
  puarchase_price_1 = "Price paid to farmer 1 (UGX per liter)",
  puarchase_price_2 = "Price paid to farmer 2 (UGX per liter)",
  puarchase_price_3 = "Price paid to farmer 3 (UGX per liter)",
  puarchase_price_4 = "Price paid to farmer 4 (UGX per liter)",
  puarchase_price_5 = "Price paid to farmer 5 (UGX per liter)",
  puarchase_price_6 = "Price paid to farmer 6 (UGX per liter)",
  farmer_link_1 = "Duration of relationship with farmer 1",
  farmer_link_2 = "Duration of relationship with farmer 2",
  farmer_link_3 = "Duration of relationship with farmer 3",
  farmer_link_4 = "Duration of relationship with farmer 4",
  farmer_link_5 = "Duration of relationship with farmer 5",
  farmer_link_6 = "Duration of relationship with farmer 6"
)

# === Define value labels ===

# Yes/No (yex)
yex_labels <- c("Yes" = 1, "No" = 2)

# Frequency (ag)
ag_labels <- c("Yes, always" = 1, "No, never" = 2, "Sometimes" = 3)

# Self collection
self_labels <- c(
  "I do everything myself" = 1,
  "I collect some but also have employees" = 2,
  "I do not pick milk myself, employees do" = 3
)

# Farmer relationship duration
farmer_link_labels <- c(
  "Started yesterday" = 1,
  "Started in the last week" = 2,
  "Started in the last month" = 3,
  "Started in the last year" = 4,
  "Started in the last 5 years" = 5,
  "More than 5 years ago" = 6,
  "Other/Don't know" = 96
)

# Treatment
treat_labels <- c("Control" = 0, "Treatment" = 1)

# Binary (for transport variables)
binary_labels <- c("No" = 0, "Yes" = 1)

# Apply value labels
df$deliver_other_mcc <- labelled(df$deliver_other_mcc, yex_labels)
df$supervised_testing <- labelled(df$supervised_testing, yex_labels)

df$use_lacto <- labelled(df$use_lacto, ag_labels)
df$use_alcohol <- labelled(df$use_alcohol, ag_labels)
df$pay_premim <- labelled(df$pay_premim, ag_labels)

df$self <- labelled(df$self, self_labels)
df$treat <- labelled(df$treat, treat_labels)

# Farmer link variables
for (i in 1:6) {
  varname <- paste0("farmer_link_", i)
  if (varname %in% names(df)) {
    df[[varname]] <- labelled(df[[varname]], farmer_link_labels)
  }
}

# Transport binary variables
transport_vars <- c("transport_bodaboda", "transport_tuktuk", "transport_bicycle",
                    "transport_car", "transport_other")
for (varname in transport_vars) {
  if (varname %in% names(df)) {
    df[[varname]] <- labelled(df[[varname]], binary_labels)
  }
}

# === Apply variable labels (after value labels to preserve them) ===
for (varname in names(var_labels)) {
  if (varname %in% names(df)) {
    var_label(df[[varname]]) <- var_labels[[varname]]
  }
}

# === Export data files ===
write_dta(df, "traders_endline.dta", version = 14)
saveRDS(df, "traders_endline.rds")

cat("Created: traders_endline.dta\n")
cat("Created: traders_endline.rds\n")

# === Generate codebook ===
codebook_lines <- c(
  "# Codebook: RMVC Trader Endline Survey",
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
  "**Study Description:** This dataset comes from the endline survey of milk traders in a randomized controlled trial examining how quality measurement and price incentives affect milk production standards in smallholder dairy value chains. The intervention made milk quality visible to market participants through milk analyzers at Milk Collection Centers (MCCs) and implemented price incentive mechanisms.",
  "",
  "**Reference:** https://www.ifpri.org/blog/when-milk-quality-pays-evidence-from-an-incentive-experiment-in-uganda/",
  "",
  "**Sample:** 99 milk traders associated with 19 Milk Collection Centers",
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
    if (varname == "trader_transporter") {
      type <- "String"
      vals <- paste(unique(na.omit(df[[varname]])), collapse = ", ")
    } else {
      rng <- range(df[[varname]], na.rm = TRUE)
      if (all(is.finite(rng))) {
        vals <- paste(rng, collapse = " to ")
      } else {
        vals <- "NA"
      }
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
  "### Yes/No Variables (`deliver_other_mcc`, `supervised_testing`)",
  "| Code | Label |",
  "|------|-------|",
  "| 1 | Yes |",
  "| 2 | No |",
  "",
  "### Frequency Variables (`use_lacto`, `use_alcohol`, `pay_premim`)",
  "| Code | Label |",
  "|------|-------|",
  "| 1 | Yes, always |",
  "| 2 | No, never |",
  "| 3 | Sometimes |",
  "",
  "### Self Collection (`self`)",
  "| Code | Label |",
  "|------|-------|",
  "| 1 | I do everything myself |",
  "| 2 | I collect some but also have employees |",
  "| 3 | I do not pick milk myself, employees do |",
  "",
  "### Farmer Relationship Duration (`farmer_link_1` to `farmer_link_6`)",
  "| Code | Label |",
  "|------|-------|",
  "| 1 | Only started buying from this farmer yesterday |",
  "| 2 | Started buying in the last week |",
  "| 3 | Started buying in the last month |",
  "| 4 | Started buying in the last year |",
  "| 5 | Started buying in the last 5 years |",
  "| 6 | Started buying more than 5 years ago |",
  "| 96 | Other/Don't know |",
  "",
  "### Transport Methods (`transport_*`)",
  "| Code | Label |",
  "|------|-------|",
  "| 0 | No |",
  "| 1 | Yes |",
  "",
  "---",
  "",
  "## Notes",
  "",
  "- `NA` values indicate missing data (question not asked due to skip logic, or respondent did not answer)",
  "- Prices are in Ugandan Shillings (UGX) per liter",
  "- Quantities are in liters",
  "- Fat and SNF (Solids-Not-Fat) percentages from milk analyzer testing",
  "- Lactometer readings indicate milk density (proxy for adulteration detection)",
  ""
)

writeLines(codebook_lines, "codebook.md")
cat("Created: codebook.md\n")

cat("\nDataverse preparation complete!\n")
