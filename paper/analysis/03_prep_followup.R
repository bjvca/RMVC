###############################################################################
## 03_prep_followup.R
##
## Data preparation for the follow-up experiment (trader-level milk quality
## monitoring via app-based data collection and quality bonuses).
##
## Reads from LOCAL CSV files (anonymized public data), not from ODK Central.
## Clean port from follow_up/analysis/initial_analysis.R.
##
## Must be run with working directory set to paper/analysis/.
##
## Outputs (saved to paper/results/):
##   prepped_submissions.rds   -- trader-day aggregates + daily averages
##   prepped_traders.rds       -- trader endline with constructed outcomes
##   prepped_farmers_fu.rds    -- farmer endline (follow-up) with outcomes
###############################################################################

rm(list = ls())

source("00_utilities.R")

library(dplyr)
library(lubridate)

## Auto-detect project root from working directory
path <- strsplit(getwd(), "/paper/analysis")[[1]]

###############################################################################
## 1. SUBMISSIONS DATA (app-based daily milk quality measurements)
###############################################################################

submissions <- read.csv(paste(path, "follow_up/data/public/samples/anonymized_submission_data.csv",
                              sep = "/"))

## Parse submission date
submissions$date <- as.Date(submissions$submission_date)

## Filter: keep only valid collection period
submissions <- subset(submissions, date >= as.Date("2025-10-31"))
submissions <- subset(submissions, date <= as.Date("2025-12-01"))

## Filter fat: plausible range 3-5.5%
submissions <- subset(submissions, Fat >= 3 & Fat <= 5.5)

## ---------------------------------------------------------------------------
## 1a. Trader-day aggregates: group by date, trader, MCC, treatment
## ---------------------------------------------------------------------------
trader_day <- submissions %>%
  group_by(date, trader_ID, MCC_ID, treatment) %>%
  summarise(
    total_qty = sum(Qty, na.rm = TRUE),
    avg_fat   = weighted.mean(Fat, Qty, na.rm = TRUE),
    avg_snf   = weighted.mean(SNF, Qty, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ## Quality bonus calculation:
    ## After Nov 15: fat threshold = 3.9
    ## Before Nov 15: fat threshold = 3.3
    ## Formula: (excess SNF above 8.5 * 100 + excess fat above threshold * 100) * quantity
    bonus = if_else(
      date >= as.Date("2025-11-15"),
      round((pmax(0, avg_snf - 8.5) * 100 + pmax(0, avg_fat - 3.9) * 100) * total_qty),
      round((pmax(0, avg_snf - 8.5) * 100 + pmax(0, avg_fat - 3.3) * 100) * total_qty)
    )
  )

## ---------------------------------------------------------------------------
## 1b. Daily averages: group by date, treatment -> means and SEs
## ---------------------------------------------------------------------------
daily_avg <- trader_day %>%
  group_by(date, treatment) %>%
  summarise(
    avg_fat   = mean(avg_fat, na.rm = TRUE),
    sd_fat    = sd(avg_fat, na.rm = TRUE),
    n         = dplyr::n(),
    se_fat    = sd(avg_fat, na.rm = TRUE) / sqrt(n),
    avg_snf   = mean(avg_snf, na.rm = TRUE),
    sd_snf    = sd(avg_snf, na.rm = TRUE),
    se_snf    = sd(avg_snf, na.rm = TRUE) / sqrt(n),
    avg_qty   = mean(total_qty, na.rm = TRUE),
    sd_qty    = sd(total_qty, na.rm = TRUE),
    se_qty    = sd(total_qty, na.rm = TRUE) / sqrt(n),
    avg_bonus = mean(bonus, na.rm = TRUE),
    sd_bonus  = sd(bonus, na.rm = TRUE),
    se_bonus  = sd(bonus, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    treat_label = ifelse(treatment == 1, "Treatment", "Control")
  )

## ---------------------------------------------------------------------------
## 1c. Stage variable for analysis (pre-treatment, stage 1, stage 2)
## ---------------------------------------------------------------------------
daily_avg$stage <- ifelse(
  daily_avg$date < as.Date("2025-11-01"), "Pre",
  ifelse(daily_avg$date < as.Date("2025-11-15"), "Stage 1", "Stage 2")
)

trader_day$stage <- ifelse(
  trader_day$date < as.Date("2025-11-01"), "Pre",
  ifelse(trader_day$date < as.Date("2025-11-15"), "Stage 1", "Stage 2")
)


###############################################################################
## 2. TRADER ENDLINE DATA
###############################################################################

traders <- read.csv(paste(path, "follow_up/data/public/traders/anonymized_trader_endline_data.csv",
                          sep = "/"))

## ---------------------------------------------------------------------------
## 2a. Purchase quantities: cap at 1000 (>1000 -> NA)
## ---------------------------------------------------------------------------
purchase_q_cols <- c("purchase_q_1", "purchase_q_2", "purchase_q_3",
                     "purchase_q_4", "purchase_q_5", "purchase_q_6")

for (col in purchase_q_cols) {
  if (col %in% names(traders)) {
    traders[[col]] <- as.numeric(as.character(traders[[col]]))
    traders[[col]][traders[[col]] > 1000] <- NA
  }
}

## ---------------------------------------------------------------------------
## 2b. Volume-weighted average price paid to farmers
## ---------------------------------------------------------------------------
## Note: column name typo "puarchase_price" is in the original data
purchase_p_cols <- c("puarchase_price_1", "puarchase_price_2", "puarchase_price_3",
                     "puarchase_price_4", "puarchase_price_5", "puarchase_price_6")

for (col in purchase_p_cols) {
  if (col %in% names(traders)) {
    traders[[col]] <- as.numeric(as.character(traders[[col]]))
  }
}

## 999 = "don't know" -> NA for price and quantity columns
for (col in c(purchase_q_cols, purchase_p_cols)) {
  if (col %in% names(traders)) {
    traders[[col]][traders[[col]] == 999] <- NA
  }
}

## Compute volume-weighted average purchase price
price_mat <- as.matrix(traders[purchase_p_cols])
qty_mat   <- as.matrix(traders[purchase_q_cols])

traders$avg_purchase_price <- rowSums(price_mat * qty_mat, na.rm = TRUE) /
  rowSums(qty_mat, na.rm = TRUE)
traders$avg_purchase_price[is.nan(traders$avg_purchase_price)] <- NA


###############################################################################
## 3. FARMER ENDLINE DATA (follow-up)
###############################################################################

farmers_fu <- read.csv(paste(path, "follow_up/data/public/farmers/anonymized_farmer_endline_data.csv",
                             sep = "/"))

## ---------------------------------------------------------------------------
## 3a. Volume-weighted average price from q_sold and p_sold columns
## ---------------------------------------------------------------------------
q_cols <- c("q_sold_1", "q_sold_2", "q_sold_3", "q_sold_4", "q_sold_5", "q_sold_6")
p_cols <- c("p_sold_1", "p_sold_2", "p_sold_3", "p_sold_4", "p_sold_5", "p_sold_6")

for (col in c(q_cols, p_cols)) {
  if (col %in% names(farmers_fu)) {
    farmers_fu[[col]] <- as.numeric(as.character(farmers_fu[[col]]))
    ## 999 = "don't know" -> NA
    farmers_fu[[col]][farmers_fu[[col]] == 999] <- NA
  }
}

price_mat_f <- as.matrix(farmers_fu[p_cols])
qty_mat_f   <- as.matrix(farmers_fu[q_cols])

farmers_fu$avg_price <- rowSums(price_mat_f * qty_mat_f, na.rm = TRUE) /
  rowSums(qty_mat_f, na.rm = TRUE)
farmers_fu$avg_price[is.nan(farmers_fu$avg_price)] <- NA

## ---------------------------------------------------------------------------
## 3b. Quality checked: any checked_1:6 == 1
## ---------------------------------------------------------------------------
checked_cols <- c("checked_1", "checked_2", "checked_3",
                  "checked_4", "checked_5", "checked_6")

for (col in checked_cols) {
  if (col %in% names(farmers_fu)) {
    farmers_fu[[col]] <- as.numeric(as.character(farmers_fu[[col]]))
  }
}

farmers_fu$quality_checked <- apply(farmers_fu[checked_cols], 1, function(row) {
  any(row == 1, na.rm = TRUE)
})

## ---------------------------------------------------------------------------
## 3c. Feeding practices
## ---------------------------------------------------------------------------
farmers_fu$used_bran     <- farmers_fu$farmer.used_bran == 1
farmers_fu$used_residu   <- farmers_fu$farmer.used_residu == 1
farmers_fu$used_lick     <- farmers_fu$farmer.used_lick == 1
farmers_fu$used_cgrazing <- farmers_fu$farmer.used_cgrazing == 1

## Anderson index of feeding practices
farmers_fu$feeding_index <- anderson_index(cbind(
  farmers_fu$used_bran,
  farmers_fu$used_residu,
  farmers_fu$used_lick,
  farmers_fu$used_cgrazing
))$index


###############################################################################
## 4. SAVE PREPPED DATA
###############################################################################

## Save trader-day and daily averages as a list
submissions_prepped <- list(
  trader_day = as.data.frame(trader_day),
  daily_avg  = as.data.frame(daily_avg)
)

saveRDS(submissions_prepped, file = paste(path, "paper/results/prepped_submissions.rds", sep = "/"))
saveRDS(traders,             file = paste(path, "paper/results/prepped_traders.rds", sep = "/"))
saveRDS(farmers_fu,          file = paste(path, "paper/results/prepped_farmers_fu.rds", sep = "/"))

cat("03_prep_followup.R completed. Prepped data saved to paper/results/\n")
