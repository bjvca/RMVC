###############################################################################
## 01_prep_main.R
##
## Data preparation for the main 2x2 factorial RCT analysis.
## Loads baseline and endline farmer/MCC data, constructs all outcome
## variables, balance variables, and saves prepped data to paper/results/.
##
## Must be run with working directory set to paper/analysis/.
## Run BEFORE any analysis scripts.
##
## Outputs (saved to paper/results/):
##   prepped_farmers.rds     -- endline farmers with all constructed outcomes
##   prepped_farmers_base.rds -- baseline farmers with baseline outcomes
##   prepped_MCCs.rds        -- endline MCCs with all constructed outcomes
##   prepped_MCCs_base.rds   -- baseline MCCs with baseline outcomes
##   MCC_balance.rds         -- baseline MCC data with balance variables
##   farmers_balance.rds     -- baseline farmer data with balance variables
###############################################################################

rm(list = ls())

source("00_utilities.R")

library(clubSandwich)
library(andersonTools)
library(car)
library(lubridate)

## Auto-detect project root from working directory
path <- strsplit(getwd(), "/paper/analysis")[[1]]

###############################################################################
## 1. LOAD RAW DATA
###############################################################################

farmers_end  <- read.csv(paste(path, "endline/data/public/farmers.csv", sep = "/"))
farmers_end  <- subset(farmers_end, check.consent == "Yes")
farmers_base <- read.csv(paste(path, "baseline/data/public/farmers.csv", sep = "/"))

MCCs_end  <- read.csv(paste(path, "endline/data/public/MCCs.csv", sep = "/"))
MCCs_end  <- subset(MCCs_end, consent == 1)
MCCs_base <- read.csv(paste(path, "baseline/data/public/MCCs.csv", sep = "/"))


###############################################################################
## 2. FARMER-LEVEL IDENTIFIERS AND TREATMENT VARIABLES
###############################################################################

## Clustering variable for farmer-level regressions
farmers_end$catch_ID <- as.factor(farmers_end$catchment)

## Treatment indicators (MCC-level assignment and farmer-level video)
farmers_end$treat <- farmers_end$treat == "T"

## Keep only farmers linked to consenting MCCs
farmers_end <- subset(farmers_end, MCC_ID_linked %in% MCCs_end$MCC_ID)

## Trader indicator from farmer_ID suffix (baseline)
farmers_base$trader <- ifelse(
  sub("^[^0-9]*[0-9]+", "", farmers_base$farmer_ID) %in% c("_T", "_T_R"), 1, 0
)

## Rename endline farmer_type to avoid collision with baseline
names(farmers_end)[names(farmers_end) == "farmer_type"] <- "farmer_type_end"

## Merge baseline farmer_type for heterogeneity analysis
farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "farmer_type")],
                     by = "farmer_ID", all.x = TRUE)
farmers_end$trader <- farmers_end$farmer_type == 2

## Override with ID-based trader classification (more reliable)
farmers_end$trader <- ifelse(
  sub("^[^0-9]*[0-9]+", "", farmers_end$farmer_ID) %in% c("_T", "_T_R"), 1, 0
)


###############################################################################
## 3. FARMER OUTCOME VARIABLES
###############################################################################

# ---------------------------------------------------------------------------
# 3a. Improved practices index (primary outcome family)
# ---------------------------------------------------------------------------
# q39 = oversowing, q39c = legume pastures, q39d = trees
# q40 in (1,3) = controlled/zero grazing, q42 = pasture conservation, q43 = supplements

farmers_end$q39c[farmers_end$q39c == "98"] <- NA
farmers_end$q39d[farmers_end$q39d == "98"] <- NA
farmers_end$q42[farmers_end$q42 == "98"]   <- NA
farmers_end$q40[farmers_end$q40 == "96"]   <- NA
farmers_end$q41[farmers_end$q41 == "99" | farmers_end$q41 == "96"] <- NA

farmers_base$b_improve_index <- anderson_index(cbind(
  farmers_base$q39 == "Yes", farmers_base$q39c == "Yes",
  farmers_base$q40 %in% c(1, 3), farmers_base$q42 == "Yes",
  farmers_base$q43 == "Yes"
))$index

farmers_end$improve_index <- anderson_index(cbind(
  farmers_end$q39 == "Yes", farmers_end$q39c == "Yes",
  farmers_end$q40 %in% c(1, 3), farmers_end$q42 == "Yes",
  farmers_end$q43 == "Yes"
))$index

farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_improve_index")],
                     by = "farmer_ID", all.x = TRUE)

# ---------------------------------------------------------------------------
# 3b. Buyer checks using milk analyzer
# ---------------------------------------------------------------------------
# check_MA = TRUE if buyer used milk analyzer for any transaction in last 7 days
farmers_end$check_MA <- farmers_end$q58.4 == "True" | farmers_end$qx5.4 == "True" |
  farmers_end$qx17.4 == "True" | farmers_end$qx29.4 == "True" |
  farmers_end$qx41.4 == "True" | farmers_end$qx53.4 == "True"
farmers_end$check_MA[farmers_end$q52 == "n/a"] <- NA
farmers_end$check_MA[farmers_end$q52 == "No"]  <- NA

farmers_base$b_check_MA <- farmers_base$q58 == "4" | farmers_base$qx5.4 == "True" |
  farmers_base$qx17.4 == "True" | farmers_base$qx29.4 == "True" |
  farmers_base$qx41.4 == "True" | farmers_base$qx53.4 == "True"
farmers_base$b_check_MA[farmers_base$q52 == "No"] <- NA

farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_check_MA")],
                     by = "farmer_ID", all.x = TRUE)

# ---------------------------------------------------------------------------
# 3c. Average sales price (quantity-weighted across buyers in last 7 days)
# ---------------------------------------------------------------------------
# Price columns: q55/qx2/qx14/qx26/qx38/qx50
# Quantity columns: q54/qx1/qx13/qx25/qx37/qx49

columns   <- c("q55", "qx2", "qx14", "qx26", "qx38", "qx50")
columns_q <- c("q54", "qx1", "qx13", "qx25", "qx37", "qx49")

## Clean price columns: 999 = "don't know" -> NA
farmers_end[columns] <- lapply(farmers_end[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA
  as.numeric(as.character(x))
})
farmers_base[columns] <- lapply(farmers_base[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA
  as.numeric(as.character(x))
})

## Clean quantity columns
farmers_end[columns_q]  <- lapply(farmers_end[columns_q],  function(x) as.numeric(as.character(x)))
farmers_base[columns_q] <- lapply(farmers_base[columns_q], function(x) as.numeric(as.character(x)))

## Quantity-weighted average price
farmers_end$avg_sales_p <- rowSums(farmers_end[columns] * farmers_end[columns_q], na.rm = TRUE) /
  rowSums(farmers_end[columns_q], na.rm = TRUE)
farmers_end$avg_sales_p[is.nan(farmers_end$avg_sales_p)] <- NA

farmers_base$b_avg_sales_p <- rowSums(farmers_base[columns] * farmers_base[columns_q], na.rm = TRUE) /
  rowSums(farmers_base[columns_q], na.rm = TRUE)
farmers_base$b_avg_sales_p[is.nan(farmers_base$b_avg_sales_p)] <- NA

## Remove outliers (prices outside 600-1500 UGX/liter)
farmers_base$b_avg_sales_p[farmers_base$b_avg_sales_p >= 1500] <- NA
farmers_base$b_avg_sales_p[farmers_base$b_avg_sales_p <= 600]  <- NA
farmers_end$avg_sales_p[farmers_end$avg_sales_p >= 1500] <- NA
farmers_end$avg_sales_p[farmers_end$avg_sales_p <= 600]  <- NA

farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_avg_sales_p")],
                     by = "farmer_ID", all.x = TRUE)

# ---------------------------------------------------------------------------
# 3d. Quality bonus from buyer
# ---------------------------------------------------------------------------
# q61/qx8/qx20/qx32/qx44/qx56 = "Does the buyer pay for higher quality milk?"
farmers_end$qx8[farmers_end$qx8 == "98"]   <- NA
farmers_end$qx44[farmers_end$qx44 == "98"] <- NA

farmers_end$gets_q_bonus <- farmers_end$q61 == "Yes" | farmers_end$qx8 == "Yes" |
  farmers_end$qx20 == "Yes" | farmers_end$qx32 == "Yes" |
  farmers_end$qx44 == "Yes" | farmers_end$qx56 == "Yes"
farmers_end$gets_q_bonus[farmers_end$q52 == "n/a" | farmers_end$q52 == "No"] <- NA

farmers_base$q61[farmers_base$q61 == "98"]   <- NA
farmers_base$qx8[farmers_base$qx8 == "98"]   <- NA
farmers_base$qx44[farmers_base$qx44 == "98"] <- NA

farmers_base$b_gets_q_bonus <- farmers_base$q61 == "Yes" | farmers_base$qx8 == "Yes" |
  farmers_base$qx20 == "Yes" | farmers_base$qx32 == "Yes" |
  farmers_base$qx44 == "Yes" | farmers_base$qx56 == "Yes"
farmers_base$b_gets_q_bonus[farmers_base$q52 == "No"] <- NA

farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_gets_q_bonus")],
                     by = "farmer_ID", all.x = TRUE)

# ---------------------------------------------------------------------------
# 3e. Bargaining power
# ---------------------------------------------------------------------------
# q65/qx12/qx24/qx36/qx48/qx60: farmer sets price or negotiates (codes 1, 3)
farmers_end$bargain_power <- farmers_end$q65 %in% c(1, 3) |
  farmers_end$qx12 %in% c(1, 3) | farmers_end$qx24 %in% c(1, 3) |
  farmers_end$qx36 %in% c(1, 3) | farmers_end$qx48 %in% c(1, 3) |
  farmers_end$qx60 %in% c(1, 3)
farmers_end$bargain_power[farmers_end$q52 == "n/a" | farmers_end$q52 == "No"] <- NA

farmers_base$b_bargain_power <- farmers_base$q65 %in% c(1, 3) |
  farmers_base$qx12 %in% c(1, 3) | farmers_base$qx24 %in% c(1, 3) |
  farmers_base$qx36 %in% c(1, 3) | farmers_base$qx48 %in% c(1, 3) |
  farmers_base$qx60 %in% c(1, 3)
farmers_base$b_bargain_power[farmers_base$q52 == "No"] <- NA

farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_bargain_power")],
                     by = "farmer_ID", all.x = TRUE)

# ---------------------------------------------------------------------------
# 3f. LATE instrument: actual machine use at MCC
# ---------------------------------------------------------------------------
MCCs_end$treat_TOT <- MCCs_end$machine_in_use %in% 1:2
farmers_end <- merge(farmers_end, MCCs_end[c("MCC_ID", "treat_TOT")],
                     by.x = "MCC_ID_linked", by.y = "MCC_ID", all.x = TRUE)

# ---------------------------------------------------------------------------
# 3g. Sales quantities (secondary outcomes)
# ---------------------------------------------------------------------------

## Sold in last week (q52)
farmers_end$q52[farmers_end$q52 == "n/a"] <- NA
farmers_end$sold_last_week <- farmers_end$q52 == "Yes"
farmers_base$b_sold_last_week <- farmers_base$q52 == "Yes"
farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_sold_last_week")],
                     by = "farmer_ID", all.x = TRUE)

## Volumes sold in dry/rainy season (q50, q50x)
columns <- c("q50", "q50x")
farmers_end[columns] <- lapply(farmers_end[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA
  as.numeric(as.character(x))
})
farmers_base[columns] <- lapply(farmers_base[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA
  as.numeric(as.character(x))
})

farmers_end$q_sold_dry    <- farmers_end$q50
farmers_base$b_q_sold_dry <- farmers_base$q50
farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_q_sold_dry")],
                     by = "farmer_ID", all.x = TRUE)

farmers_end$q_sold_wet    <- farmers_end$q50x
farmers_base$b_q_sold_wet <- farmers_base$q50x
farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_q_sold_wet")],
                     by = "farmer_ID", all.x = TRUE)

## Average quantity sold per transaction (last 7 days)
columns <- c("q54", "qx1", "qx13", "qx25", "qx37", "qx49")
farmers_end[columns] <- lapply(farmers_end[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA
  as.numeric(as.character(x))
})
farmers_base[columns] <- lapply(farmers_base[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA
  as.numeric(as.character(x))
})

farmers_end$avg_sales_q <- rowMeans(farmers_end[columns], na.rm = TRUE)
farmers_end$avg_sales_q[is.nan(farmers_end$avg_sales_q)] <- NA
farmers_end$avg_sales_q[is.na(farmers_end$avg_sales_q)]  <- 0
farmers_end$avg_sales_q[is.na(farmers_end$q52)]          <- NA

farmers_base$b_avg_sales_q <- rowMeans(farmers_base[columns], na.rm = TRUE)
farmers_base$b_avg_sales_q[is.nan(farmers_base$b_avg_sales_q)] <- NA
farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_avg_sales_q")],
                     by = "farmer_ID", all.x = TRUE)

## Remove outliers (> 500 liters)
farmers_end$q_sold_dry[farmers_end$q_sold_dry > 500]       <- NA
farmers_end$q_sold_wet[farmers_end$q_sold_wet > 500]       <- NA
farmers_end$avg_sales_q[farmers_end$avg_sales_q > 500]     <- NA
farmers_end$b_q_sold_dry[farmers_end$b_q_sold_dry > 500]   <- NA
farmers_end$b_q_sold_wet[farmers_end$b_q_sold_wet > 500]   <- NA
farmers_end$b_avg_sales_q[farmers_end$b_avg_sales_q > 500] <- NA

## IHS transform for skewed volume outcomes
farmers_end$ihs_q_sold_dry    <- ihs(farmers_end$q_sold_dry)
farmers_end$ihs_q_sold_wet    <- ihs(farmers_end$q_sold_wet)
farmers_end$ihs_avg_sales_q   <- ihs(farmers_end$avg_sales_q)
farmers_end$ihs_b_q_sold_dry  <- ihs(farmers_end$b_q_sold_dry)
farmers_end$ihs_b_q_sold_wet  <- ihs(farmers_end$b_q_sold_wet)
farmers_end$ihs_b_avg_sales_q <- ihs(farmers_end$b_avg_sales_q)

# ---------------------------------------------------------------------------
# 3h. Treatment uptake (secondary, no baseline)
# ---------------------------------------------------------------------------

farmers_end$recalls_video <- farmers_end$recalls_video == "Yes"
farmers_end$recalls_grass <- farmers_end$recalls_grass == "Yes"
farmers_end$used_grass    <- farmers_end$used_grass == "Yes"
farmers_end$used_video    <- farmers_end$used_video == "Yes"

farmers_end$knows_comp <- anderson_index(cbind(
  farmers_end$test_know_1 == 1,
  farmers_end$test_know_2 == 2,
  farmers_end$test_know_3 == 1
))$index

# ---------------------------------------------------------------------------
# 3i. Sales/market channel (secondary with baseline)
# ---------------------------------------------------------------------------

## Sold to MCC in wet/dry season
farmers_end$mcc_wet    <- farmers_end$q51 == "2"
farmers_base$b_mcc_wet <- farmers_base$q51 == "2"
farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_mcc_wet")],
                     by = "farmer_ID", all.x = TRUE)

farmers_end$mcc_dry    <- farmers_end$q51x == "2"
farmers_base$b_mcc_dry <- farmers_base$q51x == "2"
farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_mcc_dry")],
                     by = "farmer_ID", all.x = TRUE)

## Sold to MCC in last week
farmers_end$mcc_last_seek    <- farmers_end$q53.2 == "True"
farmers_base$b_mcc_last_seek <- farmers_base$q53.2 == "True"
farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_mcc_last_seek")],
                     by = "farmer_ID", all.x = TRUE)

## Wet season price (999 = "don't know" -> NA)
farmers_end$price_wet <- as.numeric(as.character(farmers_end$q51a))
farmers_end$price_wet[farmers_end$price_wet == 999] <- NA
farmers_base$b_price_wet <- as.numeric(as.character(farmers_base$q51a))
farmers_base$b_price_wet[farmers_base$b_price_wet == 999] <- NA
farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_price_wet")],
                     by = "farmer_ID", all.x = TRUE)

## Dry season price (999 = "don't know" -> NA)
farmers_end$price_dry <- as.numeric(as.character(farmers_end$q51ax))
farmers_end$price_dry[farmers_end$price_dry == 999] <- NA
farmers_base$b_price_dry <- as.numeric(as.character(farmers_base$q51ax))
farmers_base$b_price_dry[farmers_base$b_price_dry == 999] <- NA
farmers_end <- merge(farmers_end, farmers_base[c("farmer_ID", "b_price_dry")],
                     by = "farmer_ID", all.x = TRUE)

# ---------------------------------------------------------------------------
# 3j. Switching MCCs (secondary, no baseline)
# ---------------------------------------------------------------------------

farmers_end$still_connected_yes <- farmers_end$still_connected == 1
farmers_end$still_connected_yes[farmers_end$still_connected == "n/a"] <- NA

farmers_end$q51_name[is.na(farmers_end$q51_name)]   <- "NA"
farmers_end$still_supplying_wet <- as.character(farmers_end$q51_name_prev) ==
  as.character(farmers_end$q51_name)

farmers_end$q51_namex[is.na(farmers_end$q51_namex)] <- "NA"
farmers_end$still_supplying_dry <- as.character(farmers_end$q51_name_prevx) ==
  as.character(farmers_end$q51_namex)

# ---------------------------------------------------------------------------
# 3k. Demeaned treatment variables (for orthogonalized models)
# ---------------------------------------------------------------------------

farmers_end$trader_demeaned <- farmers_end$trader - mean(farmers_end$trader, na.rm = TRUE)
farmers_end$vid_demeaned    <- farmers_end$vid    - mean(farmers_end$vid, na.rm = TRUE)
farmers_end$treat_demeaned  <- farmers_end$treat  - mean(farmers_end$treat, na.rm = TRUE)


###############################################################################
## 4. MCC OUTCOME VARIABLES
###############################################################################

## Merge catchment_ID from baseline for CR2 clustering
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "catchment_ID")],
                  by = "MCC_ID", all.x = TRUE)
MCCs_end$catchment_ID <- as.factor(MCCs_end$catchment_ID)

## LATE instrument at MCC level
MCCs_end$treat_TOT <- MCCs_end$machine_in_use %in% 1:2

# ---------------------------------------------------------------------------
# 4a. Testing of incoming milk quality using milk analyzer
# ---------------------------------------------------------------------------
MCCs_end$test_MA_in <- MCCs_end$q25x3 != 3
MCCs_end$test_MA_in[MCCs_end$q25x3 == "n/a"] <- NA
MCCs_base$b_test_MA_in <- MCCs_base$q25x3 != 1
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_test_MA_in")],
                  by = "MCC_ID", all.x = TRUE)

# ---------------------------------------------------------------------------
# 4b. Testing of outgoing milk quality (SNF & fat tested for any sale)
# ---------------------------------------------------------------------------
MCCs_end$test_MA_out <- (MCCs_end$q39a == 2 & MCCs_end$q39c == 2) |
  (MCCs_end$q52a == 2 & MCCs_end$q52c == 2) |
  (MCCs_end$q62a == 2 & MCCs_end$q62c == 2) |
  (MCCs_end$q72a == 2 & MCCs_end$q72c == 2) |
  (MCCs_end$q82a == 2 & MCCs_end$q82c == 2)

MCCs_base$b_test_MA_out <- (MCCs_base$q39a == 1 | MCCs_base$q39c == 1) |
  (MCCs_base$q52a == 1 | MCCs_base$q52c == 1) |
  (MCCs_base$q62a == 1 | MCCs_base$q62c == 1) |
  (MCCs_base$q72a == 1 | MCCs_base$q72c == 1) |
  (MCCs_base$q82a == 1 | MCCs_base$q82c == 1)
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_test_MA_out")],
                  by = "MCC_ID", all.x = TRUE)

# ---------------------------------------------------------------------------
# 4c. Average price at which milk was bought from farmers (last 7 days)
# ---------------------------------------------------------------------------
MCCs_end$q25b[MCCs_end$q25b %in% c("n/a", "999")] <- NA
MCCs_end$q25b <- as.numeric(as.character(MCCs_end$q25b))
names(MCCs_end)[names(MCCs_end) == "q25b"] <- "price_bought"
MCCs_end$price_bought[MCCs_end$price_bought <= 600] <- NA

MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "q25b")],
                  by = "MCC_ID", all.x = TRUE)
names(MCCs_end)[names(MCCs_end) == "q25b"] <- "b_price_bought"
MCCs_end$b_price_bought[MCCs_end$b_price_bought < 500]  <- NA
MCCs_end$b_price_bought[MCCs_end$b_price_bought > 1250] <- NA

# ---------------------------------------------------------------------------
# 4d. Average sales price (quantity-weighted across buyers, last 7 days)
# ---------------------------------------------------------------------------
columns   <- c("q36", "q49", "q59", "q69", "q79")
columns_2 <- c("q35", "q48", "q58", "q68", "q78")

MCCs_end[columns] <- lapply(MCCs_end[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA; as.numeric(as.character(x))
})
MCCs_base[columns] <- lapply(MCCs_base[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA; x <- as.numeric(as.character(x)); x[x > 10000] <- NA; x
})
MCCs_end[columns_2] <- lapply(MCCs_end[columns_2], function(x) {
  x[x %in% c("n/a", "999")] <- NA; x <- as.numeric(as.character(x)); x[is.na(x)] <- 0; x
})
MCCs_base[columns_2] <- lapply(MCCs_base[columns_2], function(x) {
  x[x %in% c("n/a", "999")] <- NA; x <- as.numeric(as.character(x)); x[is.na(x)] <- 0; x
})

MCCs_end$avg_sales_p <- rowSums(MCCs_end[columns] * MCCs_end[columns_2], na.rm = TRUE) /
  rowSums(MCCs_end[columns_2], na.rm = TRUE)
MCCs_end$avg_sales_p[is.nan(MCCs_end$avg_sales_p)] <- NA
MCCs_end$avg_sales_p[MCCs_end$avg_sales_p == 0]    <- NA

MCCs_base$b_avg_sales_p <- rowSums(MCCs_base[columns] * MCCs_base[columns_2], na.rm = TRUE) /
  rowSums(MCCs_base[columns_2], na.rm = TRUE)
MCCs_base$b_avg_sales_p[is.nan(MCCs_base$b_avg_sales_p)] <- NA
MCCs_base$b_avg_sales_p[MCCs_base$b_avg_sales_p == 0]    <- NA
MCCs_base$b_avg_sales_p[MCCs_base$b_avg_sales_p > 2000]  <- NA
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_avg_sales_p")],
                  by = "MCC_ID", all.x = TRUE)

# ---------------------------------------------------------------------------
# 4e. Quality bonuses (gives premium to farmers / receives premium from buyer)
# ---------------------------------------------------------------------------
MCCs_end$q29[MCCs_end$q29 == "n/a"] <- NA
MCCs_end$gives_q_bonus <- MCCs_end$q29 == 1
MCCs_base$q29[MCCs_base$q29 == 98] <- NA
MCCs_base$b_gives_q_bonus <- MCCs_base$q29 == 1
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_gives_q_bonus")],
                  by = "MCC_ID", all.x = TRUE)

MCCs_end$gets_q_bonus <- ifelse(
  rowSums(MCCs_end[, c("q44", "q54", "q64", "q74", "q84")] == "n/a") == 5,
  NA,
  MCCs_end$q44 == 1 | MCCs_end$q54 == 1 | MCCs_end$q64 == 1 |
    MCCs_end$q74 == 1 | MCCs_end$q84 == 1
)
MCCs_base$b_gets_q_bonus <- ifelse(
  rowSums(MCCs_base[, c("q44", "q54", "q64", "q74", "q84")] == "n/a") ==
    ncol(MCCs_base[, c("q44", "q54", "q64", "q74", "q84")]),
  NA,
  MCCs_base$q44 == 1 | MCCs_base$q54 == 1 | MCCs_base$q64 == 1 |
    MCCs_base$q74 == 1 | MCCs_base$q84 == 1
)
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_gets_q_bonus")],
                  by = "MCC_ID", all.x = TRUE)

# ---------------------------------------------------------------------------
# 4f. MCC secondary outcomes: volumes and farmer counts
# ---------------------------------------------------------------------------
MCCs_end$nr_farmers_wet <- as.numeric(MCCs_end$Q23)
MCCs_base$b_nr_farmers_wet <- as.numeric(MCCs_base$Q23)
MCCs_base$b_nr_farmers_wet[MCCs_base$b_nr_farmers_wet >= 1000] <- NA
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_nr_farmers_wet")],
                  by = "MCC_ID", all.x = TRUE)

MCCs_end$nr_farmers_dry <- as.numeric(MCCs_end$Q24)
MCCs_base$b_nr_farmers_dry <- as.numeric(MCCs_base$Q24)
MCCs_base$b_nr_farmers_dry[MCCs_base$b_nr_farmers_dry >= 1000] <- NA
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_nr_farmers_dry")],
                  by = "MCC_ID", all.x = TRUE)

MCCs_end$nr_farmers_last_week <- as.numeric(MCCs_end$q25a)
MCCs_base$b_nr_farmers_last_week <- as.numeric(MCCs_base$q25a)
MCCs_base$b_nr_farmers_last_week[MCCs_base$b_nr_farmers_last_week >= 1000] <- NA
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_nr_farmers_last_week")],
                  by = "MCC_ID", all.x = TRUE)

MCCs_end$vol_dry <- as.numeric(MCCs_end$q27)
MCCs_base$b_vol_dry <- as.numeric(MCCs_base$qual.q27)
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_vol_dry")],
                  by = "MCC_ID", all.x = TRUE)

MCCs_end$vol_wet <- as.numeric(MCCs_end$q28)
MCCs_base$b_vol_wet <- as.numeric(MCCs_base$qual.q28)
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_vol_wet")],
                  by = "MCC_ID", all.x = TRUE)

MCCs_end$vol_last_week <- as.numeric(MCCs_end$q25c)
MCCs_base$b_vol_last_week <- as.numeric(MCCs_base$q25c)
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_vol_last_week")],
                  by = "MCC_ID", all.x = TRUE)

# ---------------------------------------------------------------------------
# 4g. MCC secondary: treatment uptake (no baseline)
# ---------------------------------------------------------------------------
MCCs_end$poster          <- MCCs_end$poster == 1
MCCs_end$machine         <- MCCs_end$machine == 1
MCCs_end$machine_project <- MCCs_end$machine_project == 1
MCCs_end$machine_in_use  <- MCCs_end$machine_in_use == 1 | MCCs_end$machine_in_use == 2
MCCs_end$test_samples    <- MCCs_end$q16c == "1"
MCCs_end$uses_app        <- MCCs_end$record_keeping.4 == "True" |
                            MCCs_end$record_keeping.5 == "True"

# ---------------------------------------------------------------------------
# 4h. MCC secondary: sales and bargaining
# ---------------------------------------------------------------------------

## Total volumes sold (last 7 days)
columns <- c("q35", "q48", "q58", "q68", "q78")
MCCs_end[columns] <- lapply(MCCs_end[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA; as.numeric(as.character(x))
})
MCCs_base[columns] <- lapply(MCCs_base[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA; as.numeric(as.character(x))
})
MCCs_end$tot_sales_q    <- rowSums(MCCs_end[columns], na.rm = TRUE)
MCCs_base$b_tot_sales_q <- rowSums(MCCs_base[columns], na.rm = TRUE)
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_tot_sales_q")],
                  by = "MCC_ID", all.x = TRUE)

## Used milk analyzer for outgoing sales
MCCs_end$test_MA <- (MCCs_end$q39a == 2 | MCCs_end$q39c == 2) |
  (MCCs_end$q52a == 2 | MCCs_end$q52c == 2) |
  (MCCs_end$q62a == 2 | MCCs_end$q62c == 2) |
  (MCCs_end$q72a == 2 | MCCs_end$q72c == 2) |
  (MCCs_end$q82a == 2 | MCCs_end$q82c == 2)

MCCs_base$b_test_MA <- (MCCs_base$q39a == 2 | MCCs_base$q39c == 2) |
  (MCCs_base$q52a == 2 | MCCs_base$q52c == 2) |
  (MCCs_base$q62a == 2 | MCCs_base$q62c == 2) |
  (MCCs_base$q72a == 2 | MCCs_base$q72c == 2) |
  (MCCs_base$q82a == 2 | MCCs_base$q82c == 2)
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_test_MA")],
                  by = "MCC_ID", all.x = TRUE)

## MCC decides on price (vs buyer or negotiation)
MCCs_end$MCC_decides <- MCCs_end$q40 == "2" | MCCs_end$q53 == "2" |
  MCCs_end$q63 == "2" | MCCs_end$q73 == "2" | MCCs_end$q83 == "2"
MCCs_base$b_MCC_decides <- MCCs_base$q40 == "2" | MCCs_base$q53 == "2" |
  MCCs_base$q63 == "2" | MCCs_base$q73 == "2" | MCCs_base$q83 == "2"
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_MCC_decides")],
                  by = "MCC_ID", all.x = TRUE)

## Buyer paid quality premium
MCCs_end$MCC_got_premium <- MCCs_end$q44 == "1" | MCCs_end$q54 == "1" |
  MCCs_end$q64 == "1" | MCCs_end$q74 == "1" | MCCs_end$q84 == "1"
MCCs_base$b_MCC_got_premium <- MCCs_base$q44 == "1" | MCCs_base$q54 == "1" |
  MCCs_base$q64 == "1" | MCCs_base$q74 == "1" | MCCs_base$q84 == "1"
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_MCC_got_premium")],
                  by = "MCC_ID", all.x = TRUE)

## Average quality premium amount (UGX per liter): 999 = "don't know" -> NA
columns <- c("q46", "q56", "q66", "q76", "q86")
MCCs_end[columns] <- lapply(MCCs_end[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA; as.numeric(as.character(x))
})
MCCs_base[columns] <- lapply(MCCs_base[columns], function(x) {
  x[x %in% c("n/a", "999")] <- NA; as.numeric(as.character(x))
})
MCCs_end$avg_prem_received <- rowMeans(MCCs_end[columns], na.rm = TRUE)
MCCs_end$avg_prem_received[is.nan(MCCs_end$avg_prem_received)] <- NA
MCCs_base$b_avg_prem_received <- rowMeans(MCCs_base[columns], na.rm = TRUE)
MCCs_base$b_avg_prem_received[is.nan(MCCs_base$b_avg_prem_received)] <- NA
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_avg_prem_received")],
                  by = "MCC_ID", all.x = TRUE)

## Sold to top 5 processors
MCCs_end$top_proc <- MCCs_end$q32.2 == "True" & MCCs_end$q33.6 != "True"
MCCs_base$b_top_proc <- MCCs_base$q32.2 == "True" & MCCs_base$q33.6 != "True"
MCCs_end <- merge(MCCs_end, MCCs_base[c("MCC_ID", "b_top_proc")],
                  by = "MCC_ID", all.x = TRUE)


###############################################################################
## 5. BALANCE VARIABLES (from balance_tables.R)
###############################################################################

# ---------------------------------------------------------------------------
# 5a. MCC balance variables
# ---------------------------------------------------------------------------
MCC_balance <- read.csv(paste(path, "baseline/data/public/MCCs.csv", sep = "/"))

## Main-table variables (5)
MCC_balance$is_coop          <- MCC_balance$secC_group.q8 == 2
MCC_balance$capacity         <- as.numeric(as.character(MCC_balance$qual.q26))
MCC_balance$quality_premium  <- { MCC_balance$q29[MCC_balance$q29 == "98"] <- NA; as.numeric(as.character(MCC_balance$q29)) == 1 }
MCC_balance$years_in_operation <- { x <- as.numeric(as.character(MCC_balance$secC_group.q6a)); x[x > 50] <- NA; x }
MCC_balance$supply_accaracides <- as.numeric(as.character(MCC_balance$q18)) %in% c(1, 2)

## Appendix-table variables (5)
MCC_balance$full_timers      <- as.numeric(as.character(MCC_balance$q14m))
MCC_balance$clients_rainy    <- { x <- as.numeric(as.character(MCC_balance$Q23)); x[x > 1000 | x < 5] <- NA; x }
MCC_balance$amount_dry       <- as.numeric(as.character(MCC_balance$qual.q27))
MCC_balance$capacity_use_dry <- { x <- MCC_balance$amount_dry / MCC_balance$capacity * 100; x[x > 100] <- NA; x }
MCC_balance$nr_milk_cans     <- as.numeric(as.character(MCC_balance$q14i))
MCC_balance$supply_credit    <- as.numeric(as.character(MCC_balance$q17)) %in% c(1, 2)

# ---------------------------------------------------------------------------
# 5b. Farmer balance variables
# ---------------------------------------------------------------------------
farmers_balance <- read.csv(paste(path, "baseline/data/public/farmers.csv", sep = "/"))

farmers_balance$hh_size  <- as.numeric(as.character(farmers_balance$q21))
farmers_balance$age_head <- as.numeric(as.character(farmers_balance$q18))
farmers_balance$age_head[is.na(farmers_balance$age_head)] <-
  as.numeric(as.character(farmers_balance$q14[is.na(farmers_balance$age_head)]))

## Herd composition: q24=local cows, q26=local heifers, q28=local calves,
##                   q30=improved cows, q32=improved heifers, q34=improved calves
## 999 = "don't know" per questionnaire instructions -- treat as NA.
herd_cols <- c("q24", "q26", "q28", "q30", "q32", "q34")
farmers_balance[herd_cols] <- lapply(farmers_balance[herd_cols], function(x) {
  x <- as.numeric(as.character(x))
  replace(x, x == 999, NA)
})

## Outlier thresholds (applied after 999 removal)
farmers_balance$q24[farmers_balance$q24 > 500] <- NA
farmers_balance$q24[farmers_balance$q26 > 200] <- NA
farmers_balance$q24[farmers_balance$q28 > 200] <- NA
farmers_balance$q24[farmers_balance$q30 > 500] <- NA
farmers_balance$q24[farmers_balance$q32 > 200] <- NA
farmers_balance$q24[farmers_balance$q34 > 200] <- NA

farmers_balance$herd_size      <- rowSums(farmers_balance[herd_cols])
farmers_balance$improved_share <- rowSums(farmers_balance[c("q30", "q32", "q34")]) / farmers_balance$herd_size

farmers_balance$liter_day_wet      <- farmers_balance$q44
farmers_balance$liter_sold_day_wet <- farmers_balance$q50
farmers_balance$sell_MCC_wet       <- (farmers_balance$q51 == 2)

## Uses steel/aluminium container for any delivery trip
farmers_balance$use_steel <- (farmers_balance$q60 %in% c("4", "6") |
                              farmers_balance$qx7 %in% c("4", "6") |
                              farmers_balance$qx19 %in% c("4", "6") |
                              farmers_balance$qx31 %in% c("4", "6") |
                              farmers_balance$qx43 %in% c("4", "6") |
                              farmers_balance$qx55 %in% c("4", "6"))

farmers_balance$coop_member   <- farmers_balance$q22 == "Yes"
farmers_balance$acaracide_exp <- as.numeric(as.character(farmers_balance$Tick3.q74)) / 3700  # USD

## Trader indicator: farmer_ID suffix _T or _T_R means delivery via trader
farmers_balance$trader <- ifelse(sub("^[^0-9]*[0-9]+", "", farmers_balance$farmer_ID) %in% c("_T", "_T_R"), 1, 0)

## Demeaned treatment variables for balance regressions
farmers_balance$vid_demeaned       <- farmers_balance$video_shown - mean(farmers_balance$video_shown, na.rm = TRUE)
farmers_balance$lactoscan_demeaned <- (farmers_balance$lactoscan == "T") - mean(farmers_balance$lactoscan == "T", na.rm = TRUE)


###############################################################################
## 6. SAVE PREPPED DATA
###############################################################################

saveRDS(farmers_end,     file = paste(path, "paper/results/prepped_farmers.rds", sep = "/"))
saveRDS(farmers_base,    file = paste(path, "paper/results/prepped_farmers_base.rds", sep = "/"))
saveRDS(MCCs_end,        file = paste(path, "paper/results/prepped_MCCs.rds", sep = "/"))
saveRDS(MCCs_base,       file = paste(path, "paper/results/prepped_MCCs_base.rds", sep = "/"))
saveRDS(MCC_balance,     file = paste(path, "paper/results/MCC_balance.rds", sep = "/"))
saveRDS(farmers_balance, file = paste(path, "paper/results/farmers_balance.rds", sep = "/"))

cat("01_prep_main.R completed. Prepped data saved to paper/results/\n")
