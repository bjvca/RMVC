### analysis
rm(list=ls())

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(readr)
  library(lubridate)
  library(fs)
  library(xml2)
  library(ruODK)
})



#grab data from ODK central server

# --- Parameters ---
ODK_BASE <- Sys.getenv("ODK_BASE")
ODK_USER <- Sys.getenv("ODK_USER")
ODK_PASS <- Sys.getenv("ODK_PASS")

OUT_DIR <- "/home/bjvca/data/projects/OneCG/RMVC/follow_up/data/raw"

limit <- 100
skip  <- 0
all_batches <- list()

repeat {
  # Construct URL with pagination
  url <- paste0(ODK_BASE, "/Submissions?$skip=", skip, "&$top=", limit)
  
  req <- request(url) |>
    req_auth_basic(user = ODK_USER, password = ODK_PASS)
  
  resp <- req_perform(req)
  data_json <- fromJSON(resp_body_string(resp), flatten = TRUE)
  
  # Check if data available
  if (is.null(data_json$value) || length(data_json$value) == 0) break
  
  # Convert to tibble
  batch <- as_tibble(data_json$value)
  all_batches[[length(all_batches) + 1]] <- batch
  
  # Move to next page
  skip <- skip + limit
}

dta <- data.frame(bind_rows(all_batches))

cat("Total records:", nrow(dta), "\n")

## correct mistake in ODK data pull (last minute change of Mushande by Bemu)

dta$treatment[dta$X__id == "uuid:e40f366b-0607-4f46-abb3-135d5f6fda16"] <- 0

###drop outlier
#dta <- subset(dta,trader != "Godfrey Tumusiime")
#dta$Fat[dta$Fat <= 3] <- NA
#dta$Fat[dta$Fat >=5] <- NA

### compare average fat between treatmen and control traders over time and create a graph

library(dplyr)
library(ggplot2)
library(lubridate)

# --- Prepare data ---
dta <- dta %>%
  mutate(date = as.Date(X__system.submissionDate))

# drop a few test data points on 29th
dta <- subset(dta, date >= as.Date("2025-10-31"))

### save raw data here
write.csv(dta, file = "raw_submission_data_final.csv", row.names = FALSE)

### up to end data
dta <- subset(dta, date <= as.Date("2025-12-1"))

dta <- subset(dta, Fat>=3 & Fat<=5.5)


trader_day <- dta %>%
  group_by(date, trader, mcc, treatment) %>%
  summarise(
    total_qty_1 = sum(Qty, na.rm = TRUE),
    avg_fat_1   = weighted.mean(Fat, Qty, na.rm = TRUE),
    avg_snf_1   = weighted.mean(SNF, Qty, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    total_bonus_1 = if_else(
      date >= as.Date("2025-11-15"),
      # New rules from Nov 15 onward (fat threshold 3.9)
      round(
        (pmax(0, avg_snf_1 - 8.5) * 100 +
           pmax(0, avg_fat_1 - 3.9) * 100) *
          total_qty_1
      ),
      # Old rules before Nov 15 (fat threshold 3.3)
      round(
        (pmax(0, avg_snf_1 - 8.5) * 100 +
           pmax(0, avg_fat_1 - 3.3) * 100) *
          total_qty_1
      )
    )
  )

# --- Step 2: Compute daily averages across traders within treatment arms ---
daily_avg <- trader_day %>%
  group_by(date, treatment) %>%
  summarise(
    avg_fat = mean(avg_fat_1, na.rm = TRUE),
    sd_fat  = sd(avg_fat_1, na.rm = TRUE),
    n = dplyr::n(),
    se_fat = sd(avg_fat_1, na.rm = TRUE) / sqrt(n),
    avg_snf = mean(avg_snf_1, na.rm = TRUE),
    sd_snf  = sd(avg_snf_1, na.rm = TRUE),
    se_snf = sd(avg_snf_1, na.rm = TRUE) / sqrt(n),
    avg_qty = mean(total_qty_1, na.rm = TRUE),
    sd_qty  = sd(total_qty_1, na.rm = TRUE),
    se_qty = sd(total_qty_1, na.rm = TRUE) / sqrt(n),
    avg_bonus = mean(total_bonus_1, na.rm = TRUE),
    sd_bonus  = sd(total_bonus_1, na.rm = TRUE),
    se_bonus = sd(total_bonus_1, na.rm = TRUE) / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    treat_label = ifelse(treatment == 1, "Treatment", "Control")
  )


# --- Plot 1: Daily evolution of average Fat ---

p_fat <- ggplot(daily_avg, aes(x = date, y = avg_fat, color = treat_label)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = avg_fat - se_fat,
                    ymax = avg_fat + se_fat),
                width = 0.15,
                alpha = 0.6) +
  geom_vline(xintercept = as.Date("2025-11-01"),
             linetype = "dashed", color = "grey30", linewidth = 0.8) +
  geom_vline(xintercept = as.Date("2025-11-15"),
             linetype = "dashed", color = "red", linewidth = 0.8) +
  annotate("text", x = as.Date("2025-11-01"), y = max(daily_avg$avg_fat, na.rm = TRUE),
           label = "Stage 1", vjust = -0.5, hjust = -0.1, size = 4, color = "grey30") +
  annotate("text", x = as.Date("2025-11-15"), y = max(daily_avg$avg_fat, na.rm = TRUE),
           label = "Stage 2", vjust = -0.5, hjust = -0.1, size = 4, color = "red") +
  labs(
    title = "Daily Evolution of Average Fat by Treatment",
    x = "Date of Submission",
    y = "Average Fat (%)",
    color = "Group"
  ) +
  theme_minimal(base_size = 13)



# --- Plot 2: Daily evolution of average SNF ---
p_snf <- ggplot(daily_avg, aes(x = date, y = avg_snf, color = treat_label)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = avg_snf - se_snf,
                    ymax = avg_snf + se_snf),
                width = 0.15,
                alpha = 0.6) +
  geom_vline(xintercept = as.Date("2025-11-01"),
             linetype = "dashed", color = "grey30", linewidth = 0.8) +
  geom_vline(xintercept = as.Date("2025-11-15"),
             linetype = "dashed", color = "red", linewidth = 0.8) +
  annotate("text", x = as.Date("2025-11-01"), y = max(daily_avg$avg_snf, na.rm = TRUE),
           label = "Stage 1", vjust = -0.5, hjust = -0.1, size = 4, color = "grey30") +
  annotate("text", x = as.Date("2025-11-15"), y = max(daily_avg$avg_snf, na.rm = TRUE),
           label = "Stage 2", vjust = -0.5, hjust = -0.1, size = 4, color = "red") +
  labs(
    title = "Daily Evolution of Average SNF by Treatment",
    x = "Date of Submission",
    y = "Average SNF (%)",
    color = "Group"
  ) +
  theme_minimal(base_size = 13)
# --- Plot 3: Daily evolution of average Quantity ---
p_qty <- ggplot(daily_avg, aes(x = date, y = avg_qty, color = treat_label)) +
  geom_line(size = 1) +
  geom_point(size = 2) +  
  geom_errorbar(aes(ymin = avg_qty - se_qty,
                    ymax = avg_qty + se_qty),
                width = 0.15,
                alpha = 0.6) +
  geom_vline(xintercept = as.Date("2025-11-01"),
             linetype = "dashed", color = "grey30", linewidth = 0.8) +
  geom_vline(xintercept = as.Date("2025-11-15"),
             linetype = "dashed", color = "red", linewidth = 0.8) +
  annotate("text", x = as.Date("2025-11-01"), y = max(daily_avg$avg_qty, na.rm = TRUE),
           label = "Stage 1", vjust = -0.5, hjust = -0.1, size = 4, color = "grey30") +
  annotate("text", x = as.Date("2025-11-15"), y = max(daily_avg$avg_qty, na.rm = TRUE),
           label = "Stage 2", vjust = -0.5, hjust = -0.1, size = 4, color = "red") +
  labs(
    title = "Daily Evolution of Average Quantity by Treatment",
    x = "Date of Submission",
    y = "Average Quantity (Liters)",
    color = "Group"
  ) +
  theme_minimal(base_size = 13)  
  # --- Plot 4: Daily evolution of bonus ---
p_bonus <- ggplot(daily_avg, aes(x = date, y = avg_bonus, color = treat_label)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = avg_bonus - se_bonus,
                    ymax = avg_bonus + se_bonus),
                width = 0.15,
                alpha = 0.6) +
  geom_vline(xintercept = as.Date("2025-11-01"),
             linetype = "dashed", color = "grey30", linewidth = 0.8) +
  geom_vline(xintercept = as.Date("2025-11-15"),
             linetype = "dashed", color = "red", linewidth = 0.8) +
  annotate("text", x = as.Date("2025-11-01"), y = max(daily_avg$avg_bonus, na.rm = TRUE),
           label = "Stage 1", vjust = -0.5, hjust = -0.1, size = 4, color = "grey30") +
  annotate("text", x = as.Date("2025-11-15"), y = max(daily_avg$avg_bonus, na.rm = TRUE),
           label = "Stage 2", vjust = -0.5, hjust = -0.1, size = 4, color = "red") +
  labs(
    title = "Daily Evolution of Average Quality Premium by Treatment",
    x = "Date of Submission",
    y = "Average Premium (UGX)",
    color = "Group"
  ) +
  theme_minimal(base_size = 13)

# --- Display all plots ---
print(p_fat)
print(p_snf)
print(p_qty)
print(p_bonus)



## run a regression to estimate treatment effect on fat
library(fixest)
library(dplyr)

# Clean data (remove missing values in key vars)


# Run regression with MCC fixed effects -  baseline balance
model_fat <- feols(avg_fat_1 ~ treatment | mcc, vcov = ~ trader, data = subset(  trader_day,  as.Date(date) <= as.Date("2025-11-01")))

# Display results
summary(model_fat)

# Run regression with MCC fixed effects
model_SNF <- feols(avg_snf_1 ~ treatment | mcc, vcov = ~ trader, data = subset(  trader_day,  as.Date(date)  <= as.Date("2025-11-01")))

# Display results
summary(model_SNF)

# Run regression with MCC fixed effects
model_Qty <- feols(total_qty_1 ~ treatment | mcc,  vcov = ~ trader, data = subset(  trader_day,   as.Date(date)  <= as.Date("2025-11-01")))

# Display results
summary(model_Qty)

# Run regression with MCC fixed effects
model_bonus <- feols(as.numeric(as.character(total_bonus_1)) ~ treatment | mcc,  vcov = ~ trader, data = subset(  trader_day,  as.Date(date)  <=  as.Date("2025-11-01")))

# Display results
summary(model_bonus)



# Run regression with MCC fixed effects -  endline balance
model_fat <- feols(avg_fat_1 ~ treatment | mcc, vcov = ~ trader,data = subset(  trader_day,   as.Date(date)  > as.Date("2025-11-03")))

# Display results
summary(model_fat)

# Run regression with MCC fixed effects
model_SNF <- feols(avg_snf_1 ~ treatment | mcc,vcov = ~ trader, data = subset(  trader_day,  as.Date(date)  > as.Date("2025-11-03")))

# Display results
summary(model_SNF)

# Run regression with MCC fixed effects
model_Qty <- feols(total_qty_1 ~ treatment | mcc,  vcov = ~ trader, data = subset(  trader_day,   as.Date(date)  > as.Date("2025-11-03")))

# Display results
summary(model_Qty)


# Run regression with MCC fixed effects
model_bonus <- feols(as.numeric(as.character(total_bonus_1)) ~ treatment | mcc,  vcov = ~ trader, data = subset(  trader_day,  as.Date(date)  > as.Date("2025-11-03")))

# Display results
summary(model_bonus)

ODK_BASE <-"https://odk.bjornvancampenhout.com/v1/projects/2/forms/Traders_Endline.svc"
ODK_USER <- Sys.getenv("ODK_USER")
ODK_PASS <- Sys.getenv("ODK_PASS")

OUT_DIR <- "/home/bjvca/data/projects/OneCG/RMVC/follow_up/data/raw"

limit <- 100
skip  <- 0
all_batches <- list()

repeat {
  # Construct URL with pagination
  url <- paste0(ODK_BASE, "/Submissions?$skip=", skip, "&$top=", limit)
  
  req <- request(url) |>
    req_auth_basic(user = ODK_USER, password = ODK_PASS)
  
  resp <- req_perform(req)
  data_json <- fromJSON(resp_body_string(resp), flatten = TRUE)
  
  # Check if data available
  if (is.null(data_json$value) || length(data_json$value) == 0) break
  
  # Convert to tibble
  batch <- as_tibble(data_json$value)
  all_batches[[length(all_batches) + 1]] <- batch
  
  # Move to next page
  skip <- skip + limit
}

dta_trader_main <- data.frame(bind_rows(all_batches))

cat("Total records:", nrow(dta_trader_main), "\n")


ODK_BASE <-"https://odk.bjornvancampenhout.com/v1/projects/2/forms/Traders_Endline.svc"


limit <- 100
skip  <- 0
all_batches <- list()

repeat {
  # Construct URL with pagination
  url <- paste0(ODK_BASE, "/Submissions.trans?$skip=", skip, "&$top=", limit)
  
  req <- request(url) |>
    req_auth_basic(user = ODK_USER, password = ODK_PASS)
  
  resp <- req_perform(req)
  data_json <- fromJSON(resp_body_string(resp), flatten = TRUE)
  
  # Check if data available
  if (is.null(data_json$value) || length(data_json$value) == 0) break
  
  # Convert to tibble
  batch <- as_tibble(data_json$value)
  all_batches[[length(all_batches) + 1]] <- batch
  
  # Move to next page
  skip <- skip + limit
}

dta_traders_transactions <- data.frame(bind_rows(all_batches))

cat("Total records:", nrow(dta_traders_transactions), "\n")

treats <- read.csv("~/data/projects/OneCG/RMVC/follow_up/questionnaire/upload_x.csv")
dta_trader_main <- merge(dta_trader_main, treats[,c("hh_key","treat")], by.x = "trader_name", by.y = "hh_key")

#merge trader main and transactions
dta_traders <- merge(dta_trader_main, dta_traders_transactions, by.x = "X__id", by.y = "X__Submissions.id")
#we need to merge in treatment assignment
### write raw data here
write.csv(dta_traders, file = "raw_trader_endline_data_final.csv", row.names = FALSE)

#### some very simple regressions
##at trader level effect on fat

model_rej <- feols((rejected_month>0 )  ~ treat | mcc, data =dta_trader_main)
model_prim <- feols((pay_premim == 1)  ~ treat | mcc, data =dta_trader_main)
model_fat <- feols(Fat_supervised ~ treat | mcc, data =dta_trader_main)
model_quant <- feols(delivered_quantity ~ treat | mcc, data =dta_trader_main)

model_self <- feols((self == 1) ~ treat | mcc, data =dta_trader_main)


##at farmer level effect on price
dta_traders$purchase_q[dta_traders$purchase_q >1000] <- NA

model_price <- feols(puarchase_price ~ treat | mcc, ,vcov = ~ trader_name, data =dta_traders)

trader_avg <- dta_traders %>%
  group_by(trader_name, treat) %>%
  summarise(
    avg_price = mean(puarchase_price, na.rm = TRUE),
    avg_quant = mean(purchase_q, na.rm = TRUE),
    .groups = "drop"
  ) 

summary(lm(avg_price ~ treat,data=trader_avg))
summary(lm(avg_quant ~ treat,data=trader_avg))

###export list with farmer names

write.csv(dta_traders[,c("mcc","trader_name","farmer_name","farmer_phone", "district","farmer_sub", "farmer_village")],file="farmer_list_endline.csv",row.names=FALSE)

setdiff(names(table(treats$mcc)), names(table(dta_trader_main$mcc)))

missing <- setdiff(names(table(treats$hh_key)), names(table(dta_trader_main$trader_name)))

treats[treats$hh_key %in% missing,]

#################### now for farmer endline


ODK_BASE <-"https://odk.bjornvancampenhout.com/v1/projects/2/forms/Farmers_Endline.svc"

limit <- 100
skip  <- 0
all_batches <- list()

repeat {
  # Construct URL with pagination
  url <- paste0(ODK_BASE, "/Submissions?$skip=", skip, "&$top=", limit)
  
  req <- request(url) |>
    req_auth_basic(user = ODK_USER, password = ODK_PASS)
  
  resp <- req_perform(req)
  data_json <- fromJSON(resp_body_string(resp), flatten = TRUE)
  
  # Check if data available
  if (is.null(data_json$value) || length(data_json$value) == 0) break
  
  # Convert to tibble
  batch <- as_tibble(data_json$value)
  all_batches[[length(all_batches) + 1]] <- batch
  
  # Move to next page
  skip <- skip + limit
}

dta_farmers_main <- data.frame(bind_rows(all_batches))

cat("Total records:", nrow(dta_farmers_main), "\n")


ODK_BASE <- "https://odk.bjornvancampenhout.com/v1/projects/2/forms/Farmers_Endline.svc"


limit <- 100
skip  <- 0
all_batches <- list()

repeat {
  # Construct URL with pagination
  url <- paste0(ODK_BASE, "/Submissions.farmer.trans?$skip=", skip, "&$top=", limit)
  
  req <- request(url) |>
    req_auth_basic(user = ODK_USER, password = ODK_PASS)
  
  resp <- req_perform(req)
  data_json <- fromJSON(resp_body_string(resp), flatten = TRUE)
  
  # Check if data available
  if (is.null(data_json$value) || length(data_json$value) == 0) break
  
  # Convert to tibble
  batch <- as_tibble(data_json$value)
  all_batches[[length(all_batches) + 1]] <- batch
  
  # Move to next page
  skip <- skip + limit
}

dta_farmers_transactions <- data.frame(bind_rows(all_batches))

cat("Total records:", nrow(dta_farmers_transactions), "\n")

treats <- read.csv("~/data/projects/OneCG/RMVC/follow_up/questionnaire/upload_x.csv")
dta_farmers_main <- merge(dta_farmers_main, treats[,c("hh_key","treat")], by.x = "trader_name", by.y = "hh_key")

#merge trader main and transactions
dta_farmers <- merge(dta_farmers_main, dta_farmers_transactions, by.x = "X__id", by.y = "X__Submissions.id")
#we need to merge in treatment assignment

### write raw data here
write.csv(dta_farmers, file = "raw_farmer_endline_data_final.csv", row.names = FALSE)


model_price <- feols(p_sold ~ treat | mcc,vcov = ~ trader_name, data =dta_farmers)
model_checked <- feols((checked == 1) ~ treat | mcc,vcov = ~ trader_name, data =dta_farmers)
model_quant <- feols(q_sold ~ treat | mcc,vcov = ~ trader_name, data =dta_farmers)

dta_farmers_avg <- dta_farmers %>%
  group_by(farmer.farmer_name) %>%
  summarise(
    p_sold = weighted.mean(p_sold, q_sold, na.rm = TRUE),
    treat = first(treat),
    mcc = first(mcc),
    trader_name = first(trader_name)
  )

feols(p_sold ~ treat | mcc,vcov = ~ trader_name, data =dta_farmers_avg)

##list missing
farmer_list <-read.csv("farmer_list_endline_sorted.csv")


setdiff(names(table(treats$trader_name)), names(table(dta_farmers$trader_name)))

missing <- setdiff(names(table(treats$hh_key)), names(table(dta_trader_main$trader_name)))

treats[treats$hh_key %in% missing,]

### How to fetch and inspect metadata from OData service
ODK_BASE <- "https://odk.bjornvancampenhout.com/v1/projects/2/forms/Farmers_Endline.svc"

meta <- request(paste0(ODK_BASE, "/$metadata")) |>
  req_auth_basic(user = ODK_USER, password = ODK_PASS) |>
  req_perform()

meta_xml <- resp_body_string(meta)
library(xml2)
doc <- read_xml(meta_xml)

# Extract entity set names
entity_sets <- xml_find_all(doc, ".//d:EntitySet", xml_ns(doc))
names <- xml_attr(entity_sets, "Name")
names
ns <- c(d = "http://docs.oasis-open.org/odata/ns/edm",
        edmx = "http://docs.oasis-open.org/odata/ns/edmx")

# Extract all entity sets
entity_sets <- xml_find_all(doc, ".//d:EntitySet", ns)
names <- xml_attr(entity_sets, "Name")

names

library(dplyr)
library(stringr)

list <- farmer_list
endline_data <- dta_farmers_main
names(endline_data)[names(endline_data) == "farmer.farmer_name"] <- "farmer_name"



clean <- function(x) str_trim(str_to_lower(x))

# Clean + de-dup at trader–farmer level
list_clean <- list %>%
  mutate(across(c(trader_name, farmer_name), clean)) %>%
  distinct(trader_name, farmer_name, district, farmer_sub)

endline_clean <- endline_data %>%
  mutate(across(c(trader_name, farmer_name), clean)) %>%
  distinct(trader_name, farmer_name)

# How many farmers should be covered per trader (from roster)
roster_by_trader <- list_clean %>%
  group_by(trader_name) %>%
  summarise(
    n_listed = n_distinct(farmer_name),
    # carry district/farmer_sub from the roster (assumes constant per trader)
    district = first(district),
    farmer_sub = first(farmer_sub),
    .groups = "drop"
  )

# How many were actually interviewed per trader
interviewed_by_trader <- endline_clean %>%
  group_by(trader_name) %>%
  summarise(n_interviewed = n_distinct(farmer_name), .groups = "drop")

# Join on trader only; keep district/farmer_sub from roster
traders_with_shortfall <- roster_by_trader %>%
  left_join(interviewed_by_trader, by = "trader_name") %>%
  mutate(
    n_interviewed = coalesce(n_interviewed, 0L),
    missing = pmax(n_listed - n_interviewed, 0L)
  ) %>%
  filter(missing > 0) %>%
  arrange(desc(missing), trader_name)

traders_with_shortfall
# View results
write.csv(traders_with_shortfall, file= "missed.csv")
write.csv(dta_farmers_main, file= "endline_farmers_raw.csv")
