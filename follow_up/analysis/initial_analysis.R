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

### compare average fat between treatmen and control traders over time and create a graph

library(dplyr)
library(ggplot2)
library(lubridate)

# --- Prepare data ---
dta <- dta %>%
  mutate(date = as.Date(X__system.submissionDate))

# drop a few test data points on 29th
dta <- subset(dta, date >= as.Date("2025-10-31"))

# --- Step 1: Aggregate at trader-date level (weighted averages) ---
trader_day <- dta %>%
  group_by(date, trader, mcc, treatment) %>%
  summarise(
    total_qty = sum(Qty, na.rm = TRUE),
    total_bonus = sum(as.numeric(as.character(bonus)), na.rm = TRUE),
    avg_fat = weighted.mean(Fat, Qty, na.rm = TRUE),
    avg_snf = weighted.mean(SNF, Qty, na.rm = TRUE),
    .groups = "drop"
  )

# --- Step 2: Compute daily averages across traders within treatment arms ---
daily_avg <- trader_day %>%
  group_by(date, treatment) %>%
  summarise(
    avg_fat = mean(avg_fat, na.rm = TRUE),
    avg_snf = mean(avg_snf, na.rm = TRUE),
    avg_qty = mean(total_qty, na.rm = TRUE),
    avg_bonus = mean(total_bonus, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    treat_label = ifelse(treatment == 1, "Treatment", "Control")
  )


# --- Plot 1: Daily evolution of average Fat ---
p_fat <- ggplot(daily_avg, aes(x = date, y = avg_fat, color = treat_label)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
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
  labs(
    title = "Daily Evolution of Average Quality Premium by Treatment",
    x = "Date of Submission",
    y = "Average Quantity (Liters)",
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
model_fat <- feols(avg_fat ~ treatment | mcc, vcov = ~ trader, data = subset(  trader_day,  as.Date(date) <= as.Date("2025-11-01")))

# Display results
summary(model_fat)

# Run regression with MCC fixed effects
model_SNF <- feols(avg_snf ~ treatment | mcc, vcov = ~ trader, data = subset(  trader_day,  as.Date(date)  <= as.Date("2025-11-01")))

# Display results
summary(model_SNF)

# Run regression with MCC fixed effects
model_Qty <- feols(total_qty ~ treatment | mcc,  vcov = ~ trader, data = subset(  trader_day,   as.Date(date)  <= as.Date("2025-11-03")))

# Display results
summary(model_Qty)

# Run regression with MCC fixed effects
model_bonus <- feols(as.numeric(as.character(total_bonus)) ~ treatment | mcc,  vcov = ~ trader, data = subset(  trader_day,  as.Date(date)  <=  as.Date("2025-11-04")))

# Display results
summary(model_bonus)



# Run regression with MCC fixed effects -  endline balance
model_fat <- feols(avg_fat ~ treatment | mcc, vcov = ~ trader,data = subset(  trader_day,   as.Date(date)  > as.Date("2025-11-03")))

# Display results
summary(model_fat)

# Run regression with MCC fixed effects
model_SNF <- feols(avg_snf ~ treatment | mcc,vcov = ~ trader, data = subset(  trader_day,  as.Date(date)  > as.Date("2025-11-03")))

# Display results
summary(model_SNF)

# Run regression with MCC fixed effects
model_Qty <- feols(total_qty ~ treatment | mcc,  vcov = ~ trader, data = subset(  trader_day,   as.Date(date)  > as.Date("2025-11-03")))

# Display results
summary(model_Qty)


# Run regression with MCC fixed effects
model_bonus <- feols(as.numeric(as.character(total_bonus)) ~ treatment | mcc,  vcov = ~ trader, data = subset(  trader_day,  as.Date(date)  > as.Date("2025-11-03")))

# Display results
summary(model_bonus)



