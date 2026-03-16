# Codebook: RMVC Farmer Endline Survey

## Study Information

**Title:** When milk quality pays: Evidence from an incentive experiment in Uganda

**Authors:** Bjorn Van Campenhout, Sarah Kariuki, Richard Ariong, Jordan Chamberlin, Benon Byarugaba, and Dennis Atuha

**Organization:** International Food Policy Research Institute (IFPRI)

**Location:** Kazo region, Uganda

**Study Description:** This dataset comes from the endline survey of dairy farmers in a randomized controlled trial examining how quality measurement and price incentives affect milk production standards in smallholder dairy value chains. The intervention made milk quality visible to market participants through milk analyzers at Milk Collection Centers (MCCs) and implemented price incentive mechanisms. Farmers were interviewed about their milk production, sales to traders, quality testing experiences, and feeding practices.

**Reference:** https://www.ifpri.org/blog/when-milk-quality-pays-evidence-from-an-incentive-experiment-in-uganda/

**Sample:** 422 dairy farmers associated with milk traders and Milk Collection Centers

---

## Variable Dictionary

| Variable | Label | Type | Values |
|----------|-------|------|--------|
| `farmer_ID` | Farmer ID (anonymized) | String | 417 unique values |
| `trader_ID` | Trader ID (anonymized) | String | 97 unique values |
| `MCC_ID` | Milk Collection Center ID (anonymized) | String | 19 unique values |
| `today` | Date of survey | String | 7 unique values |
| `district` | District | String | Kazo, Kiruhura |
| `farmer_tot_prod` | What was total milk production yesterday? (liters) | Numeric | 6 to 470 |
| `farmer_tot_sold` | How much of yesterday's production was sold? (liters) | Numeric | 0 to 450 |
| `farmer_tot_sold_trader` | How much of yesterday's production was sold to the trader? (liters) | Numeric | 0 to 450 |
| `farmer_trader_link` | How long have you been selling to this trader? | Categorical | 1=Only started selling yesterday; 2=Started selling in the last two weeks; 3=Started selling in the last month; 4=Started selling in the last year; 5=Started selling in the last five years; 6=Started selling more than five years ago; 99=Other/Don't know |
| `farmer_rejected_week` | Times milk rejected by trader in the last week | Numeric | 0 to 2 |
| `farmer_rejected_month` | Times milk rejected by trader in the last month | Numeric | 0 to 3 |
| `farmer_bett_prc` | Did the trader pay a better price for better quality milk in the last week? | Categorical | 1=Yes; 2=No |
| `farmer_used_bran` | Did you use feed supplements such as maize bran in the last week? | Categorical | 1=Yes; 2=No |
| `farmer_used_residu` | Did you use crop residues as feeding in the last week? | Categorical | 1=Yes; 2=No |
| `farmer_used_lick` | Did you use mineral licks in the last week? | Categorical | 1=Yes; 2=No |
| `farmer_used_cgrazing` | Did you use controlled grazing in the last week? | Categorical | 1=Yes; 2=No |
| `treat` | Treatment assignment (RCT) | Categorical | 0=Control; 1=Treatment |
| `q_sold_1` | Quantity sold to trader in transaction 1 (liters) | Numeric | 6 to 450 |
| `q_sold_2` | Quantity sold to trader in transaction 2 (liters) | Numeric | 6 to 950 |
| `q_sold_3` | Quantity sold to trader in transaction 3 (liters) | Numeric | 6 to 450 |
| `q_sold_4` | Quantity sold to trader in transaction 4 (liters) | Numeric | 10 to 179 |
| `q_sold_5` | Quantity sold to trader in transaction 5 (liters) | Numeric | 10 to 180 |
| `q_sold_6` | Quantity sold to trader in transaction 6 (liters) | Numeric | 10 to 185 |
| `checked_1` | Was milk quality checked in transaction 1? | Categorical | 1=Yes; 2=No |
| `checked_2` | Was milk quality checked in transaction 2? | Categorical | 1=Yes; 2=No |
| `checked_3` | Was milk quality checked in transaction 3? | Categorical | 1=Yes; 2=No |
| `checked_4` | Was milk quality checked in transaction 4? | Categorical | 1=Yes; 2=No |
| `checked_5` | Was milk quality checked in transaction 5? | Categorical | 1=Yes; 2=No |
| `checked_6` | Was milk quality checked in transaction 6? | Categorical | 1=Yes; 2=No |
| `checked_how_1` | How was milk quality checked in transaction 1? | String | Lactometer, Lactometer Alcohol Lactoscan, Lactometer Alcohol, Lactometer Lactoscan Alcohol |
| `checked_how_2` | How was milk quality checked in transaction 2? | String | Lactometer, Lactometer Alcohol Lactoscan, Lactometer Alcohol, Alcohol Lactoscan |
| `checked_how_3` | How was milk quality checked in transaction 3? | String | Lactometer, Lactometer Alcohol, Lactometer Alcohol Lactoscan |
| `checked_how_4` | How was milk quality checked in transaction 4? | String | Lactometer |
| `checked_how_5` | How was milk quality checked in transaction 5? | String | Lactometer |
| `checked_how_6` | How was milk quality checked in transaction 6? | String | Lactometer |
| `p_sold_1` | Price received per liter in transaction 1 (UGX) | Numeric | 800 to 1100 |
| `p_sold_2` | Price received per liter in transaction 2 (UGX) | Numeric | 800 to 1100 |
| `p_sold_3` | Price received per liter in transaction 3 (UGX) | Numeric | 800 to 1100 |
| `p_sold_4` | Price received per liter in transaction 4 (UGX) | Numeric | 850 to 950 |
| `p_sold_5` | Price received per liter in transaction 5 (UGX) | Numeric | 800 to 1000 |
| `p_sold_6` | Price received per liter in transaction 6 (UGX) | Numeric | 850 to 1000 |

---

## Value Labels Reference

### Treatment Status (`treat`)
| Code | Label |
|------|-------|
| 0 | Control |
| 1 | Treatment |

### Yes/No Variables (`farmer_bett_prc`, `farmer_used_bran`, `farmer_used_residu`, `farmer_used_lick`, `farmer_used_cgrazing`, `checked_1` to `checked_6`)
| Code | Label |
|------|-------|
| 1 | Yes |
| 2 | No |

### Trader Relationship Duration (`farmer_trader_link`)
| Code | Label |
|------|-------|
| 1 | Only started selling to this trader yesterday |
| 2 | Started selling in the last two weeks |
| 3 | Started selling in the last month |
| 4 | Started selling in the last year |
| 5 | Started selling in the last five years |
| 6 | Started selling more than five years ago |
| 99 | Other/Don't know |

### Quality Testing Method (`checked_how_1` to `checked_how_6`)
These are select-multiple variables stored as space-separated labels:

| Value | Description |
|-------|-------------|
| Lactometer | Tested using lactometer |
| Alcohol | Tested using alcohol test |
| Lactoscan | Tested using Lactoscan milk analyzer |
| Other | Other testing method |

---

## Notes

- `NA` values indicate missing data (question not asked due to skip logic, or respondent did not answer)
- Transactions 1-6 refer to the last 3-6 sales the farmer made to the trader, starting with the most recent
- Prices are in Ugandan Shillings (UGX) per liter
- Quantities are in liters
- `checked_how` variables contain space-separated values when multiple testing methods were used (e.g., "Lactometer Alcohol Lactoscan")

