# Codebook: RMVC Trader Endline Survey

## Study Information

**Title:** When milk quality pays: Evidence from an incentive experiment in Uganda

**Authors:** Bjorn Van Campenhout, Sarah Kariuki, Richard Ariong, Jordan Chamberlin, Benon Byarugaba, and Dennis Atuha

**Organization:** International Food Policy Research Institute (IFPRI)

**Location:** Kazo region, Uganda

**Study Description:** This dataset comes from the endline survey of milk traders in a randomized controlled trial examining how quality measurement and price incentives affect milk production standards in smallholder dairy value chains. The intervention made milk quality visible to market participants through milk analyzers at Milk Collection Centers (MCCs) and implemented price incentive mechanisms.

**Reference:** https://www.ifpri.org/blog/when-milk-quality-pays-evidence-from-an-incentive-experiment-in-uganda/

**Sample:** 99 milk traders associated with 19 Milk Collection Centers

---

## Variable Dictionary

| Variable | Label | Type | Values |
|----------|-------|------|--------|
| `trader_ID` | Trader ID (anonymized) | String | 98 unique values |
| `MCC_ID` | Milk Collection Center ID (anonymized) | String | 19 unique values |
| `trader_transporter` | Are you a trader or a transporter? | String | Trader, Both |
| `self` | Who usually collects milk from farmers? | Categorical | 1=I do everything myself; 2=I collect some but also have employees; 3=I do not pick milk myself, employees do |
| `nr_employed` | How many transporters work for you? | Numeric | 1 to 3 |
| `transport_bodaboda` | Transport used: Boda boda (motorcycle) | Categorical | 0=No; 1=Yes |
| `transport_tuktuk` | Transport used: Tuk tuk/tri-cycle | Categorical | 0=No; 1=Yes |
| `transport_bicycle` | Transport used: Bicycle | Categorical | 0=No; 1=Yes |
| `transport_car` | Transport used: Car/pickup/truck | Categorical | 0=No; 1=Yes |
| `transport_other` | Transport used: Other | Categorical | 0=No; 1=Yes |
| `delivered_quantity` | How much milk in liters did you deliver yesterday? | Numeric | 80 to 1100 |
| `deliver_source` | From how many farmers did you collect milk yesterday? | Numeric | 2 to 64 |
| `deliver_other_mcc` | Do you deliver to other MCCs besides this one? | Categorical | 1=Yes; 2=No |
| `delivered_quantity_othermcc` | How much milk was delivered to other MCCs? | Numeric | 20 to 1000 |
| `supervised_testing` | Did enumerator observe milk analyzer testing? | Categorical | 1=Yes; 2=No |
| `Fat_supervised` | Fat percentage (supervised test) | Numeric | 3.2 to 5.1 |
| `SNF_supervised` | SNF percentage (supervised test) | Numeric | 7.9 to 8.9 |
| `quantity_supervised` | Quantity tested (supervised test, liters) | Numeric | 10 to 648 |
| `rejected_week` | Liters of milk rejected in the last week | Numeric | 0 to 210 |
| `rejected_month` | Liters of milk rejected in the last month | Numeric | 0 to 300 |
| `use_lacto` | Did you use a lactometer to test milk freshness? | Categorical | 1=Yes, always; 2=No, never; 3=Sometimes |
| `lact_min` | Minimum lactometer reading you accept | Numeric | 24 to 28 |
| `use_alcohol` | Did you test freshness using alcohol test? | Categorical | 1=Yes, always; 2=No, never; 3=Sometimes |
| `pay_premim` | Did you pay farmers a premium for better quality? | Categorical | 1=Yes, always; 2=No, never; 3=Sometimes |
| `premium` | Premium amount paid (UGX per liter) | Numeric | 10 to 100 |
| `note_thks` | Survey completion notes | logical | - |
| `treat` | Treatment assignment (RCT) | Categorical | 0=Control; 1=Treatment |
| `purchase_q_1` | Quantity purchased from farmer 1 (liters) | Numeric | 5 to 220 |
| `purchase_q_2` | Quantity purchased from farmer 2 (liters) | Numeric | 3 to 380 |
| `purchase_q_3` | Quantity purchased from farmer 3 (liters) | Numeric | 5 to 250 |
| `purchase_q_4` | Quantity purchased from farmer 4 (liters) | Numeric | 5 to 701 |
| `purchase_q_5` | Quantity purchased from farmer 5 (liters) | Numeric | 2 to 200 |
| `purchase_q_6` | Quantity purchased from farmer 6 (liters) | Numeric | 50 to 50 |
| `puarchase_price_1` | Price paid to farmer 1 (UGX per liter) | Numeric | 700 to 1000 |
| `puarchase_price_2` | Price paid to farmer 2 (UGX per liter) | Numeric | 700 to 1000 |
| `puarchase_price_3` | Price paid to farmer 3 (UGX per liter) | Numeric | 700 to 1000 |
| `puarchase_price_4` | Price paid to farmer 4 (UGX per liter) | Numeric | 700 to 1000 |
| `puarchase_price_5` | Price paid to farmer 5 (UGX per liter) | Numeric | 700 to 1000 |
| `puarchase_price_6` | Price paid to farmer 6 (UGX per liter) | Numeric | 950 to 950 |
| `farmer_link_1` | Duration of relationship with farmer 1 | Categorical | 1=Started yesterday; 2=Started in the last week; 3=Started in the last month; 4=Started in the last year; 5=Started in the last 5 years; 6=More than 5 years ago; 96=Other/Don't know |
| `farmer_link_2` | Duration of relationship with farmer 2 | Categorical | 1=Started yesterday; 2=Started in the last week; 3=Started in the last month; 4=Started in the last year; 5=Started in the last 5 years; 6=More than 5 years ago; 96=Other/Don't know |
| `farmer_link_3` | Duration of relationship with farmer 3 | Categorical | 1=Started yesterday; 2=Started in the last week; 3=Started in the last month; 4=Started in the last year; 5=Started in the last 5 years; 6=More than 5 years ago; 96=Other/Don't know |
| `farmer_link_4` | Duration of relationship with farmer 4 | Categorical | 1=Started yesterday; 2=Started in the last week; 3=Started in the last month; 4=Started in the last year; 5=Started in the last 5 years; 6=More than 5 years ago; 96=Other/Don't know |
| `farmer_link_5` | Duration of relationship with farmer 5 | Categorical | 1=Started yesterday; 2=Started in the last week; 3=Started in the last month; 4=Started in the last year; 5=Started in the last 5 years; 6=More than 5 years ago; 96=Other/Don't know |
| `farmer_link_6` | Duration of relationship with farmer 6 | Categorical | 1=Started yesterday; 2=Started in the last week; 3=Started in the last month; 4=Started in the last year; 5=Started in the last 5 years; 6=More than 5 years ago; 96=Other/Don't know |

---

## Value Labels Reference

### Treatment Status (`treat`)
| Code | Label |
|------|-------|
| 0 | Control |
| 1 | Treatment |

### Yes/No Variables (`deliver_other_mcc`, `supervised_testing`)
| Code | Label |
|------|-------|
| 1 | Yes |
| 2 | No |

### Frequency Variables (`use_lacto`, `use_alcohol`, `pay_premim`)
| Code | Label |
|------|-------|
| 1 | Yes, always |
| 2 | No, never |
| 3 | Sometimes |

### Self Collection (`self`)
| Code | Label |
|------|-------|
| 1 | I do everything myself |
| 2 | I collect some but also have employees |
| 3 | I do not pick milk myself, employees do |

### Farmer Relationship Duration (`farmer_link_1` to `farmer_link_6`)
| Code | Label |
|------|-------|
| 1 | Only started buying from this farmer yesterday |
| 2 | Started buying in the last week |
| 3 | Started buying in the last month |
| 4 | Started buying in the last year |
| 5 | Started buying in the last 5 years |
| 6 | Started buying more than 5 years ago |
| 96 | Other/Don't know |

### Transport Methods (`transport_*`)
| Code | Label |
|------|-------|
| 0 | No |
| 1 | Yes |

---

## Notes

- `NA` values indicate missing data (question not asked due to skip logic, or respondent did not answer)
- Prices are in Ugandan Shillings (UGX) per liter
- Quantities are in liters
- Fat and SNF (Solids-Not-Fat) percentages from milk analyzer testing
- Lactometer readings indicate milk density (proxy for adulteration detection)

