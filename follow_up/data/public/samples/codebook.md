# Codebook: RMVC Milk Quality Submission Data

## Study Information

**Title:** When milk quality pays: Evidence from an incentive experiment in Uganda

**Authors:** Bjorn Van Campenhout, Sarah Kariuki, Richard Ariong, Jordan Chamberlin, Benon Byarugaba, and Dennis Atuha

**Organization:** International Food Policy Research Institute (IFPRI)

**Location:** Kazo region, Uganda

**Study Description:** This dataset contains milk quality submission records from a randomized controlled trial examining how quality measurement and price incentives affect milk production standards in smallholder dairy value chains. The intervention introduced milk analyzers at Milk Collection Centers (MCCs) to make milk quality visible and implemented price incentive mechanisms. Each observation represents a milk delivery by a trader to an MCC, with quality readings (fat and solids-not-fat content) recorded by the milk analyzer and the corresponding quality bonus calculated.

**Reference:** https://www.ifpri.org/blog/when-milk-quality-pays-evidence-from-an-incentive-experiment-in-uganda/

**Sample:** 5596 milk quality submissions from traders at Milk Collection Centers

---

## Variable Dictionary

| Variable | Label | Type | Values |
|----------|-------|------|--------|
| `trader_ID` | Trader ID (anonymized) | String | 100 unique values |
| `MCC_ID` | Milk Collection Center ID (anonymized) | String | 19 unique values |
| `district` | District | String | Kazo, Kiruhura |
| `treatment` | Treatment assignment (RCT) | Categorical | 0=Control; 1=Treatment |
| `Fat` | Fat content reading (%) | Numeric | 0 to 7.3 |
| `SNF` | Solids-Not-Fat reading (%) | Numeric | 5.2 to 11.1 |
| `Qty` | Quantity of milk delivered (liters) | Numeric | 4 to 977 |
| `bonus` | Quality bonus paid (UGX) | Numeric | 0 to 293700 |
| `submission_date` | Date and time of submission | String | 5596 unique values |

---

## Value Labels Reference

### Treatment Status (`treatment`)
| Code | Label |
|------|-------|
| 0 | Control |
| 1 | Treatment |

---

## Notes

- Each observation represents a single milk delivery by a trader to a Milk Collection Center (MCC)
- Traders may appear multiple times as they make repeated deliveries
- `Fat` is the fat content percentage as measured by the milk analyzer
- `SNF` is the solids-not-fat content percentage as measured by the milk analyzer
- `Qty` is the total quantity of milk delivered in liters (all cans, not just the one tested)
- `bonus` is the quality bonus in Ugandan Shillings (UGX) calculated based on quality readings and quantity
- `submission_date` is in ISO 8601 format with timezone (East Africa Time, UTC+03:00)
- Treatment MCCs had milk analyzers installed and price incentives implemented; Control MCCs did not

