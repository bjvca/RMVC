# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This directory contains anonymized public data from the RMVC (Randomized Milk Value Chain) project's follow-up farmer endline survey, prepared for distribution on Dataverse.

## Study Reference

Van Campenhout, B., Kariuki, S., Ariong, R., Chamberlin, J., Byarugaba, B., & Atuha, D. (2026). "When milk quality pays: Evidence from an incentive experiment in Uganda." IFPRI.
https://www.ifpri.org/blog/when-milk-quality-pays-evidence-from-an-incentive-experiment-in-uganda/

## Files for Dataverse Distribution

| File | Description |
|------|-------------|
| `anonymized_farmer_endline_data.csv` | Survey data (CSV format) |
| `farmers_endline.dta` | Survey data (Stata format with labels) |
| `farmers_endline.rds` | Survey data (R format with labels) |
| `codebook.md` | Variable documentation (Markdown) |
| `codebook.pdf` | Variable documentation (PDF) |
| `README.txt` | Dataset documentation |

## Supporting Files (not for distribution)

| File | Description |
|------|-------------|
| `Farmers_Endline_used.xlsx` | XLSForm survey definition (source for labels) |
| `prepare_dataverse.R` | Script that generates labeled data files and codebook |

## Regenerating Files

To regenerate the Stata/R files and codebook:
```r
Rscript prepare_dataverse.R
pandoc codebook.md -o codebook.pdf --pdf-engine=pdflatex -V geometry:margin=1in
```

Requires R packages: `readxl`, `haven`, `labelled`

## Data Structure

422 observations, 41 variables including:
- Farmer/trader/MCC identifiers (anonymized)
- Milk production and sales quantities
- Trader relationship duration
- Milk rejection history
- Quality-based pricing
- Feeding practices (bran, crop residues, mineral licks, controlled grazing)
- Transaction-level data (up to 6 recent sales: quantity, quality checks, price)
- Quality testing methods (lactometer, alcohol test, Lactoscan)
- RCT treatment assignment (`treat`: 0=Control, 1=Treatment)

See `codebook.md` for complete variable definitions.
