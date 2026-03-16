# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This directory contains anonymized public data from the RMVC (Randomized Milk Value Chain) project's follow-up trader endline survey, prepared for distribution on Dataverse.

## Study Reference

Van Campenhout, B., Kariuki, S., Ariong, R., Chamberlin, J., Byarugaba, B., & Atuha, D. (2026). "When milk quality pays: Evidence from an incentive experiment in Uganda." IFPRI.
https://www.ifpri.org/blog/when-milk-quality-pays-evidence-from-an-incentive-experiment-in-uganda/

## Files for Dataverse Distribution

| File | Description |
|------|-------------|
| `anonymized_trader_endline_data.csv` | Survey data (CSV format) |
| `traders_endline.dta` | Survey data (Stata format with labels) |
| `traders_endline.rds` | Survey data (R format with labels) |
| `codebook.md` | Variable documentation (Markdown) |
| `codebook.pdf` | Variable documentation (PDF) |
| `README.txt` | Dataset documentation |

## Supporting Files (not for distribution)

| File | Description |
|------|-------------|
| `Traders_Endline_used.xlsx` | XLSForm survey definition (source for labels) |
| `prepare_dataverse.R` | Script that generates labeled data files and codebook |

## Regenerating Files

To regenerate the Stata/R files and codebook:
```r
Rscript prepare_dataverse.R
pandoc codebook.md -o codebook.pdf --pdf-engine=pdflatex -V geometry:margin=1in
```

Requires R packages: `readxl`, `haven`, `labelled`

## Data Structure

99 observations, 45 variables including:
- Trader/MCC identifiers (anonymized)
- Trader characteristics and transport methods
- Milk delivery quantities and sources
- Quality testing practices (lactometer, alcohol test, milk analyzer)
- Pricing and premium payments
- Farmer relationship data (up to 6 farmers per trader)
- RCT treatment assignment (`treat`: 0=Control, 1=Treatment)

See `codebook.md` for complete variable definitions.
