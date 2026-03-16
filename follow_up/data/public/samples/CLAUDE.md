# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This directory contains anonymized public data from the RMVC (Randomized Milk Value Chain) project's milk quality submission records, prepared for distribution on Dataverse.

## Study Reference

Van Campenhout, B., Kariuki, S., Ariong, R., Chamberlin, J., Byarugaba, B., & Atuha, D. (2026). "When milk quality pays: Evidence from an incentive experiment in Uganda." IFPRI.
https://www.ifpri.org/blog/when-milk-quality-pays-evidence-from-an-incentive-experiment-in-uganda/

## Files for Dataverse Distribution

| File | Description |
|------|-------------|
| `anonymized_submission_data.csv` | Submission data (CSV format) |
| `submissions.dta` | Submission data (Stata format with labels) |
| `submissions.rds` | Submission data (R format with labels) |
| `codebook.md` | Variable documentation (Markdown) |
| `codebook.pdf` | Variable documentation (PDF) |
| `README.txt` | Dataset documentation |

## Supporting Files (not for distribution)

| File | Description |
|------|-------------|
| `Quality_Tracking_used.xlsx` | XLSForm survey definition (source for labels) |
| `prepare_dataverse.R` | Script that generates labeled data files and codebook |

## Regenerating Files

To regenerate the Stata/R files and codebook:
```r
Rscript prepare_dataverse.R
pandoc codebook.md -o codebook.pdf --pdf-engine=pdflatex -V geometry:margin=1in
```

Requires R packages: `readxl`, `haven`, `labelled`

## Data Structure

5596 observations, 9 variables including:
- Trader/MCC identifiers (anonymized)
- District and RCT treatment assignment (`treatment`: 0=Control, 1=Treatment)
- Milk quality readings (Fat %, SNF %)
- Quantity of milk delivered (liters)
- Quality bonus paid (UGX)
- Submission date and time

See `codebook.md` for complete variable definitions.
