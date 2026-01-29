================================================================================
RMVC Farmer Endline Survey Data
================================================================================

Title: Farmer Endline Survey Data from "When milk quality pays: Evidence from
       an incentive experiment in Uganda"

Authors: Bjorn Van Campenhout, Sarah Kariuki, Richard Ariong, Jordan Chamberlin,
         Benon Byarugaba, and Dennis Atuha

Organization: International Food Policy Research Institute (IFPRI)

--------------------------------------------------------------------------------
DESCRIPTION
--------------------------------------------------------------------------------

This dataset contains anonymized responses from dairy farmers surveyed as part
of the endline data collection for a randomized controlled trial (RCT) in
Uganda's dairy sector. The study examined how quality measurement and price
incentives affect milk production standards in smallholder dairy value chains.

The intervention introduced milk analyzers at Milk Collection Centers (MCCs)
to make milk quality visible and implemented price incentives for better
quality milk. Farmers were interviewed about their milk production, sales
to traders, quality testing experiences, and feeding practices.

Sample: 422 dairy farmers associated with milk traders and Milk Collection
Centers in the Kazo region of Uganda.

Reference:
https://www.ifpri.org/blog/when-milk-quality-pays-evidence-from-an-incentive-experiment-in-uganda/

--------------------------------------------------------------------------------
FILE MANIFEST
--------------------------------------------------------------------------------

anonymized_farmer_endline_data.csv  - Survey data in CSV format (universal)
farmers_endline.dta                 - Survey data in Stata format (with labels)
farmers_endline.rds                 - Survey data in R format (with labels)
codebook.md                         - Variable documentation (Markdown)
codebook.pdf                        - Variable documentation (PDF)
README.txt                          - This file

--------------------------------------------------------------------------------
DATA OVERVIEW
--------------------------------------------------------------------------------

The dataset includes 41 variables covering:

- Farmer and trader identification (anonymized IDs)
- Milk production and sales quantities
- Trader relationship duration
- Milk rejection history
- Quality-based pricing
- Animal feeding practices (bran, crop residues, mineral licks, controlled grazing)
- Transaction-level data for up to 6 recent sales (quantity, quality checks, price)
- Quality testing methods used (lactometer, alcohol test, Lactoscan)
- RCT treatment assignment

See codebook.md or codebook.pdf for complete variable definitions and value
labels.

--------------------------------------------------------------------------------
DATA FORMATS
--------------------------------------------------------------------------------

CSV: Universal format, readable by any software. No embedded labels.

Stata (.dta): Contains embedded variable labels and value labels. Compatible
with Stata 14 and later. Can also be read by R (haven package), Python
(pandas), and other statistical software.

R (.rds): Native R format with labelled class variables. Load with readRDS().
Requires the 'labelled' package to work with value labels.

--------------------------------------------------------------------------------
CONTACT
--------------------------------------------------------------------------------

International Food Policy Research Institute (IFPRI)
https://www.ifpri.org

--------------------------------------------------------------------------------
LICENSE
--------------------------------------------------------------------------------

[Specify license - e.g., CC BY 4.0]

================================================================================
