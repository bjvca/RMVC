================================================================================
RMVC Trader Endline Survey Data
================================================================================

Title: Trader Endline Survey Data from "When milk quality pays: Evidence from
       an incentive experiment in Uganda"

Authors: Bjorn Van Campenhout, Sarah Kariuki, Richard Ariong, Jordan Chamberlin,
         Benon Byarugaba, and Dennis Atuha

Organization: International Food Policy Research Institute (IFPRI)

--------------------------------------------------------------------------------
DESCRIPTION
--------------------------------------------------------------------------------

This dataset contains anonymized responses from milk traders surveyed as part
of the endline data collection for a randomized controlled trial (RCT) in
Uganda's dairy sector. The study examined how quality measurement and price
incentives affect milk production standards in smallholder dairy value chains.

The intervention introduced milk analyzers at Milk Collection Centers (MCCs)
to make milk quality visible and implemented price incentives for better
quality milk.

Sample: 99 milk traders associated with 19 Milk Collection Centers in the
Kazo region of Uganda.

Reference:
https://www.ifpri.org/blog/when-milk-quality-pays-evidence-from-an-incentive-experiment-in-uganda/

--------------------------------------------------------------------------------
FILE MANIFEST
--------------------------------------------------------------------------------

anonymized_trader_endline_data.csv  - Survey data in CSV format (universal)
traders_endline.dta                 - Survey data in Stata format (with labels)
traders_endline.rds                 - Survey data in R format (with labels)
codebook.md                         - Variable documentation (Markdown)
codebook.pdf                        - Variable documentation (PDF)
README.txt                          - This file

--------------------------------------------------------------------------------
DATA OVERVIEW
--------------------------------------------------------------------------------

The dataset includes 45 variables covering:

- Trader identification (anonymized trader and MCC IDs)
- Trader type (trader only vs. trader-transporter)
- Transport methods used (bodaboda, tuktuk, bicycle, car)
- Milk quantities delivered
- Quality testing practices (lactometer, alcohol test, milk analyzer)
- Pricing and premium payments
- Relationships with up to 6 supplying farmers
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
