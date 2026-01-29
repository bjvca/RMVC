================================================================================
RMVC Milk Quality Submission Data
================================================================================

Title: Milk Quality Submission Data from "When milk quality pays: Evidence from
       an incentive experiment in Uganda"

Authors: Bjorn Van Campenhout, Sarah Kariuki, Richard Ariong, Jordan Chamberlin,
         Benon Byarugaba, and Dennis Atuha

Organization: International Food Policy Research Institute (IFPRI)

--------------------------------------------------------------------------------
DESCRIPTION
--------------------------------------------------------------------------------

This dataset contains anonymized milk quality submission records collected
during a randomized controlled trial (RCT) in Uganda's dairy sector. The study
examined how quality measurement and price incentives affect milk production
standards in smallholder dairy value chains.

The intervention introduced milk analyzers at Milk Collection Centers (MCCs)
to make milk quality visible and implemented price incentives for better
quality milk. Each observation represents a milk delivery by a trader to an
MCC, with quality readings (fat and solids-not-fat content) recorded by the
milk analyzer and the corresponding quality bonus calculated.

Sample: 5596 milk quality submissions from traders at Milk Collection Centers
in the Kazo region of Uganda.

Reference:
https://www.ifpri.org/blog/when-milk-quality-pays-evidence-from-an-incentive-experiment-in-uganda/

--------------------------------------------------------------------------------
FILE MANIFEST
--------------------------------------------------------------------------------

anonymized_submission_data.csv  - Submission data in CSV format (universal)
submissions.dta                 - Submission data in Stata format (with labels)
submissions.rds                 - Submission data in R format (with labels)
codebook.md                     - Variable documentation (Markdown)
codebook.pdf                    - Variable documentation (PDF)
README.txt                      - This file

--------------------------------------------------------------------------------
DATA OVERVIEW
--------------------------------------------------------------------------------

The dataset includes 9 variables covering:

- Trader and MCC identification (anonymized IDs)
- District and treatment assignment
- Milk quality readings (fat content, solids-not-fat)
- Quantity of milk delivered
- Quality bonus paid
- Submission date and time

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
