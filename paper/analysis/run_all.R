# ===========================================================================
# run_all.R — Master script for the combined paper
# ---------------------------------------------------------------------------
# Runs all analysis scripts in order, from data preparation through
# figure generation for the two experiments:
#   Exp 1: quality measurement & digital monitoring system x information video
#   Exp 2: quality premium intervention leveraging digital monitoring system
# Must be run with working directory set to paper/analysis/.
#
# Usage:
#   setwd("/path/to/RMVC/paper/analysis")
#   source("run_all.R")
# ===========================================================================

cat("=== Starting full analysis pipeline ===\n")
cat("Working directory:", getwd(), "\n\n")

# Verify we're in the right directory
if (!grepl("/paper/analysis$", getwd())) {
  stop("Working directory must be paper/analysis/. Current: ", getwd())
}

# Ensure results directory exists
path <- strsplit(getwd(), "/paper/analysis")[[1]]
dir.create(paste(path, "paper/results", sep = "/"), showWarnings = FALSE)

cat("--- Step 0: Loading utilities ---\n")
source("00_utilities.R")

cat("\n--- Step 1: Preparing Experiment 1 data ---\n")
source("01_prep_main.R")

cat("\n--- Step 2: Running Experiment 1 analysis ---\n")
source("02_analysis_main.R")

cat("\n--- Step 3: Preparing Experiment 2 data ---\n")
source("03_prep_followup.R")

cat("\n--- Step 4: Running Experiment 2 analysis ---\n")
source("04_analysis_followup.R")

cat("\n--- Step 5: Randomization inference (5,000 permutations) ---\n")
source("06_randomization_inference.R")

cat("\n--- Step 6: Generating figures ---\n")
source("05_figures.R")

cat("\n=== Pipeline complete ===\n")
cat("Results saved to:", paste(path, "paper/results/", sep = "/"), "\n")
cat("Figures saved to:", paste(path, "paper/figures/", sep = "/"), "\n")
