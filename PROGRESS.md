# Project: RMVC — NOVAFRICA Slides, Forest Plots & Premium-Units Fix

## Objective
Support the two-experiment Ugandan dairy paper (`paper/paper.lyx`, "When Quality (doesn't) Pay") for the NOVAFRICA conference (present June 19, 2026, ~20 min, max 20 slides) and keep paper claims reproducible against the R pipeline in `paper/analysis/` + results in `paper/results/`. User edits LyX directly and compiles the PDF in LyX desktop (macOS). Standing rule: `git pull` before editing; commit/push only when the user asks.

## Current Status
Now addressing NOVAFRICA discussant comments (Rebecca Wu, U Chicago) ahead of AEJ:Applied submission. Triage: (1) composition-vs-within-supplier decomposition of Exp 1 quality gain = biggest gap (app panel `sample_submissions/dta_reports_masked.csv` has farmer_ID x date x quality, treated MCCs only); (2) pass-through heterogeneity = DONE this session; (3) Exp 1 spillover check via already-computed market-channel outcomes = cheap, pending; submission-vs-survey inconsistency already handled in paper; baseline quality data does not exist (endline-only supervised sampling).

DONE: new `paper/analysis/09_heterogeneity_price_fu.R` (exploratory heterogeneity of Exp 2 farm-gate price effect; spec mirrors 04 section D: `avg_price ~ treat | MCC_ID`, trader-clustered). Results (`paper/results/res_price_het_fu.rds`): negative effect broad-based across volume subgroups (≤median −12.6 p=.080, >median −20.7 p=.036, interaction p=.59; production split same), both districts (Kazo −23.4 p=.059, Kiruhura −13.6 p=.074); no treatment effect on new-supplier share (4.4% ctrl, coef −0.02 p=.29) or on farmer-reported quality-contingent pricing (2.7% ctrl, +0.025 p=.18) → weighs against composition and outside-options channels, most consistent with bargaining. New paragraph inserted in `paper/paper.lyx` after the three-hypotheses discussion (search "If the composition channel"), all numbers via \Sexpr from `res_price_het_fu`; loader added to LyX knitr setup chunk + `eval_sexpr.R`; script added to `run_all.R` (step 4b). Verified: lyx2tex.py + eval_sexpr.R resolve all 556 Sexpr with 0 errors; full-sample coef matches paper's −16.89. Local bare-pdflatex fails on PRE-EXISTING intro-footnote URL `&`/unicode issues, unrelated; user compiles in LyX desktop. All of this committed+pushed in 67178a6 (2026-07-20), together with the Exp 1 app decomposition, forest plots, premium-units fix, and endline/data/public/dta_reports_masked.csv (app data moved to public dir; 10_decomposition_app.R reads from there). Unmasked sample_submissions/dta_reports.csv deliberately NOT committed.

## Completed Tasks
- [x] Forest plot for Table 3 (milk quality) — `fig_forest_quality_exp1.png/.eps`
- [x] Forest plots for Tables 4 (farmer) & 5 (MCC) — `fig_forest_farmer_table4`, `fig_forest_mcc_table5` (.png/.eps)
- [x] Added 90%/95% two-tier whiskers (thick = 90% CI, thin = 95% CI); fixed subtitle clipping by moving the CI note to a bottom caption (tables 4/5 figures)
- [x] Diagnosed the trader-premium 10x error: paper text said "UGX 100 per liter for each 0.1pp"; code (`03_prep_followup.R:62-66`) and the paper's own UGX 19,000 worked example imply UGX 100 per **1.0** pp = **UGX 10 per 0.1pp**
- [x] Fixed both occurrences in `paper/paper.lyx` (lines 856, 6768) to "UGX 10 per liter for each 0.1 percentage point" (user-chosen framing)

## In Progress
- [] Nothing actively in progress

## Next Steps
- [x] Discussant item 1 DONE: `paper/analysis/10_decomposition_app.R` + new subsubsection "Within-Supplier Discipline or Supplier Turnover?" in paper.lyx (between Milk Samples LATE para and Farmer Level). Key facts: app farmer_ID is renumbered WITHIN MCC (panel key = MCC x farmer); Buy/Sale record types are a recording-convention shift (pool them); 57 early-start MCCs = H2-2023 treated rollout, 33 post-endline starters = control rollout. Findings: total vs within-supplier change nearly identical (any-water +25.6pp vs +26.5pp; fat -0.113 vs -0.109) so composition ~nil; turnover big (34.6% retention) but quality-neutral (exiters 0.34 vs stayers 0.19 AW; entrants 0.59 vs 1.06); adulteration lowest at installation (7.7% any-water, 90% tested) drifting to 33% as testing fell to 67% (testing-selection caveat stated); serious AW stayed below control 11%. Switching experimental nulls reported in same section (p=.25/.36/.81, index .45). Results in `results/res_app_decomp.rds`, loaded in eval_sexpr.R + LyX chunk; run_all.R step 4c. Verified: 582 Sexpr resolve, 0 errors, inset balance OK. PAP explicitly said it "cannot definitively decompose" selection vs deterrence — this section now does.
- [ ] Discussant item 3 (Exp 1 spillovers): report market-channel secondary outcomes (sold to MCC vs other buyers, wet/dry) from `01_prep_main.R` constructs
- [ ] User recompiles `paper/paper.pdf` in LyX and verifies the new heterogeneity paragraph + the two corrected premium sentences render (lines 856, 6768)
- [ ] Optional: swap the 3 new forest plots into `nova_africa.pptx` (slide 9 currently uses the older 3-panel forest layout) by editing `presentations/build_nova.py`
- [ ] Optional: align `fig_forest_quality_exp1` subtitle to the same caption layout as tables 4/5 for visual consistency across the trio
- [ ] Optional: add the 3 figure scripts to `paper/analysis/run_all.R` so they regenerate with the pipeline

## Decisions / Notes
- User style (enforced): NO em dashes in paper text; spell out acronyms on first use; no "Co-Authored-By: Claude" in commits; 999 = "don't know" -> NA
- Forest-plot conventions: non-index outcomes standardized by their control-group SD; Anderson indices left in native (control-SD) units; green = p<0.05, grey otherwise; summary index drawn as a diamond. Table 3 colored by RI p (samples), Table 4 by RI p (`ri$farmer$ri_p_treat`, matches Table 4 stars), Table 5 by analytic CR2 p (`res_MCCs[,5]`, matches Table 5 stars) — green/grey split identical under either basis
- Added water in Table 3 plot is NOT reverse-coded (shows true negative coefficient), per explicit user instruction
- Premium units: bonus = (max(0, SNF-8.5)*100 + max(0, fat-threshold)*100) * qty, fat in percent -> 100 UGX/L per 1pp = 10 UGX/L per 0.1pp = ~$0.0027/L (3700 UGX/USD, from `01_prep_main.R:661`). Stage-2 fat effect +0.17pp ≈ 17 UGX/L ≈ the −17 UGX/L negative farmer pass-through
- Trader economics (paper p.27, verified correct): avg control trader ~270 L/day @ 4.0% fat/8.5% SNF; Stage-1 qualifying bonus ~UGX 19,000 (~USD 5); avg bonus actually paid Stage 1 = UGX 27,000 (median 12,000); typical trading margin ~UGX 100/L (~27,000/day). UGX 19,000 example = 0.7pp excess fat x 100 x 270 — confirms per-1.0pp gradient
- This session works in `paper/analysis` + `paper/results` (the combined two-experiment "paper/" folder), NOT the older `PAP/` tree

## Issues / Blockers
- None

## Key Files
- `paper/paper.lyx` — main paper; premium fix applied at lines 856, 6768 (uncommitted)
- `paper/analysis/fig_forest_quality.R` — NEW: generates Table 3 forest plot
- `paper/analysis/fig_forest_tables45.R` — NEW: generates Table 4 (farmer) & Table 5 (MCC) forest plots
- `paper/figures/fig_forest_quality_exp1.{png,eps}` — NEW Table 3 figure
- `paper/figures/fig_forest_farmer_table4.{png,eps}` — NEW Table 4 figure
- `paper/figures/fig_forest_mcc_table5.{png,eps}` — NEW Table 5 figure
- `paper/analysis/03_prep_followup.R` — Exp 2 bonus formula (lines 62-66); source of truth for premium units
- `paper/results/{res_farmers,res_MCCs,res_samples,randomization_inference}.rds` — result arrays consumed by the figure scripts
- `presentations/nova_africa.pptx` — the deck (17 slides, committed/pushed earlier)
- `presentations/build_nova.py` — deck regeneration script
