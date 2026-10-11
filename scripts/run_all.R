################################################################################
# EOPC vs AOPC Systematic Review and Meta-Analysis
# Script: run_all.R, master runner for full reproducibility
# Purpose: Execute all 16 analysis scripts in correct dependency order
# Usage:  Rscript --vanilla run_all.R
################################################################################

.get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    fpath <- sub("--file=", "", file_arg)
    fpath <- gsub("~\\+~", " ", fpath)
    return(dirname(fpath))
  }
  for (i in seq_len(sys.nframe())) {
    ofile <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
    if (!is.null(ofile)) return(dirname(ofile))
  }
  return(".")
}

SCRIPT_DIR <- .get_script_dir()

scripts <- c(
  "00_config.R",                  # Configuration & paths
  "01_data_import.R",             # Data loading & variable preparation
  "02_primary_meta.R",            # Primary meta-analyses (OS + 37 binary)
  "03_sensitivity_pub_bias.R",    # Sensitivity analyses & publication bias
  "04_subgroup_metareg.R",        # Subgroup & meta-regression analyses
  "04b_heterogeneity_decomposition.R", # Heterogeneity decomposition (7 moderators)
  "05_rob_visualization.R",       # Risk of bias figures (ROBINS-E)
  "06_summary_tables.R",          # Summary statistics tables
  "07_prisma_flowchart.R",        # PRISMA 2020 flow diagram
  "08_ipd_reconstruction.R",      # IPD reconstruction & RMST (Piciucchi 2015)
  "09_dose_response.R",           # Dose-response meta-regression (Figure 5)
  "10_influence_cumulative.R",    # Influence diagnostics & leave-one-out
  "11_figure3_summary.R",         # Figure 3 summary forest plot (37 binary outcomes)
  "12_lancet_tables.R",           # Main manuscript tables (Table 1-3)
  "14_supplementary_etables.R",   # Supplementary eTables 1-5
  "13_assemble_submission.R"      # Final assembly & export
)

cat("\n========================================\n")
cat("EOPC Meta-Analysis: Full Pipeline Runner\n")
cat("========================================\n")
cat("Running", length(scripts), "scripts in sequence...\n\n")

errors <- c()
for (i in seq_along(scripts)) {
  script_path <- file.path(SCRIPT_DIR, scripts[i])
  cat(sprintf("[%02d/%02d] Running: %s\n", i, length(scripts), scripts[i]))
  
  result <- tryCatch({
    source(script_path, local = FALSE, chdir = TRUE)
    "OK"
  }, error = function(e) {
    cat(sprintf("  ERROR: %s\n", e$message))
    e$message
  })
  
  if (result != "OK") {
    errors <- c(errors, sprintf("%s: %s", scripts[i], result))
  }
}

cat("\n========================================\n")
cat("Pipeline Complete\n")
cat("========================================\n")
if (length(errors) == 0) {
  cat("All scripts ran successfully.\n")
} else {
  cat("Errors encountered in", length(errors), "scripts:\n")
  for (err in errors) cat("  - ", err, "\n")
}
