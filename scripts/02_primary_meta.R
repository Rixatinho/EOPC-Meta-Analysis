################################################################################
# EOPC vs AOPC Systematic Review and Meta-Analysis
# Script 02: Primary Meta-Analyses (Forest Plots)
# Input:  prepared_data.RData
# Output: meta_results.RData, forest plots (TIFF+PDF)
################################################################################

# Robust script directory detection (handles conda run ~+~ encoding)
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
source(file.path(.get_script_dir(), "00_config.R"))

# Load prepared data
load(file.path(PATHS$stat_data, "prepared_data.RData"))

# ==============================================================================
# PART 1: Time-to-Event Meta-Analysis (HR) - Generic Inverse-Variance
# ==============================================================================

run_hr_meta <- function(dat, outcome_label, filename_prefix, fp_opts = list(),
                        save_opts = list()) {
  if (is.null(dat) || nrow(dat) == 0) {
    cat("Skipping", outcome_label, "- no data.\n")
    return(NULL)
  }

  cat("\n====", outcome_label, "====\n")
  cat("Studies:", nrow(dat), "\n")

  m <- metagen(
    TE      = logHR,
    seTE    = seLogHR,
    data    = dat,
    studlab = Study_Label,
    sm      = "HR",
    method.tau = SETTINGS$method_tau,
    method.random.ci = SETTINGS$method_ci,
    adhoc.hakn.ci = SETTINGS$adhoc_hakn,
    prediction = SETTINGS$prediction,
    title   = paste0(outcome_label, ": EOPC vs AOPC")
  )

  cat("\n--- Summary ---\n")
  print(summary(m))

  cat(sprintf("\nHeterogeneity: tau^2=%.4f; I^2=%.1f%%; Q=%.2f (df=%d, p=%s)\n",
              m$tau2, m$I2 * 100, m$Q, m$df.Q, format_pval(m$pval.Q)))

  if (SETTINGS$prediction && !is.null(m$lower.predict) && m$k >= 5) {
    cat(sprintf("Prediction interval: [%.2f, %.2f]\n",
                exp(m$lower.predict), exp(m$upper.predict)))
  } else if (m$k < 5) {
    cat("Prediction interval: not reported (k < 5, unreliable)\n")
  }

  # Forest plot — Lancet table-based layout (forestploter)
  forest_file <- file.path(PATHS$forest, paste0(filename_prefix, "_forest"))

  fp <- tryCatch(
    do.call(lancet_fp_forest,
            c(list(m = m, outcome_label = outcome_label, sort_year = dat$Year), fp_opts)),
    error = function(e) {
      cat("  forestploter error:", e$message, " - falling back to meta::forest\n")
      NULL
    }
  )

  if (!is.null(fp)) {
    do.call(save_fp_figure, c(list(fp_result = fp, filename = forest_file), save_opts))
  }

  return(m)
}

# Run primary HR meta-analyses
# OS forest (Figure 4): print-width 6.75in (OUP 171mm two-column max) so 9pt base
# renders 1:1 (>= 8pt minimum); use_autofit = FALSE keeps the designed CI column
# width for balanced margins. DFS/RFS keep oversized appendix-style defaults.
meta_os  <- run_hr_meta(dat_os,  "Overall Survival",          "OS",
                        fp_opts = list(base_size = 9,
                                       blank_width = 42,
                                       core_padding = grid::unit(c(2.2, 2.0), "mm"),
                                       width = 6.75, height = 6.6),
                        save_opts = list(use_autofit = FALSE))
# DFS: k=2 produces unreliable estimates (extreme CI); report descriptively only
if (nrow(dat_dfs) >= 3) {
  meta_dfs <- run_hr_meta(dat_dfs, "Disease-Free Survival",     "DFS")
} else {
  cat("\nDFS: k=", nrow(dat_dfs), "< 3, not pooled (insufficient studies).\n")
  cat("  Individual study HRs will be reported descriptively in appendix.\n")
  meta_dfs <- NULL
}
meta_rfs <- run_hr_meta(dat_rfs, "Recurrence-Free Survival",  "RFS")

# ==============================================================================
# PART 2: Binary Outcome Meta-Analysis (OR) - Mantel-Haenszel
# ==============================================================================

run_binary_meta <- function(dat, outcome_label, filename_prefix, or_direction_label) {
  if (is.null(dat) || nrow(dat) == 0) {
    cat("Skipping", outcome_label, "- no data.\n")
    return(NULL)
  }

  cat("\n====", outcome_label, "====\n")
  cat("Studies:", nrow(dat), "\n")

  m <- metabin(
    event.e = event_e,
    n.e     = n_e,
    event.c = event_c,
    n.c     = n_c,
    data    = dat,
    studlab = Study_Label,
    sm      = "OR",
    method  = SETTINGS$or_method,
    method.tau = SETTINGS$method_tau,
    method.random.ci = SETTINGS$method_ci,
    adhoc.hakn.ci = SETTINGS$adhoc_hakn,
    prediction = SETTINGS$prediction,
    title   = paste0(outcome_label, ": EOPC vs AOPC")
  )

  cat("\n--- Summary ---\n")
  print(summary(m))

  # Forest plot — Lancet table-based layout (forestploter)
  forest_file <- file.path(PATHS$forest, paste0(filename_prefix, "_forest"))

  fp <- tryCatch(
    lancet_fp_forest(m, outcome_label = outcome_label, sort_year = dat$Year),
    error = function(e) {
      cat("  forestploter error:", e$message, " - skipping\n")
      NULL
    }
  )

  if (!is.null(fp)) {
    save_fp_figure(fp, forest_file)
  }

  return(m)
}

# Run binary meta-analyses — Demographics
meta_male        <- run_binary_meta(dat_male,        "Male Proportion",         "Male",        "OR>1: more males in EOPC")
meta_smoking     <- run_binary_meta(dat_smoking,     "Smoking History",         "Smoking",     "OR>1: more smoking in EOPC")
meta_diabetes    <- run_binary_meta(dat_diabetes,    "Diabetes History",        "Diabetes",    "OR>1: more diabetes in EOPC")
meta_family_hx   <- run_binary_meta(dat_family_hx,   "Family History",          "FamilyHx",    "OR>1: more family hx in EOPC")

# Run binary meta-analyses — Tumour characteristics
meta_stage_iv    <- run_binary_meta(dat_stage_iv,    "Stage IV Proportion",     "StageIV",     "OR>1: more Stage IV in EOPC")
meta_stage_i     <- run_binary_meta(dat_stage_i,     "Stage I Proportion",      "StageI",      "OR>1: more Stage I in EOPC")
meta_stage_ii    <- run_binary_meta(dat_stage_ii,    "Stage II Proportion",     "StageII",     "OR>1: more Stage II in EOPC")
meta_stage_iii   <- run_binary_meta(dat_stage_iii,   "Stage III Proportion",    "StageIII",    "OR>1: more Stage III in EOPC")
meta_poorly_diff <- run_binary_meta(dat_poorly_diff, "Poorly Differentiated",   "PoorlyDiff",  "OR>1: more poorly diff in EOPC")
meta_mod_diff    <- run_binary_meta(dat_mod_diff,    "Moderately Differentiated","ModDiff",     "OR>1: more mod diff in EOPC")
meta_well_diff   <- run_binary_meta(dat_well_diff,   "Well Differentiated",     "WellDiff",    "OR>1: more well diff in EOPC")
meta_ln_positive <- run_binary_meta(dat_ln_positive, "Lymph Node Positive",     "LNPositive",  "OR>1: more LN+ in EOPC")
meta_metastatic  <- run_binary_meta(dat_metastatic,  "Metastatic Disease",      "Metastatic",  "OR>1: more metastatic in EOPC")
meta_tumor_head  <- run_binary_meta(dat_tumor_head,  "Tumour Head Location",    "TumorHead",   "OR>1: more head in EOPC")
meta_tumor_body  <- run_binary_meta(dat_tumor_body,  "Tumour Body Location",    "TumorBody",   "OR>1: more body in EOPC")
meta_tumor_tail  <- run_binary_meta(dat_tumor_tail,  "Tumour Tail Location",    "TumorTail",   "OR>1: more tail in EOPC")
meta_vasc_inv    <- run_binary_meta(dat_vasc_inv,    "Vascular Invasion",       "VascInv",     "OR>1: more vasc inv in EOPC")

# Run binary meta-analyses — Treatment
meta_surgery     <- run_binary_meta(dat_surgery,     "Surgical Resection",      "Surgery",     "OR>1: more surgery in EOPC")
meta_chemo       <- run_binary_meta(dat_chemo,       "Chemotherapy",            "Chemo",       "OR>1: more chemo in EOPC")
meta_chemo_neo   <- run_binary_meta(dat_chemo_neo,   "Neoadjuvant Chemo",       "ChemoNeo",    "OR>1: more neoadj chemo in EOPC")
meta_chemo_adj   <- run_binary_meta(dat_chemo_adj,   "Adjuvant Chemo",          "ChemoAdj",    "OR>1: more adj chemo in EOPC")
meta_rt          <- run_binary_meta(dat_rt,          "Radiotherapy",            "RT",          "OR>1: more RT in EOPC")
meta_whipple     <- run_binary_meta(dat_whipple,     "Whipple Procedure",       "Whipple",     "OR>1: more Whipple in EOPC")
meta_distal      <- run_binary_meta(dat_distal,      "Distal Pancreatectomy",   "Distal",      "OR>1: more distal in EOPC")
meta_r0          <- run_binary_meta(dat_r0,          "R0 Resection",            "R0",          "OR>1: more R0 in EOPC")

# Run binary meta-analyses — Time-point overall survival
meta_os_5yr <- run_binary_meta(dat_os_5yr, "5-Year Overall Survival", "OS5yr", "OR>1: higher 5yr OS in EOPC")
meta_os_3yr <- run_binary_meta(dat_os_3yr, "3-Year Overall Survival", "OS3yr", "OR>1: higher 3yr OS in EOPC")
meta_os_1yr <- run_binary_meta(dat_os_1yr, "1-Year Overall Survival", "OS1yr", "OR>1: higher 1yr OS in EOPC")
meta_os_2yr <- run_binary_meta(dat_os_2yr, "2-Year Overall Survival", "OS2yr", "OR>1: higher 2yr OS in EOPC")

# Run binary meta-analyses — Additional Demographics (data maximization)
meta_alcohol     <- run_binary_meta(dat_alcohol,     "Alcohol History",          "Alcohol",         "OR>1: more alcohol in EOPC")

# Run binary meta-analyses — Race/Ethnicity
meta_white       <- run_binary_meta(dat_white,       "White Race",               "White",           "OR>1: more White in EOPC")
meta_black       <- run_binary_meta(dat_black,       "Black Race",               "Black",           "OR>1: more Black in EOPC")
meta_asian       <- run_binary_meta(dat_asian,       "Asian Race",               "Asian",           "OR>1: more Asian in EOPC")

# Run binary meta-analyses — Additional Tumour
meta_pni         <- run_binary_meta(dat_pni,         "Perineural Invasion",      "PNI",             "OR>1: more PNI in EOPC")

# Run binary meta-analyses — Additional Treatment
meta_no_tx       <- run_binary_meta(dat_no_tx,       "No Treatment",             "NoTx",            "OR>1: more untreated in EOPC")
meta_palliative  <- run_binary_meta(dat_palliative,  "Palliative Chemotherapy",  "PalliativeChemo", "OR>1: more palliative in EOPC")
meta_total_panc  <- run_binary_meta(dat_total_panc,  "Total Pancreatectomy",     "TotalPanc",       "OR>1: more total panc in EOPC")

# ==============================================================================
# PART 3: Save All Results
# ==============================================================================

save(meta_os, meta_dfs, meta_rfs,
     # Demographics
     meta_male, meta_smoking, meta_diabetes, meta_family_hx,
     # Tumour characteristics
     meta_stage_iv, meta_stage_i, meta_stage_ii, meta_stage_iii,
     meta_poorly_diff, meta_mod_diff, meta_well_diff,
     meta_ln_positive, meta_metastatic,
     meta_tumor_head, meta_tumor_body, meta_tumor_tail, meta_vasc_inv,
     # Treatment
     meta_surgery, meta_chemo, meta_chemo_neo, meta_chemo_adj,
     meta_rt, meta_whipple, meta_distal, meta_r0,
     # Time-point survival
     meta_os_5yr, meta_os_3yr, meta_os_2yr, meta_os_1yr,
     # Additional demographics (data maximization)
     meta_alcohol, meta_white, meta_black, meta_asian,
     # Additional tumour
     meta_pni,
     # Additional treatment
     meta_no_tx, meta_palliative, meta_total_panc,
     file = file.path(PATHS$tables, "meta_results.RData"))

# -- Summary helper --
.pk <- function(m) if (!is.null(m)) m$k else 0

cat("\n\n========================================\n")
cat("All primary meta-analyses complete.\n")
cat("HR outcomes: OS(", .pk(meta_os),
    "), DFS(", .pk(meta_dfs),
    "), RFS(", .pk(meta_rfs), ")\n")
cat("OR — Demographics: Male(", .pk(meta_male),
    "), Smoking(", .pk(meta_smoking),
    "), Diabetes(", .pk(meta_diabetes),
    "), FamilyHx(", .pk(meta_family_hx),
    "), Alcohol(", .pk(meta_alcohol), ")\n")
cat("OR — Race: White(", .pk(meta_white),
    "), Black(", .pk(meta_black), ")\n")
cat("OR — Tumour: StageI(", .pk(meta_stage_i),
    "), StageII(", .pk(meta_stage_ii),
    "), StageIII(", .pk(meta_stage_iii),
    "), StageIV(", .pk(meta_stage_iv),
    "), PoorlyDiff(", .pk(meta_poorly_diff),
    "), ModDiff(", .pk(meta_mod_diff),
    "), WellDiff(", .pk(meta_well_diff), ")\n")
cat("         LN+(", .pk(meta_ln_positive),
    "), Mets(", .pk(meta_metastatic),
    "), Head(", .pk(meta_tumor_head),
    "), Body(", .pk(meta_tumor_body),
    "), Tail(", .pk(meta_tumor_tail),
    "), VascInv(", .pk(meta_vasc_inv),
    "), PNI(", .pk(meta_pni), ")\n")
cat("OR — Treatment: Surgery(", .pk(meta_surgery),
    "), Chemo(", .pk(meta_chemo),
    "), ChemoNeo(", .pk(meta_chemo_neo),
    "), ChemoAdj(", .pk(meta_chemo_adj),
    "), RT(", .pk(meta_rt),
    "), Whipple(", .pk(meta_whipple),
    "), Distal(", .pk(meta_distal),
    "), R0(", .pk(meta_r0), ")\n")
cat("         NoTx(", .pk(meta_no_tx),
    "), Palliative(", .pk(meta_palliative),
    "), TotalPanc(", .pk(meta_total_panc), ")\n")
cat("OR — Time-point OS: 5yr(", .pk(meta_os_5yr),
    "), 3yr(", .pk(meta_os_3yr),
    "), 1yr(", .pk(meta_os_1yr), ")\n")
cat("========================================\n")
