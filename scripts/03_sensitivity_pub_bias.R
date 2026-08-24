################################################################################
# EOPC vs AOPC Systematic Review and Meta-Analysis
# Script 03: Sensitivity Analyses + Publication Bias
# Input:  meta_results.RData, prepared_data.RData
# Output: Sensitivity summary table, funnel plots, trim-fill plots
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
source(file.path(.get_script_dir(), "00_config.R"))

load(file.path(PATHS$tables, "meta_results.RData"))
load(file.path(PATHS$stat_data, "prepared_data.RData"))

# ==============================================================================
# PART 1: SENSITIVITY ANALYSES (S1-S8 for OS)
# ==============================================================================

run_os_sensitivity <- function() {
  if (is.null(meta_os) || meta_os$k < 3) {
    cat("OS meta-analysis has <3 studies. Skipping sensitivity analyses.\n")
    return(NULL)
  }

  results <- list()

  # Helper: extract key stats from a meta object
  extract_stats <- function(m, label) {
    data.frame(
      Analysis = label,
      k = m$k,
      HR = sprintf("%.2f", exp(m$TE.random)),
      CI_95 = sprintf("[%.2f, %.2f]", exp(m$lower.random), exp(m$upper.random)),
      P = format_pval(m$pval.random),
      I2 = sprintf("%.1f%%", m$I2 * 100),
      tau2 = sprintf("%.4f", m$tau2),
      stringsAsFactors = FALSE
    )
  }

  # Primary analysis (reference)
  results[["Primary"]] <- extract_stats(meta_os, "Primary (REML + HK)")

  # S1: Exclude NOS < 7
  s1_data <- dat_os %>% filter(Total_Stars >= 7)
  if (nrow(s1_data) >= 2) {
    s1 <- metagen(TE = logHR, seTE = seLogHR, data = s1_data, studlab = Study_Label,
                  sm = "HR", method.tau = "REML", method.random.ci = "HK",
                  adhoc.hakn.ci = SETTINGS$adhoc_hakn)
    results[["S1"]] <- extract_stats(s1, "S1: Exclude NOS < 7")
  }

  # S2: Only age threshold = 50
  s2_data <- dat_os %>% filter(Age_Threshold_Num == 50)
  if (nrow(s2_data) >= 2) {
    s2 <- metagen(TE = logHR, seTE = seLogHR, data = s2_data, studlab = Study_Label,
                  sm = "HR", method.tau = "REML", method.random.ci = "HK",
                  adhoc.hakn.ci = SETTINGS$adhoc_hakn)
    results[["S2"]] <- extract_stats(s2, "S2: Age threshold = 50 only")
  }

  # S3: Only multivariable-adjusted HR
  s3_data <- dat_os %>% filter(HR_Source_Group == "Multivariable")
  if (nrow(s3_data) >= 2) {
    s3 <- metagen(TE = logHR, seTE = seLogHR, data = s3_data, studlab = Study_Label,
                  sm = "HR", method.tau = "REML", method.random.ci = "HK",
                  adhoc.hakn.ci = SETTINGS$adhoc_hakn)
    results[["S3"]] <- extract_stats(s3, "S3: Multivariable-adjusted HR only")
  }

  # S4: SEER/NCDB overlap — keep only one study per cluster (largest N)
  if ("Database_Cluster" %in% names(dat_os)) {
    seer_ncdb <- dat_os %>% filter(Database_Cluster %in% c("SEER", "NCDB"))
    if (nrow(seer_ncdb) >= 2) {
      # Keep the largest study per cluster, drop the rest
      keep_ids <- seer_ncdb %>%
        group_by(Database_Cluster) %>%
        slice_max(Total_N_num, n = 1, with_ties = FALSE) %>%
        pull(Study_Label)
      drop_ids <- setdiff(seer_ncdb$Study_Label, keep_ids)
      s4_data <- dat_os %>% filter(!Study_Label %in% drop_ids)
      if (nrow(s4_data) >= 2) {
        s4 <- metagen(TE = logHR, seTE = seLogHR, data = s4_data, studlab = Study_Label,
                      sm = "HR", method.tau = "REML", method.random.ci = "HK",
                      adhoc.hakn.ci = SETTINGS$adhoc_hakn)
        results[["S4"]] <- extract_stats(s4,
          paste0("S4: SEER/NCDB overlap resolved (kept ", length(keep_ids),
                 ", dropped ", length(drop_ids), ")"))
      }
    }
  }

  # S5: Fixed-effect model
  s5 <- update(meta_os, random = FALSE, common = TRUE)
  results[["S5"]] <- data.frame(
    Analysis = "S5: Fixed-effect (IV)",
    k = s5$k,
    HR = sprintf("%.2f", exp(s5$TE.common)),
    CI_95 = sprintf("[%.2f, %.2f]", exp(s5$lower.common), exp(s5$upper.common)),
    P = format_pval(s5$pval.common),
    I2 = sprintf("%.1f%%", s5$I2 * 100),
    tau2 = "N/A",
    stringsAsFactors = FALSE
  )

  # S6: Exclude largest database study
  largest_idx <- which.max(dat_os$Total_N_num)
  if (length(largest_idx) > 0 && meta_os$k > 2) {
    s6_data <- dat_os[-largest_idx, ]
    s6 <- metagen(TE = logHR, seTE = seLogHR, data = s6_data, studlab = Study_Label,
                  sm = "HR", method.tau = "REML", method.random.ci = "HK",
                  adhoc.hakn.ci = SETTINGS$adhoc_hakn)
    results[["S6"]] <- extract_stats(s6,
      paste0("S6: Exclude largest (", dat_os$Study_Label[largest_idx], ")"))
  }

  # S7: DerSimonian-Laird estimator
  s7 <- update(meta_os, method.tau = "DL")
  results[["S7"]] <- extract_stats(s7, "S7: DerSimonian-Laird tau^2")

  # S8: Exclude ROBINS-E Serious
  s8_data <- dat_os %>% filter(ROBINS_E_Group == "Low/Moderate")
  if (nrow(s8_data) >= 2) {
    s8 <- metagen(TE = logHR, seTE = seLogHR, data = s8_data, studlab = Study_Label,
                  sm = "HR", method.tau = "REML", method.random.ci = "HK",
                  adhoc.hakn.ci = SETTINGS$adhoc_hakn)
    results[["S8"]] <- extract_stats(s8, "S8: Exclude ROBINS-E Serious")
  }

  # Combine results
  sens_table <- bind_rows(results)
  write_xlsx(sens_table, file.path(PATHS$tables, "Sensitivity_OS.xlsx"))
  cat("\nOS Sensitivity Analysis Summary:\n")
  print(sens_table)

  return(sens_table)
}

sens_os <- run_os_sensitivity()

# ==============================================================================
# PART 2: PUBLICATION BIAS
# ==============================================================================

run_pub_bias <- function(meta_obj, dat, outcome_label, prefix) {
  if (is.null(meta_obj) || meta_obj$k < 3) {
    cat("Skipping publication bias for", outcome_label, "- need >= 3 studies.\n")
    return(NULL)
  }

  cat("\n==== Publication Bias:", outcome_label, "(k=", meta_obj$k, ") ====\n")

  # Pre-compute Egger's and Begg's p-values for annotation
  egger_p <- NULL
  begg_p  <- NULL
  if (meta_obj$k >= 10) {
    egger <- tryCatch(metabias(meta_obj, method.bias = "Egger", plotit = FALSE),
                      error = function(e) NULL)
    if (!is.null(egger)) egger_p <- egger$p.value
    begg <- tryCatch(metabias(meta_obj, method.bias = "Begg", plotit = FALSE),
                     error = function(e) NULL)
    if (!is.null(begg)) begg_p <- begg$p.value
  }

  # Standard funnel plot — ggplot2 with non-overlapping labels
  funnel_file <- file.path(PATHS$funnel, paste0(prefix, "_funnel"))

  p_funnel <- lancet_gg_funnel(meta_obj, outcome_label = outcome_label,
                                show_labels = TRUE,
                                egger_p = egger_p, begg_p = begg_p)
  save_figure_gg(p_funnel, funnel_file, width = 8, height = 7)

  # Contour-enhanced funnel plot
  contour_file <- file.path(PATHS$funnel, paste0(prefix, "_contour_funnel"))
  p_contour <- lancet_gg_contour_funnel(meta_obj, outcome_label = outcome_label)
  save_figure_gg(p_contour, contour_file, width = 8, height = 7)

  # Egger's test (already pre-computed above for funnel annotations)
  if (meta_obj$k >= 10) {
    cat("\n--- Egger's Regression Test ---\n")
    if (!is.null(egger)) {
      cat(sprintf("  Egger's test: bias=%.3f, p=%s\n",
                  egger$estimate[1], format_pval(egger$p.value)))
    }
  } else {
    cat("\n--- Egger's test: not performed (k=", meta_obj$k, "< 10, insufficient power) ---\n")
  }

  # Begg's test (already pre-computed above)
  if (meta_obj$k >= 10) {
    cat("\n--- Begg's Rank Correlation Test ---\n")
    if (!is.null(begg)) {
      cat(sprintf("  Begg's test: p=%s\n", format_pval(begg$p.value)))
    }
  }

  # Peters' test (for binary outcomes, k >= 10)
  if (meta_obj$k >= 10 && meta_obj$sm == "OR") {
    cat("\n--- Peters' Test (for binary outcomes) ---\n")
    peters <- tryCatch({
      metabias(meta_obj, method.bias = "Peters", plotit = FALSE)
    }, error = function(e) {
      cat("  Peters' test error:", e$message, "\n")
      NULL
    })
    if (!is.null(peters)) {
      cat(sprintf("  Peters' test: p=%s\n", format_pval(peters$p.value)))
    }
  }

  # Trim-and-fill
  if (meta_obj$k >= 3 && SETTINGS$trim_fill) {
    cat("\n--- Trim and Fill ---\n")
    tf <- tryCatch(trimfill(meta_obj), error = function(e) {
      cat("  Trim-fill error:", e$message, "\n")
      NULL
    })
    if (!is.null(tf)) {
      cat(sprintf("  Imputed studies: %d\n", tf$k0))
      cat(sprintf("  Adjusted estimate: %.2f [%.2f, %.2f]\n",
                  exp(tf$TE.random), exp(tf$lower.random), exp(tf$upper.random)))

      tf_file <- file.path(PATHS$funnel, paste0(prefix, "_trimfill"))
      p_tf <- lancet_gg_funnel(tf,
                outcome_label = paste0(outcome_label, " (Trim & Fill)"),
                show_labels = FALSE)
      save_figure_gg(p_tf, tf_file, width = 8, height = 7)
    }
  }

  # Fail-safe N (dual method: Rosenthal for legacy, Orwin for realistic estimate)
  # NOTE: fsn() requires an rma model object, not raw vectors
  if (meta_obj$k >= 10) {
    cat("\n--- Fail-safe N ---\n")
    rma_temp <- tryCatch(
      rma(yi = meta_obj$TE, sei = meta_obj$seTE, method = "REML"),
      error = function(e) NULL)

    if (!is.null(rma_temp)) {
      fsn_rosen <- tryCatch({
        fsn(x = rma_temp, type = "Rosenthal")
      }, error = function(e) NULL)
      if (!is.null(fsn_rosen)) {
        cat("  Rosenthal's N:", fsn_rosen$fsnum, "\n")
      }

      fsn_orwin <- tryCatch({
        fsn(x = rma_temp, type = "Orwin", target = log(1.0))
      }, error = function(e) NULL)
      if (!is.null(fsn_orwin)) {
        cat("  Orwin's N (target HR=1.0):", fsn_orwin$fsnum, "\n")
      }
    }
  }
}

# Run publication bias for OS (primary)
run_pub_bias(meta_os, dat_os, "Overall Survival", "OS")

# Run for binary outcomes with sufficient studies (k >= 10 for Egger's test)
binary_metas <- list(
  # Demographics
  list(m = meta_male,        d = dat_male,        label = "Male Proportion",        prefix = "Male"),
  list(m = meta_smoking,     d = dat_smoking,     label = "Smoking History",         prefix = "Smoking"),
  list(m = meta_diabetes,    d = dat_diabetes,    label = "Diabetes History",        prefix = "Diabetes"),
  list(m = meta_family_hx,   d = dat_family_hx,   label = "Family History",          prefix = "FamilyHx"),
  # Tumour characteristics
  list(m = meta_stage_iv,    d = dat_stage_iv,    label = "Stage IV",                prefix = "StageIV"),
  list(m = meta_stage_i,     d = dat_stage_i,     label = "Stage I",                 prefix = "StageI"),
  list(m = meta_stage_ii,    d = dat_stage_ii,    label = "Stage II",                prefix = "StageII"),
  list(m = meta_stage_iii,   d = dat_stage_iii,   label = "Stage III",               prefix = "StageIII"),
  list(m = meta_poorly_diff, d = dat_poorly_diff, label = "Poorly Differentiated",   prefix = "PoorlyDiff"),
  list(m = meta_mod_diff,    d = dat_mod_diff,    label = "Moderately Differentiated",prefix = "ModDiff"),
  list(m = meta_well_diff,   d = dat_well_diff,   label = "Well Differentiated",     prefix = "WellDiff"),
  list(m = meta_ln_positive, d = dat_ln_positive, label = "Lymph Node Positive",     prefix = "LNPositive"),
  list(m = meta_metastatic,  d = dat_metastatic,  label = "Metastatic Disease",      prefix = "Metastatic"),
  list(m = meta_tumor_head,  d = dat_tumor_head,  label = "Tumour Head",             prefix = "TumorHead"),
  list(m = meta_tumor_body,  d = dat_tumor_body,  label = "Tumour Body",             prefix = "TumorBody"),
  list(m = meta_tumor_tail,  d = dat_tumor_tail,  label = "Tumour Tail",             prefix = "TumorTail"),
  list(m = meta_vasc_inv,    d = dat_vasc_inv,    label = "Vascular Invasion",       prefix = "VascInv"),
  # Treatment
  list(m = meta_surgery,     d = dat_surgery,     label = "Surgical Resection",      prefix = "Surgery"),
  list(m = meta_chemo,       d = dat_chemo,       label = "Chemotherapy",            prefix = "Chemo"),
  list(m = meta_chemo_neo,   d = dat_chemo_neo,   label = "Neoadjuvant Chemo",       prefix = "ChemoNeo"),
  list(m = meta_chemo_adj,   d = dat_chemo_adj,   label = "Adjuvant Chemo",          prefix = "ChemoAdj"),
  list(m = meta_rt,          d = dat_rt,          label = "Radiotherapy",             prefix = "RT"),
  list(m = meta_whipple,     d = dat_whipple,     label = "Whipple Procedure",       prefix = "Whipple"),
  list(m = meta_distal,      d = dat_distal,      label = "Distal Pancreatectomy",   prefix = "Distal"),
  list(m = meta_r0,          d = dat_r0,          label = "R0 Resection",            prefix = "R0"),
  # Time-point survival
  list(m = meta_os_5yr,      d = dat_os_5yr,      label = "5-Year OS",               prefix = "OS5yr"),
  list(m = meta_os_3yr,      d = dat_os_3yr,      label = "3-Year OS",               prefix = "OS3yr"),
  list(m = meta_os_1yr,      d = dat_os_1yr,      label = "1-Year OS",               prefix = "OS1yr"),
  list(m = meta_os_2yr,      d = dat_os_2yr,      label = "2-Year OS",               prefix = "OS2yr"),
  list(m = meta_os_2yr,      d = dat_os_2yr,      label = "2-Year OS",               prefix = "OS2yr"),
  # Additional demographics (data maximization)
  list(m = meta_alcohol,     d = dat_alcohol,     label = "Alcohol History",          prefix = "Alcohol"),
  list(m = meta_white,       d = dat_white,       label = "White Race",               prefix = "White"),
  list(m = meta_black,       d = dat_black,       label = "Black Race",               prefix = "Black"),
  list(m = meta_asian,       d = dat_asian,       label = "Asian Race",               prefix = "Asian"),
  # Additional tumour
  list(m = meta_pni,         d = dat_pni,         label = "Perineural Invasion",      prefix = "PNI"),
  # Additional treatment
  list(m = meta_no_tx,       d = dat_no_tx,       label = "No Treatment",             prefix = "NoTx"),
  list(m = meta_palliative,  d = dat_palliative,  label = "Palliative Chemo",         prefix = "PalliativeChemo"),
  list(m = meta_total_panc,  d = dat_total_panc,  label = "Total Pancreatectomy",     prefix = "TotalPanc")
)

for (item in binary_metas) {
  if (!is.null(item$m) && item$m$k >= 10) {
    run_pub_bias(item$m, item$d, item$label, item$prefix)
  }
}

# ==============================================================================
# PART 3: SEER/NCDB OVERLAP SENSITIVITY — BINARY OUTCOMES
# ==============================================================================

run_seer_ncdb_sensitivity_binary <- function(binary_metas_list) {
  cat("\n\n==== SEER/NCDB Overlap Sensitivity — Binary Outcomes ====\n")

  results <- list()

  for (item in binary_metas_list) {
    m <- item$m
    d <- item$d
    if (is.null(m) || m$k < 5 || !"Database_Cluster" %in% names(d)) next

    seer_ncdb <- d %>% filter(Database_Cluster %in% c("SEER", "NCDB"))
    if (nrow(seer_ncdb) < 2) next

    # Keep the largest study per cluster, drop the rest
    keep_ids <- seer_ncdb %>%
      group_by(Database_Cluster) %>%
      slice_max(Total_N_num, n = 1, with_ties = FALSE) %>%
      pull(Study_Label)
    drop_ids <- setdiff(seer_ncdb$Study_Label, keep_ids)
    if (length(drop_ids) == 0) next

    s_data <- d %>% filter(!Study_Label %in% drop_ids)
    if (nrow(s_data) < 2) next

    s_meta <- tryCatch({
      metabin(event.e = event_e, n.e = n_e, event.c = event_c, n.c = n_c,
              data = s_data, studlab = Study_Label, sm = "OR",
              method = SETTINGS$or_method, method.tau = SETTINGS$method_tau,
              method.random.ci = SETTINGS$method_ci,
              adhoc.hakn.ci = SETTINGS$adhoc_hakn,
              prediction = SETTINGS$prediction)
    }, error = function(e) NULL)

    if (!is.null(s_meta)) {
      results[[item$prefix]] <- data.frame(
        Outcome = item$label,
        k_primary = m$k,
        OR_primary = sprintf("%.2f [%.2f, %.2f]", exp(m$TE.random),
                             exp(m$lower.random), exp(m$upper.random)),
        k_sensitivity = s_meta$k,
        OR_sensitivity = sprintf("%.2f [%.2f, %.2f]", exp(s_meta$TE.random),
                                 exp(s_meta$lower.random), exp(s_meta$upper.random)),
        Dropped = length(drop_ids),
        stringsAsFactors = FALSE
      )
      cat(sprintf("  %s: k=%d→%d, OR %.2f→%.2f (dropped %d)\n",
                  item$label, m$k, s_meta$k,
                  exp(m$TE.random), exp(s_meta$TE.random), length(drop_ids)))
    }
  }

  if (length(results) > 0) {
    sens_binary <- bind_rows(results)
    write_xlsx(sens_binary, file.path(PATHS$tables, "Sensitivity_SEER_NCDB_Binary.xlsx"))
    cat("\nSEER/NCDB binary sensitivity table saved.\n")
    print(sens_binary)
    return(sens_binary)
  } else {
    cat("No binary outcomes had sufficient SEER/NCDB overlap for sensitivity analysis.\n")
    return(NULL)
  }
}

sens_seer_binary <- run_seer_ncdb_sensitivity_binary(binary_metas)

# ==============================================================================
# NEW: Non-overlapping patient total (R3-1) + Table S5 (renumbered from S15, 2026-08-25)
# ==============================================================================

cat("\n\n==== Non-overlapping Patient Count (R3-1) ===\n")

# 1. Overall non-overlapping N: resolve overlap at study level
# Keep largest study per SEER cluster, largest per NCDB cluster
overlap_studies <- master %>% filter(Database_Cluster %in% c("SEER", "NCDB"))
if (nrow(overlap_studies) > 0) {
  keep_study_ids <- overlap_studies %>%
    group_by(Database_Cluster) %>%
    slice_max(Total_N_num, n = 1, with_ties = FALSE) %>%
    pull(Study_ID)
  drop_study_ids <- setdiff(overlap_studies$Study_ID, keep_study_ids)

  N_overlap <- sum(master$Total_N_num, na.rm = TRUE)
  N_nonoverlap <- sum(master$Total_N_num[!master$Study_ID %in% drop_study_ids], na.rm = TRUE)

  cat(sprintf("  Overlapping total N: %s\n", format(N_overlap, scientific = FALSE)))
  cat(sprintf("  Non-overlapping estimate: %s (dropped %d overlapping studies)\n",
              format(N_nonoverlap, scientific = FALSE), length(drop_study_ids)))
  cat(sprintf("  Difference: %s patients potentially double-counted\n",
              format(N_overlap - N_nonoverlap, scientific = FALSE)))

  # 2. Per-outcome overlap resolution table (Table S5, previously S15)
  outcome_overlap <- list()

  # OS outcome
  os_overlap <- dat_os %>% filter(Database_Cluster %in% c("SEER", "NCDB"))
  if (nrow(os_overlap) >= 2) {
    os_keep <- os_overlap %>%
      group_by(Database_Cluster) %>%
      slice_max(Total_N_num, n = 1, with_ties = FALSE) %>% pull(Study_Label)
    os_drop <- setdiff(os_overlap$Study_Label, os_keep)
    outcome_overlap[["OS"]] <- data.frame(
      Outcome = "Overall Survival", k_primary = nrow(dat_os),
      N_primary = sum(dat_os$Total_N_num, na.rm = TRUE),
      k_resolved = nrow(dat_os) - length(os_drop),
      N_resolved = sum(dat_os$Total_N_num[!dat_os$Study_Label %in% os_drop], na.rm = TRUE),
      Dropped_studies = length(os_drop),
      stringsAsFactors = FALSE
    )
  } else {
    outcome_overlap[["OS"]] <- data.frame(
      Outcome = "Overall Survival", k_primary = nrow(dat_os),
      N_primary = sum(dat_os$Total_N_num, na.rm = TRUE),
      k_resolved = nrow(dat_os), N_resolved = N_primary,
      Dropped_studies = 0, stringsAsFactors = FALSE
    )
  }

  # Binary outcomes
  for (item in binary_metas) {
    d <- item$d
    m <- item$m
    if (is.null(m)) continue
    bin_overlap <- d %>% filter(Database_Cluster %in% c("SEER", "NCDB"))
    if (nrow(bin_overlap) >= 2) {
      bin_keep <- bin_overlap %>%
        group_by(Database_Cluster) %>%
        slice_max(Total_N_num, n = 1, with_ties = FALSE) %>% pull(Study_Label)
      bin_drop <- setdiff(bin_overlap$Study_Label, bin_keep)
      outcome_overlap[[item$prefix]] <- data.frame(
        Outcome = item$label, k_primary = m$k,
        N_primary = sum(d$Total_N_num, na.rm = TRUE),
        k_resolved = m$k - length(bin_drop),
        N_resolved = sum(d$Total_N_num[!d$Study_Label %in% bin_drop], na.rm = TRUE),
        Dropped_studies = length(bin_drop),
        stringsAsFactors = FALSE
      )
    } else {
      outcome_overlap[[item$prefix]] <- data.frame(
        Outcome = item$label, k_primary = m$k,
        N_primary = sum(d$Total_N_num, na.rm = TRUE),
        k_resolved = m$k, N_resolved = sum(d$Total_N_num, na.rm = TRUE),
        Dropped_studies = 0, stringsAsFactors = FALSE
      )
    }
  }

  table_s15 <- bind_rows(outcome_overlap)
  write_xlsx(table_s15, file.path(PATHS$tables, "Table_S05_Overlap_Resolution.xlsx"))
  cat("\nTable S5 (Overlap resolution per outcome; previously S15) saved.\n")
  print(table_s15)
}

# ==============================================================================
# NEW: HR source stratification table (R3-6) + Tables S3a-S3b (renumbered from S16a/b, 2026-08-25)
# ==============================================================================

cat("\n\n==== HR Source Stratification (R3-6) ===\n")

if (!is.null(meta_os) && "HR_Source_Group" %in% names(dat_os)) {
  # Multivariable-adjusted only
  adj_data <- dat_os %>% filter(HR_Source_Group == "Multivariable")
  if (nrow(adj_data) >= 2) {
    meta_adj <- metagen(TE = logHR, seTE = seLogHR, data = adj_data,
                        studlab = Study_Label, sm = "HR",
                        method.tau = "REML", method.random.ci = "HK",
                        adhoc.hakn.ci = SETTINGS$adhoc_hakn,
                        prediction = SETTINGS$prediction)
    cat(sprintf("  Adjusted HR only: k=%d, HR=%.2f [%s]\n",
                meta_adj$k, exp(meta_adj$TE.random),
                sprintf("%.2f, %.2f", exp(meta_adj$lower.random), exp(meta_adj$upper.random))))
  }

  # Univariable only
  unadj_data <- dat_os %>% filter(HR_Source_Group == "Univariable")
  if (nrow(unadj_data) >= 2) {
    meta_unadj <- metagen(TE = logHR, seTE = seLogHR, data = unadj_data,
                          studlab = Study_Label, sm = "HR",
                          method.tau = "REML", method.random.ci = "HK",
                          adhoc.hakn.ci = SETTINGS$adhoc_hakn,
                          prediction = SETTINGS$prediction)
    cat(sprintf("  Unadjusted HR only: k=%d, HR=%.2f [%s]\n",
                meta_unadj$k, exp(meta_unadj$TE.random),
                sprintf("%.2f, %.2f", exp(meta_unadj$lower.random), exp(meta_unadj$upper.random))))
  }

  # IPD only
  ipd_data <- dat_os %>% filter(HR_Source_Group == "IPD")
  if (nrow(ipd_data) >= 1) {
    if (nrow(ipd_data) >= 2) {
      meta_ipd <- metagen(TE = logHR, seTE = seLogHR, data = ipd_data,
                          studlab = Study_Label, sm = "HR",
                          method.tau = "REML", method.random.ci = "HK",
                          adhoc.hakn.ci = SETTINGS$adhoc_hakn,
                          prediction = SETTINGS$prediction)
      cat(sprintf("  IPD-reconstructed HR only: k=%d, HR=%.2f [%s]\n",
                  meta_ipd$k, exp(meta_ipd$TE.random),
                  sprintf("%.2f, %.2f", exp(meta_ipd$lower.random), exp(meta_ipd$upper.random))))
    } else {
      # Single IPD study: report study-level estimate
      meta_ipd <- list(
        k = 1,
        TE.random = ipd_data$logHR[1],
        lower.random = ipd_data$logHR[1] - 1.96 * ipd_data$seLogHR[1],
        upper.random = ipd_data$logHR[1] + 1.96 * ipd_data$seLogHR[1],
        pval.random = 2 * (1 - pnorm(abs(ipd_data$logHR[1] / ipd_data$seLogHR[1]))),
        I2 = 0,
        lower.predict = NA,
        upper.predict = NA
      )
      cat(sprintf("  IPD-reconstructed HR: single study (k=1), HR=%.2f [%.2f, %.2f]\n",
                  exp(meta_ipd$TE.random), exp(meta_ipd$lower.random), exp(meta_ipd$upper.random)))
    }
  }

  # Build Table S3a (HR source stratification, previously S16a)
  table_s16 <- data.frame(
    Stratum = c("All (primary)"),
    k = c(meta_os$k),
    HR = c(sprintf("%.2f", exp(meta_os$TE.random))),
    CI = c(sprintf("%.2f to %.2f", exp(meta_os$lower.random), exp(meta_os$upper.random))),
    P = c(format_pval(meta_os$pval.random)),
    I2 = c(sprintf("%.1f%%", meta_os$I2 * 100)),
    PI = c(if(!is.null(meta_os$lower.predict))
           sprintf("%.2f to %.2f", exp(meta_os$lower.predict), exp(meta_os$upper.predict)) else "--"),
    stringsAsFactors = FALSE
  )

  if (exists("meta_adj")) {
    table_s16 <- rbind(table_s16, data.frame(
      Stratum = "Multivariable-adjusted",
      k = meta_adj$k,
      HR = sprintf("%.2f", exp(meta_adj$TE.random)),
      CI = sprintf("%.2f to %.2f", exp(meta_adj$lower.random), exp(meta_adj$upper.random)),
      P = format_pval(meta_adj$pval.random),
      I2 = sprintf("%.1f%%", meta_adj$I2 * 100),
      PI = if(!is.null(meta_adj$lower.predict))
           sprintf("%.2f to %.2f", exp(meta_adj$lower.predict), exp(meta_adj$upper.predict)) else "--",
      stringsAsFactors = FALSE
    ))
  }
  if (exists("meta_unadj")) {
    table_s16 <- rbind(table_s16, data.frame(
      Stratum = "Univariable",
      k = meta_unadj$k,
      HR = sprintf("%.2f", exp(meta_unadj$TE.random)),
      CI = sprintf("%.2f to %.2f", exp(meta_unadj$lower.random), exp(meta_unadj$upper.random)),
      P = format_pval(meta_unadj$pval.random),
      I2 = sprintf("%.1f%%", meta_unadj$I2 * 100),
      PI = if(!is.null(meta_unadj$lower.predict))
           sprintf("%.2f to %.2f", exp(meta_unadj$lower.predict), exp(meta_unadj$upper.predict)) else "--",
      stringsAsFactors = FALSE
    ))
  }
  if (exists("meta_ipd")) {
    table_s16 <- rbind(table_s16, data.frame(
      Stratum = "IPD-reconstructed",
      k = meta_ipd$k,
      HR = sprintf("%.2f", exp(meta_ipd$TE.random)),
      CI = sprintf("%.2f to %.2f", exp(meta_ipd$lower.random), exp(meta_ipd$upper.random)),
      P = format_pval(meta_ipd$pval.random),
      I2 = sprintf("%.1f%%", meta_ipd$I2 * 100),
      PI = if(!is.null(meta_ipd$lower.predict))
           sprintf("%.2f to %.2f", exp(meta_ipd$lower.predict), exp(meta_ipd$upper.predict)) else "--",
      stringsAsFactors = FALSE
    ))
  }

  # Add per-study HR source listing
  hr_source_detail <- dat_os %>%
    dplyr::select(Study_Label, hr_val, lci_val, uci_val, HR_Source_Group, Total_N_num) %>%
    mutate(
      HR_display = sprintf("%.2f (%.2f to %.2f)", hr_val, lci_val, uci_val),
      N_display = format(Total_N_num, scientific = FALSE)
    ) %>%
    dplyr::select(Study_Label, HR_display, HR_Source_Group, N_display)

  write_xlsx(table_s16, file.path(PATHS$tables, "Table_S03a_HR_Source_Stratification.xlsx"))
  write_xlsx(hr_source_detail, file.path(PATHS$tables, "Table_S03b_HR_Source_Detail.xlsx"))
  cat("\nTable S3a/S3b (HR source stratification; previously S16a/b) saved.\n")
  print(table_s16)
}

cat("\n\nSensitivity and publication bias analyses complete.\n")
