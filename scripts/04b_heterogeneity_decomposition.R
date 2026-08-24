################################################################################
# EOPC vs AOPC Systematic Review and Meta-Analysis
# Script 04b: Heterogeneity Decomposition & Extended Meta-Regression
# Purpose: Problem 3C (binary meta-regression with Region) +
#          Problem 3D (tau-squared decomposition analysis)
# Input:  meta_results.RData, prepared_data.RData
# Output: Decomposition tables, extended meta-regression, summary for manuscript
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

cat("\n========================================\n")
cat("04b: Heterogeneity Decomposition Analysis\n")
cat("========================================\n")

# ==============================================================================
# PART 1: Extended meta-regression with Geographic Region for binary outcomes
# ==============================================================================

# Define all binary outcomes (same as 04, but we process ALL with k>=5)
binary_outcomes <- list(
  list(m = meta_male,        d = dat_male,        label = "Male Proportion",         prefix = "Male"),
  list(m = meta_smoking,     d = dat_smoking,     label = "Smoking History",          prefix = "Smoking"),
  list(m = meta_diabetes,    d = dat_diabetes,    label = "Diabetes History",         prefix = "Diabetes"),
  list(m = meta_family_hx,   d = dat_family_hx,   label = "Family History",           prefix = "FamilyHx"),
  list(m = meta_alcohol,     d = dat_alcohol,     label = "Alcohol History",           prefix = "Alcohol"),
  list(m = meta_white,       d = dat_white,       label = "White Race",                prefix = "White"),
  list(m = meta_black,       d = dat_black,       label = "Black Race",                prefix = "Black"),
  list(m = meta_asian,       d = dat_asian,       label = "Asian Race",                prefix = "Asian"),
  list(m = meta_stage_iv,    d = dat_stage_iv,    label = "Stage IV",                 prefix = "StageIV"),
  list(m = meta_stage_i,     d = dat_stage_i,     label = "Stage I",                  prefix = "StageI"),
  list(m = meta_stage_ii,    d = dat_stage_ii,    label = "Stage II",                 prefix = "StageII"),
  list(m = meta_stage_iii,   d = dat_stage_iii,   label = "Stage III",                prefix = "StageIII"),
  list(m = meta_poorly_diff, d = dat_poorly_diff, label = "Poorly Differentiated",    prefix = "PoorlyDiff"),
  list(m = meta_mod_diff,    d = dat_mod_diff,    label = "Moderately Differentiated", prefix = "ModDiff"),
  list(m = meta_well_diff,   d = dat_well_diff,   label = "Well Differentiated",      prefix = "WellDiff"),
  list(m = meta_ln_positive, d = dat_ln_positive, label = "Lymph Node Positive",      prefix = "LNPositive"),
  list(m = meta_metastatic,  d = dat_metastatic,  label = "Metastatic Disease",       prefix = "Metastatic"),
  list(m = meta_tumor_head,  d = dat_tumor_head,  label = "Tumour Head",              prefix = "TumorHead"),
  list(m = meta_tumor_body,  d = dat_tumor_body,  label = "Tumour Body",              prefix = "TumorBody"),
  list(m = meta_tumor_tail,  d = dat_tumor_tail,  label = "Tumour Tail",              prefix = "TumorTail"),
  list(m = meta_vasc_inv,    d = dat_vasc_inv,    label = "Vascular Invasion",        prefix = "VascInv"),
  list(m = meta_pni,         d = dat_pni,         label = "Perineural Invasion",      prefix = "PNI"),
  list(m = meta_surgery,     d = dat_surgery,     label = "Surgical Resection",       prefix = "Surgery"),
  list(m = meta_chemo,       d = dat_chemo,       label = "Chemotherapy",             prefix = "Chemo"),
  list(m = meta_chemo_neo,   d = dat_chemo_neo,   label = "Neoadjuvant Chemo",        prefix = "ChemoNeo"),
  list(m = meta_chemo_adj,   d = dat_chemo_adj,   label = "Adjuvant Chemo",           prefix = "ChemoAdj"),
  list(m = meta_rt,          d = dat_rt,          label = "Radiotherapy",              prefix = "RT"),
  list(m = meta_whipple,     d = dat_whipple,     label = "Whipple Procedure",        prefix = "Whipple"),
  list(m = meta_distal,      d = dat_distal,      label = "Distal Pancreatectomy",    prefix = "Distal"),
  list(m = meta_r0,          d = dat_r0,          label = "R0 Resection",             prefix = "R0"),
  list(m = meta_no_tx,       d = dat_no_tx,       label = "No Treatment",             prefix = "NoTx"),
  list(m = meta_palliative,  d = dat_palliative,  label = "Palliative Chemo",         prefix = "PalliativeChemo"),
  list(m = meta_total_panc,  d = dat_total_panc,  label = "Total Pancreatectomy",     prefix = "TotalPanc"),
  list(m = meta_os_5yr,      d = dat_os_5yr,      label = "5-Year OS",                prefix = "OS5yr"),
  list(m = meta_os_3yr,      d = dat_os_3yr,      label = "3-Year OS",                prefix = "OS3yr"),
  list(m = meta_os_1yr,      d = dat_os_1yr,      label = "1-Year OS",                prefix = "OS1yr"),
  list(m = meta_os_2yr,      d = dat_os_2yr,      label = "2-Year OS",                prefix = "OS2yr")
)

# Define all moderators for decomposition (7 total: 6 original + Study Period Mid)
all_moderators <- list(
  list(var = "Region_Group",       label = "Geographic Region",  type = "categorical"),
  list(var = "Data_Source_Group",   label = "Data Source Type",   type = "categorical"),
  list(var = "Age_Threshold_Num",   label = "Age Threshold",      type = "continuous"),
  list(var = "Year",               label = "Publication Year",   type = "continuous"),
  list(var = "Study_Period_Mid",   label = "Study Period Midpoint", type = "continuous"),
  list(var = "Total_N_num",        label = "Sample Size (log)",  type = "log"),
  list(var = "EOPC_Proportion",    label = "EOPC Proportion",   type = "continuous")
)

# ==============================================================================
# FUNCTION: Run univariate meta-regression for one moderator
# ==============================================================================
run_single_metareg <- function(meta_obj, dat, mod_info) {
  if (is.null(meta_obj) || meta_obj$k < 5) return(NULL)
  if (!mod_info$var %in% names(dat)) return(NULL)

  mod_vals <- dat[[mod_info$var]]
  if (all(is.na(mod_vals))) return(NULL)

  # For categorical: need >= 2 levels with >= 2 studies each
  if (mod_info$type == "categorical") {
    tbl <- table(mod_vals[!is.na(mod_vals)])
    if (length(tbl) < 2 || any(tbl < 2)) return(NULL)
  }

  if (mod_info$type == "log") mod_vals <- log(mod_vals + 1)

  res <- tryCatch({
    if (mod_info$type == "categorical") {
      rma(yi = meta_obj$TE, sei = meta_obj$seTE,
          mods = ~ factor(mod_vals), method = "REML", test = "knha")
    } else {
      rma(yi = meta_obj$TE, sei = meta_obj$seTE,
          mods = ~ mod_vals, method = "REML", test = "knha")
    }
  }, error = function(e) NULL)

  if (is.null(res)) return(NULL)

  # Extract R2 and omnibus test
  r2 <- ifelse(!is.null(res$R2), res$R2, NA_real_)
  # Ensure R2 is not negative (metafor clips at 0, but just in case)
  if (!is.na(r2) && r2 < 0) r2 <- 0

  return(list(
    rma = res,
    R2 = r2,
    QM = res$QM,
    QMp = res$QMp,
    tau2 = res$tau2,
    k = res$k
  ))
}

# ==============================================================================
# PART 2: Full tau-squared decomposition for all outcomes
# ==============================================================================

cat("\n==== Tau-squared Decomposition ====\n")

decomp_results <- list()

for (item in binary_outcomes) {
  if (is.null(item$m) || item$m$k < 5) next

  cat("\n--- ", item$label, " (k=", item$m$k, ") ---\n", sep = "")

  # Total tau2 from the main meta-analysis
  tau2_total <- item$m$tau2
  I2_total <- item$m$I2

  # Skip if tau2 is 0 (no heterogeneity to decompose)
  if (tau2_total == 0) {
    cat("  tau2 = 0, skipping decomposition.\n")
    next
  }

  # Run univariate meta-regression for each moderator
  mod_results <- list()
  for (mod in all_moderators) {
    res <- run_single_metareg(item$m, item$d, mod)
    if (!is.null(res)) {
      mod_results[[mod$label]] <- res
      cat(sprintf("  %s: R2=%.1f%%, QM=%.2f, p=%s\n",
                  mod$label, res$R2, res$QM, format_pval(res$QMp)))
    }
  }

  # Compute decomposition row
  row <- data.frame(
    Outcome = item$label,
    k = item$m$k,
    OR = sprintf("%.2f", exp(item$m$TE.random)),
    I2 = sprintf("%.1f%%", I2_total * 100),
    tau2_total = sprintf("%.4f", tau2_total),
    stringsAsFactors = FALSE
  )

  for (mod in all_moderators) {
    r2_col <- paste0("R2_", gsub(" ", "_", mod$label))
    p_col <- paste0("p_", gsub(" ", "_", mod$label))
    if (mod$label %in% names(mod_results)) {
      mr <- mod_results[[mod$label]]
      row[[r2_col]] <- sprintf("%.1f%%", mr$R2)
      row[[p_col]] <- format_pval(mr$QMp)
    } else {
      row[[r2_col]] <- "N/A"
      row[[p_col]] <- "N/A"
    }
  }

  # Best single moderator
  if (length(mod_results) > 0) {
    r2_vals <- sapply(mod_results, function(x) x$R2)
    best_idx <- which.max(r2_vals)
    row$Best_Single_Moderator <- names(mod_results)[best_idx]
    row$Best_Single_R2 <- sprintf("%.1f%%", r2_vals[best_idx])
  } else {
    row$Best_Single_Moderator <- "None"
    row$Best_Single_R2 <- "0.0%"
  }

  decomp_results[[item$prefix]] <- row
}

# Add OS
if (!is.null(meta_os) && meta_os$k >= 5) {
  cat("\n--- Overall Survival (k=", meta_os$k, ") ---\n", sep = "")
  tau2_os <- meta_os$tau2

  mod_results_os <- list()
  for (mod in all_moderators) {
    res <- run_single_metareg(meta_os, dat_os, mod)
    if (!is.null(res)) {
      mod_results_os[[mod$label]] <- res
      cat(sprintf("  %s: R2=%.1f%%, QM=%.2f, p=%s\n",
                  mod$label, res$R2, res$QM, format_pval(res$QMp)))
    }
  }

  row_os <- data.frame(
    Outcome = "Overall Survival (HR)",
    k = meta_os$k,
    OR = sprintf("%.2f", exp(meta_os$TE.random)),
    I2 = sprintf("%.1f%%", meta_os$I2 * 100),
    tau2_total = sprintf("%.4f", tau2_os),
    stringsAsFactors = FALSE
  )

  for (mod in all_moderators) {
    r2_col <- paste0("R2_", gsub(" ", "_", mod$label))
    p_col <- paste0("p_", gsub(" ", "_", mod$label))
    if (mod$label %in% names(mod_results_os)) {
      mr <- mod_results_os[[mod$label]]
      row_os[[r2_col]] <- sprintf("%.1f%%", mr$R2)
      row_os[[p_col]] <- format_pval(mr$QMp)
    } else {
      row_os[[r2_col]] <- "N/A"
      row_os[[p_col]] <- "N/A"
    }
  }

  if (length(mod_results_os) > 0) {
    r2_vals <- sapply(mod_results_os, function(x) x$R2)
    best_idx <- which.max(r2_vals)
    row_os$Best_Single_Moderator <- names(mod_results_os)[best_idx]
    row_os$Best_Single_R2 <- sprintf("%.1f%%", r2_vals[best_idx])
  } else {
    row_os$Best_Single_Moderator <- "None"
    row_os$Best_Single_R2 <- "0.0%"
  }

  decomp_results[["OS"]] <- row_os
}

# Combine and save
decomp_table <- bind_rows(decomp_results)
write_xlsx(decomp_table, file.path(PATHS$tables, "Heterogeneity_Decomposition.xlsx"))
cat("\n\nDecomposition table saved.\n")

# ==============================================================================
# PART 3: Multivariate meta-regression for key binary outcomes (k >= 15)
# ==============================================================================

cat("\n==== Multivariate Meta-Regression for Binary Outcomes ====\n")

# Key outcomes with sufficient k
key_binary <- list(
  list(m = meta_chemo,     d = dat_chemo,     label = "Chemotherapy",       prefix = "Chemo"),
  list(m = meta_male,      d = dat_male,      label = "Male Proportion",    prefix = "Male"),
  list(m = meta_surgery,   d = dat_surgery,   label = "Surgical Resection", prefix = "Surgery"),
  list(m = meta_stage_iv,  d = dat_stage_iv,  label = "Stage IV",           prefix = "StageIV"),
  list(m = meta_metastatic,d = dat_metastatic,label = "Metastatic Disease", prefix = "Metastatic"),
  list(m = meta_tumor_head,d = dat_tumor_head,label = "Tumour Head",        prefix = "TumorHead"),
  list(m = meta_rt,        d = dat_rt,        label = "Radiotherapy",        prefix = "RT"),
  list(m = meta_ln_positive,d = dat_ln_positive,label = "Lymph Node Positive",prefix = "LNPositive"),
  list(m = meta_black,     d = dat_black,     label = "Black Race",          prefix = "Black"),
  list(m = meta_white,     d = dat_white,     label = "White Race",          prefix = "White"),
  list(m = meta_poorly_diff,d = dat_poorly_diff,label = "Poorly Differentiated",prefix = "PoorlyDiff")
)

mv_results <- list()

for (item in key_binary) {
  if (is.null(item$m) || item$m$k < 15) next

  dat_i <- item$d
  m_i <- item$m

  cat("\n--- Multivariate:", item$label, "(k=", m_i$k, ") ---\n")

  # MV1: Region + Data Source (categorical)
  mv1 <- tryCatch({
    valid <- !is.na(dat_i$Region_Group) & !is.na(dat_i$Data_Source_Group)
    if (sum(valid) < 10) stop("Too few valid obs")
    rma(yi = m_i$TE[valid], sei = m_i$seTE[valid],
        mods = ~ factor(dat_i$Region_Group[valid]) + factor(dat_i$Data_Source_Group[valid]),
        method = "REML", test = "knha")
  }, error = function(e) { cat("  MV1 error:", e$message, "\n"); NULL })

  if (!is.null(mv1)) {
    cat(sprintf("  MV1 (Region+DataSource): R2=%.1f%%, QM=%.2f, p=%s\n",
                ifelse(!is.null(mv1$R2), mv1$R2, 0), mv1$QM, format_pval(mv1$QMp)))
    mv_results[[paste0(item$prefix, "_MV1")]] <- data.frame(
      Outcome = item$label, Model = "Region + Data Source",
      k = mv1$k, R2 = ifelse(!is.null(mv1$R2), sprintf("%.1f%%", mv1$R2), "0.0%"),
      QM = sprintf("%.2f", mv1$QM), QMp = format_pval(mv1$QMp),
      tau2_residual = sprintf("%.4f", mv1$tau2),
      stringsAsFactors = FALSE
    )
  }

  # MV2: Region + Age Threshold + Year (mixed)
  mv2 <- tryCatch({
    valid <- !is.na(dat_i$Region_Group) & !is.na(dat_i$Age_Threshold_Num) & !is.na(dat_i$Year)
    if (sum(valid) < 10) stop("Too few valid obs")
    rma(yi = m_i$TE[valid], sei = m_i$seTE[valid],
        mods = ~ factor(dat_i$Region_Group[valid]) + dat_i$Age_Threshold_Num[valid] + dat_i$Year[valid],
        method = "REML", test = "knha")
  }, error = function(e) { cat("  MV2 error:", e$message, "\n"); NULL })

  if (!is.null(mv2)) {
    cat(sprintf("  MV2 (Region+AgeThreshold+Year): R2=%.1f%%, QM=%.2f, p=%s\n",
                ifelse(!is.null(mv2$R2), mv2$R2, 0), mv2$QM, format_pval(mv2$QMp)))
    mv_results[[paste0(item$prefix, "_MV2")]] <- data.frame(
      Outcome = item$label, Model = "Region + Age Threshold + Year",
      k = mv2$k, R2 = ifelse(!is.null(mv2$R2), sprintf("%.1f%%", mv2$R2), "0.0%"),
      QM = sprintf("%.2f", mv2$QM), QMp = format_pval(mv2$QMp),
      tau2_residual = sprintf("%.4f", mv2$tau2),
      stringsAsFactors = FALSE
    )
  }

  # MV3: Data Source + Age Threshold + log(N) (three predictors)
  mv3 <- tryCatch({
    valid <- !is.na(dat_i$Data_Source_Group) & !is.na(dat_i$Age_Threshold_Num) &
             !is.na(dat_i$Total_N_num)
    if (sum(valid) < 10) stop("Too few valid obs")
    rma(yi = m_i$TE[valid], sei = m_i$seTE[valid],
        mods = ~ factor(dat_i$Data_Source_Group[valid]) +
                 dat_i$Age_Threshold_Num[valid] + log(dat_i$Total_N_num[valid] + 1),
        method = "REML", test = "knha")
  }, error = function(e) { cat("  MV3 error:", e$message, "\n"); NULL })

  if (!is.null(mv3)) {
    cat(sprintf("  MV3 (DataSource+AgeThreshold+logN): R2=%.1f%%, QM=%.2f, p=%s\n",
                ifelse(!is.null(mv3$R2), mv3$R2, 0), mv3$QM, format_pval(mv3$QMp)))
    mv_results[[paste0(item$prefix, "_MV3")]] <- data.frame(
      Outcome = item$label, Model = "Data Source + Age Threshold + log(N)",
      k = mv3$k, R2 = ifelse(!is.null(mv3$R2), sprintf("%.1f%%", mv3$R2), "0.0%"),
      QM = sprintf("%.2f", mv3$QM), QMp = format_pval(mv3$QMp),
      tau2_residual = sprintf("%.4f", mv3$tau2),
      stringsAsFactors = FALSE
    )
  }
}

if (length(mv_results) > 0) {
  mv_table <- bind_rows(mv_results)
  write_xlsx(mv_table, file.path(PATHS$tables, "MetaRegression_Binary_Multivariate.xlsx"))
  cat("\nMultivariate meta-regression table saved.\n")
}

# ==============================================================================
# PART 4: Summary statistics for manuscript text
# ==============================================================================

cat("\n\n========================================\n")
cat("SUMMARY FOR MANUSCRIPT TEXT\n")
cat("========================================\n")

# 1. Median R2 across all outcomes
all_r2 <- list()
for (item in binary_outcomes) {
  if (is.null(item$m) || item$m$k < 5) next
  for (mod in all_moderators) {
    res <- run_single_metareg(item$m, item$d, mod)
    if (!is.null(res)) {
      all_r2[[length(all_r2) + 1]] <- data.frame(
        Outcome = item$label, Moderator = mod$label, R2 = res$R2,
        p = res$QMp, k = res$k, stringsAsFactors = FALSE
      )
    }
  }
}
# Add OS
if (!is.null(meta_os)) {
  for (mod in all_moderators) {
    res <- run_single_metareg(meta_os, dat_os, mod)
    if (!is.null(res)) {
      all_r2[[length(all_r2) + 1]] <- data.frame(
        Outcome = "Overall Survival", Moderator = mod$label, R2 = res$R2,
        p = res$QMp, k = res$k, stringsAsFactors = FALSE
      )
    }
  }
}

if (length(all_r2) > 0) {
  r2_df <- bind_rows(all_r2)

  cat("\n--- Aggregate R2 statistics ---\n")
  cat("Total moderator-outcome tests:", nrow(r2_df), "\n")
  cat("Median R2:", sprintf("%.1f%%", median(r2_df$R2, na.rm = TRUE)), "\n")
  cat("Mean R2:", sprintf("%.1f%%", mean(r2_df$R2, na.rm = TRUE)), "\n")
  cat("IQR R2:", sprintf("%.1f%%--%.1f%%",
      quantile(r2_df$R2, 0.25, na.rm = TRUE),
      quantile(r2_df$R2, 0.75, na.rm = TRUE)), "\n")
  cat("R2 > 0:", sum(r2_df$R2 > 0, na.rm = TRUE), "of", nrow(r2_df),
      "(", sprintf("%.0f%%", 100 * sum(r2_df$R2 > 0, na.rm = TRUE) / nrow(r2_df)), ")\n")
  cat("R2 > 10%:", sum(r2_df$R2 > 10, na.rm = TRUE), "\n")
  cat("R2 > 20%:", sum(r2_df$R2 > 20, na.rm = TRUE), "\n")
  cat("R2 > 50%:", sum(r2_df$R2 > 50, na.rm = TRUE), "\n")

  cat("\n--- R2 by moderator ---\n")
  by_mod <- r2_df %>%
    group_by(Moderator) %>%
    summarise(
      n_tests = n(),
      median_R2 = sprintf("%.1f%%", median(R2, na.rm = TRUE)),
      mean_R2 = sprintf("%.1f%%", mean(R2, na.rm = TRUE)),
      max_R2 = sprintf("%.1f%%", max(R2, na.rm = TRUE)),
      n_R2_gt0 = sum(R2 > 0, na.rm = TRUE),
      n_p_lt05 = sum(p < 0.05, na.rm = TRUE),
      .groups = "drop"
    )
  print(as.data.frame(by_mod))

  cat("\n--- Top R2 values (>10%) ---\n")
  top_r2 <- r2_df %>%
    filter(R2 > 10) %>%
    arrange(desc(R2))
  print(as.data.frame(top_r2))

  cat("\n--- Significant moderators (p<0.05) ---\n")
  sig_mods <- r2_df %>%
    filter(p < 0.05) %>%
    arrange(p)
  print(as.data.frame(sig_mods))

  # Save comprehensive R2 table
  write_xlsx(r2_df, file.path(PATHS$tables, "MetaRegression_All_R2.xlsx"))
}

# ==============================================================================
# PART 5: Formal heterogeneity source attribution
# ==============================================================================

cat("\n\n--- Formal Heterogeneity Source Attribution ---\n")

# For the 4 main sources: Data Source, Region, Age Threshold, Unmeasured
# Compute: median R2 for each source across outcomes as the best estimate
# of variance explained

cat("\nAttribution (median R2 across all outcomes):\n")
sources <- c("Data Source Type", "Geographic Region", "Age Threshold",
             "Publication Year", "Study Period Midpoint", "Sample Size (log)", "EOPC Proportion")

source_r2 <- list()
for (src in sources) {
  vals <- r2_df$R2[r2_df$Moderator == src]
  if (length(vals) > 0) {
    cat(sprintf("  %s: median R2 = %.1f%%, mean R2 = %.1f%%, max = %.1f%% (n=%d)\n",
                src, median(vals), mean(vals), max(vals), length(vals)))
    source_r2[[src]] <- list(median = median(vals), mean = mean(vals),
                              max = max(vals), n = length(vals))
  }
}

# Combined attribution estimate (multivariate R2)
if (length(mv_results) > 0) {
  mv_df <- bind_rows(mv_results)
  cat("\n\nMultivariate R2 summary (best combined models):\n")
  for (model_type in unique(mv_df$Model)) {
    subset_df <- mv_df[mv_df$Model == model_type, ]
    r2_vals <- as.numeric(gsub("%", "", subset_df$R2))
    cat(sprintf("  %s: median R2 = %.1f%%, mean = %.1f%%, range = %.1f%%--%.1f%%\n",
                model_type, median(r2_vals), mean(r2_vals),
                min(r2_vals), max(r2_vals)))
  }
}

# Estimated unmeasured component
total_measured_median <- sum(sapply(source_r2[c("Data Source Type", "Geographic Region",
                                                 "Age Threshold")],
                                    function(x) x$median))
cat(sprintf("\nCombined measured sources (sum of median R2): %.1f%%\n", total_measured_median))
cat(sprintf("Estimated unmeasured (molecular + other): %.1f%%\n", 100 - total_measured_median))

cat("\n\n04b: Heterogeneity Decomposition Analysis COMPLETE.\n")
cat("Output files:\n")
cat("  - Heterogeneity_Decomposition.xlsx\n")
cat("  - MetaRegression_Binary_Multivariate.xlsx\n")
cat("  - MetaRegression_All_R2.xlsx\n")
