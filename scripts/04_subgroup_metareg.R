################################################################################
# EOPC vs AOPC Systematic Review and Meta-Analysis
# Script 04: Subgroup Analysis + Meta-Regression
# Input:  meta_results.RData, prepared_data.RData
# Output: Subgroup forest plots, meta-regression bubble plots
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
# PART 1: SUBGROUP ANALYSES (6 pre-specified for OS)
# ==============================================================================

run_subgroup <- function(meta_obj, dat, outcome_label, subgroup_var,
                         subgroup_label, prefix) {
  if (is.null(meta_obj) || nrow(dat) < 3) return(NULL)
  if (!subgroup_var %in% names(dat)) {
    cat("Variable", subgroup_var, "not found. Skipping.\n")
    return(NULL)
  }
  if (all(is.na(dat[[subgroup_var]]))) {
    cat("All NA for", subgroup_var, ". Skipping.\n")
    return(NULL)
  }

  # Check minimum studies per subgroup
  sg_counts <- table(dat[[subgroup_var]])
  if (length(sg_counts) < 2) {
    cat("Only 1 subgroup level for", subgroup_label, ". Skipping.\n")
    return(NULL)
  }

  cat("\n--- Subgroup:", subgroup_label, "for", outcome_label, "---\n")

  m_sub <- tryCatch(
    update(meta_obj, subgroup = dat[[subgroup_var]]),
    error = function(e) {
      cat("  Subgroup analysis failed (", e$message, "). Skipping.\n")
      NULL
    }
  )
  if (is.null(m_sub)) return(NULL)

  # Print subgroup test
  cat(sprintf("Test for subgroup differences: Q=%.2f, df=%d, p=%s\n",
              m_sub$Q.b.random, m_sub$df.Q.b, format_pval(m_sub$pval.Q.b.random)))

  # Forest plot: Lancet table-based layout (forestploter)
  sub_file <- file.path(PATHS$subgroup,
                        paste0(prefix, "_by_", gsub(" ", "_", subgroup_label)))

  fp <- tryCatch(
    lancet_fp_subgroup(m_sub,
                        outcome_label = outcome_label,
                        subgroup_label = subgroup_label,
                        sort_year = dat$Year),
    error = function(e) {
      cat("  forestploter subgroup error:", e$message, " - skipping plot\n")
      NULL
    }
  )

  if (!is.null(fp)) {
    save_fp_figure(fp, sub_file)
  }

  return(m_sub)
}

# Run subgroups for OS
if (!is.null(meta_os) && nrow(dat_os) >= 5) {
  subgroups <- list(
    list(var = "Age_Threshold_Group", label = "Age Threshold"),
    list(var = "Region_Group",        label = "Geographic Region"),
    list(var = "Data_Source_Group",    label = "Data Source"),
    list(var = "NOS_Quality_Group",   label = "NOS Quality"),
    list(var = "ROBINS_E_Group",      label = "ROBINS-E Risk"),
    list(var = "HR_Source_Group",     label = "HR Source"),
    list(var = "Publication_Era",    label = "Publication Era")
  )

  subgroup_results <- list()
  for (sg in subgroups) {
    res <- run_subgroup(meta_os, dat_os, "Overall Survival",
                       sg$var, sg$label, "OS")
    if (!is.null(res)) subgroup_results[[sg$label]] <- res
  }
}

# ==============================================================================
# PART 2: META-REGRESSION (for OS if k >= 10)
# ==============================================================================

run_metaregression <- function(meta_obj, dat, outcome_label, prefix) {
  if (is.null(meta_obj) || meta_obj$k < 10) {
    cat("Meta-regression for", outcome_label, "requires k >= 10. Current k =",
        ifelse(is.null(meta_obj), 0, meta_obj$k), "\n")
    return(NULL)
  }

  cat("\n==== Meta-Regression:", outcome_label, "====\n")

  rma_obj <- tryCatch(
    rma(yi = meta_obj$TE, sei = meta_obj$seTE, method = "REML"),
    error = function(e) {
      cat("  Base rma failed:", e$message, ". Skipping meta-regression.\n")
      NULL
    }
  )
  if (is.null(rma_obj)) return(NULL)

  # Define potential moderators
  moderators <- list(
    list(var = "Year",              label = "Publication Year",   type = "continuous"),
    list(var = "EOPC_Proportion",   label = "EOPC Proportion",   type = "continuous"),
    list(var = "Total_N_num",       label = "Sample Size (log)",  type = "log"),
    list(var = "Age_Threshold_Num", label = "Age Threshold",      type = "continuous"),
    list(var = "Data_Source_Group", label = "Data Source Type",    type = "categorical")
  )

  reg_results <- list()
  for (mod in moderators) {
    if (!mod$var %in% names(dat)) next
    mod_vals <- dat[[mod$var]]
    if (all(is.na(mod_vals))) next

    if (mod$type == "log") mod_vals <- log(mod_vals + 1)

    cat("\n--- Moderator:", mod$label, "---\n")

    rma_mod <- tryCatch({
      if (mod$type == "categorical") {
        rma(yi = meta_obj$TE, sei = meta_obj$seTE,
            mods = ~ factor(mod_vals), method = "REML", test = "knha")
      } else {
        rma(yi = meta_obj$TE, sei = meta_obj$seTE,
            mods = ~ mod_vals, method = "REML", test = "knha")
      }
    }, error = function(e) {
      cat("  Error:", e$message, "\n")
      NULL
    })

    if (is.null(rma_mod)) next

    print(rma_mod)

    reg_results[[mod$label]] <- data.frame(
      Moderator = mod$label,
      Coefficient = sprintf("%.4f", rma_mod$beta[2]),
      SE = sprintf("%.4f", rma_mod$se[2]),
      P_value = format_pval(rma_mod$pval[2]),
      R2 = ifelse(!is.null(rma_mod$R2), sprintf("%.1f%%", rma_mod$R2), "N/A"),
      stringsAsFactors = FALSE
    )

    # Bubble plot (only for continuous moderators): ggplot2
    if (mod$type != "categorical") {
      bubble_file <- file.path(PATHS$subgroup,
                               paste0(prefix, "_metareg_", gsub(" ", "_", mod$label)))

      bub_df <- data.frame(
        x = mod_vals, y = meta_obj$TE, se = meta_obj$seTE,
        w = 1 / meta_obj$seTE^2, lab = meta_obj$studlab
      )
      bub_df <- bub_df[!is.na(bub_df$x) & !is.na(bub_df$y), ]

      x_seq <- seq(min(bub_df$x, na.rm = TRUE), max(bub_df$x, na.rm = TRUE), length.out = 100)
      pred_res <- predict(rma_mod, newmods = x_seq)
      pred_df <- data.frame(x = x_seq, y = pred_res$pred,
                             lo = pred_res$ci.lb, hi = pred_res$ci.ub)

      p_bub <- ggplot(bub_df, aes(x = x, y = y)) +
        geom_ribbon(data = pred_df, aes(x = x, y = y, ymin = lo, ymax = hi),
                    fill = LANCET_STYLE$col_ci_band, inherit.aes = FALSE) +
        geom_line(data = pred_df, aes(x = x, y = y), colour = LANCET_STYLE$col_regression,
                  linewidth = 1, inherit.aes = FALSE) +
        geom_hline(yintercept = 0, linetype = "dashed", colour = LANCET_STYLE$col_structural) +
        geom_point(aes(size = w), shape = 21, fill = LANCET_STYLE$col_bubble,
                   colour = "black", alpha = 0.7) +
        scale_size_continuous(range = c(1.5, 8), guide = "none") +
        labs(x = mod$label, y = paste0("log(", meta_obj$sm, ")"),
             title = paste0(outcome_label, " ~ ", mod$label),
             subtitle = sprintf("Slope = %.4f, p = %s, R2 = %s",
                                rma_mod$beta[2], format_pval(rma_mod$pval[2]),
                                ifelse(!is.null(rma_mod$R2), sprintf("%.1f%%", rma_mod$R2), "N/A"))) +
        lancet_theme(base_size = 10) +
        theme(plot.subtitle = element_text(face = "italic", size = 9, colour = LANCET_STYLE$col_structural))

      save_figure_gg(p_bub, bubble_file, width = 8, height = 7)

      # Meta-regression diagnostic plots (residuals, Q-Q, Cook's distance)
      diag_file <- file.path(PATHS$subgroup,
                             paste0(prefix, "_metareg_diagnostics_", gsub(" ", "_", mod$label)))
      tryCatch({
        rs <- rstudent(rma_mod)
        fit_vals <- fitted(rma_mod)
        cd <- cooks.distance(rma_mod)
        hv <- hatvalues(rma_mod)
        save_figure(
          quote({
            lancet_par(mar = c(5, 5, 3.5, 1.5))
            par(mfrow = c(2, 2))
            # 1. Residuals vs Fitted
            plot(fit_vals, rs$z, pch = 19, col = LANCET_STYLE$col_bubble,
                 xlab = "Fitted values", ylab = "Studentized residuals",
                 main = "Residuals vs Fitted")
            abline(h = 0, lty = 2, col = LANCET_STYLE$col_structural)
            abline(h = c(-2, 2), lty = 3, col = LANCET_STYLE$col_structural)
            # 2. Normal Q-Q
            qqnorm(rs$z, pch = 19, col = LANCET_STYLE$col_bubble, main = "Normal Q-Q")
            qqline(rs$z, col = LANCET_STYLE$col_structural, lty = 2)
            # 3. Cook's Distance
            plot(cd, type = "h", lwd = 2, col = LANCET_STYLE$col_bubble,
                 main = "Cook's Distance", ylab = "Cook's D", xlab = "Study")
            abline(h = 4 / length(cd), lty = 2, col = LANCET_STYLE$col_structural)
            # 4. Leverage (Hat values)
            plot(hv, type = "h", lwd = 2, col = LANCET_STYLE$col_bubble,
                 main = "Leverage", ylab = "Hat values", xlab = "Study")
            abline(h = 2 * mean(hv), lty = 2, col = LANCET_STYLE$col_structural)
            par(mfrow = c(1, 1))
          }),
          filename = diag_file,
          width = 10, height = 8
        )
      }, error = function(e) cat("  Diagnostic plots error:", e$message, "\n"))
    }
  }

  if (length(reg_results) > 0) {
    reg_table <- bind_rows(reg_results)
    write_xlsx(reg_table, file.path(PATHS$tables,
                                    paste0("MetaRegression_", prefix, ".xlsx")))
    cat("\nMeta-regression summary:\n")
    print(reg_table)
  }
}

run_metaregression(meta_os, dat_os, "Overall Survival", "OS")

# ==============================================================================
# PART 2b: MULTIVARIATE META-REGRESSION (for OS if k >= 15)
# ==============================================================================

run_multivariate_metareg <- function(meta_obj, dat, outcome_label, prefix) {
  if (is.null(meta_obj) || meta_obj$k < 15) {
    cat("Multivariate meta-regression for", outcome_label, "requires k >= 15. Current k =",
        ifelse(is.null(meta_obj), 0, meta_obj$k), "\n")
    return(NULL)
  }

  cat("\n==== Multivariate Meta-Regression:", outcome_label, "====\n")

  mv_results <- list()

  # Model MV1: Age_Threshold_Num + Year
  cat("\n--- MV1: Age Threshold + Publication Year ---\n")
  mv1 <- tryCatch({
    rma(yi = meta_obj$TE, sei = meta_obj$seTE,
        mods = ~ dat$Age_Threshold_Num + dat$Year,
        method = "REML", test = "knha")
  }, error = function(e) { cat("  Error:", e$message, "\n"); NULL })

  if (!is.null(mv1)) {
    print(mv1)
    mv_results[["MV1"]] <- data.frame(
      Model = "MV1: Age Threshold + Year",
      k = mv1$k,
      Moderator_1 = "Age Threshold",
      Beta_1 = sprintf("%.4f", mv1$beta[2]),
      P_1 = format_pval(mv1$pval[2]),
      Moderator_2 = "Publication Year",
      Beta_2 = sprintf("%.4f", mv1$beta[3]),
      P_2 = format_pval(mv1$pval[3]),
      R2 = ifelse(!is.null(mv1$R2), sprintf("%.1f%%", mv1$R2), "N/A"),
      QM = sprintf("%.2f", mv1$QM),
      QM_p = format_pval(mv1$QMp),
      stringsAsFactors = FALSE
    )
  }

  # Model MV2: Age_Threshold_Num + Data_Source_Group
  cat("\n--- MV2: Age Threshold + Data Source ---\n")
  mv2 <- tryCatch({
    rma(yi = meta_obj$TE, sei = meta_obj$seTE,
        mods = ~ dat$Age_Threshold_Num + factor(dat$Data_Source_Group),
        method = "REML", test = "knha")
  }, error = function(e) { cat("  Error:", e$message, "\n"); NULL })

  if (!is.null(mv2)) {
    print(mv2)
    mv_results[["MV2"]] <- data.frame(
      Model = "MV2: Age Threshold + Data Source",
      k = mv2$k,
      Moderator_1 = "Age Threshold",
      Beta_1 = sprintf("%.4f", mv2$beta[2]),
      P_1 = format_pval(mv2$pval[2]),
      Moderator_2 = "Data Source (Institution)",
      Beta_2 = sprintf("%.4f", mv2$beta[3]),
      P_2 = format_pval(mv2$pval[3]),
      R2 = ifelse(!is.null(mv2$R2), sprintf("%.1f%%", mv2$R2), "N/A"),
      QM = sprintf("%.2f", mv2$QM),
      QM_p = format_pval(mv2$QMp),
      stringsAsFactors = FALSE
    )
  }

  # Model MV3: Year + log(Total_N)
  cat("\n--- MV3: Publication Year + log(Sample Size) ---\n")
  mv3 <- tryCatch({
    rma(yi = meta_obj$TE, sei = meta_obj$seTE,
        mods = ~ dat$Year + log(dat$Total_N_num + 1),
        method = "REML", test = "knha")
  }, error = function(e) { cat("  Error:", e$message, "\n"); NULL })

  if (!is.null(mv3)) {
    print(mv3)
    mv_results[["MV3"]] <- data.frame(
      Model = "MV3: Year + log(Sample Size)",
      k = mv3$k,
      Moderator_1 = "Publication Year",
      Beta_1 = sprintf("%.4f", mv3$beta[2]),
      P_1 = format_pval(mv3$pval[2]),
      Moderator_2 = "log(Sample Size)",
      Beta_2 = sprintf("%.4f", mv3$beta[3]),
      P_2 = format_pval(mv3$pval[3]),
      R2 = ifelse(!is.null(mv3$R2), sprintf("%.1f%%", mv3$R2), "N/A"),
      QM = sprintf("%.2f", mv3$QM),
      QM_p = format_pval(mv3$QMp),
      stringsAsFactors = FALSE
    )
  }

  if (length(mv_results) > 0) {
    mv_table <- bind_rows(mv_results)
    write_xlsx(mv_table, file.path(PATHS$tables,
                                    paste0("MetaRegression_", prefix, "_Multivariate.xlsx")))
    cat("\nMultivariate meta-regression summary:\n")
    print(mv_table)
  }

  return(mv_results)
}

mv_os <- run_multivariate_metareg(meta_os, dat_os, "Overall Survival", "OS")

# ==============================================================================
# PART 3: SUBGROUP ANALYSES + META-REGRESSION FOR BINARY OUTCOMES
# ==============================================================================

# 6 subgroups (drops HR_Source_Group, which is OS-specific; adds Publication_Era)
binary_subgroups <- list(
  list(var = "Age_Threshold_Group", label = "Age Threshold"),
  list(var = "Region_Group",        label = "Geographic Region"),
  list(var = "Data_Source_Group",    label = "Data Source"),
  list(var = "NOS_Quality_Group",   label = "NOS Quality"),
  list(var = "ROBINS_E_Group",      label = "ROBINS-E Risk"),
  list(var = "Publication_Era",     label = "Publication Era")
)

# Full binary outcomes list
binary_outcomes <- list(
  # Demographics
  list(m = meta_male,        d = dat_male,        label = "Male Proportion",        prefix = "Male"),
  list(m = meta_smoking,     d = dat_smoking,     label = "Smoking History",         prefix = "Smoking"),
  list(m = meta_diabetes,    d = dat_diabetes,    label = "Diabetes History",        prefix = "Diabetes"),
  list(m = meta_family_hx,   d = dat_family_hx,   label = "Family History",          prefix = "FamilyHx"),
  list(m = meta_alcohol,     d = dat_alcohol,     label = "Alcohol History",          prefix = "Alcohol"),
  list(m = meta_white,       d = dat_white,       label = "White Race",               prefix = "White"),
  list(m = meta_black,       d = dat_black,       label = "Black Race",               prefix = "Black"),
  list(m = meta_asian,       d = dat_asian,       label = "Asian Race",               prefix = "Asian"),
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
  list(m = meta_pni,         d = dat_pni,         label = "Perineural Invasion",     prefix = "PNI"),
  # Treatment
  list(m = meta_surgery,     d = dat_surgery,     label = "Surgical Resection",      prefix = "Surgery"),
  list(m = meta_chemo,       d = dat_chemo,       label = "Chemotherapy",            prefix = "Chemo"),
  list(m = meta_chemo_neo,   d = dat_chemo_neo,   label = "Neoadjuvant Chemo",       prefix = "ChemoNeo"),
  list(m = meta_chemo_adj,   d = dat_chemo_adj,   label = "Adjuvant Chemo",          prefix = "ChemoAdj"),
  list(m = meta_rt,          d = dat_rt,          label = "Radiotherapy",             prefix = "RT"),
  list(m = meta_whipple,     d = dat_whipple,     label = "Whipple Procedure",       prefix = "Whipple"),
  list(m = meta_distal,      d = dat_distal,      label = "Distal Pancreatectomy",   prefix = "Distal"),
  list(m = meta_r0,          d = dat_r0,          label = "R0 Resection",            prefix = "R0"),
  list(m = meta_no_tx,       d = dat_no_tx,       label = "No Treatment",            prefix = "NoTx"),
  list(m = meta_palliative,  d = dat_palliative,  label = "Palliative Chemo",        prefix = "PalliativeChemo"),
  list(m = meta_total_panc,  d = dat_total_panc,  label = "Total Pancreatectomy",    prefix = "TotalPanc"),
  # Time-point survival
  list(m = meta_os_5yr,      d = dat_os_5yr,      label = "5-Year OS",               prefix = "OS5yr"),
  list(m = meta_os_3yr,      d = dat_os_3yr,      label = "3-Year OS",               prefix = "OS3yr"),
  list(m = meta_os_1yr,      d = dat_os_1yr,      label = "1-Year OS",               prefix = "OS1yr"),
  list(m = meta_os_2yr,      d = dat_os_2yr,      label = "2-Year OS",               prefix = "OS2yr")
)

cat("\n\n==== PART 3: Binary Outcome Subgroup Analyses ====\n")

binary_subgroup_summary <- list()

for (item in binary_outcomes) {
  if (is.null(item$m) || item$m$k < 10) next

  cat("\n\n---- Binary subgroup:", item$label, "(k=", item$m$k, ") ----\n")

  for (sg in binary_subgroups) {
    res <- run_subgroup(item$m, item$d, item$label, sg$var, sg$label, item$prefix)
    if (!is.null(res)) {
      key <- paste0(item$prefix, "_", sg$var)
      binary_subgroup_summary[[key]] <- data.frame(
        Outcome = item$label,
        Subgroup = sg$label,
        k = item$m$k,
        Q_between = sprintf("%.2f", res$Q.b.random),
        df = res$df.Q.b,
        P_interaction = format_pval(res$pval.Q.b.random),
        stringsAsFactors = FALSE
      )
    }
  }

  # Meta-regression for k >= 20
  if (item$m$k >= 20) {
    cat("\n  Running meta-regression for", item$label, "...\n")
    run_metaregression(item$m, item$d, item$label, item$prefix)
  }
}

# Save binary subgroup interaction summary
if (length(binary_subgroup_summary) > 0) {
  bs_table <- bind_rows(binary_subgroup_summary)

  # Multiple comparison correction (Benjamini-Hochberg FDR)
  bs_table <- bs_table %>%
    mutate(
      P_raw = as.numeric(gsub("< ", "0.00", P_interaction)),
      P_FDR = p.adjust(P_raw, method = "BH"),
      P_interaction_FDR = ifelse(P_FDR < 0.001, "< 0.001", sprintf("%.3f", P_FDR)),
      Significant_raw = P_raw < 0.05,
      Significant_FDR = P_FDR < 0.05
    )

  write_xlsx(bs_table, file.path(PATHS$tables, "Subgroup_Binary_Summary.xlsx"))
  cat("\n\nBinary subgroup interaction summary saved.\n")
  cat("Total interaction tests:", nrow(bs_table), "\n")
  cat("Outcomes analysed:", length(unique(bs_table$Outcome)), "\n")

  cat("\nSignificant interactions (raw p<0.05):",
      sum(bs_table$Significant_raw, na.rm = TRUE), "\n")
  sig_raw <- bs_table %>% filter(Significant_raw)
  if (nrow(sig_raw) > 0) {
    cat("  By moderator:\n")
    print(table(sig_raw$Subgroup))
  }

  cat("\nSignificant interactions (FDR<0.05):",
      sum(bs_table$Significant_FDR, na.rm = TRUE), "\n")
  sig_fdr <- bs_table %>% filter(Significant_FDR)
  if (nrow(sig_fdr) > 0) {
    cat("  By moderator:\n")
    print(table(sig_fdr$Subgroup))
    print(sig_fdr)
  } else {
    cat("  None\n")
  }
}

cat("\n\nSubgroup and meta-regression analyses complete.\n")
