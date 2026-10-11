################################################################################
# EOPC vs AOPC Systematic Review and Meta-Analysis
# Script 10: Influence Diagnostics + Cumulative Meta-Analysis
# Input:  meta_results.RData, prepared_data.RData
# Output: Influence plots, Baujat plot, cumulative meta forest plot
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
# PART 1: Influence Diagnostics (OS primary)
# ==============================================================================

run_influence_diagnostics <- function(meta_obj, dat, outcome_label, prefix) {
  if (is.null(meta_obj) || meta_obj$k < 3) {
    cat("Skipping influence diagnostics for", outcome_label, "- need >= 3 studies.\n")
    return(NULL)
  }

  cat("\n==== Influence Diagnostics:", outcome_label, "(k=", meta_obj$k, ") ====\n")

  # Refit with metafor for diagnostics
  # Filter out studies with NA/Inf TE or seTE (can occur with metabin zero-cell corrections)
  valid_idx <- which(is.finite(meta_obj$TE) & is.finite(meta_obj$seTE) & meta_obj$seTE > 0)
  if (length(valid_idx) < 3) {
    cat("  Too few valid studies after filtering (", length(valid_idx), "). Skipping.\n")
    return(NULL)
  }
  rma_obj <- rma(yi = meta_obj$TE[valid_idx], sei = meta_obj$seTE[valid_idx],
                 method = "REML", slab = meta_obj$studlab[valid_idx])

  # 1a. Influence measures (Cook's D, DFBETAS, hat values, etc.)
  inf <- influence(rma_obj)
  cat("\nInfluence measures:\n")
  print(inf)

  inf_file <- file.path(PATHS$influence, paste0(prefix, "_influence_diagnostics"))
  save_figure(
    quote({ lancet_par(mar = c(4, 5, 2, 1.5));
            plot(inf) }),
    filename = inf_file,
    width = 12, height = 10
  )

  # 1b. Baujat plot (ggplot2 Lancet style)
  cat("\n--- Baujat Plot ---\n")
  baujat_file <- file.path(PATHS$influence, paste0(prefix, "_baujat"))
  tryCatch({
    # Compute Baujat components manually
    r_obj <- rma_obj
    contrib_Q <- numeric(r_obj$k)
    influence_est <- numeric(r_obj$k)
    for (i_b in seq_len(r_obj$k)) {
      fit_i <- rma(yi = r_obj$yi[-i_b], sei = sqrt(r_obj$vi[-i_b]), method = "REML")
      contrib_Q[i_b] <- (r_obj$yi[i_b] - fit_i$beta[1])^2 / r_obj$vi[i_b]
      influence_est[i_b] <- abs(r_obj$beta[1] - fit_i$beta[1])
    }
    baujat_df <- data.frame(
      study = r_obj$slab,
      contrib = contrib_Q,
      influence = influence_est,
      stringsAsFactors = FALSE
    )
    p_baujat <- ggplot(baujat_df, aes(x = contrib, y = influence)) +
      geom_point(shape = 19, size = 2.5, colour = LANCET_STYLE$col_bubble) +
      ggrepel::geom_text_repel(aes(label = study), size = 2.3, colour = LANCET_STYLE$col_emphasis,
                                max.overlaps = 20, segment.colour = LANCET_STYLE$col_structural,
                                segment.size = 0.25, box.padding = 0.4,
                                force = 1.5, seed = 42) +
      labs(x = "Contribution to Q statistic",
           y = "Influence on pooled result",
           title = paste0(outcome_label, ": Baujat Plot")) +
      lancet_theme(base_size = 10)
    save_figure_gg(p_baujat, baujat_file, width = 8, height = 7)
  }, error = function(e) {
    # Fallback to base R baujat
    cat("  ggplot2 Baujat error:", e$message, ", falling back to base R\n")
    save_figure(
      quote({
        lancet_par(mar = c(5, 5, 3, 2))
        baujat(rma_obj,
               main = paste0(outcome_label, ": Baujat Plot"),
               xlab = "Contribution to Q statistic",
               ylab = "Influence on pooled result")
      }),
      filename = baujat_file,
      width = 8, height = 7
    )
  })

  # 1c. Identify influential studies
  cooks_d <- cooks.distance(rma_obj)
  threshold <- 4 / meta_obj$k
  influential <- which(cooks_d > threshold)
  if (length(influential) > 0) {
    cat("\nInfluential studies (Cook's D >", round(threshold, 3), "):\n")
    for (i in influential) {
      cat(sprintf("  %s: Cook's D = %.4f\n", meta_obj$studlab[i], cooks_d[i]))
    }
  } else {
    cat("\nNo studies exceed Cook's D threshold of", round(threshold, 3), "\n")
  }

  # 1d. GOSH plot (if feasible - computationally intensive)
  if (meta_obj$k <= 20) {
    cat("\n--- GOSH Plot ---\n")
    gosh_file <- file.path(PATHS$influence, paste0(prefix, "_gosh"))
    tryCatch({
      gosh_res <- gosh(rma_obj, subsets = min(2^meta_obj$k, 10000))
      save_figure(
        quote({ lancet_par(mar = c(4, 5, 2, 1.5));
                plot(gosh_res, main = paste0(outcome_label, ": GOSH Plot")) }),
        filename = gosh_file,
        width = 10, height = 8
      )
      cat("GOSH plot saved.\n")
    }, error = function(e) {
      cat("GOSH plot error:", e$message, "\n")
    })
  } else {
    cat("GOSH plot skipped (k > 20, too computationally intensive).\n")
  }

  return(inf)
}

inf_os <- run_influence_diagnostics(meta_os, dat_os, "Overall Survival", "OS")

# ==============================================================================
# PART 2: Leave-One-Out Analysis
# ==============================================================================

run_leave_one_out <- function(meta_obj, outcome_label, prefix) {
  if (is.null(meta_obj) || meta_obj$k < 3) return(NULL)

  cat("\n==== Leave-One-Out:", outcome_label, "====\n")

  loo <- metainf(meta_obj, pooled = "random")

  loo_file <- file.path(PATHS$influence, paste0(prefix, "_leave_one_out"))
  fdims <- compute_forest_dims(meta_obj$k)
  fstyle <- lancet_forest_style()

  save_figure(
    quote({
      par(family = LANCET_FONT, bg = "white")
      forest(loo,
             main = paste0(outcome_label, ": Leave-One-Out Sensitivity"),
             col.diamond = fstyle$col.diamond,
             col.diamond.lines = fstyle$col.diamond,
             col.bg = LANCET_STYLE$col_square,
             col.border = LANCET_STYLE$col_square,
             plotwidth = fstyle$plotwidth,
             fontsize = fdims$fontsize, spacing = fdims$spacing,
             lwd = fstyle$lwd,
             col.label.left = "black", col.label.right = "black")
    }),
    filename = loo_file,
    height = fdims$height
  )

  cat("Leave-one-out range: HR",
      sprintf("%.2f - %.2f\n",
              min(exp(loo$TE[1:meta_obj$k])),
              max(exp(loo$TE[1:meta_obj$k]))))

  return(loo)
}

loo_os <- run_leave_one_out(meta_os, "Overall Survival", "OS")

# ==============================================================================
# PART 3: Cumulative Meta-Analysis (by publication year)
# ==============================================================================

run_cumulative_meta <- function(meta_obj, dat, outcome_label, prefix) {
  if (is.null(meta_obj) || meta_obj$k < 3) return(NULL)

  cat("\n==== Cumulative Meta-Analysis:", outcome_label, "====\n")

  # metacum handles sorting internally via sortvar, so no pre-sorting is needed
  cum <- metacum(meta_obj, pooled = "random", sortvar = dat$Year)

  cum_file <- file.path(PATHS$influence, paste0(prefix, "_cumulative"))
  fdims <- compute_forest_dims(meta_obj$k)
  fstyle <- lancet_forest_style()

  save_figure(
    quote({
      par(family = LANCET_FONT, bg = "white")
      forest(cum,
             main = paste0(outcome_label, ": Cumulative Meta-Analysis (by Year)"),
             col.diamond = fstyle$col.diamond,
             col.diamond.lines = fstyle$col.diamond,
             col.bg = LANCET_STYLE$col_square,
             col.border = LANCET_STYLE$col_square,
             plotwidth = fstyle$plotwidth,
             fontsize = fdims$fontsize, spacing = fdims$spacing,
             lwd = fstyle$lwd,
             col.label.left = "black", col.label.right = "black")
    }),
    filename = cum_file,
    height = fdims$height
  )

  return(cum)
}

cum_os <- run_cumulative_meta(meta_os, dat_os, "Overall Survival", "OS")

# ==============================================================================
# Run for key binary outcomes with sufficient studies
# ==============================================================================

binary_analyses <- list(
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

for (item in binary_analyses) {
  if (!is.null(item$m) && item$m$k >= 10) {
    run_influence_diagnostics(item$m, item$d, item$label, item$prefix)
    run_leave_one_out(item$m, item$label, item$prefix)
    run_cumulative_meta(item$m, item$d, item$label, item$prefix)
  }
}

cat("\n\nInfluence diagnostics and cumulative meta-analysis complete.\n")
