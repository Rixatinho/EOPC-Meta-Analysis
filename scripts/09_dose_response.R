################################################################################
# EOPC vs AOPC Systematic Review and Meta-Analysis
# Script 09: Age Threshold Dose-Response Meta-Analysis
# Input:  prepared_data.RData (dat_os with Age_Threshold_Num)
# Output: Dose-response curve, model comparison table
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

load(file.path(PATHS$stat_data, "prepared_data.RData"))

# ==============================================================================
# Dose-Response Meta-Analysis: Age Threshold vs OS HR
# ==============================================================================

# Filter studies with both OS HR and age threshold
dr_data <- dat_os %>%
  filter(!is.na(Age_Threshold_Num), !is.na(logHR), !is.na(seLogHR)) %>%
  arrange(Age_Threshold_Num)

cat("Dose-response data:", nrow(dr_data), "studies\n")
cat("Age thresholds present:", sort(unique(dr_data$Age_Threshold_Num)), "\n")
cat("Distinct thresholds:", length(unique(dr_data$Age_Threshold_Num)), "\n")

# Check prerequisites
n_thresholds <- length(unique(dr_data$Age_Threshold_Num))
n_studies <- nrow(dr_data)

if (n_thresholds < 3 || n_studies < 6) {
  cat("\nInsufficient data for dose-response analysis.\n")
  cat("Need: >= 3 distinct thresholds and >= 6 studies.\n")
  cat("Have:", n_thresholds, "thresholds and", n_studies, "studies.\n")
  cat("Saving empty results for downstream compatibility.\n")

  dr_results <- NULL
  save(dr_results, file = file.path(PATHS$tables, "dose_response_results.RData"))
  quit(save = "no", status = 0)
}

# ==============================================================================
# Method 1: Meta-regression approach (simpler, always works)
# ==============================================================================

cat("\n==== Method 1: Meta-Regression (Age Threshold ~ OS HR) ====\n")

# Linear meta-regression
rma_linear <- rma(yi = logHR, sei = seLogHR,
                  mods = ~ Age_Threshold_Num,
                  data = dr_data, method = "REML", test = "knha")
cat("\nLinear meta-regression:\n")
print(rma_linear)

# Visualization: ggplot2 bubble plot
bubble_file <- file.path(PATHS$dose_response, "age_threshold_bubble")

bub_df <- data.frame(
  x = dr_data$Age_Threshold_Num,
  y = dr_data$logHR,
  se = dr_data$seLogHR,
  w = 1 / dr_data$seLogHR^2,
  lab = dr_data$Study_Label
)

x_seq <- seq(min(bub_df$x), max(bub_df$x), length.out = 100)
pred_lin <- predict(rma_linear, newmods = x_seq)
pred_df <- data.frame(x = x_seq, y = pred_lin$pred,
                       lo = pred_lin$ci.lb, hi = pred_lin$ci.ub)

p_bub <- ggplot(bub_df, aes(x = x, y = y)) +
  geom_ribbon(data = pred_df, aes(x = x, y = y, ymin = lo, ymax = hi),
              fill = LANCET_STYLE$col_ci_band, inherit.aes = FALSE) +
  geom_line(data = pred_df, aes(x = x, y = y), colour = LANCET_STYLE$col_regression,
            linewidth = 1, inherit.aes = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = LANCET_STYLE$col_structural) +
  geom_point(aes(size = w), shape = 21, fill = LANCET_STYLE$col_bubble,
             colour = "black", alpha = 0.7) +
  scale_size_continuous(range = c(1.5, 8), guide = "none") +
  labs(x = "Age Threshold (years)", y = "log(HR)",
       title = "Exploratory: Age Threshold vs Overall Survival") +
  lancet_theme(base_size = 10)

save_figure_gg(p_bub, bubble_file, width = 8, height = 7)

# ==============================================================================
# Note on dosresmeta: The dosresmeta package requires multiple dose levels
# WITHIN each study (e.g., multiple exposure categories per cohort). Our data
# has only one age threshold per study, making dosresmeta inapplicable.
# The meta-regression approach above (Method 1) is the correct method for
# across-study dose-response with single-level exposure per study.
# ==============================================================================

# Non-linear check via quadratic meta-regression
cat("\n==== Non-linear Check: Quadratic Meta-Regression ====\n")
rma_quad <- tryCatch({
  rma(yi = logHR, sei = seLogHR,
      mods = ~ Age_Threshold_Num + I(Age_Threshold_Num^2),
      data = dr_data, method = "REML", test = "knha")
}, error = function(e) {
  cat("Quadratic model error:", e$message, "\n")
  NULL
})

if (!is.null(rma_quad)) {
  cat("\nQuadratic meta-regression:\n")
  print(rma_quad)
  cat(sprintf("\nQuadratic term p-value: %s\n",
              format_pval(rma_quad$pval[3])))
  cat("Interpretation: p < 0.05 suggests non-linear relationship.\n")

  # Formal model comparison: linear vs quadratic
  cat("\n==== Model Comparison (Linear vs Quadratic) ====\n")
  aic_lin  <- AIC(rma_linear)
  aic_quad <- AIC(rma_quad)
  cat(sprintf("  Linear  AIC: %.2f\n", aic_lin))
  cat(sprintf("  Quadratic AIC: %.2f\n", aic_quad))
  cat(sprintf("  Delta AIC (lin - quad): %.2f\n", aic_lin - aic_quad))

  # Likelihood ratio test
  lrt_stat <- -2 * (logLik(rma_linear) - logLik(rma_quad))
  lrt_p    <- pchisq(as.numeric(lrt_stat), df = 1, lower.tail = FALSE)
  cat(sprintf("  LRT chi-sq: %.3f, df=1, p=%s\n", as.numeric(lrt_stat), format_pval(lrt_p)))
  cat(sprintf("  Preferred model: %s\n",
              ifelse(aic_quad < aic_lin, "Quadratic", "Linear")))
}

# Dose-response curve: use quadratic model if significant, otherwise linear
dose_seq <- seq(min(dr_data$Age_Threshold_Num),
                max(dr_data$Age_Threshold_Num),
                length.out = 100)

use_quad <- !is.null(rma_quad) && rma_quad$pval[3] < 0.05
best_model <- if (use_quad) rma_quad else rma_linear
model_label <- if (use_quad) "Quadratic" else "Linear"
cat(sprintf("\nUsing %s model for dose-response visualization.\n", model_label))

dr_curve_file <- file.path(PATHS$dose_response, "age_threshold_dose_response")

# Build predictions
if (use_quad) {
  newmods_mat <- cbind(dose_seq, dose_seq^2)
  pred_best <- predict(best_model, newmods = newmods_mat)
} else {
  pred_best <- predict(best_model, newmods = dose_seq)
}

dr_curve_df <- data.frame(
  x = dose_seq, y = exp(pred_best$pred),
  lo = exp(pred_best$ci.lb), hi = exp(pred_best$ci.ub)
)
dr_pts <- data.frame(
  x = dr_data$Age_Threshold_Num, y = exp(dr_data$logHR),
  w = 1 / dr_data$seLogHR,
  lab = dr_data$Study_Label
)

# Linear model predictions (for comparison overlay when best model is quadratic)
pred_lin_curve <- predict(rma_linear, newmods = dose_seq)
lin_curve_df <- data.frame(
  x = dose_seq, y = exp(pred_lin_curve$pred)
)

# Decide which studies to label (avoid clutter):
# Label studies at non-modal thresholds, or extreme residuals at modal threshold
modal_threshold <- as.numeric(names(sort(table(dr_pts$x), decreasing = TRUE))[1])
residuals_best <- dr_pts$y - approx(dr_curve_df$x, dr_curve_df$y, dr_pts$x)$y
med_resid <- median(abs(residuals_best), na.rm = TRUE)
dr_pts$show_label <- (dr_pts$x != modal_threshold) |
                     (abs(residuals_best) > med_resid * 1.5)

# Model statistics for annotation
aic_lin_val  <- AIC(rma_linear)
aic_best_val <- AIC(best_model)
nonlin_p_txt <- if (!is.null(rma_quad) && length(rma_quad$pval) >= 3) {
  format_pval(rma_quad$pval[3])
} else {
  "NA"
}
ann_label <- paste0(
  model_label, " model (AIC = ", sprintf("%.1f", aic_best_val), ")\n",
  "Linear model (AIC = ", sprintf("%.1f", aic_lin_val), ")\n",
  "Non-linearity P = ", nonlin_p_txt, "\n",
  "k = ", n_studies, " studies"
)

p_dr <- ggplot() +
  # Best model CI band
  geom_ribbon(data = dr_curve_df, aes(x = x, ymin = lo, ymax = hi),
              fill = LANCET_STYLE$col_ci_band) +
  # Linear comparison line (dashed gray): always shown
  geom_line(data = lin_curve_df, aes(x = x, y = y),
            colour = LANCET_STYLE$col_structural, linewidth = 0.7,
            linetype = "dashed") +
  # Best model line (solid blue)
  geom_line(data = dr_curve_df, aes(x = x, y = y),
            colour = LANCET_STYLE$col_regression, linewidth = 1.1) +
  # Null reference line at HR = 1
  geom_hline(yintercept = 1, linetype = "dotted", colour = "black",
             linewidth = 0.5) +
  # Study bubbles (size proportional to precision)
  geom_point(data = dr_pts, aes(x = x, y = y, size = w), shape = 21,
             fill = LANCET_STYLE$col_bubble, colour = "black", alpha = 0.7) +
  scale_size_continuous(range = c(2, 9), guide = "none") +
  # Study labels (selected studies only)
  # size in mm: 3.0 mm = 8.5 pt (>= 8pt minimum at final print size)
  ggrepel::geom_text_repel(
    data = dr_pts[dr_pts$show_label, ],
    aes(x = x, y = y, label = lab),
    size = 3.0, colour = LANCET_STYLE$col_emphasis,
    max.overlaps = 20, segment.colour = LANCET_STYLE$col_structural,
    segment.size = 0.25, box.padding = 0.4, point.padding = 0.3,
    min.segment.length = 0.2, force = 3, seed = 42,
    family = LANCET_FONT
  ) +
  # Model statistics annotation box (top-left)
  annotate("label", x = min(dr_pts$x) + 0.5, y = 3.2,
           label = ann_label, hjust = 0, vjust = 1,
           size = 3.4, family = LANCET_FONT,
           fill = "white", colour = "black",
           label.size = 0.3, label.padding = unit(0.4, "lines")) +
  coord_cartesian(ylim = c(0.3, 3.5)) +
  labs(x = "Age Threshold (years)",
       y = "Hazard Ratio (EOPC vs AOPC)") +
  lancet_theme(base_size = 11) +
  theme(
    plot.title = element_blank(),
    plot.margin = margin(t = 12, r = 12, b = 14, l = 12, unit = "pt")
  )

# Figure 5: print-width 6.75in (OUP 171mm two-column max); fonts render 1:1
save_figure_gg(p_dr, dr_curve_file, width = 6.75, height = 5.2)

# ==============================================================================
# Method 3: Restricted Cubic Splines (RCS) via rms package
# dosresmeta requires within-study dose levels (inapplicable here).
# rms::rcs() inside metafor::rma() is the correct approach for across-study
# dose-response with single-level exposure per study.
# ==============================================================================

rma_rcs <- NULL
rcs_nonlin_p <- NA

if (n_thresholds >= 4) {  # RCS with 3 knots needs >= 4 unique x values
  cat("\n==== Method 3: Restricted Cubic Splines (RCS) ====\n")

  library(rms)
  # Knots at 10th, 50th, 90th percentiles (Harrell recommendation)
  knots_rcs <- quantile(dr_data$Age_Threshold_Num, probs = c(0.10, 0.50, 0.90))
  cat("RCS knots:", paste(sprintf("%.1f", knots_rcs), collapse = ", "), "\n")

  rma_rcs <- tryCatch({
    rcs_terms <- rcs(dr_data$Age_Threshold_Num, knots_rcs)
    rma(yi = logHR, sei = seLogHR, mods = rcs_terms,
        data = dr_data, method = "REML", test = "knha")
  }, error = function(e) {
    cat("RCS model error:", e$message, "\n")
    NULL
  })

  if (!is.null(rma_rcs)) {
    cat("\nRCS meta-regression:\n")
    print(rma_rcs)

    # Non-linearity test: test the second RCS term (non-linear component)
    # In rma with rcs(), the 2nd moderator coefficient captures non-linearity
    if (length(rma_rcs$pval) >= 3) {
      rcs_nonlin_p <- rma_rcs$pval[3]
      cat(sprintf("\nRCS non-linearity test: p=%s\n", format_pval(rcs_nonlin_p)))
    }

    # AIC comparison: linear vs quadratic vs RCS
    aic_rcs <- AIC(rma_rcs)
    cat(sprintf("\nModel comparison (AIC):\n"))
    cat(sprintf("  Linear:    %.2f\n", AIC(rma_linear)))
    if (!is.null(rma_quad)) cat(sprintf("  Quadratic: %.2f\n", AIC(rma_quad)))
    cat(sprintf("  RCS:       %.2f\n", aic_rcs))

    # RCS dose-response curve: ggplot2
    rcs_curve_file <- file.path(PATHS$dose_response, "age_threshold_rcs_curve")

    rcs_seq <- seq(min(dr_data$Age_Threshold_Num),
                   max(dr_data$Age_Threshold_Num), length.out = 100)
    rcs_newmods <- rcs(rcs_seq, knots_rcs)
    pred_rcs_vals <- predict(rma_rcs, newmods = rcs_newmods)

    rcs_df <- data.frame(x = rcs_seq, y = exp(pred_rcs_vals$pred),
                          lo = exp(pred_rcs_vals$ci.lb), hi = exp(pred_rcs_vals$ci.ub))
    rcs_pts <- data.frame(x = dr_data$Age_Threshold_Num, y = exp(dr_data$logHR),
                           w = 1 / dr_data$seLogHR)

    p_rcs <- ggplot() +
      geom_ribbon(data = rcs_df, aes(x = x, ymin = lo, ymax = hi),
                  fill = LANCET_STYLE$col_ci_band) +
      geom_line(data = rcs_df, aes(x = x, y = y), colour = LANCET_STYLE$col_regression, linewidth = 1) +
      geom_hline(yintercept = 1, linetype = "dashed", colour = LANCET_STYLE$col_structural) +
      geom_point(data = rcs_pts, aes(x = x, y = y, size = w), shape = 21,
                 fill = LANCET_STYLE$col_bubble, colour = "black", alpha = 0.7) +
      scale_size_continuous(range = c(1.5, 8), guide = "none") +
      coord_cartesian(ylim = c(0.3, 3.5)) +
      labs(x = "Age Threshold (years)", y = "Hazard Ratio",
           title = "Exploratory: Age Threshold vs OS Hazard Ratio") +
      lancet_theme(base_size = 10)

    save_figure_gg(p_rcs, rcs_curve_file, width = 8, height = 7)
  }
} else {
  cat("\nRCS analysis skipped: need >= 4 distinct thresholds, have", n_thresholds, "\n")
}

# Save results (include all models)
dr_results <- list(
  rma_linear = rma_linear,
  rma_quad   = rma_quad,
  rma_rcs    = rma_rcs,
  use_quad   = use_quad,
  rcs_nonlin_p = rcs_nonlin_p,
  data = dr_data
)
save(dr_results, file = file.path(PATHS$tables, "dose_response_results.RData"))

cat("\nDose-response analysis complete.\n")
