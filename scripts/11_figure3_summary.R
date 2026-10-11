#!/usr/bin/env Rscript
# ==============================================================================
# Figure 3: Summary forest plot of all binary outcomes (EOPC vs AOPC)
# forestploter table-based layout: JNCI publication quality
#
# Layout (2026-08-24): TWO stacked panels to eliminate text overlap
#   Upper block: Panels A-C (Demographics, Staging, Pathology), 22 outcomes
#   Lower block: Panels D-E (Treatment, Time-Point Survival), 15 outcomes
# Both blocks share identical column widths (globally padded) and a common
# x-axis range widened to c(0.08, 11) so wide CIs (e.g. 2-Year OS 0.11-8.23)
# never intrude into the OR text column.
# Data source: meta_results.RData (direct meta objects), numbers unchanged
# ==============================================================================

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
suppressPackageStartupMessages({
  library(grid)
  library(gridExtra)
})

load(file.path(PATHS$tables, "meta_results.RData"))

# ==============================================================================
# Define outcome list with panel assignment (order = display order top-to-bottom)
# ==============================================================================
outcome_defs <- list(
  # Panel A: Demographics & Risk Factors
  list(m = "meta_male",        label = "Male Proportion",          panel = "A"),
  list(m = "meta_smoking",     label = "Smoking History",          panel = "A"),
  list(m = "meta_diabetes",    label = "Diabetes History",         panel = "A"),
  list(m = "meta_family_hx",   label = "Family History",           panel = "A"),
  list(m = "meta_alcohol",     label = "Alcohol History",          panel = "A"),
  list(m = "meta_white",       label = "White Race",               panel = "A"),
  list(m = "meta_black",       label = "Black Race",               panel = "A"),
  list(m = "meta_asian",       label = "Asian Race",               panel = "A"),
  # Panel B: Tumor Staging
  list(m = "meta_stage_i",     label = "Stage I",                  panel = "B"),
  list(m = "meta_stage_ii",    label = "Stage II",                 panel = "B"),
  list(m = "meta_stage_iii",   label = "Stage III",                panel = "B"),
  list(m = "meta_stage_iv",    label = "Stage IV",                 panel = "B"),
  list(m = "meta_ln_positive", label = "LN Positive",              panel = "B"),
  list(m = "meta_metastatic",  label = "Metastatic",               panel = "B"),
  # Panel C: Differentiation, Location & Pathology
  list(m = "meta_poorly_diff", label = "Poorly Diff.",             panel = "C"),
  list(m = "meta_mod_diff",    label = "Mod. Diff.",               panel = "C"),
  list(m = "meta_well_diff",   label = "Well Diff.",               panel = "C"),
  list(m = "meta_tumor_head",  label = "Tumor Head",              panel = "C"),
  list(m = "meta_tumor_body",  label = "Tumor Body",               panel = "C"),
  list(m = "meta_tumor_tail",  label = "Tumor Tail",               panel = "C"),
  list(m = "meta_vasc_inv",    label = "Vascular Inv.",            panel = "C"),
  list(m = "meta_pni",         label = "Perineural Inv.",          panel = "C"),
  # Panel D: Treatment Patterns
  list(m = "meta_surgery",     label = "Surgery",                  panel = "D"),
  list(m = "meta_chemo",       label = "Chemotherapy",             panel = "D"),
  list(m = "meta_chemo_neo",   label = "Neoadj. Chemo",            panel = "D"),
  list(m = "meta_chemo_adj",   label = "Adj. Chemo",               panel = "D"),
  list(m = "meta_rt",          label = "Radiotherapy",             panel = "D"),
  list(m = "meta_whipple",     label = "Whipple",                  panel = "D"),
  list(m = "meta_distal",      label = "Distal Pancreatect.",      panel = "D"),
  list(m = "meta_r0",          label = "R0 Resection",             panel = "D"),
  list(m = "meta_no_tx",       label = "No Treatment",             panel = "D"),
  list(m = "meta_palliative",  label = "Palliative Chemo",         panel = "D"),
  list(m = "meta_total_panc",  label = "Total Pancreatect.",       panel = "D"),
  # Panel E: Time-Point Survival
  list(m = "meta_os_5yr",      label = "5-Year OS",                panel = "E"),
  list(m = "meta_os_3yr",      label = "3-Year OS",                panel = "E"),
  list(m = "meta_os_2yr",      label = "2-Year OS",                panel = "E"),
  list(m = "meta_os_1yr",      label = "1-Year OS",                panel = "E")
)

panel_labels <- c(
  A = "A) Demographics & Risk Factors",
  B = "B) Tumor Staging",
  C = "C) Diff., Location & Pathology",
  D = "D) Treatment Patterns",
  E = "E) Time-Point Survival"
)

# Block assignment: upper = A+B+C, lower = D+E
block_of <- c(A = "upper", B = "upper", C = "upper", D = "lower", E = "lower")

# ==============================================================================
# Extract data from meta objects
# ==============================================================================
rows <- list()
for (item in outcome_defs) {
  obj <- tryCatch(get(item$m), error = function(e) NULL)
  if (is.null(obj)) {
    cat("  Skipping", item$label, "- meta object not found\n")
    next
  }

  or_est  <- exp(obj$TE.random)
  or_lo   <- exp(obj$lower.random)
  or_hi   <- exp(obj$upper.random)
  i2_pct  <- obj$I2 * 100
  p_val   <- obj$pval.random
  k_studies <- obj$k
  n_total   <- tryCatch(sum(obj$n.e) + sum(obj$n.c), error = function(e) NA_real_)

  rows[[length(rows) + 1]] <- data.frame(
    Outcome   = item$label,
    panel_key = item$panel,
    OR        = or_est,
    OR_lo     = or_lo,
    OR_hi     = or_hi,
    I2        = i2_pct,
    P_value   = p_val,
    k         = k_studies,
    N         = n_total,
    stringsAsFactors = FALSE
  )
}

df <- do.call(rbind, rows)
cat("Extracted", nrow(df), "outcomes from meta_results.RData\n")

# ==============================================================================
# Build display rows (text + effect data), then pad columns to GLOBAL widths
# so the two stacked blocks have identical, aligned column positions
# ==============================================================================
max_k <- max(df$k, na.rm = TRUE)

disp <- list()   # each: list(outcome, k, N, or, i2, p, block, est, lo, hi, sz, is_sum)

add_header <- function(panel_key) {
  disp[[length(disp) + 1]] <<- list(
    outcome = panel_labels[[panel_key]], k = "", N = "", or = "", i2 = "", p = "",
    block = block_of[[panel_key]], est = NA, lo = NA, hi = NA, sz = NA, is_sum = TRUE
  )
}

for (panel_key in c("A", "B", "C", "D", "E")) {
  panel_df <- df[df$panel_key == panel_key, ]
  if (nrow(panel_df) == 0) next
  add_header(panel_key)
  for (i in seq_len(nrow(panel_df))) {
    r <- panel_df[i, ]
    n_text <- if (is.na(r$N)) {
      "\u2014"
    } else if (r$N >= 1e6) {
      paste0(formatC(r$N / 1e3, format = "f", digits = 0, big.mark = ","), "K")
    } else {
      formatC(r$N, format = "d", big.mark = ",")
    }
    p_text <- ifelse(r$P_value < 0.001, "<0.001",
                     sprintf("%.3f", r$P_value))
    disp[[length(disp) + 1]] <- list(
      outcome = paste0("   ", r$Outcome),
      k       = as.character(r$k),
      N       = n_text,
      or      = sprintf("%.2f [%.2f, %.2f]", r$OR, r$OR_lo, r$OR_hi),
      i2      = sprintf("%.0f%%", r$I2),
      p       = p_text,
      block   = block_of[[panel_key]],
      est     = r$OR, lo = r$OR_lo, hi = r$OR_hi,
      sz      = 0.15 + 0.20 * (r$k / max_k),
      is_sum  = FALSE
    )
  }
}

# ---- Global column widths (max over BOTH blocks -> guaranteed alignment) ----
w_out <- max(nchar(sapply(disp, `[[`, "outcome")))
w_k   <- max(nchar(sapply(disp, `[[`, "k")))
w_N   <- max(nchar(sapply(disp, `[[`, "N")))
w_or  <- max(nchar(sapply(disp, `[[`, "or")))
w_i2  <- max(nchar(sapply(disp, `[[`, "i2")))
w_p   <- max(nchar(sapply(disp, `[[`, "p")))
pad_l <- function(x, w) formatC(x, width = w, flag = "-")   # left-justified

# Outcome column: pad well BEYOND the longest (bold) panel header so the
# regular-text padded width dominates the column width in BOTH blocks,
# a bold 30-char header measures ~33 regular chars, so +3 was insufficient
# and the stacked columns misaligned by ~10 pt. +6 gives clear dominance.
outcome_pad <- w_out + 6

i2_header <- paste0("I", "\u00B2")
blank <- paste(rep(" ", 26), collapse = "")   # CI plot area width

# ==============================================================================
# Build a forestploter data table + effect vectors for one block
# ==============================================================================
build_block <- function(block_name) {
  sel <- Filter(function(d) d$block == block_name, disp)
  all_rows <- list()
  est_v <- numeric(0); lo_v <- numeric(0); hi_v <- numeric(0)
  sz_v  <- numeric(0); is_sum <- logical(0)
  for (d in sel) {
    all_rows[[length(all_rows) + 1]] <- data.frame(
      Outcome  = pad_l(d$outcome, outcome_pad),
      k        = pad_l(d$k, w_k),
      N        = pad_l(d$N, w_N),
      ci_space = blank,
      or_ci    = pad_l(d$or, w_or),
      i2_col   = pad_l(d$i2, w_i2),
      P        = pad_l(d$p, w_p),
      stringsAsFactors = FALSE
    )
    names(all_rows[[length(all_rows)]]) <-
      c("Outcome", "k", "N", " ", "OR [95% CI]", i2_header, "P")
    est_v  <- c(est_v, d$est)
    lo_v   <- c(lo_v, d$lo)
    hi_v   <- c(hi_v, d$hi)
    sz_v   <- c(sz_v, d$sz)
    is_sum <- c(is_sum, d$is_sum)
  }
  list(dt = do.call(rbind, all_rows),
       est = est_v, lo = lo_v, hi = hi_v, sz = sz_v, is_sum = is_sum)
}

blk_up <- build_block("upper")
blk_lo <- build_block("lower")
cat(sprintf("Upper block: %d rows | Lower block: %d rows\n",
            nrow(blk_up$dt), nrow(blk_lo$dt)))

# ==============================================================================
# forestploter theme (JNCI print-size: 6.75 in wide, all text >= 8 pt at 1:1)
# ==============================================================================
tm <- forestploter::forest_theme(
  base_size     = 8.4,
  base_family   = LANCET_FONT,
  ci_pch        = 15,
  ci_col        = LANCET_COLORS[["blue"]],
  ci_fill       = LANCET_COLORS[["blue"]],
  ci_alpha      = 1,
  ci_lty        = 1,
  ci_lwd        = 1.0,
  ci_Theight    = 0.12,
  refline_gp    = gpar(lty = "dashed", lwd = 0.8,
                        col = LANCET_STYLE$col_structural),
  summary_fill  = LANCET_COLORS[["black"]],
  summary_col   = LANCET_COLORS[["black"]],
  footnote_gp   = gpar(cex = 1.0, fontface = "italic", col = "black"),
  arrow_type    = "closed",
  arrow_label_just = "end",
  title_just    = "left",
  title_gp      = gpar(cex = 1.2, fontface = "bold", col = "black"),
  # NOTE: fontsize must be explicit because a bare gpar(cex=...) leaves the grid
  # default 12 pt on the tick labels, which then collides with the xlab
  xaxis_gp      = gpar(fontsize = 8.4, cex = 1.0, lwd = 0.6),
  # Style alignment (2026-08-24): match Fig 2/4/5 with a WHITE column header,
  # barely-there alternating rows (#F8F9FA like lancet_fp_theme), single
  # Wong-blue data color. No dark header band, no navy accents.
  core = list(
    bg_params = list(fill = c("white", "#F8F9FA")),
    padding   = unit(c(1.8, 0.9), "mm")
  ),
  colhead = list(
    bg_params = list(fill = "white"),
    padding   = unit(c(1.8, 0.9), "mm")
  )
)

# ==============================================================================
# Build the two forest plots (shared x-scale wide enough for all CIs)
# ==============================================================================
ticks <- c(0.1, 0.5, 1, 2, 5, 10)
xlim_range <- c(0.08, 11)

make_forest <- function(blk, with_xlab) {
  p <- forestploter::forest(
    data       = blk$dt,
    est        = blk$est,
    lower      = blk$lo,
    upper      = blk$hi,
    sizes      = blk$sz,
    is_summary = blk$is_sum,
    ci_column  = 4,
    ref_line   = 1,
    x_trans    = "log",
    xlim       = xlim_range,
    ticks_at   = ticks,
    arrow_lab  = c("\u2190 Higher in AOPC", "Higher in EOPC \u2192"),
    xlab       = if (with_xlab) "Odds Ratio" else NULL,
    theme      = tm
  )
  # Style alignment: SINGLE Wong-blue data color (ci_col from theme), no
  # red/green direction coding, matching the other main figures (Fig 2/4/5).
  p
}

p_up <- make_forest(blk_up, with_xlab = FALSE)
p_lo <- make_forest(blk_lo, with_xlab = TRUE)

# ---- Emphasize panel header rows (bold) --------------------------------------
# forestploter's is_summary only styles the CI diamond; the row text stays
# regular. Post-process the gtable: every "core-fg" text grob sitting on a
# summary row gets fontface="bold" (BLACK, not navy, unlike the other main figures
# use pure-black bold headers; bold alone makes the panel letters stand out
# from the regular-weight data rows).
emphasize_summary <- function(p, is_sum) {
  lay <- p$layout
  for (i in seq_len(nrow(lay))) {
    if (!identical(lay$name[i], "core-fg")) next
    g <- p$grobs[[i]]
    if (class(g)[1] != "text") next
    r <- lay$t[i] - 2L          # colhead occupies gtable row 2 -> data row = t-2
    if (r >= 1L && r <= length(is_sum) && isTRUE(is_sum[r])) {
      g$gp <- modifyList(g$gp, grid::gpar(fontface = "bold", col = "black"))
      p$grobs[[i]] <- g
    }
  }
  p
}
p_up <- emphasize_summary(p_up, blk_up$is_sum)
p_lo <- emphasize_summary(p_lo, blk_lo$is_sum)

# ---- Force identical column widths across the two stacked blocks ----
# forestploter column widths are lazy grobwidth units resolved per gtable at
# draw time; text padding alone cannot guarantee the two blocks agree (the
# bold panel headers make the upper Outcome column ~0.29 in wider). Resolve
# the upper block's widths to ABSOLUTE inches on a cairo device (same engine
# as the final PDF) and assign to BOTH gtables -> pixel-perfect alignment.
OUT_WIDTH  <- 6.75
OUT_HEIGHT <- 9.9
tmp_dev <- tempfile(fileext = ".pdf")
cairo_pdf(tmp_dev, width = OUT_WIDTH, height = OUT_HEIGHT, family = LANCET_FONT)
w_up_abs <- sapply(seq_along(p_up$widths), function(i) {
  v <- tryCatch(
    grid::convertUnit(p_up$widths[i], "in", valueOnly = TRUE),
    error = function(e) NA_real_
  )
  as.numeric(v)
})
dev.off()
unlink(tmp_dev)
stopifnot(!any(is.na(w_up_abs)))
p_up$widths <- grid::unit(w_up_abs, "in")
p_lo$widths <- grid::unit(w_up_abs, "in")

# ---- Resolve ACTUAL block heights (absolute inches) ---------------------------
# Heights proportional to row counts ("null" units) stretch each block to fill
# the fixed page: the upper block's content is shorter than its share, leaving
# a ~111 pt void between panels C and D. Instead, measure each gtable's real
# height on a cairo device (same engine as the final PDF) and stack exactly.
tmp_dev2 <- tempfile(fileext = ".pdf")
cairo_pdf(tmp_dev2, width = OUT_WIDTH, height = 20, family = LANCET_FONT)
resolve_h <- function(p) {
  sum(sapply(seq_along(p$heights), function(i) {
    as.numeric(grid::convertUnit(p$heights[i], "in", valueOnly = TRUE))
  }))
}
h_up_abs <- resolve_h(p_up)
h_lo_abs <- resolve_h(p_lo)
dev.off()
unlink(tmp_dev2)
cat(sprintf("Resolved block heights: upper %.2f in, lower %.2f in\n",
            h_up_abs, h_lo_abs))
stopifnot(!any(is.na(c(h_up_abs, h_lo_abs))),
          h_up_abs > 4, h_lo_abs > 3)   # 25 + 17 rows must be far taller

GAP_IN <- 0.14   # breathing room between the two blocks
OUT_HEIGHT <- round(h_up_abs + h_lo_abs + GAP_IN + 0.10, 2)

fig_grob <- gridExtra::arrangeGrob(
  p_up, p_lo, ncol = 1,
  heights = unit(c(h_up_abs, h_lo_abs), "in"),
  padding = unit(GAP_IN, "in")
)

# ==============================================================================
# Save: TIFF 300 DPI + PDF, written to /tmp first, then copied into the project
# (avoids cairo/iCloud silent-write failures on large vector files)
# ==============================================================================
OUT_DPI    <- 300

tmp_base <- file.path(tempdir(), "Figure_3_Binary_Summary")

ragg::agg_tiff(
  filename    = paste0(tmp_base, ".tiff"),
  width       = OUT_WIDTH,
  height      = OUT_HEIGHT,
  units       = "in",
  res         = OUT_DPI,
  bg          = "white",
  compression = "lzw"
)
grid.newpage()
grid.draw(fig_grob)
dev.off()

cairo_pdf(
  filename = paste0(tmp_base, ".pdf"),
  width    = OUT_WIDTH,
  height   = OUT_HEIGHT,
  bg       = "white",
  family   = LANCET_FONT
)
grid.newpage()
grid.draw(fig_grob)
dev.off()

out_dir <- file.path(PROJECT_ROOT, "08_Manuscript", "figures")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
fig_path <- file.path(out_dir, "Figure_3_Binary_Summary")
file.copy(paste0(tmp_base, ".tiff"), paste0(fig_path, ".tiff"), overwrite = TRUE)
file.copy(paste0(tmp_base, ".pdf"),  paste0(fig_path, ".pdf"),  overwrite = TRUE)

cat("\nFigure 3 (Binary Summary) generated:\n")
cat("  Outcomes:", nrow(df), "\n")
cat("  Layout: 2 stacked blocks (A-C upper / D-E lower), aligned columns\n")
cat("  Dimensions: 6.75 x 9.9 in, 300 DPI (final print size)\n")
cat("  Base font: 8.4 pt (>= 8 pt at 1:1 print scale)\n")
cat("  x-axis: log OR, xlim 0.08-11 (fits widest CI 0.11-8.23)\n")
cat("  Color: Wong blue (#0072B2) single-color CI, black bold panel headers\n")
