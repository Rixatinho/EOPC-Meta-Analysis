################################################################################
# EOPC vs AOPC Systematic Review and Meta-Analysis
# Script 00: Master Configuration & Environment Setup
# Target: Journal of the National Cancer Institute (JNCI)
################################################################################

# ---- 1. Load Required Packages ----
required_packages <- c(
  "meta",          # Core meta-analysis engine (metagen, metabin, forest)
  "metafor",       # Advanced meta-analysis (rma, influence, regplot)
  "metasens",      # Advanced publication bias (Copas, limit meta)
  "ggplot2",       # Publication-quality plots
  "readxl",        # Read Excel data extraction sheets
  "writexl",       # Write Excel output
  "openxlsx",      # Read/write Excel with sheet control
  "dplyr",         # Data manipulation
  "tidyr",         # Data reshaping
  "stringr",       # String operations
  "purrr",         # Functional programming
  "tibble",        # Modern data frames
  "forestplot",    # Classic forest plots
  "forestploter",  # Advanced customizable forest plots
  "robvis",        # Risk of bias visualization (ROBINS-E)
  "grid",          # Grid graphics
  "gridExtra",     # Multi-panel layouts
  "patchwork",     # ggplot composition
  "ggrepel",       # Non-overlapping labels
  "ggsci",         # Journal color palettes (Lancet, NEJM, etc.)
  "RColorBrewer",  # Color palettes
  "scales",        # Axis formatting
  "officer",       # Word/PPT export
  "flextable",     # Publication tables
  "MASS",          # Statistical functions
  "survival",      # Survival analysis core
  "IPDfromKM",     # Guyot algorithm: KM -> pseudo-IPD
  "survRM2",       # Restricted Mean Survival Time
  "dosresmeta",    # Dose-response meta-analysis
  "rms",           # Restricted cubic splines
  "knitr",         # Report generation
  "ggtext"         # Rich text (HTML/Markdown) in ggplot2 labels
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    warning(paste("Package", pkg, "not installed. Run: install.packages('", pkg, "')"),
            immediate. = TRUE)
    next
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

# Fix MASS::select masking dplyr::select
select <- dplyr::select
filter <- dplyr::filter
# Fix forestploter::forest masking meta::forest
forest <- meta::forest

cat("All", length(required_packages), "packages loaded successfully.\n")

# ---- 1b. Visual Constants (Wong/Okabe-Ito colorblind-safe palette) ----
# Reference: Wong B. Points of view: Color blindness. Nat Methods 2011;8:441.
# All colors verified for deuteranopia, protanopia, and tritanopia compatibility.
LANCET_FONT <- "Arial"

LANCET_COLORS <- c(
  blue    = "#0072B2",   # Wong blue (was ggsci #00468B)
  red     = "#D55E00",   # Wong vermillion (was ggsci #ED0000)
  green   = "#009E73",   # Wong bluish green (was ggsci #42B540)
  teal    = "#56B4E9",   # Wong sky blue (was ggsci #0099B4)
  purple  = "#CC79A7",   # Wong reddish purple (was ggsci #925E9F)
  orange  = "#E69F00",   # Wong orange (new)
  yellow  = "#F0E442",   # Wong yellow (new, for highlights only)
  salmon  = "#FDAF91",   # Retained: prediction band tint
  crimson = "#AD002A",   # Retained: reference line emphasis
  gray    = "#ADB6B6",   # Retained: structural elements
  black   = "#1B1919"    # Retained: near-black for diamonds
)

# ---- 2. Project Paths ----
# Auto-detect project root for reproducibility (R3-9: code sharing)
# Strategy: 1) Check hardcoded dev path; 2) Detect from script location; 3) Fallback to cwd
detect_project_root <- function() {
  # 1) Original development machine (iCloud path)
  hardcoded <- "/Users/rishat/Library/Mobile Documents/com~apple~CloudDocs/个人文档/Word/文献写作/0 待写/3.早发胰腺癌 Meta 分析"
  if (dir.exists(file.path(hardcoded, "04_Data_Extraction"))) return(hardcoded)

  # 2) Detect from main script location (when cloned from GitHub)
  #    scripts/ -> 06_Statistical_Analysis/ -> PROJECT_ROOT/
  script_dir <- tryCatch({
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("--file=", args, value = TRUE)
    if (length(file_arg) > 0) {
      fpath <- sub("--file=", "", file_arg)
      fpath <- gsub("~\\+~", " ", fpath)
      dirname(fpath)
    } else {
      ofile <- NULL
      for (i in seq_len(sys.nframe())) {
        ofile <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
        if (!is.null(ofile)) break
      }
      if (!is.null(ofile)) dirname(ofile) else NULL
    }
  }, error = function(e) NULL)

  if (!is.null(script_dir)) {
    stats_dir <- dirname(script_dir)
    root <- dirname(stats_dir)
    if (dir.exists(file.path(root, "04_Data_Extraction"))) return(root)
  }

  # 3) Fallback: current working directory
  cwd <- getwd()
  if (dir.exists(file.path(cwd, "04_Data_Extraction"))) return(cwd)

  # 4) Walk up from cwd looking for project signature
  for (up in 0:4) {
    candidate <- dirname(cwd)
    for (j in seq_len(up)) candidate <- dirname(candidate)
    if (dir.exists(file.path(candidate, "04_Data_Extraction"))) return(candidate)
  }

  stop("Cannot detect project root. Please set PROJECT_ROOT manually in 00_config.R.")
}

PROJECT_ROOT <- detect_project_root()

PATHS <- list(
  data          = file.path(PROJECT_ROOT, "04_Data_Extraction"),
  quality       = file.path(PROJECT_ROOT, "05_Quality_Assessment"),
  stats         = file.path(PROJECT_ROOT, "06_Statistical_Analysis"),
  scripts       = file.path(PROJECT_ROOT, "06_Statistical_Analysis", "scripts"),
  stat_data     = file.path(PROJECT_ROOT, "06_Statistical_Analysis", "data"),
  forest        = file.path(PROJECT_ROOT, "06_Statistical_Analysis", "output", "forest_plots"),
  funnel        = file.path(PROJECT_ROOT, "06_Statistical_Analysis", "output", "funnel_plots"),
  sensitivity   = file.path(PROJECT_ROOT, "06_Statistical_Analysis", "output", "sensitivity"),
  subgroup      = file.path(PROJECT_ROOT, "06_Statistical_Analysis", "output", "subgroup"),
  tables        = file.path(PROJECT_ROOT, "06_Statistical_Analysis", "output", "tables"),
  dose_response = file.path(PROJECT_ROOT, "06_Statistical_Analysis", "output", "dose_response"),
  ipd_rmst      = file.path(PROJECT_ROOT, "06_Statistical_Analysis", "output", "ipd_rmst"),
  influence     = file.path(PROJECT_ROOT, "06_Statistical_Analysis", "output", "influence"),
  km_data       = file.path(PROJECT_ROOT, "06_Statistical_Analysis", "data", "km_digitized")
)

# Auto-create all output directories
for (p in PATHS) {
  if (!dir.exists(p)) dir.create(p, recursive = TRUE)
}

# ---- 3. Global Analysis Settings (JNCI Standard) ----
SETTINGS <- list(
  # Statistical model
  method_tau    = "REML",        # Restricted maximum-likelihood for tau^2
  method_ci     = "HK",          # Hartung-Knapp correction (meta 8.x)
  adhoc_hakn    = "ci",           # Anti-conservatism safeguard (expand CI if Q/df<1)
  random        = TRUE,          # Random-effects model (primary)
  fixed         = FALSE,         # Fixed-effect for sensitivity comparison

  # Heterogeneity
  prediction    = TRUE,          # 95% Prediction intervals (JNCI requirement)

  # Effect measures
  hr_method     = "Inverse",     # Inverse-variance for HR pooling
  or_method     = "MH",          # Mantel-Haenszel for OR pooling

  # Publication bias
  funnel_test   = "Egger",       # Egger's test for funnel asymmetry
  trim_fill     = TRUE,          # Duval & Tweedie trim-and-fill

  # Output format (JNCI requirement)
  figure_format = "tiff",
  figure_dpi    = 300,
  figure_width  = 10,            # inches
  figure_height = 8,
  alpha         = 0.05,

  # RMST settings
  rmst_tau_months = c(12, 24, 36, 60),

  # Dose-response settings
  dose_knots    = 3              # RCS knots for non-linear dose-response
)

# ---- 3b. Visual Style (Wong/Okabe-Ito colorblind-safe palette) ----
LANCET_STYLE <- list(
  # Forest plot: Wong blue data elements, near-black diamond
  col_diamond       = LANCET_COLORS[["black"]],    # #1B1919 near-black
  col_diamond_fixed = LANCET_COLORS[["gray"]],     # #ADB6B6 gray
  col_predict       = LANCET_COLORS[["salmon"]],   # #FDAF91 prediction band
  col_square        = LANCET_COLORS[["blue"]],     # #0072B2 Wong blue
  col_ci_line       = LANCET_COLORS[["blue"]],     # #0072B2 Wong blue

  # Funnel plot: Wong blue points
  col_funnel_pt  = LANCET_COLORS[["blue"]],        # #00468B
  col_funnel_bg  = LANCET_COLORS[["gray"]],        # #ADB6B6
  col_contour    = scales::alpha(LANCET_COLORS[["blue"]], c(0.30, 0.18, 0.08)),

  # PRISMA flowchart: subtle phase-coded fills
  col_box        = "white",
  col_box_border = "black",
  col_prisma_fill_id   = scales::alpha(LANCET_COLORS[["blue"]], 0.08),
  col_prisma_fill_excl = scales::alpha(LANCET_COLORS[["red"]], 0.08),
  col_prisma_fill_incl = scales::alpha(LANCET_COLORS[["blue"]], 0.12),

  # NOS stacked bar: Wong colorblind-safe palette domains
  col_nos = c(Selection     = "#0072B2",
              Comparability = "#009E73",
              Outcome       = "#CC79A7"),

  # Forest plot: colour-coded by effect direction (Wong palette)
  col_favors_eopc   = LANCET_COLORS[["blue"]],     # #0072B2 favours EOPC
  col_favors_aopc   = LANCET_COLORS[["red"]],      # #D55E00 favours AOPC
  col_diamond_re    = LANCET_COLORS[["black"]],    # #1B1919 pooled diamond
  col_null_line     = LANCET_COLORS[["gray"]],     # #ADB6B6 null-effect line
  lwd_null          = 1.0,

  # KM curves: unified, EOPC=blue (primary), AOPC=vermillion (comparator)
  col_km_eopc = LANCET_COLORS[["blue"]],           # #0072B2
  col_km_aopc = LANCET_COLORS[["red"]],            # #D55E00

  # Dose-response / bubble plots
  col_bubble     = LANCET_COLORS[["blue"]],        # #00468B
  col_ci_band    = scales::alpha(LANCET_COLORS[["blue"]], 0.15),

  # Convenience aliases
  col_structural = LANCET_COLORS[["gray"]],        # #ADB6B6 all structural elements
  col_emphasis   = LANCET_COLORS[["black"]],       # #1B1919 emphasis text/lines
  col_regression = LANCET_COLORS[["blue"]]         # #00468B regression lines
)

# ---- 4. Helper Functions ----

#' Convert HR and 95% CI to log(HR) and SE(log(HR))
hr_to_log <- function(hr, ci_lower, ci_upper) {
  logHR   <- log(hr)
  seLogHR <- (log(ci_upper) - log(ci_lower)) / (2 * qnorm(0.975))
  return(list(logHR = logHR, seLogHR = seLogHR))
}

#' Clean percentage values from mixed formats
#' Handles: "45.3%", "45.3", "NR", NA, numeric, "71.8% (of resected)"
clean_percentage <- function(x) {
  if (is.numeric(x)) return(ifelse(is.na(x), NA_real_, x))
  x <- as.character(x)
  x <- trimws(x)
  # NR or empty -> NA
  x[x %in% c("NR", "NR ", "N/R", "NA", "N/A", "-", "--", "")] <- NA_character_
  # Remove annotations in parentheses: "71.8% (of resected)" -> "71.8%"
  x <- sub("\\s*\\(.*\\)", "", x)
  # Remove % sign
  x <- sub("%", "", x)
  return(suppressWarnings(as.numeric(x)))
}

#' Normalize age threshold to numeric
#' Handles: "<=50", "<50", "50", "≤50", "50 years"
normalize_age_threshold <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("[<>=\u2264\u2265]", "", x)
  x <- gsub("\\s*years?\\s*", "", x, ignore.case = TRUE)
  return(suppressWarnings(as.numeric(x)))
}

#' Compute event count from percentage and sample size
compute_events <- function(pct, n) {
  ifelse(is.na(pct) | is.na(n), NA_integer_, as.integer(round(pct / 100 * n)))
}

#' Save figure in TIFF + PDF (base R plot expression)
#' NOTE: eval in parent.frame() to access calling function's variables
save_figure <- function(plot_expr, filename,
                        width = SETTINGS$figure_width,
                        height = SETTINGS$figure_height,
                        dpi = SETTINGS$figure_dpi) {
  # Use cairo if available (supports compression); fall back to default
  tiff_args <- list(filename = paste0(filename, ".tiff"), width = width,
                    height = height, units = "in", res = dpi)
  if (capabilities("cairo")) {
    tiff_args$type <- "cairo"
    tiff_args$compression <- "lzw"
  }
  do.call(tiff, tiff_args)
  par(family = LANCET_FONT, bg = "white")
  eval(plot_expr, envir = parent.frame())
  dev.off()

  cairo_pdf(paste0(filename, ".pdf"), width = width, height = height)
  par(family = LANCET_FONT, bg = "white")
  eval(plot_expr, envir = parent.frame())
  dev.off()

  cat("Saved:", basename(filename), ".tiff/.pdf\n")
}

#' Save ggplot object in TIFF + PDF
save_figure_gg <- function(gg_obj, filename,
                           width = SETTINGS$figure_width,
                           height = SETTINGS$figure_height,
                           dpi = SETTINGS$figure_dpi) {
  if (capabilities("cairo")) {
    ggsave(paste0(filename, ".tiff"), plot = gg_obj, width = width,
           height = height, dpi = dpi, compression = "lzw",
           device = grDevices::tiff, type = "cairo")
  } else {
    ggsave(paste0(filename, ".tiff"), plot = gg_obj, width = width,
           height = height, dpi = dpi, device = "tiff")
  }
  ggsave(paste0(filename, ".pdf"), plot = gg_obj, width = width,
         height = height, device = cairo_pdf)
  cat("Saved:", basename(filename), ".tiff/.pdf\n")
}

#' Format p-value for display
format_pval <- function(p) {
  ifelse(p < 0.001, "< 0.001", sprintf("%.3f", p))
}

#' Format HR/OR with 95% CI
format_hr_ci <- function(hr, lo, hi, digits = 2) {
  sprintf(paste0("%.", digits, "f [%.", digits, "f, %.", digits, "f]"),
          hr, lo, hi)
}

#' Safely load RData file
safe_load <- function(filepath, envir = parent.frame()) {
  if (file.exists(filepath)) {
    load(filepath, envir = envir)
    return(TRUE)
  } else {
    cat("NOTE: File not found, skipping:", basename(filepath), "\n")
    return(FALSE)
  }
}

#' JNCI-style ggplot2 theme
lancet_theme <- function(base_size = 10) {
  theme_minimal(base_size = base_size, base_family = LANCET_FONT) +
    theme(
      plot.title    = element_text(face = "bold", size = base_size + 2, colour = "black"),
      plot.title.position = "plot",
      axis.title    = element_text(face = "bold", colour = "black"),
      axis.text     = element_text(colour = "black"),
      axis.line     = element_line(colour = "black", linewidth = 0.4),
      axis.ticks    = element_line(colour = "black", linewidth = 0.3),
      legend.position = "bottom",
      legend.key.size = unit(0.35, "cm"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border     = element_blank(),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.background  = element_rect(fill = "white", colour = NA),
      strip.text = element_text(face = "bold", colour = "black", size = rel(0.95)),
      strip.background = element_rect(fill = "white", colour = NA)
    )
}

#' Set JNCI-standard base R par() for consistent typography
lancet_par <- function(mar = c(5, 4.5, 3, 2), ...) {
  par(family = LANCET_FONT, mar = mar, bg = "white",
      cex.main = 1.0, cex.lab = 1.0, cex.axis = 0.9,
      font.main = 2, col.main = "black", col.lab = "black",
      col.axis = "black", las = 1, lwd = 0.8, tck = -0.015, ...)
}

#' Compute dynamic forest plot dimensions based on number of studies
compute_forest_dims <- function(k) {
  list(
    height   = max(6, 3.5 + k * 0.45),
    fontsize = ifelse(k > 25, 9, 10),
    spacing  = ifelse(k > 25, 1.0, 1.1)
  )
}

#' Return shared JNCI-style meta::forest() aesthetic parameters
lancet_forest_style <- function() {
  list(
    col.diamond        = LANCET_STYLE$col_diamond_re,
    col.diamond.lines  = LANCET_STYLE$col_diamond_re,
    col.diamond.common = LANCET_STYLE$col_diamond_fixed,
    col.predict        = LANCET_STYLE$col_predict,
    col.square         = LANCET_STYLE$col_square,
    col.square.lines   = LANCET_STYLE$col_square,
    print.tau2         = TRUE,
    print.I2           = TRUE,
    print.pval.Q       = TRUE,
    prediction         = TRUE,
    squaresize         = 0.5,
    lwd                = 1.2,
    plotwidth           = grid::unit(8, "cm")
  )
}

# ---- 5. JNCI Publication-Quality Forest Plots (forestploter) ----

#' Create JNCI-style forestploter theme
#' @param base_size Base font size (pt, rendered 1:1 at final canvas width)
#' @param core_padding Row padding for core cells; shrink for print-width (6.75in) figures
#' @param xaxis_cex X-axis label cex multiplier (keep effective size >= 8pt)
lancet_fp_theme <- function(base_size = 11,
                            core_padding = unit(c(4.5, 3.5), "mm"),
                            xaxis_cex = 0.9) {
  forestploter::forest_theme(
    base_size     = base_size,
    base_family   = LANCET_FONT,
    ci_pch        = 15,
    ci_col        = LANCET_COLORS[["blue"]],
    ci_fill       = LANCET_COLORS[["blue"]],
    ci_alpha      = 1,
    ci_lty        = 1,
    ci_lwd        = 1.8,
    ci_Theight    = 0.2,
    refline_gp    = gpar(lty = "dashed", lwd = 1, col = LANCET_STYLE$col_structural),
    summary_fill  = LANCET_COLORS[["black"]],
    summary_col   = LANCET_COLORS[["black"]],
    footnote_gp   = gpar(cex = 0.75, fontface = "italic", col = "black"),
    arrow_type    = "closed",
    arrow_label_just = "end",
    title_just    = "left",
    title_gp      = gpar(cex = 1.15, fontface = "bold", col = "black"),
    xaxis_gp      = gpar(cex = xaxis_cex, lwd = 0.6),
    core = list(
      bg_params = list(fill = c("white", "#F8F9FA")),
      padding   = core_padding
    ),
    colhead = list(
      bg_params = list(fill = "white"),
      padding   = core_padding
    )
  )
}

#' Build JNCI-style forest plot from meta object using forestploter
#' @param m meta object (metagen or metabin)
#' @param outcome_label Title for the plot
#' @param sort_year Numeric vector of publication years (same length as k) for sorting
#' @param show_prediction Show prediction interval row
#' @param base_size Override base font pt (NULL = default 11; use ~9 for 6.75in print-width figures)
#' @param blank_width Override CI-column blank width (NULL = 22 binary / 42 HR; use ~26 for print-width HR)
#' @param core_padding Override cell padding (grid::unit; NULL = theme default)
#' @param width Override canvas width in inches (NULL = auto)
#' @param height Override canvas height in inches (NULL = auto)
#' @return list(plot = grob, width = inches, height = inches)
lancet_fp_forest <- function(m,
                              outcome_label = "",
                              sort_year = NULL,
                              show_prediction = TRUE,
                              base_size = NULL,
                              blank_width = NULL,
                              core_padding = NULL,
                              width = NULL,
                              height = NULL) {

  k <- m$k
  is_bin <- inherits(m, "metabin")
  sm <- m$sm  # "HR" or "OR"

  # Sort by year if provided
  idx <- seq_len(k)
  if (!is.null(sort_year) && length(sort_year) == k) {
    idx <- order(sort_year)
  }

  # Extract study data (sorted)
  labs  <- m$studlab[idx]
  te_i  <- m$TE[idx]
  lo_i  <- m$lower[idx]
  hi_i  <- m$upper[idx]
  w_i   <- m$w.random[idx]

  # Back-transform to original scale
  est   <- exp(te_i)
  lo    <- exp(lo_i)
  hi    <- exp(hi_i)

  # Weights
  w_pct <- sprintf("%.1f", 100 * w_i / sum(w_i))

  # CI sizes proportional to weight
  sizes <- 0.3 + 0.6 * (w_i / max(w_i))

  # Format effect text (replace NA with "NC" for non-calculable ORs)
  ci_txt <- ifelse(is.na(est) | is.na(lo) | is.na(hi),
                   "NC",
                   sprintf("%.2f [%.2f, %.2f]", est, lo, hi))

  # CI column placeholder (wider for HR plots to avoid arrow label overlap)
  blank_w <- if (is.null(blank_width)) (if (is_bin) 22 else 42) else blank_width
  blank <- paste(rep(" ", blank_w), collapse = "")

  # Build data frame based on outcome type
  if (is_bin) {
    ev_e <- m$event.e[idx]; n_e <- m$n.e[idx]
    ev_c <- m$event.c[idx]; n_c <- m$n.c[idx]

    # Format Events/N, replacing NA with "NR"
    fmt_en <- function(ev, n) {
      ifelse(is.na(ev) | is.na(n), "NR", sprintf("%d/%d", ev, n))
    }

    dt <- data.frame(
      Study              = labs,
      `EOPC\nEvents/N`   = fmt_en(ev_e, n_e),
      `AOPC\nEvents/N`   = fmt_en(ev_c, n_c),
      ` `                = blank,
      `OR [95% CI]`      = ci_txt,
      `Weight\n(%)`      = w_pct,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    ci_col <- 4
    eff_col_name <- "OR [95% CI]"
  } else {
    dt <- data.frame(
      Study              = labs,
      ` `                = blank,
      `HR [95% CI]`      = ci_txt,
      `Weight\n(%)`      = w_pct,
      check.names = FALSE, stringsAsFactors = FALSE
    )
    ci_col <- 2
    eff_col_name <- "HR [95% CI]"
  }

  # Separator row
  sep <- dt[1, , drop = FALSE]; sep[1, ] <- ""

  # Random-effects summary
  re_est <- exp(m$TE.random)
  re_lo  <- exp(m$lower.random)
  re_hi  <- exp(m$upper.random)
  re_txt <- sprintf("%.2f [%.2f, %.2f]", re_est, re_lo, re_hi)
  het_txt <- sprintf("I2=%.1f%%, tau2=%.4f, p=%s",
                     m$I2 * 100, m$tau2, format_pval(m$pval.Q))

  re_row <- sep
  re_row$Study <- paste0("Random effects model")
  re_row[[eff_col_name]] <- re_txt

  # Prediction interval
  pi_row <- NULL
  pi_vals <- c(NA_real_, NA_real_, NA_real_)
  if (show_prediction && !is.null(m$lower.predict) && k >= 5) {
    pi_lo_v <- exp(m$lower.predict)
    pi_hi_v <- exp(m$upper.predict)
    pi_row <- sep
    pi_row$Study <- "Prediction interval"
    pi_row[[eff_col_name]] <- sprintf("[%.2f, %.2f]", pi_lo_v, pi_hi_v)
    pi_vals <- c(NA_real_, pi_lo_v, pi_hi_v)
  }

  # Assemble all rows: studies + sep + RE + PI
  dt_all <- rbind(dt, sep, re_row)
  if (!is.null(pi_row)) dt_all <- rbind(dt_all, pi_row)

  # Numeric vectors for est/lower/upper
  est_v  <- c(est, NA, re_est)
  lo_v   <- c(lo,  NA, re_lo)
  hi_v   <- c(hi,  NA, re_hi)
  sz_v   <- c(sizes, NA, NA)
  is_sum <- c(rep(FALSE, k), FALSE, TRUE)

  if (!is.null(pi_row)) {
    est_v  <- c(est_v, pi_vals[1])
    lo_v   <- c(lo_v, pi_vals[2])
    hi_v   <- c(hi_v, pi_vals[3])
    sz_v   <- c(sz_v, NA)
    is_sum <- c(is_sum, TRUE)
  }

  # X-axis ticks
  all_ci <- c(lo_v, hi_v)
  all_ci <- all_ci[!is.na(all_ci) & is.finite(all_ci)]
  if (sm == "HR") {
    candidate_ticks <- c(0.1, 0.2, 0.25, 0.5, 1, 2, 4, 5, 10)
  } else {
    candidate_ticks <- c(0.05, 0.1, 0.2, 0.25, 0.5, 1, 2, 4, 5, 8, 10, 15)
  }
  ci_min <- min(all_ci) * 0.85
  ci_max <- max(all_ci) * 1.15
  ticks <- candidate_ticks[candidate_ticks >= ci_min & candidate_ticks <= ci_max]
  if (!1 %in% ticks) ticks <- sort(c(ticks, 1))
  if (length(ticks) < 3) ticks <- c(0.5, 1, 2)

  xlim_range <- c(min(ticks) * 0.75, max(ticks) * 1.25)

  # Bold study label for summary rows
  n_all <- nrow(dt_all)
  for (i in seq_len(n_all)) {
    if (is_sum[i]) {
      dt_all$Study[i] <- dt_all$Study[i]  # forestploter handles bold via is_summary
    }
  }

  # Heterogeneity info as data table rows (avoids footnote overlap with x-axis)
  het_row1 <- sep
  het_row1$Study <- sprintf("Heterogeneity: I\u00B2 = %.1f%%, \u03C4\u00B2 = %.4f",
                            m$I2 * 100, m$tau2)
  het_row2 <- sep
  het_row2$Study <- sprintf("Q = %.2f (df = %d), p = %s",
                            m$Q, m$df.Q, format_pval(m$pval.Q))
  dt_all <- rbind(dt_all, het_row1, het_row2)
  est_v  <- c(est_v, NA, NA)
  lo_v   <- c(lo_v,  NA, NA)
  hi_v   <- c(hi_v,  NA, NA)
  sz_v   <- c(sz_v,  NA, NA)
  is_sum <- c(is_sum, FALSE, FALSE)

  # Create forestploter plot
  p <- forestploter::forest(
    data       = dt_all,
    est        = est_v,
    lower      = lo_v,
    upper      = hi_v,
    sizes      = sz_v,
    is_summary = is_sum,
    ci_column  = ci_col,
    ref_line   = 1,
    x_trans    = "log",
    xlim       = xlim_range,
    ticks_at   = ticks,
    arrow_lab  = c("\u2190 EOPC", "AOPC \u2192"),
    xlab       = sm,
    title      = outcome_label,
    theme      = if (is.null(base_size) && is.null(core_padding)) {
      lancet_fp_theme()
    } else {
      lancet_fp_theme(base_size = if (is.null(base_size)) 11 else base_size,
                      core_padding = if (is.null(core_padding)) unit(c(4.5, 3.5), "mm") else core_padding)
    }
  )

  # Recommended dimensions (extra height for footer: arrows + footnote)
  rec_height <- if (is.null(height)) max(5, 2.8 + k * 0.32) else height
  rec_width  <- if (is.null(width)) (if (is_bin) 13 else 11) else width

  return(list(plot = p, width = rec_width, height = rec_height))
}

#' Build JNCI-style subgroup forest plot from meta subgroup object
#' @param m_sub meta object with subgroup (from update(m, subgroup = ...))
#' @param outcome_label Title
#' @param subgroup_label Subgroup moderator name
#' @param sort_year Numeric vector for sorting
#' @return list(plot, width, height)
lancet_fp_subgroup <- function(m_sub,
                                outcome_label = "",
                                subgroup_label = "",
                                sort_year = NULL) {

  k <- m_sub$k
  sm <- m_sub$sm
  is_bin <- inherits(m_sub, "metabin")

  # Sort
  idx <- seq_len(k)
  if (!is.null(sort_year) && length(sort_year) == k) {
    idx <- order(sort_year)
  }

  labs  <- m_sub$studlab[idx]
  te_i  <- m_sub$TE[idx]
  lo_i  <- m_sub$lower[idx]
  hi_i  <- m_sub$upper[idx]
  w_i   <- m_sub$w.random[idx]
  sg_i  <- m_sub$subgroup[idx]

  est   <- exp(te_i)
  lo    <- exp(lo_i)
  hi    <- exp(hi_i)
  w_pct <- sprintf("%.1f", 100 * w_i / sum(w_i))
  sizes <- 0.3 + 0.6 * (w_i / max(w_i))
  ci_txt <- ifelse(is.na(est) | is.na(lo) | is.na(hi),
                   "NC",
                   sprintf("%.2f [%.2f, %.2f]", est, lo, hi))

  blank_width <- if (is_bin) 22 else 42
  blank <- paste(rep(" ", blank_width), collapse = "")

  # Helper: format Events/N, replacing NA with "NR"
  fmt_en_sg <- function(ev, n) {
    ifelse(is.na(ev) | is.na(n), "NR", sprintf("%d/%d", ev, n))
  }

  # Build per-subgroup structure
  sg_levels <- unique(sg_i[!is.na(sg_i)])
  all_rows <- list()
  est_v <- numeric(0); lo_v <- numeric(0); hi_v <- numeric(0)
  sz_v <- numeric(0); is_sum <- logical(0)

  for (sg_level in sg_levels) {
    sg_idx <- which(sg_i == sg_level)
    sg_k <- length(sg_idx)

    # Subgroup header
    if (is_bin) {
      hdr <- data.frame(Study = paste0("Subgroup: ", sg_level),
                         `EOPC\nEvents/N` = "", `AOPC\nEvents/N` = "",
                         ` ` = blank, `OR [95% CI]` = "", `Weight\n(%)` = "",
                         check.names = FALSE, stringsAsFactors = FALSE)
    } else {
      hdr <- data.frame(Study = paste0("Subgroup: ", sg_level),
                         ` ` = blank, `HR [95% CI]` = "", `Weight\n(%)` = "",
                         check.names = FALSE, stringsAsFactors = FALSE)
    }
    all_rows[[length(all_rows) + 1]] <- hdr
    est_v <- c(est_v, NA); lo_v <- c(lo_v, NA); hi_v <- c(hi_v, NA)
    sz_v <- c(sz_v, NA); is_sum <- c(is_sum, TRUE)

    # Study rows
    for (j in sg_idx) {
      if (is_bin) {
        row <- data.frame(Study = paste0("  ", labs[j]),
                           `EOPC\nEvents/N` = fmt_en_sg(m_sub$event.e[idx[j]], m_sub$n.e[idx[j]]),
                           `AOPC\nEvents/N` = fmt_en_sg(m_sub$event.c[idx[j]], m_sub$n.c[idx[j]]),
                           ` ` = blank, `OR [95% CI]` = ci_txt[j], `Weight\n(%)` = w_pct[j],
                           check.names = FALSE, stringsAsFactors = FALSE)
      } else {
        row <- data.frame(Study = paste0("  ", labs[j]),
                           ` ` = blank, `HR [95% CI]` = ci_txt[j], `Weight\n(%)` = w_pct[j],
                           check.names = FALSE, stringsAsFactors = FALSE)
      }
      all_rows[[length(all_rows) + 1]] <- row
      est_v <- c(est_v, est[j]); lo_v <- c(lo_v, lo[j]); hi_v <- c(hi_v, hi[j])
      sz_v <- c(sz_v, sizes[j]); is_sum <- c(is_sum, FALSE)
    }

    # Subgroup subtotal
    sg_te <- m_sub$TE.random.w[sg_level]
    sg_lo <- m_sub$lower.random.w[sg_level]
    sg_hi <- m_sub$upper.random.w[sg_level]
    if (!is.na(sg_te)) {
      sg_est <- exp(sg_te); sg_lo_e <- exp(sg_lo); sg_hi_e <- exp(sg_hi)
      eff_col <- if (is_bin) "OR [95% CI]" else "HR [95% CI]"
      if (is_bin) {
        sub_row <- data.frame(Study = paste0("  Subtotal (k=", sg_k, ")"),
                               `EOPC\nEvents/N` = "", `AOPC\nEvents/N` = "",
                               ` ` = blank,
                               `OR [95% CI]` = sprintf("%.2f [%.2f, %.2f]", sg_est, sg_lo_e, sg_hi_e),
                               `Weight\n(%)` = "",
                               check.names = FALSE, stringsAsFactors = FALSE)
      } else {
        sub_row <- data.frame(Study = paste0("  Subtotal (k=", sg_k, ")"),
                               ` ` = blank,
                               `HR [95% CI]` = sprintf("%.2f [%.2f, %.2f]", sg_est, sg_lo_e, sg_hi_e),
                               `Weight\n(%)` = "",
                               check.names = FALSE, stringsAsFactors = FALSE)
      }
      all_rows[[length(all_rows) + 1]] <- sub_row
      est_v <- c(est_v, sg_est); lo_v <- c(lo_v, sg_lo_e); hi_v <- c(hi_v, sg_hi_e)
      sz_v <- c(sz_v, NA); is_sum <- c(is_sum, TRUE)
    }
  }

  # Overall random-effects
  re_est <- exp(m_sub$TE.random)
  re_lo  <- exp(m_sub$lower.random)
  re_hi  <- exp(m_sub$upper.random)
  re_txt <- sprintf("%.2f [%.2f, %.2f]", re_est, re_lo, re_hi)
  het_txt <- sprintf("I2=%.1f%%", m_sub$I2 * 100)
  eff_col <- if (is_bin) "OR [95% CI]" else "HR [95% CI]"

  # Separator
  if (is_bin) {
    sep <- data.frame(Study = "", `EOPC\nEvents/N` = "", `AOPC\nEvents/N` = "",
                       ` ` = blank, `OR [95% CI]` = "", `Weight\n(%)` = "",
                       check.names = FALSE, stringsAsFactors = FALSE)
  } else {
    sep <- data.frame(Study = "", ` ` = blank, `HR [95% CI]` = "", `Weight\n(%)` = "",
                       check.names = FALSE, stringsAsFactors = FALSE)
  }
  all_rows[[length(all_rows) + 1]] <- sep
  est_v <- c(est_v, NA); lo_v <- c(lo_v, NA); hi_v <- c(hi_v, NA)
  sz_v <- c(sz_v, NA); is_sum <- c(is_sum, FALSE)

  # Overall row
  overall <- sep
  overall$Study <- paste0("Overall (", het_txt, ")")
  overall[[eff_col]] <- re_txt
  all_rows[[length(all_rows) + 1]] <- overall
  est_v <- c(est_v, re_est); lo_v <- c(lo_v, re_lo); hi_v <- c(hi_v, re_hi)
  sz_v <- c(sz_v, NA); is_sum <- c(is_sum, TRUE)

  # Subgroup test info as data table row (avoids footnote overlap with x-axis)
  test_row <- sep
  test_row$Study <- sprintf("Test for subgroup differences: Q=%.2f, df=%d, p=%s",
                            m_sub$Q.b.random, m_sub$df.Q.b, format_pval(m_sub$pval.Q.b.random))
  all_rows[[length(all_rows) + 1]] <- test_row
  est_v <- c(est_v, NA); lo_v <- c(lo_v, NA); hi_v <- c(hi_v, NA)
  sz_v <- c(sz_v, NA); is_sum <- c(is_sum, FALSE)

  dt_all <- do.call(rbind, all_rows)
  ci_col <- if (is_bin) 4 else 2

  # X-axis
  all_ci <- c(lo_v, hi_v)
  all_ci <- all_ci[!is.na(all_ci) & is.finite(all_ci)]
  ticks <- c(0.1, 0.2, 0.25, 0.5, 1, 2, 4, 5, 8, 10)
  ci_min <- min(all_ci) * 0.75; ci_max <- max(all_ci) * 1.25
  ticks <- ticks[ticks >= ci_min & ticks <= ci_max]
  if (!1 %in% ticks) ticks <- sort(c(ticks, 1))
  if (length(ticks) < 3) ticks <- c(0.5, 1, 2)

  p <- forestploter::forest(
    data       = dt_all,
    est        = est_v,
    lower      = lo_v,
    upper      = hi_v,
    sizes      = sz_v,
    is_summary = is_sum,
    ci_column  = ci_col,
    ref_line   = 1,
    x_trans    = "log",
    xlim       = c(min(ticks) * 0.75, max(ticks) * 1.25),
    ticks_at   = ticks,
    arrow_lab  = c("\u2190 EOPC", "AOPC \u2192"),
    xlab       = sm,
    title      = paste0(outcome_label, " by ", subgroup_label),
    theme      = lancet_fp_theme()
  )

  n_all <- nrow(dt_all)
  rec_height <- max(5, 2.8 + n_all * 0.30)
  rec_width  <- if (is_bin) 14 else 11

  return(list(plot = p, width = rec_width, height = rec_height))
}

#' Save forestploter plot to TIFF + PDF
#' @param use_autofit TRUE shrinks columns to content (legacy default); FALSE keeps
#'   the designed blank-space CI column width (use for print-width 6.75in figures)
save_fp_figure <- function(fp_result, filename,
                            width = NULL, height = NULL,
                            dpi = SETTINGS$figure_dpi,
                            use_autofit = TRUE) {
  p <- fp_result$plot
  w <- if (!is.null(width)) width else fp_result$width
  h <- if (!is.null(height)) height else fp_result$height

  # TIFF
  tiff_args <- list(filename = paste0(filename, ".tiff"),
                    width = w, height = h, units = "in", res = dpi)
  if (capabilities("cairo")) {
    tiff_args$type <- "cairo"
    tiff_args$compression <- "lzw"
  }
  do.call(tiff, tiff_args)
  if (use_autofit) plot(p, autofit = TRUE) else plot(p)
  dev.off()

  # PDF
  cairo_pdf(paste0(filename, ".pdf"), width = w, height = h)
  if (use_autofit) plot(p, autofit = TRUE) else plot(p)
  dev.off()

  cat("Saved:", basename(filename), ".tiff/.pdf\n")
}

#' JNCI-style ggplot2 funnel plot with non-overlapping labels
lancet_gg_funnel <- function(m, outcome_label = "", show_labels = TRUE,
                              egger_p = NULL, begg_p = NULL) {
  sm <- m$sm
  sm_label <- ifelse(sm == "HR", "Hazard Ratio", "Odds Ratio")

  te <- m$TE
  se <- m$seTE
  study_labs <- m$studlab
  re_te <- m$TE.random

  # Remove NA/Inf values
  valid <- is.finite(te) & is.finite(se) & se > 0
  te <- te[valid]; se <- se[valid]; study_labs <- study_labs[valid]

  df <- data.frame(
    study  = study_labs,
    te     = te,
    se     = se,
    exp_te = exp(te),
    stringsAsFactors = FALSE
  )

  # Funnel boundaries (pseudo 95% CI)
  se_max <- max(se) * 1.1
  se_seq <- seq(0, se_max, length.out = 200)
  funnel_lo <- exp(re_te - 1.96 * se_seq)
  funnel_hi <- exp(re_te + 1.96 * se_seq)

  funnel_poly <- data.frame(
    x = c(exp(re_te), funnel_lo[length(funnel_lo)], funnel_hi[length(funnel_hi)]),
    y = c(0, se_max, se_max)
  )

  # Build annotation text
  ann_parts <- c()
  if (!is.null(egger_p)) ann_parts <- c(ann_parts, paste0("Egger p = ", format_pval(egger_p)))
  if (!is.null(begg_p))  ann_parts <- c(ann_parts, paste0("Begg p = ", format_pval(begg_p)))
  ann_text <- if (length(ann_parts) > 0) paste(ann_parts, collapse = "    ") else NULL

  p <- ggplot(df, aes(x = exp_te, y = se)) +
    geom_polygon(data = funnel_poly, aes(x = x, y = y),
                 fill = LANCET_STYLE$col_ci_band, colour = LANCET_STYLE$col_structural,
                 linetype = "dashed", linewidth = 0.4) +
    geom_vline(xintercept = exp(re_te), linetype = "solid",
               colour = LANCET_COLORS[["crimson"]], linewidth = 0.5) +
    geom_vline(xintercept = 1, linetype = "dotted",
               colour = LANCET_STYLE$col_structural, linewidth = 0.3) +
    geom_point(shape = 19, size = 2, colour = LANCET_STYLE$col_funnel_pt) +
    scale_x_continuous(trans = "log10", name = sm_label) +
    scale_y_reverse(name = "Standard Error",
                    expand = expansion(mult = c(0.05, 0.02))) +
    labs(title = outcome_label) +
    lancet_theme(base_size = 10) +
    theme(
      plot.title = element_text(size = 11, face = "bold"),
      panel.grid.minor = element_blank()
    )

  if (show_labels) {
    p <- p + ggrepel::geom_text_repel(
      aes(label = study), size = 2.5, colour = LANCET_STYLE$col_emphasis,
      max.overlaps = 25, segment.colour = LANCET_STYLE$col_structural,
      segment.size = 0.25, box.padding = 0.3, point.padding = 0.2,
      min.segment.length = 0.3, force = 2, seed = 42
    )
  }

  if (!is.null(ann_text)) {
    p <- p + annotate("text", x = max(df$exp_te) * 0.9,
                       y = se_max * 0.95,
                       label = ann_text, size = 3, fontface = "italic",
                       colour = LANCET_STYLE$col_emphasis, hjust = 1)
  }

  return(p)
}

#' JNCI-style contour-enhanced funnel plot (ggplot2)
lancet_gg_contour_funnel <- function(m, outcome_label = "") {
  sm <- m$sm
  sm_label <- ifelse(sm == "HR", "Hazard Ratio", "Odds Ratio")

  te <- m$TE
  se <- m$seTE
  re_te <- m$TE.random

  # Remove NA/Inf values
  valid <- is.finite(te) & is.finite(se) & se > 0
  te <- te[valid]; se <- se[valid]

  df <- data.frame(te = te, se = se, exp_te = exp(te), stringsAsFactors = FALSE)

  se_max <- max(se, na.rm = TRUE) * 1.1
  if (!is.finite(se_max) || se_max <= 0) return(NULL)
  se_seq <- seq(0.001, se_max, length.out = 200)

  # Contour polygons for significance regions (wider = less significant)
  contour_polys <- list()
  fills <- c("p < 0.01" = LANCET_STYLE$col_contour[1],
             "p < 0.05" = LANCET_STYLE$col_contour[2],
             "p < 0.1"  = LANCET_STYLE$col_contour[3])
  for (level in c(0.10, 0.05, 0.01)) {
    z <- qnorm(1 - level / 2)
    lo <- exp(re_te - z * se_seq)
    hi <- exp(re_te + z * se_seq)
    # Build polygon: top-left (lo at se_max) -> apex -> top-right (hi at se_max) -> back
    poly_df <- data.frame(
      x = c(lo, rev(hi)),
      y = c(se_seq, rev(se_seq)),
      level = paste0("p < ", level),
      stringsAsFactors = FALSE
    )
    contour_polys[[length(contour_polys) + 1]] <- poly_df
  }
  contour_all <- do.call(rbind, contour_polys)
  contour_all$level <- factor(contour_all$level,
                               levels = c("p < 0.1", "p < 0.05", "p < 0.01"))

  p <- ggplot(df, aes(x = exp_te, y = se)) +
    geom_polygon(data = contour_all, aes(x = x, y = y, fill = level),
                 colour = NA, inherit.aes = FALSE, alpha = 0.9) +
    scale_fill_manual(values = fills, name = "Significance") +
    geom_vline(xintercept = exp(re_te), colour = LANCET_COLORS[["crimson"]],
               linewidth = 0.5) +
    geom_vline(xintercept = 1, linetype = "dotted",
               colour = LANCET_STYLE$col_structural, linewidth = 0.3) +
    geom_point(shape = 19, size = 2, colour = LANCET_STYLE$col_funnel_pt) +
    scale_x_continuous(trans = "log10", name = sm_label) +
    scale_y_reverse(name = "Standard Error",
                    expand = expansion(mult = c(0.05, 0.02))) +
    labs(title = outcome_label) +
    lancet_theme(base_size = 10) +
    theme(plot.title = element_text(size = 11, face = "bold"),
          panel.grid.minor = element_blank(),
          legend.position = "bottom")

  return(p)
}

# ---- 6. Startup Message ----
cat("\n========================================\n")
cat("EOPC Meta-Analysis Configuration\n")
cat("========================================\n")
cat("Project root:", PROJECT_ROOT, "\n")
cat("Tau^2 estimator:", SETTINGS$method_tau, "\n")
cat("CI method:", SETTINGS$method_ci, "(Hartung-Knapp)\n")
cat("Prediction intervals:", SETTINGS$prediction, "\n")
cat("Figure format:", SETTINGS$figure_format, "@", SETTINGS$figure_dpi, "DPI\n")
cat("Forest engine: forestploter (table-based layout)\n")
cat("========================================\n")
