################################################################################
# EOPC vs AOPC Systematic Review and Meta-Analysis
# Script 07: PRISMA 2020 Flow Diagram (V4 - Lancet Submission-Ready)
# Input:  Hard-coded verified PRISMA numbers
# Output: PRISMA flow diagram (TIFF 300 DPI + PDF, embedded fonts)
#
# Design: JNCI print-size submission polish (2026-08-24)
#   - Sized at FINAL PRINT WIDTH 6.75 in (171 mm, OUP two-column max)
#     so no downscaling occurs at typesetting -> fonts print 1:1
#   - Unified BASE_SIZE = 10 pt font system (.pt conversion), min 8 pt
#   - Wong/Lancet blue/red colour-coded box fills (subtle tints)
#   - Focal inversion on final inclusion box (solid Lancet blue + white text)
#   - ggtext::geom_richtext for HTML formatting (vectorized)
#   - Orthogonal merge routing with small arrow heads
#   - ragg backend for high-quality anti-aliased output
#   - 6.75 x 9.3 inch portrait format, 300 DPI
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
suppressPackageStartupMessages({
  library(scales)
  library(ragg)
})

# ==============================================================================
# PRISMA 2020 Data (Verified - AGENTS.md)
# ==============================================================================

prisma <- list(
  db_records     = 1786,
  db_screened    = 1786,
  db_excluded    = 1712,
  db_ft_assessed = 74,
  db_ft_excluded = 28,
  db_included    = 46,
  excl_F2 = 6,   # Wrong population/comparison
  excl_F6 = 3,   # Insufficient data
  excl_F8 = 6,   # Other reasons
  excl_extraction = 13,  # Excluded during data extraction
  other_records    = 5,
  other_assessed   = 5,
  other_excluded   = 3,
  other_included   = 2,
  total_included   = 48
)

# ==============================================================================
# Generate PRISMA 2020 Flow Diagram
# ==============================================================================

generate_prisma_2020 <- function(nums) {
  prisma_file <- file.path(PATHS$tables, "PRISMA_2020_flowchart")

  # ---- Typography (unified .pt conversion; min 8 pt at 1:1 print scale) ----
  BASE_SIZE  <- 10
  SMALL_SIZE <- 8.5
  COUNT_SIZE <- 11
  LINE_HEIGHT <- 1.20

  # Number formatting helper
  fmt_num <- function(x) scales::comma(x, accuracy = 1)

  # ---- Colour palette ----
  # Refined, print-friendly palette: deep navy + warm terracotta accent
  c_navy     <- "#1A2F4A"                    # Primary structure / borders
  c_navy_light <- "#4A6580"                  # Secondary text
  c_fill_inc <- "#F7F9FB"                    # Very subtle blue-gray fill
  c_fill_exc <- "#FBF0ED"                    # Pale terracotta fill
  c_excl_bdr <- "#A84A3D"                    # Muted terracotta border
  c_arrow    <- "#7F8C8D"                    # Cool grey arrows
  c_phase    <- "#5D6D7E"                    # Phase labels
  c_black    <- "#1B1919"
  c_darkgray <- "#2C3E50"                    # Exclusion detail text

  # ---- Box layout (relative 0-100 grid) ----
  box_df <- data.frame(
    id   = 1:10,
    xmin = c(  5,  70,   5,  38,   5,  38,  71,   5,  70,  20),
    xmax = c( 35,  95,  35,  70,  35,  71,  95,  35,  95,  80),
    ymin = c( 86,  86,  70,  70,  52,  42,  52,  28,  28,   5),
    ymax = c( 96,  96,  78,  78,  60,  70,  60,  36,  36,  17),
    fill = c(
      c_fill_inc, c_fill_inc,     # Identification
      c_fill_inc, c_fill_exc,     # Screening
      c_fill_inc, c_fill_exc,     # Eligibility
      c_fill_inc,                  # Other assessed
      c_fill_inc, c_fill_inc,     # Included
      c_navy                       # Final total (focal inversion)
    ),
    border = c(
      rep(c_navy, 3), c_excl_bdr,
      c_navy, c_excl_bdr,
      c_navy,
      rep(c_navy, 3)
    ),
    lwd = c(rep(0.35, 9), 0),
    text_col = c(rep(c_black, 9), "white"),
    hjust = c(0.5, 0.5, 0.5, 0.5, 0.5, 0, 0.5, 0.5, 0.5, 0.5),
    xpos  = c( NA,  NA,  NA,  NA,  NA, 40,  NA,  NA,  NA,  NA),
    stringsAsFactors = FALSE
  )
  box_df$xmid <- (box_df$xmin + box_df$xmax) / 2
  box_df$ymid <- (box_df$ymin + box_df$ymax) / 2
  box_df$xpos <- ifelse(is.na(box_df$xpos), box_df$xmid, box_df$xpos)

  # ---- Text labels (HTML formatting via ggtext) ----
  box_df$label <- c(
    # Box 1: Database identification
    paste0("<b>Records identified</b>",
           "<br><b>from databases</b>",
           "<br><span style='color:", c_navy_light, "; font-size:", SMALL_SIZE, "pt;'>5 databases</span>",
           "<br><span style='font-size:", COUNT_SIZE, "pt;'><b>n = ", fmt_num(nums$db_records), "</b></span>"),
    # Box 2: Other methods
    paste0("<b>Records from</b>",
           "<br><b>other methods</b>",
           "<br><span style='color:", c_navy_light, "; font-size:", SMALL_SIZE, "pt;'>reference tracing</span>",
           "<br><span style='font-size:", COUNT_SIZE, "pt;'><b>n = ", nums$other_records, "</b></span>"),
    # Box 3: Screened
    paste0("<b>Records screened</b>",
           "<br><span style='font-size:", COUNT_SIZE, "pt;'><b>n = ", fmt_num(nums$db_screened), "</b></span>"),
    # Box 4: Excluded
    paste0("<span style='color:", c_excl_bdr, ";'><b>Records excluded</b></span>",
           "<br><span style='font-size:", COUNT_SIZE, "pt;'><b>n = ", fmt_num(nums$db_excluded), "</b></span>"),
    # Box 5: Full-text assessed
    paste0("<b>Full-text articles</b>",
           "<br><b>assessed for eligibility</b>",
           "<br><span style='font-size:", COUNT_SIZE, "pt;'><b>n = ", nums$db_ft_assessed, "</b></span>"),
    # Box 6: Full-text excluded (LEFT-ALIGNED, bullet list, dark-gray detail text)
    paste0("<span style='color:", c_excl_bdr, ";'><b>Full-text articles</b></span><br>",
           "<span style='color:", c_excl_bdr, ";'><b>excluded (n = ",
           nums$db_ft_excluded, ")</b></span><br>",
           "<span style='font-size:", SMALL_SIZE, "pt; color:", c_darkgray, ";'>",
           "&#8226; No age comparison: <b>", nums$excl_F2, "</b><br>",
           "&#8226; Insufficient data: <b>", nums$excl_F6, "</b><br>",
           "&#8226; Other reasons: <b>", nums$excl_F8, "</b><br>",
           "&#8226; Extraction excluded: <b>", nums$excl_extraction, "</b>",
           "<br>&nbsp;&nbsp;&#8211; Incompatible cutoff: <b>3</b>",
           "<br>&nbsp;&nbsp;&#8211; Incomplete data: <b>10</b>",
           "</span>"),
    # Box 7: Other methods assessed
    paste0("<b>Reports assessed</b>",
           "<br><span style='font-size:", COUNT_SIZE, "pt;'><b>n = ", nums$other_assessed, "</b></span>",
           "<br><span style='color:", c_excl_bdr, "; font-size:", SMALL_SIZE, "pt;'>Excluded: ",
           nums$other_excluded, "</span>"),
    # Box 8: Included from databases
    paste0("<b>Studies included</b>",
           "<br><b>from databases</b>",
           "<br><span style='font-size:", COUNT_SIZE, "pt;'><b>n = ", nums$db_included, "</b></span>"),
    # Box 9: Included from other methods
    paste0("<b>Studies included</b>",
           "<br><b>from other methods</b>",
           "<br><span style='font-size:", COUNT_SIZE, "pt;'><b>n = ", nums$other_included, "</b></span>"),
    # Box 10: Final total (focal inversion - white text on deep navy)
    paste0("<span style='font-size:", BASE_SIZE, "pt;'><b>Total studies included</b></span><br>",
           "<span style='color:#B0C4DE; font-size:", SMALL_SIZE, "pt;'>systematic review and meta-analysis</span>",
           "<br><br><b style='font-size:", COUNT_SIZE * 1.3, "pt;'>n = ", nums$total_included, "</b>",
           "<br><span style='color:#B0C4DE; font-size:", SMALL_SIZE, "pt;'>N = 1,659,432 patients</span>")
  )

  # ---- Routing: arrows (with arrowheads) ----
  arrows_df <- data.frame(
    x    = c(20, 20, 20, 35, 35, 85, 85, 52),
    y    = c(86, 70, 52, 74, 56, 86, 52, 22),
    xend = c(20, 20, 20, 38, 38, 85, 85, 52),
    yend = c(78, 60, 36, 74, 56, 60, 36, 17),
    stringsAsFactors = FALSE
  )

  # Routing: orthogonal merge lines (no arrowheads)
  lines_df <- data.frame(
    x    = c(20, 20, 85, 85),
    xend = c(20, 40, 85, 65),
    y    = c(28, 22, 28, 22),
    yend = c(22, 22, 22, 22),
    stringsAsFactors = FALSE
  )

  # ---- Phase labels ----
  phase_df <- data.frame(
    y = c(91, 74, 56, 22),
    label = c("IDENTIFICATION", "SCREENING", "ELIGIBILITY", "INCLUSION"),
    stringsAsFactors = FALSE
  )

  # ---- Build plot ----
  p <- ggplot() +
    # Left divider line (subtle)
    annotate("segment", x = 3, xend = 3, y = 2, yend = 98,
             colour = "#E2E8F0", linewidth = 0.3) +

    # Phase labels (rotated, in left margin)
    geom_text(data = phase_df,
              aes(x = 1, y = y, label = label),
              angle = 90, family = LANCET_FONT, size = BASE_SIZE / .pt,
              fontface = "plain", colour = c_phase, hjust = 0.5) +

    # Orthogonal merge lines (no arrows, drawn first)
    geom_segment(data = lines_df,
                 aes(x = x, y = y, xend = xend, yend = yend),
                 linewidth = 0.3, colour = c_arrow,
                 lineend = "square") +

    # Arrow segments (closed, small head)
    geom_segment(data = arrows_df,
                 aes(x = x, y = y, xend = xend, yend = yend),
                 linewidth = 0.35, colour = c_arrow,
                 arrow = arrow(length = unit(0.14, "cm"), type = "closed")) +

    # Boxes
    geom_rect(data = box_df,
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                  fill = fill, colour = border, linewidth = lwd)) +
    scale_fill_identity() +
    scale_colour_identity() +
    scale_linewidth_identity() +

    # Box text (ggtext richtext, vectorized)
    ggtext::geom_richtext(
      data = box_df,
      aes(x = xpos, y = ymid, label = label,
          hjust = hjust, colour = text_col),
      family = LANCET_FONT, size = BASE_SIZE / .pt,
      fill = NA, label.color = NA,
      lineheight = LINE_HEIGHT, vjust = 0.5
    ) +

    # Coordinate system
    coord_cartesian(xlim = c(-1, 100), ylim = c(0, 100), expand = FALSE) +
    theme_void() +
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.margin      = margin(12, 12, 12, 12),
      text             = element_text(family = LANCET_FONT, size = BASE_SIZE)
    )

  # ---- Save with ragg (high-quality anti-aliased output) ----
  # Final print size: 6.75 in wide (171 mm, OUP two-column maximum).
  # Fonts render 1:1 (no downscaling), so all text is >= 8 pt in print.
  OUTPUT_WIDTH  <- 6.75
  OUTPUT_HEIGHT <- 9.3
  OUTPUT_DPI    <- 300

  # ---- Text-width self-check (labels must fit inside their boxes) ----
  # Plot area in inches after 12-pt margins on a 6.75 in canvas
  plot_w <- OUTPUT_WIDTH - 2 * 12 / 72
  unit_in <- plot_w / 101  # xlim spans -1..100 = 101 units
  check_fit <- function(box_id, label_html, base_pt) {
    lines <- strsplit(label_html, "<br>")[[1]]
    lines <- trimws(gsub("<[^>]+>", "", lines))
    lines <- gsub("&nbsp;|&#8226;|&#8211;", " ", lines)
    lines <- lines[nzchar(lines)]
    box_w <- (box_df$xmax[box_id] - box_df$xmin[box_id]) * unit_in
    # open a measuring device at true canvas size (cairo supports Arial)
    tmp_dev <- tempfile(fileext = ".pdf")
    cairo_pdf(tmp_dev, width = OUTPUT_WIDTH, height = OUTPUT_HEIGHT)
    w <- max(sapply(lines, function(l)
      strwidth(l, units = "inches", family = LANCET_FONT, ps = base_pt)))
    dev.off()
    unlink(tmp_dev)
    ok <- w <= box_w * 0.94
    cat(sprintf("  [%s] box %d: widest line %.2f in vs box %.2f in %s\n",
                ifelse(ok, "OK ", "WARN"), box_id, w, box_w,
                ifelse(ok, "", "<-- TOO WIDE")))
    invisible(ok)
  }
  cat("\nLabel fit check (6.75-in canvas):\n")
  ok_all <- TRUE
  for (bid in 1:10) {
    base_pt <- if (bid %in% c(6)) SMALL_SIZE else BASE_SIZE
    ok_all <- check_fit(bid, box_df$label[bid], base_pt) && ok_all
  }
  if (!ok_all) cat("  WARNING: at least one label may overflow its box.\n")

  # TIFF (LZW compression, 300 DPI)
  ragg::agg_tiff(
    filename    = paste0(prisma_file, ".tiff"),
    width       = OUTPUT_WIDTH,
    height      = OUTPUT_HEIGHT,
    units       = "in",
    res         = OUTPUT_DPI,
    bg          = "white",
    compression = "lzw"
  )
  print(p)
  dev.off()

  # PDF (vector, embedded fonts via cairo)
  cairo_pdf(
    filename = paste0(prisma_file, ".pdf"),
    width    = OUTPUT_WIDTH,
    height   = OUTPUT_HEIGHT,
    bg       = "white",
    family   = LANCET_FONT
  )
  print(p)
  dev.off()

  cat("PRISMA 2020 flow diagram saved (V4 Lancet submission-ready):",
      basename(prisma_file), ".tiff/.pdf\n")
}

generate_prisma_2020(prisma)
