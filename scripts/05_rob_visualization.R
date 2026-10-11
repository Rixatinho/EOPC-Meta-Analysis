################################################################################
# EOPC vs AOPC Systematic Review and Meta-Analysis
# Script 05: Risk of Bias Visualization (NOS + ROBINS-E)
# Input:  NOS_Assessment.xlsx, ROBINS_E_Assessment.xlsx
# Output: NOS bar chart, ROBINS-E traffic light + summary plots
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

# Load prepared data to get included study IDs
load(file.path(PATHS$stat_data, "prepared_data.RData"))
included_ids <- master$Study_ID

# ==============================================================================
# PART 1: NOS Visualization
# ==============================================================================

create_nos_plot <- function() {
  nos_file <- file.path(PATHS$quality, "NOS_Assessment.xlsx")
  nos_summary <- read_excel(nos_file, sheet = "Summary")

  # Filter to included studies only
  nos_summary <- nos_summary[nos_summary$Study_ID %in% included_ids, ]

  cat("NOS data loaded:", nrow(nos_summary), "studies (filtered to included)\n")

  # Prepare long-format data
  nos_long <- nos_summary %>%
    dplyr::select(Study_ID, Selection_Stars, Comparability_Stars, Outcome_Stars,
                  Total_Stars) %>%
    mutate(
      Selection_Stars     = as.numeric(Selection_Stars),
      Comparability_Stars = as.numeric(Comparability_Stars),
      Outcome_Stars       = as.numeric(Outcome_Stars),
      Total_Stars         = as.numeric(Total_Stars),
      Study_ID            = gsub("_(\\d{4})$", " (\\1)", Study_ID),
      Study_ID            = str_trunc(Study_ID, 40)
    ) %>%
    tidyr::pivot_longer(
      cols = c(Selection_Stars, Comparability_Stars, Outcome_Stars),
      names_to = "Domain",
      values_to = "Stars"
    ) %>%
    mutate(
      Domain = factor(gsub("_Stars", "", Domain),
                      levels = c("Selection", "Comparability", "Outcome"))
    )

  # Stacked bar chart: sorted by total score, with quality summary annotation
  # Use AHRQ domain-based Quality_Grade (consistent with manuscript text)
  n_good <- sum(nos_summary$Quality_Grade == "Good", na.rm = TRUE)
  n_fair <- sum(nos_summary$Quality_Grade == "Fair", na.rm = TRUE)
  n_poor <- sum(nos_summary$Quality_Grade == "Poor", na.rm = TRUE)
  quality_label <- sprintf(
    "Good: %d (%d%%)  |  Fair: %d (%d%%)  |  Poor: %d (%d%%)",
    n_good, round(100 * n_good / nrow(nos_summary)),
    n_fair, round(100 * n_fair / nrow(nos_summary)),
    n_poor, round(100 * n_poor / nrow(nos_summary)))

  p <- ggplot(nos_long, aes(x = reorder(Study_ID, -as.numeric(Total_Stars)),
                            y = Stars, fill = Domain)) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    coord_flip() +
    scale_fill_manual(values = LANCET_STYLE$col_nos) +
    geom_hline(yintercept = 7, linetype = "dashed", color = LANCET_STYLE$col_structural, alpha = 0.6) +
    annotate("text", x = max(1, nrow(nos_summary) * 0.05), y = 7.3,
             label = "Good quality (>=7)",
             size = 2.8, color = LANCET_STYLE$col_structural, hjust = 0) +
    labs(x = "", y = "NOS Stars (max 9)",
         title = "Newcastle-Ottawa Scale Quality Assessment",
         caption = quality_label,
         fill = "Domain") +
    lancet_theme(base_size = 9) +
    theme(legend.position = "bottom",
          plot.caption = element_text(hjust = 0.5, size = 8.5, face = "bold"),
          axis.text.y = element_text(size = 7),
          plot.margin = margin(10, 15, 10, 5))

  nos_plot_file <- file.path(PATHS$tables, "NOS_scores")
  save_figure_gg(p, nos_plot_file, width = 10, height = max(6, nrow(nos_summary) * 0.30))

  # Quality distribution summary
  cat("\nNOS Quality Distribution:\n")
  print(table(nos_summary$Quality_Grade))

  return(p)
}

nos_plot <- create_nos_plot()

# ==============================================================================
# PART 2: ROBINS-E Visualization
# ==============================================================================

create_robins_e_plots <- function() {
  rob_file <- file.path(PATHS$quality, "ROBINS_E_Assessment.xlsx")
  robins_summary <- read_excel(rob_file, sheet = "Summary")

  # Filter to included studies only (ROBINS-E Excel uses "Study" column)
  robins_summary <- robins_summary[robins_summary$Study %in% included_ids, ]

  cat("\nROBINS-E data loaded:", nrow(robins_summary), "studies (filtered to included)\n")

  # robvis expects specific column names and values
  # robvis 0.3.0 does not natively support ROBINS-E; use ROBINS-I format
  # (both share D1-D7 + Overall structure)
  robins_for_robvis <- robins_summary %>%
    dplyr::select(Study, D1, D2, D3, D4, D5, D6, D7, Overall) %>%
    as.data.frame()

  # Standardize values for robvis (capitalize first letter)
  standardize_rob <- function(x) {
    x <- as.character(x)
    x <- trimws(x)
    case_when(
      grepl("^[Ll]ow$", x) ~ "Low",
      grepl("^[Mm]oderate$", x) ~ "Moderate",
      grepl("^[Ss]erious$", x) ~ "Serious",
      grepl("^[Cc]ritical$", x) ~ "Critical",
      TRUE ~ x
    )
  }

  for (col in c("D1", "D2", "D3", "D4", "D5", "D6", "D7", "Overall")) {
    robins_for_robvis[[col]] <- standardize_rob(robins_for_robvis[[col]])
  }

  # Add Weight column required by robvis
  robins_for_robvis$Weight <- 1

  # ROBINS-E domain labels
  robins_e_domains <- c(
    D1 = "Confounding", D2 = "Selection",
    D3 = "Classification\nof exposure", D4 = "Deviations from\nexposure",
    D5 = "Missing data", D6 = "Measurement\nof outcome",
    D7 = "Selection of\nreported result", Overall = "Overall"
  )

  # Helper: fix ROBINS-I domain labels to ROBINS-E terminology
  fix_robins_e_labels <- function(p) {
    p$data[] <- lapply(p$data, function(col) {
      if (is.character(col) || is.factor(col)) {
        col <- gsub("Bias in classification of interventions",
                     "Bias in classification of exposures", col)
        col <- gsub("Bias due to deviations from intended interventions",
                     "Bias due to deviations from intended exposures", col)
      }
      col
    })
    p
  }

  # --- Traffic light plot (individual studies) ---
  # Custom ggplot2 implementation (replaces robvis for full label control)
  domain_labels <- c(
    "D1" = "Confounding",
    "D2" = "Participant\nselection",
    "D3" = "Exposure\nclassification",
    "D4" = "Exposure\ndeviations",
    "D5" = "Missing\ndata",
    "D6" = "Outcome\nmeasurement",
    "D7" = "Reported\nresult",
    "Overall" = "Overall"
  )

  traffic_file <- file.path(PATHS$tables, "ROBINS_E_traffic_light")
  traffic_height <- max(10, nrow(robins_for_robvis) * 0.42)

  tryCatch({
    # Reshape to long format
    robins_long <- robins_for_robvis %>%
      dplyr::select(-Weight) %>%
      tidyr::pivot_longer(
        cols = c(D1, D2, D3, D4, D5, D6, D7, Overall),
        names_to  = "domain_code",
        values_to = "judgement"
      )
    robins_long$domain <- factor(domain_labels[robins_long$domain_code],
                                  levels = domain_labels)

    # Format study names: "Author_Year" -> "Author (Year)"
    robins_long$Study <- gsub("_(\\d{4}[a-z]?)$", " (\\1)", robins_long$Study)
    robins_long$Study <- gsub("_", " ", robins_long$Study)
    # Reverse alphabetical so first study is at top
    study_order <- rev(sort(unique(robins_long$Study)))
    robins_long$Study <- factor(robins_long$Study, levels = study_order)

    # Colour & symbol mappings (robvis-consistent)
    rob_colours <- c("Low" = "#02C77D", "Moderate" = "#E2C600",
                     "Serious" = "#E2301C", "Critical" = "#820000")
    rob_symbols <- c("Low" = "+", "Moderate" = "\u2013",
                     "Serious" = "\u2013", "Critical" = "\u00D7")
    robins_long$symbol <- rob_symbols[robins_long$judgement]
    robins_long$judgement <- factor(robins_long$judgement,
                                    levels = c("Low", "Moderate", "Serious", "Critical"))

    p_traffic <- ggplot(robins_long, aes(x = domain, y = Study)) +
      geom_point(aes(colour = judgement), size = 5, shape = 15) +
      geom_text(aes(label = symbol), size = 3.2, fontface = "bold",
                colour = "white") +
      scale_colour_manual(values = rob_colours, name = "Judgement",
                          drop = FALSE) +
      scale_x_discrete(position = "top") +
      labs(title = "Risk of Bias Domains",
           caption = "ROBINS-E (Risk Of Bias In Non-randomized Studies of Exposures)") +
      lancet_theme(base_size = 10) +
      theme(
        axis.title       = element_blank(),
        axis.text.x.top  = element_text(size = 9, face = "bold", hjust = 0.5),
        axis.text.y      = element_text(size = 8),
        axis.ticks       = element_blank(),
        legend.position  = "bottom",
        legend.title     = element_text(face = "bold", size = 9),
        legend.text      = element_text(size = 8.5),
        panel.grid       = element_blank(),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.title       = element_text(hjust = 0.5, face = "bold", size = 11),
        plot.caption     = element_text(hjust = 0, face = "italic", size = 8.5),
        plot.margin      = margin(5, 10, 5, 5)
      ) +
      guides(colour = guide_legend(override.aes = list(size = 5)))

    save_figure_gg(p_traffic, traffic_file, width = 14, height = traffic_height)
    cat("ROBINS-E traffic light plot saved.\n")
  }, error = function(e) {
    cat("ROBINS-E traffic light error:", e$message, "\n")
  })

  # --- Summary plot (domain level) with colorblind-safe palette ---
  # JNCI print-size version (2026-08-24): horizontal stacked bars,
  # 6.75 in (171 mm) final print width, all text >= 8.5 pt at 1:1 scale.
  summary_file <- file.path(PATHS$tables, "ROBINS_E_summary")
  tryCatch({
    robins_summary_long <- robins_for_robvis %>%
      dplyr::select(-Weight) %>%
      tidyr::pivot_longer(
        cols = c(D1, D2, D3, D4, D5, D6, D7, Overall),
        names_to  = "domain_code",
        values_to = "judgement"
      ) %>%
      dplyr::mutate(
        judgement = factor(judgement, levels = c("Low", "Moderate", "Serious", "Critical"))
      )

    # Okabe-Ito colorblind-safe palette for risk-of-bias judgements
    rob_colours_safe <- c(
      "Low"      = "#009E73",   # green-blue
      "Moderate" = "#F0E442",   # yellow
      "Serious"  = "#D55E00",   # vermillion
      "Critical" = "#882255"    # reddish purple
    )
    # Label text colour: black on yellow, white on the darker fills
    rob_txt_col <- c("Low" = "white", "Moderate" = "black",
                     "Serious" = "white", "Critical" = "white")

    # Full domain names for the horizontal layout (printed horizontally,
    # no line breaks needed; longest = 28 chars at 9.5 pt)
    domain_full <- c(
      "D1" = "Confounding",
      "D2" = "Selection of participants",
      "D3" = "Classification of exposure",
      "D4" = "Deviations from exposures",
      "D5" = "Missing data",
      "D6" = "Measurement of outcome",
      "D7" = "Selection of reported result",
      "Overall" = "Overall judgement"
    )

    # Counts and cumulative midpoints for in-bar percentage labels
    dom_cnt <- robins_summary_long %>%
      dplyr::filter(!is.na(judgement)) %>%
      dplyr::count(domain_code, judgement, .drop = FALSE) %>%
      dplyr::group_by(domain_code) %>%
      dplyr::mutate(
        prop = n / sum(n),
        mid  = cumsum(prop) - prop / 2,
        domain = unname(domain_full[domain_code])
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        domain = factor(domain, levels = rev(c(
          unname(domain_full[c("D1","D2","D3","D4","D5","D6","D7")]),
          "Overall judgement"))),   # Overall judgement on top
        pct_lab = paste0(round(100 * prop), "%")
      )

    p_summary <- ggplot2::ggplot(
        dom_cnt, ggplot2::aes(x = domain, y = prop, fill = judgement)) +
      ggplot2::geom_col(width = 0.72, colour = "white", linewidth = 0.25) +
      ggplot2::geom_text(
        ggplot2::aes(label = ifelse(prop >= 0.055, pct_lab, ""),
                     colour = judgement),
        position = ggplot2::position_stack(vjust = 0.5),
        size = 8.5 / .pt, fontface = "bold", family = LANCET_FONT,
        show.legend = FALSE) +
      ggplot2::scale_colour_manual(values = rob_txt_col) +
      ggplot2::scale_fill_manual(
        values = rob_colours_safe,
        name = "Risk of bias judgement",
        drop = FALSE
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        expand = ggplot2::expansion(mult = c(0, 0)),
        breaks = seq(0, 1, 0.25)
      ) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = "",
        y = "Proportion of studies (k = 48)",
        caption = "ROBINS-E: Risk Of Bias In Non-randomized Studies of Exposures"
      ) +
      lancet_theme(base_size = 11) +
      ggplot2::theme(
        axis.text.y  = ggplot2::element_text(size = 9.5, colour = "black",
                                             lineheight = 0.9),
        axis.text.x  = ggplot2::element_text(size = 9, colour = "black"),
        axis.title.x = ggplot2::element_text(size = 10, face = "bold"),
        legend.position = "bottom",
        legend.title = ggplot2::element_text(size = 9.5, face = "bold"),
        legend.text  = ggplot2::element_text(size = 9),
        legend.key.size = grid::unit(0.38, "cm"),
        plot.caption = ggplot2::element_text(hjust = 0, face = "italic",
                                             size = 8.5),
        panel.grid   = ggplot2::element_blank(),
        plot.margin  = ggplot2::margin(8, 12, 6, 4)
      )

    save_figure_gg(p_summary, summary_file, width = 6.75, height = 4.2)
    cat("ROBINS-E summary plot saved.\n")
  }, error = function(e) {
    cat("ROBINS-E summary error:", e$message, "\n")
  })

  # Overall RoB distribution
  cat("\nROBINS-E Overall Distribution:\n")
  print(table(robins_for_robvis$Overall))
}

create_robins_e_plots()

cat("\nRisk of bias visualization complete.\n")
