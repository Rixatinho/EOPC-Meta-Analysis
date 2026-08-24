#!/usr/bin/env Rscript
# ==============================================================================
# Script 12: Lancet-Formatted Word Tables (Table 1 + Table 3)
# Output: 08_Manuscript/tables/Table_1.docx, Table_3.docx
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

out_dir <- file.path(PROJECT_ROOT, "08_Manuscript", "tables")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ==============================================================================
# Lancet table styling constants
# ==============================================================================
lancet_header_bg  <- "white"
lancet_header_fg  <- "black"
lancet_body_size  <- 7.5
lancet_header_size <- 8
lancet_alt_row    <- "white"
lancet_font       <- LANCET_FONT

style_lancet_ft <- function(ft, ncols) {
  ft <- ft %>%
    fontsize(size = lancet_body_size, part = "body") %>%
    fontsize(size = lancet_header_size, part = "header") %>%
    font(fontname = lancet_font, part = "all") %>%
    bold(part = "header") %>%
    color(color = lancet_header_fg, part = "header") %>%
    bg(bg = lancet_header_bg, part = "header") %>%
    align(align = "center", part = "header") %>%
    align(j = 1, align = "left", part = "body") %>%
    align(j = 2:ncols, align = "center", part = "body") %>%
    border_remove() %>%
    hline_top(border = fp_border(color = "black", width = 2.0), part = "header") %>%
    hline_bottom(border = fp_border(color = "black", width = 0.5), part = "header") %>%
    hline_bottom(border = fp_border(color = "black", width = 2.0), part = "body") %>%
    padding(padding.top = 2, padding.bottom = 2, part = "all")

  return(ft)
}

# Helper for openxlsx workbook styling (also used in Table S16)
style_lancet_wb <- function(wb, sheet, df, wrap_cols = NULL, center_cols = NULL) {
  hs <- createStyle(fontName = LANCET_FONT, fontSize = 9, fontColour = "#000000",
                    fgFill = "#FFFFFF", halign = "center", valign = "center",
                    textDecoration = "bold", border = "TopBottom",
                    borderColour = "#000000", borderStyle = "medium")
  bs <- createStyle(fontName = LANCET_FONT, fontSize = 8.5, halign = "left", valign = "center")
  bb <- createStyle(border = "bottom", borderColour = "#000000", borderStyle = "medium")
  nr <- nrow(df); nc <- ncol(df)
  addStyle(wb, sheet, hs, rows = 1, cols = 1:nc, gridExpand = TRUE)
  addStyle(wb, sheet, bs, rows = 2:(nr+1), cols = 1:nc, gridExpand = TRUE, stack = TRUE)
  addStyle(wb, sheet, bb, rows = nr+1, cols = 1:nc, gridExpand = TRUE, stack = TRUE)
  if (!is.null(wrap_cols)) {
    ws <- createStyle(wrapText = TRUE)
    for (wc in wrap_cols)
      addStyle(wb, sheet, ws, rows = 1:(nr+1), cols = wc, gridExpand = TRUE, stack = TRUE)
  }
  if (!is.null(center_cols)) {
    cs <- createStyle(halign = "center")
    for (cc in center_cols)
      addStyle(wb, sheet, cs, rows = 2:(nr+1), cols = cc, gridExpand = TRUE, stack = TRUE)
  }
  setColWidths(wb, sheet, cols = 1:nc, widths = "auto")
}

# ==============================================================================
# TABLE 1: Study Characteristics (Portrait A4)
# ==============================================================================
cat("\n=== Generating Table 1: Study Characteristics ===\n")

t1_file <- file.path(PATHS$tables, "Table1_Study_Characteristics.xlsx")
if (!file.exists(t1_file)) stop("Table1_Study_Characteristics.xlsx not found at: ", t1_file)

t1_raw <- read_excel(t1_file)
cat("  Read", nrow(t1_raw), "rows from Table 1 source\n")

# Select and rename columns for Lancet format
# Available columns may vary; select what's available
available_cols <- names(t1_raw)
cat("  Available columns:", paste(available_cols, collapse = ", "), "\n")

# Build display table
t1 <- t1_raw %>%
  dplyr::select(
    Study = any_of(c("Study_Label", "Study")),
    Country = any_of(c("Country")),
    Region = any_of(c("Region_Group", "Region")),
    `Data Source` = any_of(c("Data_Source", "Data Source")),
    Period = any_of(c("Study_Period", "Period")),
    `Age Threshold` = any_of(c("Age_Threshold", "Age Threshold")),
    `Total N` = any_of(c("Total_N", "Total N")),
    `EOPC N` = any_of(c("EOPC_N", "EOPC N")),
    `AOPC N` = any_of(c("AOPC_N", "AOPC N")),
    NOS = any_of(c("Total_Stars", "NOS", "NOS_Score")),
    `ROBINS-E` = any_of(c("ROBINS_E_Overall", "ROBINS-E", "ROBINS_E"))
  )

# Format numeric columns
if ("Total N" %in% names(t1))
  t1$`Total N` <- formatC(as.numeric(t1$`Total N`), format = "d", big.mark = ",")
if ("EOPC N" %in% names(t1))
  t1$`EOPC N` <- formatC(as.numeric(t1$`EOPC N`), format = "d", big.mark = ",")
if ("AOPC N" %in% names(t1))
  t1$`AOPC N` <- formatC(as.numeric(t1$`AOPC N`), format = "d", big.mark = ",")

cat("  Table 1 display columns:", paste(names(t1), collapse = ", "), "\n")
cat("  Table 1 rows:", nrow(t1), "\n")

# ==============================================================================
# Reference number mapping for all 48 included studies
# ==============================================================================
ref_map <- c(
  "Piciucchi (2015)" = 7,
  "Ansari (2019)" = 8,
  "Alese (2020)" = 9,
  "Castet (2023)" = 10,
  "Kim (2023)" = 11,
  "Ogobuiro (2023)" = 15,
  "Ntala (2018)" = 27,
  "Duffy (2009)" = 28,
  "Alkashash (2021)" = 29,
  "Amin (2013)" = 30,
  "Bannon (2018)" = 31,
  "Beeghly-Fadiel (2016)" = 32,
  "Bernards (2015)" = 33,
  "Bunduc (2018)" = 34,
  "Dai (2020)" = 35,
  "Eguchi (2016)" = 36,
  "Gamboa (2026)" = 37,
  "He (2013)" = 38,
  "Jayakrishnan (2025)" = 39,
  "Kadakia (2021)" = 40,
  "Kang (2017)" = 41,
  "König (2022)" = 42,
  "Luo (2022)" = 43,
  "Mendis (2024)" = 44,
  "Mughal (2026)" = 45,
  "Ohmoto (2016)" = 46,
  "Ordonez (2020)" = 47,
  "Pretta (2024)" = 48,
  "Ramai (2021)" = 49,
  "Rashad (2024)" = 50,
  "Remond (2024)" = 51,
  "Ren (2023)" = 52,
  "Saadat (2021)" = 53,
  "Sehgal (2014)" = 54,
  "Shi (2020)" = 55,
  "Takeda (2022)" = 56,
  "Tang (2021)" = 57,
  "Tingstedt (2011)" = 58,
  "Tschaidse (2025)" = 59,
  "Varghese (2021)" = 60,
  "Wang (2020)" = 61,
  "Wang (2022)" = 62,
  "Weiss (2024)" = 63,
  "Wheeler (2014)" = 64,
  "Whitley (2023)" = 65,
  "Zhang (2024)" = 66,
  "Zheng (2022)" = 67,
  "Zironda (2024)" = 68
)

# Add reference numbers to Study column
t1$Ref <- ref_map[t1$Study]
unmapped <- sum(is.na(t1$Ref))
if (unmapped > 0) {
  cat("  WARNING:", unmapped, "studies not found in ref_map:",
      paste(t1$Study[is.na(t1$Ref)], collapse = ", "), "\n")
}
cat("  Mapped", sum(!is.na(t1$Ref)), "of", nrow(t1), "studies to reference numbers\n")

# Build flextable
ft1 <- flextable(t1[, setdiff(names(t1), "Ref")]) %>%
  autofit()

# Add superscript reference numbers to Study column using compose
ft1 <- compose(
  ft1, j = "Study",
  value = as_paragraph(
    as_chunk(t1$Study),
    as_sup(as_chunk(as.integer(t1$Ref)))
  )
)

ft1 <- style_lancet_ft(ft1, ncol(t1) - 1)  # -1 for hidden Ref column

# Adjust column widths for portrait A4 (6.3 inch usable)
ft1 <- ft1 %>%
  set_caption(caption = "Table 1. Characteristics of 48 studies included in the systematic review and meta-analysis") %>%
  width(j = 1, width = 1.3) %>%   # Study
  set_table_properties(layout = "autofit")

# Export to Word (Portrait A4)
doc1 <- read_docx()
doc1 <- body_add_par(doc1, "Table 1", style = "heading 1")
doc1 <- body_add_flextable(doc1, ft1)
doc1 <- body_add_par(doc1, "")
doc1 <- body_add_par(doc1,
  "Superscript numbers indicate reference numbers in the main text. NOS = Newcastle-Ottawa Scale (ref 15; 0-9 stars; Good >=7, Fair 5-6, Poor <5). ROBINS-E = Risk Of Bias In Non-randomised Studies of Exposures (ref 16; Low/Moderate/Serious/Critical). EOPC = early-onset pancreatic cancer. AOPC = average-onset pancreatic cancer.",
  style = "Normal")

t1_out <- file.path(out_dir, "Table_1.docx")
print(doc1, target = t1_out)
cat("  Table 1 saved:", t1_out, "\n")

# ==============================================================================
# TABLE 3: GRADE Summary of Findings (Landscape A4)
# ==============================================================================
cat("\n=== Generating Table 3: GRADE Summary of Findings ===\n")

grade_file <- file.path(PATHS$tables, "GRADE_Evidence_Table.xlsx")
if (!file.exists(grade_file)) stop("GRADE_Evidence_Table.xlsx not found at: ", grade_file)

g_raw <- read_excel(grade_file)
g_raw$Outcome <- gsub("Tumour", "Tumor", g_raw$Outcome)
cat("  Read", nrow(g_raw), "rows from GRADE table\n")

# Assign category for grouping
category_map <- c(
  "Overall Survival" = "Survival",
  "Disease-Free Survival" = "Survival",
  "Recurrence-Free Survival" = "Survival",
  "Male Proportion" = "Demographics",
  "Smoking History" = "Demographics",
  "Diabetes History" = "Demographics",
  "Family History" = "Demographics",
  "Alcohol History" = "Demographics",
  "White Race" = "Demographics",
  "Black Race" = "Demographics",
  "Asian Race" = "Demographics",
  "Stage I" = "Staging",
  "Stage II" = "Staging",
  "Stage III" = "Staging",
  "Stage IV" = "Staging",
  "LN Positive" = "Staging",
  "Metastatic Disease" = "Staging",
  "Poorly Differentiated" = "Tumor Features",
  "Moderately Differentiated" = "Tumor Features",
  "Well Differentiated" = "Tumor Features",
  "Tumor Head" = "Tumor Features",
  "Tumor Body" = "Tumor Features",
  "Tumor Tail" = "Tumor Features",
  "Vascular Invasion" = "Tumor Features",
  "Perineural Invasion" = "Tumor Features",
  "Surgical Resection" = "Treatment",
  "Chemotherapy" = "Treatment",
  "Neoadjuvant Chemo" = "Treatment",
  "Adjuvant Chemo" = "Treatment",
  "Radiotherapy" = "Treatment",
  "Whipple Procedure" = "Treatment",
  "Distal Pancreatectomy" = "Treatment",
  "R0 Resection" = "Treatment",
  "No Treatment" = "Treatment",
  "Palliative Chemotherapy" = "Treatment",
  "Total Pancreatectomy" = "Treatment",
  "5-Year OS" = "Time-Point Survival",
  "3-Year OS" = "Time-Point Survival",
  "2-Year OS" = "Time-Point Survival",
  "1-Year OS" = "Time-Point Survival"
)

g_raw$Category <- category_map[g_raw$Outcome]
g_raw$Category[is.na(g_raw$Category)] <- "Other"

# Clean GRADE domain ratings: extract just the rating text (before parenthetical notes)
clean_rating <- function(x) {
  x <- as.character(x)
  # Extract: "No downgrade", "-1 Serious", "-2 Very serious", "Not assessed"
  rating <- sub("\\s*\\(.*\\)$", "", x)
  return(rating)
}

# Select columns for display
t2 <- g_raw %>%
  mutate(
    Category = factor(Category, levels = c("Survival", "Demographics", "Staging",
                                            "Tumor Features", "Treatment",
                                            "Time-Point Survival")),
    RoB_clean = clean_rating(Risk_of_Bias),
    Incon_clean = clean_rating(Inconsistency),
    Indirect_clean = clean_rating(Indirectness),
    Imprec_clean = clean_rating(Imprecision),
    PubBias_clean = clean_rating(Publication_Bias)
  ) %>%
  arrange(Category) %>%
  dplyr::select(
    Category,
    Outcome,
    k = Studies_k,
    `Effect [95% CI]` = Effect,
    `Risk of Bias` = RoB_clean,
    Inconsistency = Incon_clean,
    Indirectness = Indirect_clean,
    Imprecision = Imprec_clean,
    `Pub. Bias` = PubBias_clean,
    Certainty
  )

cat("  Table 3 display columns:", paste(names(t2), collapse = ", "), "\n")

# Build flextable with category grouping
ft2 <- as_grouped_data(t2, groups = "Category") %>%
  as_flextable(hide_grouplabel = TRUE) %>%
  autofit()

# Style header rows (category groups) - bold
ft2 <- ft2 %>%
  fontsize(size = lancet_body_size, part = "body") %>%
  fontsize(size = lancet_header_size, part = "header") %>%
  font(fontname = lancet_font, part = "all") %>%
  bold(part = "header") %>%
  color(color = lancet_header_fg, part = "header") %>%
  bg(bg = lancet_header_bg, part = "header") %>%
  align(align = "center", part = "header") %>%
  align(j = 1, align = "left", part = "body") %>%
  border_remove() %>%
  hline_top(border = fp_border(color = "black", width = 2.0), part = "header") %>%
  hline_bottom(border = fp_border(color = "black", width = 0.5), part = "header") %>%
  hline_bottom(border = fp_border(color = "black", width = 2.0), part = "body") %>%
  padding(padding.top = 2, padding.bottom = 2, part = "all")

ft2 <- ft2 %>%
  set_caption(caption = "Table 3. GRADE Summary of Findings for EOPC vs AOPC comparisons") %>%
  set_table_properties(layout = "autofit")

# Highlight certainty column with color
# (conditional formatting)
n_body_rows <- nrow_part(ft2, part = "body")
for (i in seq_len(n_body_rows)) {
  # Get certainty value from the last visible column
  cert_val <- tryCatch({
    ft2$body$dataset$Certainty[i]
  }, error = function(e) NA)

  if (!is.na(cert_val)) {
    cert_bg <- switch(as.character(cert_val),
      "High"     = "#4CAF50",
      "Moderate" = "#FFC107",
      "Low"      = "#FF9800",
      "Very Low" = "#F44336",
      "white"
    )
    cert_fg <- if (cert_val %in% c("High", "Very Low")) "white" else "black"
    ft2 <- bg(ft2, i = i, j = "Certainty", bg = cert_bg, part = "body")
    ft2 <- color(ft2, i = i, j = "Certainty", color = cert_fg, part = "body")
    ft2 <- bold(ft2, i = i, j = "Certainty", part = "body")
  }
}

# Export to Word (Landscape A4)
doc2 <- read_docx()
# Set landscape orientation
doc2 <- body_add_par(doc2, "Table 3", style = "heading 1")
doc2 <- body_add_flextable(doc2, ft2)
doc2 <- body_add_par(doc2, "")
doc2 <- body_add_par(doc2,
  paste0(
    "All outcomes start at Low certainty (observational studies; ref 21 for GRADE methodology). ",
    "GRADE domains: Risk of Bias assessed using NOS (ref 15) and ROBINS-E (ref 16; dual-track); ",
    "Inconsistency based on I2 and Cochran's Q; ",
    "Indirectness based on population, exposure, comparator, and outcome directness; ",
    "Imprecision based on CI width and study count; ",
    "Publication bias based on Egger's/Peters' test (k>=10). ",
    "Effect measures: HR for survival outcomes, OR for binary outcomes. ",
    "k = number of studies."
  ),
  style = "Normal"
)

# Add landscape section
landscape_section <- prop_section(
  page_size = page_size(orient = "landscape", width = 11.69, height = 8.27),
  page_margins = page_mar(bottom = 0.5, top = 0.5, right = 0.5, left = 0.5)
)
doc2 <- body_end_block_section(doc2, block_section(landscape_section))

t2_out <- file.path(out_dir, "Table_3.docx")
print(doc2, target = t2_out)
cat("  Table 3 saved:", t2_out, "\n")

cat("\nAll Lancet-formatted Word tables generated.\n")

# ==============================================================================
# TABLE S16: Stage-Specific HR Data (renumbered from S14, 2026-08-25; auto-generated for reproducibility)
# ==============================================================================
cat("\n=== Generating Table S16 (previously S14): Stage-Specific HR Data ===\n")

s14_data <- data.frame(
  Study = c("Mendis 2024", "Mendis 2024", "Mendis 2024",
            "Castet 2023", "Castet 2023"),
  `Stage Stratum` = c("Resected", "Locally advanced unresectable", "Metastatic",
                       "First-line metastatic (OS)", "First-line metastatic (OS, adjusted)"),
  `HR (EOPC vs AOPC)` = c(0.46, 0.47, 0.66, 0.65, 0.43),
  `95% CI` = c("0.29 to 0.74", "0.32 to 0.69", "0.48 to 0.89",
               "0.45 to 0.95", "0.24 to 0.79"),
  Adjustment = c("Univariable (Kaplan-Meier + log-rank; Cox model)",
                  "Univariable (Kaplan-Meier + log-rank; Cox model)",
                  "Univariable (Kaplan-Meier + log-rank; Cox model)",
                  "Univariable",
                  "Multivariable (stage + ECOG-PS)"),
  `N (EOPC/AOPC)` = c("66/394", "74/383", "87/410", "NR", "NR"),
  stringsAsFactors = FALSE, check.names = FALSE
)

# List of 16 OS studies without stage-specific HRs
s14_no_data <- data.frame(
  `Studies without stage-specific HRs` = c(
    "Gamboa 2026", "Mughal 2026", "Jayakrishnan 2025", "Pretta 2024",
    "Remond 2024", "Weiss 2024", "Zironda 2024", "Kim 2023",
    "Ren 2023", "Wang 2022", "Ramai 2021", "Ordonez 2020",
    "Ansari 2019", "Beeghly-Fadiel 2016", "Piciucchi 2015",
    "Amin 2013"
  ),
  stringsAsFactors = FALSE, check.names = FALSE
)

s14_wb <- createWorkbook()
addWorksheet(s14_wb, "Stage_Specific_HR")
writeData(s14_wb, "Stage_Specific_HR", s14_data)
style_lancet_wb(s14_wb, "Stage_Specific_HR", s14_data,
                wrap_cols = c(2, 5), center_cols = c(3, 4, 6))
setColWidths(s14_wb, "Stage_Specific_HR",
             cols = 1:6, widths = c(16, 32, 18, 16, 45, 14))

addWorksheet(s14_wb, "No_Data_Studies")
writeData(s14_wb, "No_Data_Studies", s14_no_data)
style_lancet_wb(s14_wb, "No_Data_Studies", s14_no_data)

s14_out <- file.path(PATHS$tables, "Table_S16_Stage_Specific_HR.xlsx")
saveWorkbook(s14_wb, s14_out, overwrite = TRUE)
cat("  [OK] Table S16 (previously S14):", nrow(s14_data), "stage-specific HRs +",
    nrow(s14_no_data), "studies without data ->", basename(s14_out), "\n")
