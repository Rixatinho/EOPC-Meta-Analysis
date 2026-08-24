#!/usr/bin/env Rscript
# ==============================================================================
# Script 14: Generate Supplementary eTables 1-5
# Creates 5 xlsx files in output/tables/ from existing source data:
#   eTable 1: Search Strategies (5 databases)
#   eTable 2: Studies Excluded at Full-Text Stage (29 studies)
#   eTable 3: NOS Quality Assessment Details (50 studies)
#   eTable 4: ROBINS-E Assessment Details (50 studies)
#   eTable 5: Extended Study Characteristics (50 studies)
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

load(file.path(PATHS$stat_data, "prepared_data.RData"))
included_ids <- master$Study_ID
cat("Loaded master data:", nrow(master), "included studies\n")

# ==============================================================================
# Shared formatting helpers
# ==============================================================================
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

add_quality_color <- function(wb, sheet, col_idx, n_row, values) {
  colors <- list(Good = "#C6EFCE", Fair = "#FFEB9C", Poor = "#FFC7CE",
                 Low = "#C6EFCE", Moderate = "#FFEB9C",
                 Serious = "#FFC7CE", Critical = "#FF6B6B")
  for (i in seq_along(values)) {
    v <- trimws(as.character(values[i]))
    if (!is.na(v) && v %in% names(colors))
      addStyle(wb, sheet, createStyle(fgFill = colors[[v]], halign = "center"),
               rows = i + 1, cols = col_idx, stack = TRUE)
  }
}

# ==============================================================================
# eTable 1: Search Strategies
# ==============================================================================
create_etable_1 <- function() {
  cat("\n=== eTable 1: Search Strategies ===\n")
  logs_dir <- file.path(PROJECT_ROOT, "02_Search_Strategy", "Logs")

  parse_log <- function(filename) {
    lines <- readLines(file.path(logs_dir, filename), warn = FALSE)
    get_field <- function(pat) {
      m <- grep(pat, lines, value = TRUE)[1]
      if (is.na(m)) return("")
      trimws(sub(paste0("^", pat), "", m))
    }
    recs_raw <- get_field("Records Retrieved:")
    recs <- as.integer(gsub("[^0-9].*", "", trimws(recs_raw)))
    list(date = get_field("Search Date:"), records = recs)
  }

  pm <- parse_log("PubMed_Search_Log.txt")
  em <- parse_log("Embase_Search_Log.txt")
  ws <- parse_log("WoS_Search_Log.txt")
  co <- parse_log("Cochrane_Search_Log.txt")
  sc <- parse_log("Scopus_Search_Log.txt")

  ss <- c(
    PubMed = paste0(
      '("Pancreatic Neoplasms"[MeSH Terms] OR "pancreatic cancer"[tiab] OR "pancreatic ductal adenocarcinoma"[tiab] OR "pancreatic adenocarcinoma"[tiab] OR "PDAC"[tiab] OR "pancreas cancer"[tiab] OR "cancer of the pancreas"[tiab]) AND ("early-onset"[tiab] OR "early onset"[tiab] OR "young-onset"[tiab] OR "young onset"[tiab] OR "young adults"[tiab] OR "young patients"[tiab] OR "adolescent and young adult"[tiab] OR "AYA"[tiab] OR "under 50"[tiab] OR "younger than 50"[tiab] OR "below 50"[tiab] OR "under 45"[tiab] OR "under 40"[tiab] OR "younger than 40"[tiab]) AND ("Survival"[MeSH] OR "Prognosis"[MeSH] OR "Treatment Outcome"[MeSH] OR "survival"[tiab] OR "prognosis"[tiab] OR "mortality"[tiab] OR "clinicopathological"[tiab] OR "clinicopathologic"[tiab] OR "clinical characteristics"[tiab] OR "clinical features"[tiab] OR "treatment pattern"[tiab] OR "surgical resection"[tiab] OR "chemotherapy"[tiab] OR "overall survival"[tiab]) NOT ("neuroendocrine"[tiab] OR "islet cell"[tiab] OR "solid pseudopapillary"[tiab] OR "IPMN"[tiab] OR "intraductal papillary"[tiab] OR "ampullary"[tiab] OR "ampulla of Vater"[tiab])',
      '\nFilters: English[Language]; Humans[MeSH]'),
    Embase = paste0(
      '#1 exp pancreas cancer/ or exp pancreatic ductal adenocarcinoma/\n',
      '#2 (pancreatic cancer or pancreatic ductal adenocarcinoma or pancreatic adenocarcinoma or PDAC or pancreas cancer).ti,ab.\n',
      '#3 1 or 2\n',
      '#4 (early-onset or early onset or young-onset or young onset or young adults or young patients or "adolescent and young adult" or AYA or under 50 or younger than 50 or below 50 or under 45 or under 40 or younger than 40).ti,ab.\n',
      '#5 exp young adult/\n',
      '#6 4 or 5\n',
      '#7 exp survival/ or exp prognosis/ or exp treatment outcome/\n',
      '#8 (survival or prognosis or mortality or clinicopathological or clinicopathologic or clinical characteristics or clinical features or treatment pattern or surgical resection or chemotherapy or overall survival).ti,ab.\n',
      '#9 7 or 8\n',
      '#10 (neuroendocrine or islet cell or solid pseudopapillary or IPMN or intraductal papillary or ampullary or ampulla of Vater).ti,ab.\n',
      '#11 exp neuroendocrine tumor/\n',
      '#12 10 or 11\n',
      '#13 3 and 6 and 9 not 12\n',
      '#14 limit 13 to (human and english language)\n',
      '#15 limit 14 to (article or article in press) not (conference abstract or conference paper)'),
    WoS = paste0(
      'TS=("pancreatic cancer" OR "pancreatic ductal adenocarcinoma" OR "pancreatic adenocarcinoma" OR "PDAC" OR "pancreas cancer" OR "pancreatic neoplasm*") AND TS=("early-onset" OR "early onset" OR "young-onset" OR "young onset" OR "young adults" OR "young patients" OR "adolescent and young adult" OR "AYA" OR "under 50" OR "younger than 50" OR "below 50" OR "under 45" OR "under 40" OR "younger than 40") AND TS=("survival" OR "prognosis" OR "mortality" OR "clinicopatholog*" OR "clinical characteristics" OR "clinical features" OR "treatment pattern*" OR "surgical resection" OR "chemotherapy" OR "overall survival") NOT TS=("neuroendocrine" OR "islet cell" OR "solid pseudopapillary" OR "IPMN" OR "intraductal papillary" OR "ampullary")',
      '\nRefined by: Languages = English; Document Types = Article'),
    Cochrane = paste0(
      '#1 MeSH descriptor: [Pancreatic Neoplasms] explode all trees OR ("pancreatic cancer" OR "pancreatic ductal adenocarcinoma" OR "pancreatic adenocarcinoma" OR "PDAC"):ti,ab,kw\n',
      '#2 ("early-onset" OR "early onset" OR "young-onset" OR "young onset" OR "young adults" OR "young patients" OR "adolescent and young adult" OR "under 50" OR "younger than 50" OR "under 45" OR "under 40" OR "younger than 40"):ti,ab,kw\n',
      '#3 MeSH descriptor: [Survival] explode all trees OR MeSH descriptor: [Prognosis] explode all trees OR MeSH descriptor: [Treatment Outcome] explode all trees OR ("survival" OR "prognosis" OR "mortality" OR "clinicopatholog*" OR "clinical characteristics" OR "treatment pattern" OR "overall survival"):ti,ab,kw\n',
      '#4 ("neuroendocrine" OR "islet cell" OR "solid pseudopapillary" OR "IPMN" OR "intraductal papillary" OR "ampullary"):ti,ab,kw\n',
      '#5 #1 AND #2 AND #3 NOT #4'),
    Scopus = paste0(
      'TITLE-ABS-KEY(("pancreatic cancer" OR "pancreatic ductal adenocarcinoma" OR "pancreatic adenocarcinoma" OR "PDAC" OR "pancreas cancer") AND ("early-onset" OR "early onset" OR "young-onset" OR "young onset" OR "young adults" OR "young patients" OR "adolescent and young adult" OR "AYA" OR "under 50" OR "younger than 50" OR "under 45" OR "under 40" OR "younger than 40") AND ("survival" OR "prognosis" OR "mortality" OR "clinicopatholog*" OR "clinical characteristics" OR "clinical features" OR "treatment pattern*" OR "surgical resection" OR "chemotherapy" OR "overall survival") AND NOT ("neuroendocrine" OR "islet cell" OR "solid pseudopapillary" OR "IPMN" OR "intraductal papillary" OR "ampullary"))',
      '\nAND LANGUAGE(english) AND DOCTYPE(ar)')
  )

  total_recs <- sum(pm$records, em$records, ws$records, co$records, sc$records)
  df <- data.frame(
    Database = c("PubMed", "Embase (Ovid)", "Web of Science", "Cochrane CENTRAL", "Scopus", "Total"),
    Platform = c("NCBI", "Ovid (1974-2026 Mar 18)", "Core Collection", "Cochrane Library", "Elsevier", ""),
    Search_Date = c(pm$date, em$date, ws$date, co$date, sc$date, pm$date),
    Search_Strategy = c(ss, paste0("Deduplication: DOI (719) + PMID (9) + exact title (23) + fuzzy title (5) = 756 duplicates removed")),
    Records_Retrieved = c(pm$records, em$records, ws$records, co$records, sc$records, total_recs),
    Unique_After_Dedup = c(NA, NA, NA, NA, NA, 1786),
    stringsAsFactors = FALSE, check.names = FALSE
  )

  wb <- createWorkbook()
  addWorksheet(wb, "Search_Strategies")
  writeData(wb, "Search_Strategies", df)
  style_lancet_wb(wb, "Search_Strategies", df, wrap_cols = 4, center_cols = c(3, 5, 6))
  setColWidths(wb, "Search_Strategies", cols = c(1,2,3,4,5,6), widths = c(22,24,14,80,18,18))
  setRowHeights(wb, "Search_Strategies", rows = 2:6, heights = 160)

  out_path <- file.path(PATHS$tables, "eTable_01_Search_Strategies.xlsx")
  saveWorkbook(wb, out_path, overwrite = TRUE)
  cat("  [OK] eTable 1:", nrow(df), "rows ->", basename(out_path), "\n")
}

# ==============================================================================
# eTable 2: Studies Excluded at Full-Text Stage
# ==============================================================================
create_etable_2 <- function() {
  cat("\n=== eTable 2: Studies Excluded at Full-Text Stage ===\n")
  screening <- read.csv(
    file.path(PROJECT_ROOT, "03_Screening", "Full_Text_Screening.csv"),
    stringsAsFactors = FALSE, fileEncoding = "UTF-8"
  )

  # Part 1: Studies explicitly marked as Exclude/Excluded in CSV
  csv_excluded <- screening[screening$Final_Decision %in% c("Exclude", "Excluded"), ]

  # Part 2: Studies marked Include in CSV but NOT in final 50 (age threshold exclusions)
  csv_include_with_id <- screening[screening$Final_Decision == "Include" &
                                     !is.na(screening$Study_ID) &
                                     nchar(screening$Study_ID) > 0, ]
  age_excluded <- csv_include_with_id[!(csv_include_with_id$Study_ID %in% included_ids), ]
  if (nrow(age_excluded) > 0) {
    age_excluded$Exclusion_Code <- "AT"
    age_excluded$Exclusion_Reason <- "Incompatible age threshold (>=70 years)"
  }

  excluded <- rbind(csv_excluded, age_excluded)

  reason_map <- c(
    "F2" = "No direct EOPC vs AOPC comparison or insufficient data",
    "F6" = "Full text not available",
    "F8" = "Non-original research (systematic review/meta-analysis/GBD analysis)",
    "AT" = "Incompatible age threshold (>=70 years)"
  )

  excluded$Clean_Reason <- sapply(seq_len(nrow(excluded)), function(i) {
    code <- trimws(as.character(excluded$Exclusion_Code[i]))
    if (code %in% names(reason_map)) return(reason_map[code])
    reason <- trimws(as.character(excluded$Exclusion_Reason[i]))
    if (!is.na(reason) && nchar(reason) > 0 && reason != "NA") return(reason)
    "Excluded during data extraction cross-validation"
  })
  excluded$Author_Year <- paste0(excluded$First_Author, " (", excluded$Year, ")")
  excluded <- excluded[order(-excluded$Year, excluded$First_Author), ]

  df <- data.frame(
    No = seq_len(nrow(excluded)),
    `First Author (Year)` = excluded$Author_Year,
    Title = excluded$Title,
    `Exclusion Reason` = excluded$Clean_Reason,
    stringsAsFactors = FALSE, check.names = FALSE
  )

  wb <- createWorkbook()
  addWorksheet(wb, "Excluded_Studies")
  writeData(wb, "Excluded_Studies", df)
  style_lancet_wb(wb, "Excluded_Studies", df, wrap_cols = c(3, 4), center_cols = 1)
  setColWidths(wb, "Excluded_Studies", cols = c(1,2,3,4), widths = c(5,24,60,48))

  n_f2 <- sum(grepl("No direct EOPC", df$`Exclusion Reason`))
  n_f6 <- sum(grepl("Full text not", df$`Exclusion Reason`))
  n_f8 <- sum(grepl("Non-original", df$`Exclusion Reason`))
  n_at <- sum(grepl("age threshold", df$`Exclusion Reason`, ignore.case = TRUE))
  n_ext <- nrow(df) - n_f2 - n_f6 - n_f8 - n_at
  fn_row <- nrow(df) + 3
  writeData(wb, "Excluded_Studies",
            data.frame(x = paste0("F2: No EOPC vs AOPC comparison (n=", n_f2,
                                  "); F6: Full text not available (n=", n_f6,
                                  "); F8: Non-original research (n=", n_f8,
                                  "); Age threshold incompatibility (n=", n_at,
                                  "); Extraction-phase exclusion (n=", n_ext, ")")),
            startRow = fn_row, colNames = FALSE)
  addStyle(wb, "Excluded_Studies",
           createStyle(fontName = "Arial", fontSize = 8, textDecoration = "italic"),
           rows = fn_row, cols = 1, stack = TRUE)

  out_path <- file.path(PATHS$tables, "eTable_02_Excluded_Studies.xlsx")
  saveWorkbook(wb, out_path, overwrite = TRUE)
  cat("  [OK] eTable 2:", nrow(df), "excluded ->", basename(out_path), "\n")
  cat("       F2:", n_f2, "| F6:", n_f6, "| F8:", n_f8,
      "| AT:", n_at, "| Extraction:", n_ext, "\n")
}

# ==============================================================================
# eTable 3: NOS Quality Assessment
# ==============================================================================
create_etable_3 <- function() {
  cat("\n=== eTable 3: NOS Quality Assessment ===\n")
  nos_all <- readxl::read_excel(
    file.path(PATHS$quality, "NOS_Assessment.xlsx"), sheet = "NOS_Cohort"
  )
  nos <- nos_all[nos_all$Study_ID %in% included_ids, ]
  cat("  Filtered:", nrow(nos), "of", nrow(nos_all), "studies\n")

  label_map <- setNames(master$Study_Label, master$Study_ID)

  df <- data.frame(
    Study = label_map[nos$Study_ID],
    S1 = nos$S1, S2 = nos$S2, S3 = nos$S3, S4 = nos$S4,
    Selection = as.integer(master$Selection_Stars[match(nos$Study_ID, master$Study_ID)]),
    C1 = nos$C1,
    Comparability = as.integer(master$Comparability_Stars[match(nos$Study_ID, master$Study_ID)]),
    O1 = nos$O1, O2 = nos$O2, O3 = nos$O3,
    Outcome = as.integer(master$Outcome_Stars[match(nos$Study_ID, master$Study_ID)]),
    Total = as.integer(nos$Total_Stars),
    Grade = nos$Quality_Grade,
    stringsAsFactors = FALSE, check.names = FALSE
  )
  df <- df[order(df$Study), ]

  wb <- createWorkbook()
  addWorksheet(wb, "NOS_Assessment")
  writeData(wb, "NOS_Assessment", df)
  style_lancet_wb(wb, "NOS_Assessment", df, center_cols = 2:ncol(df))
  setColWidths(wb, "NOS_Assessment", cols = 1, widths = 24)
  setColWidths(wb, "NOS_Assessment", cols = 2:ncol(df), widths = 12)
  add_quality_color(wb, "NOS_Assessment", ncol(df), nrow(df), df$Grade)

  n_good <- sum(df$Grade == "Good", na.rm = TRUE)
  n_fair <- sum(df$Grade == "Fair", na.rm = TRUE)
  fn_row <- nrow(df) + 3
  writeData(wb, "NOS_Assessment",
            data.frame(x = paste0("S1-S4: Selection domain; C1: Comparability domain; O1-O3: Outcome domain. ",
                                  "Good (>=7): ", n_good, " (", round(n_good/nrow(df)*100), "%); ",
                                  "Fair (5-6): ", n_fair, " (", round(n_fair/nrow(df)*100), "%)")),
            startRow = fn_row, colNames = FALSE)
  addStyle(wb, "NOS_Assessment",
           createStyle(fontName = "Arial", fontSize = 8, textDecoration = "italic"),
           rows = fn_row, cols = 1, stack = TRUE)

  out_path <- file.path(PATHS$tables, "eTable_03_NOS_Assessment.xlsx")
  saveWorkbook(wb, out_path, overwrite = TRUE)
  cat("  [OK] eTable 3:", nrow(df), "studies ->", basename(out_path), "\n")
}

# ==============================================================================
# eTable 4: ROBINS-E Assessment
# ==============================================================================
create_etable_4 <- function() {
  cat("\n=== eTable 4: ROBINS-E Assessment ===\n")
  rob_all <- readxl::read_excel(
    file.path(PATHS$quality, "ROBINS_E_Assessment.xlsx"), sheet = "Summary"
  )
  rob <- rob_all[rob_all$Study %in% included_ids, ]
  cat("  Filtered:", nrow(rob), "of", nrow(rob_all), "studies\n")

  label_map <- setNames(master$Study_Label, master$Study_ID)
  df <- data.frame(
    Study = label_map[rob$Study],
    `D1: Confounding` = rob$D1,
    `D2: Selection` = rob$D2,
    `D3: Classification` = rob$D3,
    `D4: Deviations` = rob$D4,
    `D5: Missing Data` = rob$D5,
    `D6: Measurement` = rob$D6,
    `D7: Reporting` = rob$D7,
    Overall = rob$Overall,
    stringsAsFactors = FALSE, check.names = FALSE
  )
  df <- df[order(df$Study), ]

  wb <- createWorkbook()
  addWorksheet(wb, "ROBINS_E_Assessment")
  writeData(wb, "ROBINS_E_Assessment", df)
  style_lancet_wb(wb, "ROBINS_E_Assessment", df, center_cols = 2:ncol(df))
  setColWidths(wb, "ROBINS_E_Assessment", cols = 1, widths = 24)
  setColWidths(wb, "ROBINS_E_Assessment", cols = 2:ncol(df), widths = 16)
  for (ci in 2:ncol(df))
    add_quality_color(wb, "ROBINS_E_Assessment", ci, nrow(df), df[[ci]])

  n_mod <- sum(df$Overall == "Moderate", na.rm = TRUE)
  n_ser <- sum(df$Overall == "Serious", na.rm = TRUE)
  fn_row <- nrow(df) + 3
  writeData(wb, "ROBINS_E_Assessment",
            data.frame(x = paste0("D1: Confounding; D2: Participant selection; D3: Exposure classification; ",
                                  "D4: Deviations from intended exposures; D5: Missing data; D6: Outcome measurement; ",
                                  "D7: Selection of reported result. ",
                                  "Moderate: ", n_mod, " (", round(n_mod/nrow(df)*100), "%); ",
                                  "Serious: ", n_ser, " (", round(n_ser/nrow(df)*100), "%)")),
            startRow = fn_row, colNames = FALSE)
  addStyle(wb, "ROBINS_E_Assessment",
           createStyle(fontName = "Arial", fontSize = 8, textDecoration = "italic"),
           rows = fn_row, cols = 1, stack = TRUE)

  out_path <- file.path(PATHS$tables, "eTable_04_ROBINS_E_Assessment.xlsx")
  saveWorkbook(wb, out_path, overwrite = TRUE)
  cat("  [OK] eTable 4:", nrow(df), "studies ->", basename(out_path), "\n")
}

# ==============================================================================
# eTable 5: Extended Study Characteristics
# ==============================================================================
create_etable_5 <- function() {
  cat("\n=== eTable 5: Extended Study Characteristics ===\n")
  fp <- function(x) { x <- suppressWarnings(as.numeric(as.character(x))); ifelse(is.na(x), "NR", sprintf("%.1f", x)) }
  fn <- function(x) { x <- suppressWarnings(as.numeric(as.character(x))); ifelse(is.na(x), "NR", format(x, big.mark = ",")) }
  fhr <- function(hr, lo, hi) {
    hr <- suppressWarnings(as.numeric(as.character(hr)))
    lo <- suppressWarnings(as.numeric(as.character(lo)))
    hi <- suppressWarnings(as.numeric(as.character(hi)))
    ifelse(is.na(hr), "NR", sprintf("%.2f [%.2f, %.2f]", hr, lo, hi))
  }

  m <- master[order(master$Study_Label), ]
  df <- data.frame(
    Study = m$Study_Label,
    Country = m$Country,
    Region = m$Region_Group,
    `Data Source` = m$Data_Source,
    `Study Period` = m$Study_Period,
    `Age Threshold` = m$Age_Threshold,
    `N Total` = fn(m$Total_N_num),
    `N EOPC` = fn(m$EOPC_N_num),
    `N AOPC` = fn(m$AOPC_N_num),
    `Male% EOPC` = fp(m$Male_Pct_EOPC),
    `Male% AOPC` = fp(m$Male_Pct_AOPC),
    `Smoking% EOPC` = fp(m$Smoking_Pct_EOPC),
    `Smoking% AOPC` = fp(m$Smoking_Pct_AOPC),
    `Diabetes% EOPC` = fp(m$Diabetes_Pct_EOPC),
    `Diabetes% AOPC` = fp(m$Diabetes_Pct_AOPC),
    `FamilyHx% EOPC` = fp(m$Family_Hx_Pct_EOPC),
    `FamilyHx% AOPC` = fp(m$Family_Hx_Pct_AOPC),
    `StageI% EOPC` = fp(m$Stage_I_Pct_EOPC),
    `StageI% AOPC` = fp(m$Stage_I_Pct_AOPC),
    `StageII% EOPC` = fp(m$Stage_II_Pct_EOPC),
    `StageII% AOPC` = fp(m$Stage_II_Pct_AOPC),
    `StageIII% EOPC` = fp(m$Stage_III_Pct_EOPC),
    `StageIII% AOPC` = fp(m$Stage_III_Pct_AOPC),
    `StageIV% EOPC` = fp(m$Stage_IV_Pct_EOPC),
    `StageIV% AOPC` = fp(m$Stage_IV_Pct_AOPC),
    `LN+% EOPC` = fp(m$LN_Positive_Pct_EOPC),
    `LN+% AOPC` = fp(m$LN_Positive_Pct_AOPC),
    `Head% EOPC` = fp(m$Tumor_Head_Pct_EOPC),
    `Head% AOPC` = fp(m$Tumor_Head_Pct_AOPC),
    `PoorDiff% EOPC` = fp(m$Poorly_Diff_Pct_EOPC),
    `PoorDiff% AOPC` = fp(m$Poorly_Diff_Pct_AOPC),
    `Surgery% EOPC` = fp(m$Surgery_Pct_EOPC),
    `Surgery% AOPC` = fp(m$Surgery_Pct_AOPC),
    `Chemo% EOPC` = fp(m$Chemo_Any_Pct_EOPC),
    `Chemo% AOPC` = fp(m$Chemo_Any_Pct_AOPC),
    `RT% EOPC` = fp(m$RT_Pct_EOPC),
    `RT% AOPC` = fp(m$RT_Pct_AOPC),
    `mOS EOPC` = fp(m$Median_OS_EOPC),
    `mOS AOPC` = fp(m$Median_OS_AOPC),
    `OS HR [95% CI]` = fhr(m$OS_HR, m$OS_HR_LCI, m$OS_HR_UCI),
    `DFS HR [95% CI]` = fhr(m$DFS_HR, m$DFS_HR_LCI, m$DFS_HR_UCI),
    `RFS HR [95% CI]` = fhr(m$RFS_HR, m$RFS_HR_LCI, m$RFS_HR_UCI),
    `NOS Stars` = m$Total_Stars,
    `ROBINS-E` = m$ROBINS_E_Overall,
    stringsAsFactors = FALSE, check.names = FALSE
  )

  wb <- createWorkbook()
  addWorksheet(wb, "Extended_Characteristics")
  writeData(wb, "Extended_Characteristics", df)
  style_lancet_wb(wb, "Extended_Characteristics", df,
                  wrap_cols = c(4, 5, 40, 41, 42), center_cols = 7:ncol(df))
  setColWidths(wb, "Extended_Characteristics", cols = 1, widths = 22)
  setColWidths(wb, "Extended_Characteristics", cols = 2:6, widths = 16)
  setColWidths(wb, "Extended_Characteristics", cols = 7:ncol(df), widths = 13)
  freezePane(wb, "Extended_Characteristics", firstCol = TRUE, firstRow = TRUE)
  rob_col <- which(colnames(df) == "ROBINS-E")
  add_quality_color(wb, "Extended_Characteristics", rob_col, nrow(df), df$`ROBINS-E`)

  out_path <- file.path(PATHS$tables, "eTable_05_Extended_Characteristics.xlsx")
  saveWorkbook(wb, out_path, overwrite = TRUE)
  cat("  [OK] eTable 5:", nrow(df), "studies x", ncol(df), "cols ->", basename(out_path), "\n")
}

# ==============================================================================
# Run all
# ==============================================================================
cat("\n========================================\n")
cat("Generating Supplementary eTables 1-5\n")
cat("========================================\n")

create_etable_1()
create_etable_2()
create_etable_3()
create_etable_4()
create_etable_5()

cat("\n========================================\n")
cat("All 5 eTables generated in:", PATHS$tables, "\n")
cat("========================================\n")
