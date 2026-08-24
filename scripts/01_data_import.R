################################################################################
# EOPC vs AOPC Systematic Review and Meta-Analysis
# Script 01: Data Import & Preprocessing
# Input:  Data_Extraction_Master_Filled_v2.xlsx (6 sheets)
#         NOS_Assessment.xlsx, ROBINS_E_Assessment.xlsx
# Output: prepared_data.RData
################################################################################

# Robust script directory detection (handles conda run ~+~ encoding)
.get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    fpath <- sub("--file=", "", file_arg)
    fpath <- gsub("~\\+~", " ", fpath)  # Fix conda run space encoding
    return(dirname(fpath))
  }
  for (i in seq_len(sys.nframe())) {
    ofile <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
    if (!is.null(ofile)) return(dirname(ofile))
  }
  return(".")
}
source(file.path(.get_script_dir(), "00_config.R"))

# ==============================================================================
# 1. Read Data Extraction Sheets
# ==============================================================================

data_file <- file.path(PATHS$data, "Data_Extraction_Master_Filled_v2.xlsx")
if (!file.exists(data_file)) {
  stop("Data file not found: ", data_file)
}

part_a <- read_excel(data_file, sheet = "Part_A_Study_Info")
part_b <- read_excel(data_file, sheet = "Part_B_Demographics")
part_c <- read_excel(data_file, sheet = "Part_C_Tumor")
part_d <- read_excel(data_file, sheet = "Part_D_Treatment")
part_e <- read_excel(data_file, sheet = "Part_E_Survival")
part_f <- read_excel(data_file, sheet = "Part_F_Notes")

cat("Raw data imported:", nrow(part_a), "rows across 6 sheets.\n")

# ==============================================================================
# 2. Filter to Included Studies (CV_Status == "Done")
# ==============================================================================

included_ids <- part_a %>%
  filter(CV_Status == "Done") %>%
  filter(is.na(Exclusion_Status) | Exclusion_Status != "EXCLUDED") %>%
  pull(Study_ID)

cat("Included studies (CV_Status='Done'):", length(included_ids), "\n")

part_a <- part_a %>% filter(Study_ID %in% included_ids)
part_b <- part_b %>% filter(Study_ID %in% included_ids)
part_c <- part_c %>% filter(Study_ID %in% included_ids)
part_d <- part_d %>% filter(Study_ID %in% included_ids)
part_e <- part_e %>% filter(Study_ID %in% included_ids)
part_f <- part_f %>% filter(Study_ID %in% included_ids)

# ==============================================================================
# 3. Parse Study_ID -> First_Author + Year + Study_Label
# ==============================================================================

part_a <- part_a %>%
  mutate(
    First_Author = sub("_\\d{4}$", "", Study_ID),
    Year         = as.integer(sub("^.*_(\\d{4})$", "\\1", Study_ID)),
    Study_Label  = paste0(First_Author, " (", Year, ")")
  )

# ==============================================================================
# 4. Read Quality Assessment Data
# ==============================================================================

nos_file <- file.path(PATHS$quality, "NOS_Assessment.xlsx")
rob_file <- file.path(PATHS$quality, "ROBINS_E_Assessment.xlsx")

# NOS Summary
nos_summary <- read_excel(nos_file, sheet = "Summary") %>%
  dplyr::select(Study_ID, Selection_Stars, Comparability_Stars, Outcome_Stars,
                Total_Stars, Quality_Grade) %>%
  mutate(Total_Stars = as.numeric(Total_Stars))

# ROBINS-E Summary
robins_summary <- read_excel(rob_file, sheet = "Summary") %>%
  rename(Study_ID = Study) %>%
  dplyr::select(Study_ID, D1, D2, D3, D4, D5, D6, D7, Overall) %>%
  rename(ROBINS_E_Overall = Overall)

cat("NOS assessment:", nrow(nos_summary), "studies\n")
cat("ROBINS-E assessment:", nrow(robins_summary), "studies\n")

# ==============================================================================
# 5. Build Master Dataset
# ==============================================================================

# Merge all parts by Study_ID
master <- part_a %>%
  left_join(part_b, by = "Study_ID") %>%
  left_join(part_c, by = "Study_ID") %>%
  left_join(part_d, by = "Study_ID") %>%
  left_join(part_e, by = "Study_ID") %>%
  left_join(nos_summary, by = "Study_ID") %>%
  left_join(robins_summary, by = "Study_ID")

cat("Master dataset:", nrow(master), "studies x", ncol(master), "variables.\n")

# ==============================================================================
# 6. Prepare Subgroup Variables
# ==============================================================================

master <- master %>%
  mutate(
    # Age threshold (numeric)
    Age_Threshold_Num = normalize_age_threshold(Age_Threshold),

    # Age threshold group
    Age_Threshold_Group = case_when(
      Age_Threshold_Num <= 40 ~ "<=40",
      Age_Threshold_Num <= 45 ~ "<=45",
      Age_Threshold_Num <= 50 ~ "<=50",
      Age_Threshold_Num <= 55 ~ "<=55",
      TRUE ~ "Other"
    ),

    # Region group
    Region_Group = case_when(
      Region %in% c("North America", "USA", "Canada") ~ "North America",
      Region %in% c("Europe", "Western Europe", "Southern Europe") ~ "Europe",
      Region %in% c("Asia", "East Asia", "Southeast Asia", "Asia-Pacific") ~ "Asia",
      TRUE ~ "Other"
    ),

    # Data source group
    Data_Source_Group = case_when(
      grepl("SEER|NCDB|NCD|NLCS|Registry|Database|database|registry",
            Data_Source, ignore.case = TRUE) ~ "Database",
      TRUE ~ "Institution"
    ),

    # Database cluster (for SEER/NCDB overlap sensitivity)
    Database_Cluster = case_when(
      grepl("SEER", Data_Source, ignore.case = TRUE) ~ "SEER",
      grepl("NCDB|National Cancer Data", Data_Source, ignore.case = TRUE) ~ "NCDB",
      TRUE ~ "Other"
    ),

    # NOS quality group
    NOS_Quality_Group = ifelse(Total_Stars >= 7, "Good (NOS>=7)", "Fair (NOS<7)"),

    # ROBINS-E group
    ROBINS_E_Group = ifelse(ROBINS_E_Overall %in% c("Low", "Moderate"),
                            "Low/Moderate", "Serious"),

    # Sample size
    Total_N_num = suppressWarnings(as.numeric(Total_N)),
    EOPC_N_num  = suppressWarnings(as.numeric(EOPC_N)),
    AOPC_N_num  = suppressWarnings(as.numeric(AOPC_N)),

    # EOPC proportion
    EOPC_Proportion = ifelse(!is.na(EOPC_N_num) & !is.na(Total_N_num) & Total_N_num > 0,
                             EOPC_N_num / Total_N_num, NA_real_),

    # Publication era (for temporal trend subgroup analysis)
    Publication_Era = ifelse(Year < 2020, "Pre-2020", "2020+"),

    # Study period midpoint (treatment era moderator; R3-3)
    # Parse Study_Period string -> extract first & last 4-digit year -> midpoint
    Study_Period_Mid = purrr::map_dbl(as.character(Study_Period), function(sp) {
      yrs <- as.integer(regmatches(sp, gregexpr("\\d{4}", sp))[[1]])
      if (length(yrs) >= 2) mean(yrs) else if (length(yrs) == 1) yrs[1] else NA_real_
    })
  )

# ==============================================================================
# 7. Prepare Time-to-Event Data (OS, DFS, RFS)
# ==============================================================================

prepare_hr_data <- function(dat, hr_col, lci_col, uci_col, source_col, label) {
  d <- dat %>%
    mutate(
      hr_val  = suppressWarnings(as.numeric(.data[[hr_col]])),
      lci_val = suppressWarnings(as.numeric(.data[[lci_col]])),
      uci_val = suppressWarnings(as.numeric(.data[[uci_col]])),
      hr_source_raw = as.character(.data[[source_col]])
    ) %>%
    filter(!is.na(hr_val), !is.na(lci_val), !is.na(uci_val),
           hr_val > 0, lci_val > 0, uci_val > 0) %>%
    mutate(
      logHR   = log(hr_val),
      seLogHR = (log(uci_val) - log(lci_val)) / (2 * qnorm(0.975)),
      HR_Source_Group = case_when(
        Study_ID == "Piciucchi_2015" ~ "Multivariable", # published HR 0.7 (0.4-1.1) is stage-adjusted multivariable Cox; IPD-reconstructed 0.74 reported separately (script 08)
        Study_ID == "Castet_2023" ~ "Multivariable",
        Study_ID == "Mendis_2024" ~ "Univariable",
        grepl("ultivar", hr_source_raw, ignore.case = TRUE) ~ "Multivariable",
        grepl("nivar",   hr_source_raw, ignore.case = TRUE) ~ "Univariable",
        grepl("IPD|reconstruct", hr_source_raw, ignore.case = TRUE) ~ "IPD",
        TRUE ~ "Other"
      )
    )

  cat(label, ":", nrow(d), "studies with valid HR data.\n")
  return(d)
}

# Exclude Dai_2020: reports cancer-specific survival (CSS) using Fine-Gray
# subdistribution hazard model, NOT overall survival. Cannot pool with Cox OS HRs.
dat_os <- prepare_hr_data(master, "OS_HR", "OS_HR_LCI", "OS_HR_UCI",
                          "OS_HR_Source", "OS") %>%
  filter(Study_ID != "Dai_2020")
cat("  [Excluded Dai_2020 from OS: CSS via Fine-Gray model, not OS]\n")
dat_dfs <- prepare_hr_data(master, "DFS_HR", "DFS_HR_LCI", "DFS_HR_UCI",
                           "DFS_HR_Source", "DFS")
dat_rfs <- prepare_hr_data(master, "RFS_HR", "RFS_HR_LCI", "RFS_HR_UCI",
                           "RFS_HR", "RFS")  # RFS may not have Source column

# --- DFS/RFS deduplication ---
# Some studies report one endpoint (DFS or RFS) that was recorded in both columns
# with identical HRs. To avoid double-counting in separate meta-analyses,
# we assign each to the endpoint terminology used in the original publication:
#   Zironda_2024: paper reports "recurrence-free survival" → keep in RFS only
#   Kim_2023:     paper reports "recurrence-free survival" → keep in RFS only
# Confirmed via manual review of original publications.
rfs_in_paper <- c("Zironda_2024", "Kim_2023")  # Studies whose DFS column is actually RFS

if (nrow(dat_dfs) > 0 && nrow(dat_rfs) > 0) {
  overlap_ids <- intersect(dat_dfs$Study_ID, dat_rfs$Study_ID)
  for (sid in overlap_ids) {
    hr_dfs <- dat_dfs$hr_val[dat_dfs$Study_ID == sid]
    hr_rfs <- dat_rfs$hr_val[dat_rfs$Study_ID == sid]
    if (length(hr_dfs) == 1 && length(hr_rfs) == 1 && hr_dfs == hr_rfs) {
      if (sid %in% rfs_in_paper) {
        # Paper reports RFS → keep in RFS, remove from DFS
        dat_dfs <- dat_dfs %>% filter(Study_ID != sid)
        cat("  [Dedup]", sid, ": paper reports RFS, removed from DFS\n")
      } else {
        # Paper reports DFS → keep in DFS, remove from RFS
        dat_rfs <- dat_rfs %>% filter(Study_ID != sid)
        cat("  [Dedup]", sid, ": paper reports DFS, removed from RFS\n")
      }
    }
  }
  cat("  After dedup: DFS k=", nrow(dat_dfs), ", RFS k=", nrow(dat_rfs), "\n")
}

# ==============================================================================
# 8. Prepare Binary Outcome Data (OR from percentages)
# ==============================================================================

prepare_binary_data <- function(dat, pct_eopc_col, pct_aopc_col, label) {
  d <- dat %>%
    mutate(
      pct_e = clean_percentage(.data[[pct_eopc_col]]),
      pct_c = clean_percentage(.data[[pct_aopc_col]])
    ) %>%
    filter(!is.na(pct_e), !is.na(pct_c),
           !is.na(EOPC_N_num), !is.na(AOPC_N_num),
           EOPC_N_num > 0, AOPC_N_num > 0) %>%
    mutate(
      event_e  = compute_events(pct_e, EOPC_N_num),
      event_c  = compute_events(pct_c, AOPC_N_num),
      n_e      = as.integer(EOPC_N_num),
      n_c      = as.integer(AOPC_N_num)
    ) %>%
    # Ensure events don't exceed sample size
    mutate(
      event_e = pmin(event_e, n_e),
      event_c = pmin(event_c, n_c)
    )

  cat(label, ":", nrow(d), "studies with valid binary data.\n")
  return(d)
}

# --- Demographics ---
dat_male <- prepare_binary_data(master, "Male_Pct_EOPC", "Male_Pct_AOPC", "Male %")
dat_smoking   <- prepare_binary_data(master, "Smoking_Pct_EOPC",   "Smoking_Pct_AOPC",   "Smoking %")
dat_diabetes  <- prepare_binary_data(master, "Diabetes_Pct_EOPC",  "Diabetes_Pct_AOPC",  "Diabetes %")
dat_family_hx <- prepare_binary_data(master, "Family_Hx_Pct_EOPC", "Family_Hx_Pct_AOPC", "Family Hx %")

# --- Tumor characteristics ---
dat_stage_iv     <- prepare_binary_data(master, "Stage_IV_Pct_EOPC",     "Stage_IV_Pct_AOPC",     "Stage IV %")
dat_stage_i      <- prepare_binary_data(master, "Stage_I_Pct_EOPC",      "Stage_I_Pct_AOPC",      "Stage I %")
dat_stage_ii     <- prepare_binary_data(master, "Stage_II_Pct_EOPC",     "Stage_II_Pct_AOPC",     "Stage II %")
dat_stage_iii    <- prepare_binary_data(master, "Stage_III_Pct_EOPC",    "Stage_III_Pct_AOPC",    "Stage III %")
dat_poorly_diff  <- prepare_binary_data(master, "Poorly_Diff_Pct_EOPC",  "Poorly_Diff_Pct_AOPC",  "Poorly Diff %")
dat_mod_diff     <- prepare_binary_data(master, "Mod_Diff_Pct_EOPC",     "Mod_Diff_Pct_AOPC",     "Mod Diff %")
dat_well_diff    <- prepare_binary_data(master, "Well_Diff_Pct_EOPC",    "Well_Diff_Pct_AOPC",    "Well Diff %")
dat_ln_positive  <- prepare_binary_data(master, "LN_Positive_Pct_EOPC",  "LN_Positive_Pct_AOPC",  "LN Positive %")
dat_metastatic   <- prepare_binary_data(master, "Metastatic_Pct_EOPC",   "Metastatic_Pct_AOPC",   "Metastatic %")
dat_tumor_head   <- prepare_binary_data(master, "Tumor_Head_Pct_EOPC",   "Tumor_Head_Pct_AOPC",   "Tumor Head %")
dat_tumor_body   <- prepare_binary_data(master, "Tumor_Body_Pct_EOPC",   "Tumor_Body_Pct_AOPC",   "Tumor Body %")
dat_tumor_tail   <- prepare_binary_data(master, "Tumor_Tail_Pct_EOPC",   "Tumor_Tail_Pct_AOPC",   "Tumor Tail %")
dat_vasc_inv     <- prepare_binary_data(master, "Vascular_Invasion_Pct_EOPC", "Vascular_Invasion_Pct_AOPC", "Vascular Invasion %")

# --- Treatment ---
dat_surgery        <- prepare_binary_data(master, "Surgery_Pct_EOPC",           "Surgery_Pct_AOPC",           "Surgery %")
dat_chemo          <- prepare_binary_data(master, "Chemo_Any_Pct_EOPC",         "Chemo_Any_Pct_AOPC",         "Chemo %")
dat_chemo_neo      <- prepare_binary_data(master, "Chemo_Neoadjuvant_Pct_EOPC", "Chemo_Neoadjuvant_Pct_AOPC", "Neoadjuvant Chemo %")
dat_chemo_adj      <- prepare_binary_data(master, "Chemo_Adjuvant_Pct_EOPC",    "Chemo_Adjuvant_Pct_AOPC",    "Adjuvant Chemo %")
dat_rt             <- prepare_binary_data(master, "RT_Pct_EOPC",                "RT_Pct_AOPC",                "RT %")
dat_whipple        <- prepare_binary_data(master, "Whipple_Pct_EOPC",           "Whipple_Pct_AOPC",           "Whipple %")
dat_distal         <- prepare_binary_data(master, "Distal_Pct_EOPC",            "Distal_Pct_AOPC",            "Distal Panc %")
dat_r0             <- prepare_binary_data(master, "R0_Resection_Pct_EOPC",      "R0_Resection_Pct_AOPC",      "R0 Resection %")

# --- Time-point overall survival (pooled as binary OR) ---
dat_os_5yr <- prepare_binary_data(master, "OS_5yr_EOPC", "OS_5yr_AOPC", "5-yr OS %")
dat_os_3yr <- prepare_binary_data(master, "OS_3yr_EOPC", "OS_3yr_AOPC", "3-yr OS %")
dat_os_1yr <- prepare_binary_data(master, "OS_1yr_EOPC", "OS_1yr_AOPC", "1-yr OS %")
dat_os_2yr <- prepare_binary_data(master, "OS_2yr_EOPC", "OS_2yr_AOPC", "2-yr OS %")

# --- Additional Demographics (data maximization) ---
dat_alcohol <- prepare_binary_data(master, "Alcohol_Pct_EOPC", "Alcohol_Pct_AOPC", "Alcohol %")

# --- Race/Ethnicity ---
dat_white <- prepare_binary_data(master, "White_Pct_EOPC", "White_Pct_AOPC", "White Race %")
dat_black <- prepare_binary_data(master, "Black_Pct_EOPC", "Black_Pct_AOPC", "Black Race %")
dat_asian <- prepare_binary_data(master, "Asian_Pct_EOPC", "Asian_Pct_AOPC", "Asian Race %")
# Exclude ethnically homogeneous cohorts (100%/100% Asian → zero informative contrast)
dat_asian <- dat_asian %>% filter(!Study_ID %in% c("Tang_2021", "Eguchi_2016", "Ohmoto_2016"))
cat("  [Filtered Asian Race: excluded 3 homogeneous cohorts, k=", nrow(dat_asian), "]\n")

# --- Additional Tumor characteristics ---
dat_pni <- prepare_binary_data(master, "Perineural_Invasion_Pct_EOPC", "Perineural_Invasion_Pct_AOPC", "Perineural Invasion %")

# --- Additional Treatment ---
dat_no_tx       <- prepare_binary_data(master, "No_Treatment_Pct_EOPC",          "No_Treatment_Pct_AOPC",          "No Treatment %")
dat_palliative  <- prepare_binary_data(master, "Palliative_Chemo_Pct_EOPC",      "Palliative_Chemo_Pct_AOPC",      "Palliative Chemo %")
dat_total_panc  <- prepare_binary_data(master, "Total_Pancreatectomy_Pct_EOPC",  "Total_Pancreatectomy_Pct_AOPC",  "Total Pancreatectomy %")

# ==============================================================================
# 9. Validation
# ==============================================================================

cat("\n==== Data Validation ====\n")

# Check Study_ID consistency
all_ids_match <- all(part_b$Study_ID %in% part_a$Study_ID) &&
                 all(part_c$Study_ID %in% part_a$Study_ID) &&
                 all(part_d$Study_ID %in% part_a$Study_ID) &&
                 all(part_e$Study_ID %in% part_a$Study_ID)
cat("Study_ID consistency across sheets:", ifelse(all_ids_match, "PASS", "FAIL"), "\n")

# Check HR data
cat("\nOS HR direction check (HR<1 = EOPC better):\n")
if (nrow(dat_os) > 0) {
  cat("  HR < 1 (EOPC better):", sum(dat_os$hr_val < 1), "studies\n")
  cat("  HR > 1 (AOPC better):", sum(dat_os$hr_val > 1), "studies\n")
  cat("  HR range:", sprintf("%.2f - %.2f", min(dat_os$hr_val), max(dat_os$hr_val)), "\n")
}

# Check quality data merge
qa_coverage <- sum(!is.na(master$Total_Stars))
cat("\nNOS quality data coverage:", qa_coverage, "/", nrow(master), "studies\n")
cat("NOS distribution:\n")
if (qa_coverage > 0) print(table(master$NOS_Quality_Group, useNA = "ifany"))

robins_coverage <- sum(!is.na(master$ROBINS_E_Overall))
cat("ROBINS-E coverage:", robins_coverage, "/", nrow(master), "studies\n")
if (robins_coverage > 0) print(table(master$ROBINS_E_Group, useNA = "ifany"))

# Subgroup distribution
cat("\nAge Threshold distribution:\n")
print(table(master$Age_Threshold_Group, useNA = "ifany"))
cat("Region distribution:\n")
print(table(master$Region_Group, useNA = "ifany"))
cat("Data Source distribution:\n")
print(table(master$Data_Source_Group, useNA = "ifany"))
cat("Publication Era distribution:\n")
print(table(master$Publication_Era, useNA = "ifany"))

# ==============================================================================
# 10. Save Prepared Data
# ==============================================================================

save(master, dat_os, dat_dfs, dat_rfs,
     # Demographics
     dat_male, dat_smoking, dat_diabetes, dat_family_hx,
     # Tumor characteristics
     dat_stage_iv, dat_stage_i, dat_stage_ii, dat_stage_iii,
     dat_poorly_diff, dat_mod_diff, dat_well_diff,
     dat_ln_positive, dat_metastatic,
     dat_tumor_head, dat_tumor_body, dat_tumor_tail, dat_vasc_inv,
     # Treatment
     dat_surgery, dat_chemo, dat_chemo_neo, dat_chemo_adj,
     dat_rt, dat_whipple, dat_distal, dat_r0,
     # Time-point survival
     dat_os_5yr, dat_os_3yr, dat_os_2yr, dat_os_1yr,
     # Additional demographics (data maximization)
     dat_alcohol, dat_white, dat_black, dat_asian,
     # Additional tumor
     dat_pni,
     # Additional treatment
     dat_no_tx, dat_palliative, dat_total_panc,
     # Quality assessment
     nos_summary, robins_summary,
     file = file.path(PATHS$stat_data, "prepared_data.RData"))

cat("\nPrepared data saved to:", file.path(PATHS$stat_data, "prepared_data.RData"), "\n")
cat("Total:", nrow(master), "studies ready for analysis.\n")
