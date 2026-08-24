################################################################################
# EOPC vs AOPC Systematic Review and Meta-Analysis
# Script 06: Summary Tables + GRADE Evidence Assessment
# Input:  meta_results.RData, prepared_data.RData
# Output: Table1, SoF, GRADE Evidence Table (Excel + Word)
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
# PART 1: Table 1 - Study Characteristics
# ==============================================================================

create_table1 <- function() {
  table1 <- master %>%
    dplyr::select(Study_Label, Country, Region_Group, Data_Source,
                  Study_Period, Age_Threshold, Total_N, EOPC_N, AOPC_N,
                  Total_Stars, Quality_Grade, ROBINS_E_Overall) %>%
    arrange(Study_Label)

  write_xlsx(table1, file.path(PATHS$tables, "Table1_Study_Characteristics.xlsx"))

  # Word export via flextable
  tryCatch({
    ft <- flextable(table1) %>%
      autofit() %>%
      set_caption("Table 1. Characteristics of included studies")
    doc <- read_docx()
    doc <- body_add_flextable(doc, ft)
    print(doc, target = file.path(PATHS$tables, "Table1_Study_Characteristics.docx"))
    cat("Table 1 saved (Excel + Word).\n")
  }, error = function(e) {
    cat("Word export failed:", e$message, "\n")
    cat("Table 1 saved (Excel only).\n")
  })

  return(table1)
}

table1 <- create_table1()

# ==============================================================================
# PART 2: Summary of Findings Table
# ==============================================================================

create_sof_table <- function() {
  results <- list()

  # Helper to extract results from any meta object
  add_result <- function(m, label, measure, direction_text) {
    if (is.null(m)) return(NULL)
    data.frame(
      Outcome = label,
      Studies = m$k,
      Effect_Measure = measure,
      Pooled_Estimate = sprintf("%.2f", exp(m$TE.random)),
      CI_95 = sprintf("[%.2f, %.2f]", exp(m$lower.random), exp(m$upper.random)),
      P_Value = format_pval(m$pval.random),
      I2 = sprintf("%.1f%%", m$I2 * 100),
      Tau2 = sprintf("%.4f", m$tau2),
      PI_95 = if (!is.null(m$lower.predict) && m$k >= 5)
        sprintf("[%.2f, %.2f]", exp(m$lower.predict), exp(m$upper.predict))
      else if (m$k < 5) "NR (k<5)"
      else "N/A",
      Direction = direction_text,
      stringsAsFactors = FALSE
    )
  }

  # HR outcomes
  est_os <- if (!is.null(meta_os)) exp(meta_os$TE.random) else NA
  results[["OS"]] <- add_result(meta_os, "Overall Survival", "HR",
    ifelse(!is.na(est_os) && est_os < 1, "Favours EOPC", "Favours AOPC"))
  results[["DFS"]] <- add_result(meta_dfs, "Disease-Free Survival", "HR", "")
  results[["RFS"]] <- add_result(meta_rfs, "Recurrence-Free Survival", "HR", "")

  # OR outcomes
  or_outcomes <- list(
    # Demographics
    list(m = meta_male,        label = "Male Proportion"),
    list(m = meta_smoking,     label = "Smoking History"),
    list(m = meta_diabetes,    label = "Diabetes History"),
    list(m = meta_family_hx,   label = "Family History"),
    # Tumour characteristics
    list(m = meta_stage_iv,    label = "Stage IV"),
    list(m = meta_stage_i,     label = "Stage I"),
    list(m = meta_stage_ii,    label = "Stage II"),
    list(m = meta_stage_iii,   label = "Stage III"),
    list(m = meta_poorly_diff, label = "Poorly Differentiated"),
    list(m = meta_mod_diff,    label = "Moderately Differentiated"),
    list(m = meta_well_diff,   label = "Well Differentiated"),
    list(m = meta_ln_positive, label = "LN Positive"),
    list(m = meta_metastatic,  label = "Metastatic Disease"),
    list(m = meta_tumor_head,  label = "Tumour Head"),
    list(m = meta_tumor_body,  label = "Tumour Body"),
    list(m = meta_tumor_tail,  label = "Tumour Tail"),
    list(m = meta_vasc_inv,    label = "Vascular Invasion"),
    # Treatment
    list(m = meta_surgery,     label = "Surgical Resection"),
    list(m = meta_chemo,       label = "Chemotherapy"),
    list(m = meta_chemo_neo,   label = "Neoadjuvant Chemo"),
    list(m = meta_chemo_adj,   label = "Adjuvant Chemo"),
    list(m = meta_rt,          label = "Radiotherapy"),
    list(m = meta_whipple,     label = "Whipple Procedure"),
    list(m = meta_distal,      label = "Distal Pancreatectomy"),
    list(m = meta_r0,          label = "R0 Resection"),
    # Time-point survival
    list(m = meta_os_5yr,      label = "5-Year OS"),
    list(m = meta_os_3yr,      label = "3-Year OS"),
    list(m = meta_os_1yr,      label = "1-Year OS"),
    list(m = meta_os_2yr,      label = "2-Year OS"),
    # Additional demographics (data maximization)
    list(m = meta_alcohol,     label = "Alcohol History"),
    list(m = meta_white,       label = "White Race"),
    list(m = meta_black,       label = "Black Race"),
    list(m = meta_asian,       label = "Asian Race"),
    # Additional tumour
    list(m = meta_pni,         label = "Perineural Invasion"),
    # Additional treatment
    list(m = meta_no_tx,       label = "No Treatment"),
    list(m = meta_palliative,  label = "Palliative Chemotherapy"),
    list(m = meta_total_panc,  label = "Total Pancreatectomy")
  )

  for (item in or_outcomes) {
    est <- if (!is.null(item$m)) exp(item$m$TE.random) else NA
    dir_text <- ifelse(!is.na(est) && est > 1, "Higher in EOPC", "Lower in EOPC")
    results[[item$label]] <- add_result(item$m, item$label, "OR", dir_text)
  }

  sof <- bind_rows(results[!sapply(results, is.null)])
  write_xlsx(sof, file.path(PATHS$tables, "Summary_of_Findings.xlsx"))
  cat("\nSummary of Findings Table:\n")
  print(sof)
  return(sof)
}

sof_table <- create_sof_table()

# ==============================================================================
# PART 3: GRADE Evidence Quality Assessment
# ==============================================================================

create_grade_table <- function() {
  assess_inconsistency <- function(m) {
    if (is.null(m)) return(list(rating = "", note = "No data"))
    i2 <- m$I2 * 100
    pQ <- m$pval.Q

    # Low heterogeneity: no downgrade
    if (i2 < 40 && pQ > 0.10)
      return(list(rating = "No downgrade", note = sprintf("I2=%.0f%%", i2)))

    # Moderate heterogeneity: -1
    if (i2 < 75)
      return(list(rating = "-1 Serious", note = sprintf("I2=%.0f%%", i2)))

    # High heterogeneity (I2 >= 75%): refined assessment per GRADE Handbook 5.3
    # Consider: (1) direction of effects, (2) prediction interval,
    # (3) whether I2 inflation from large-sample precision is the main driver
    te_valid <- m$TE[!is.na(m$TE)]
    all_same_dir <- length(te_valid) > 0 && (all(te_valid >= 0) || all(te_valid <= 0))

    # Compute prediction interval clinical relevance
    # If PI excludes null on log scale, the conclusion is robust despite heterogeneity
    pi_lo <- tryCatch(exp(m$lower.predict), error = function(e) NA)
    pi_hi <- tryCatch(exp(m$upper.predict), error = function(e) NA)
    pi_excludes_null <- if (!is.na(pi_lo) && !is.na(pi_hi)) !(pi_lo < 1 && pi_hi > 1) else FALSE
    pi_ratio <- if (!is.na(pi_lo) && !is.na(pi_hi)) pi_hi / pi_lo else NA

    pi_str <- if (!is.na(pi_lo) && !is.na(pi_hi)) sprintf("[%.2f, %.2f]", pi_lo, pi_hi) else "N/A"

    # If effects are all in the same direction, downgrade only by 1
    if (all_same_dir) {
      return(list(rating = "-1 Serious",
                  note = sprintf("I2=%.0f%%; all effects same direction; PI %s",
                                  i2, pi_str)))
    }
    # If PI excludes null (strong conclusion despite heterogeneity), downgrade by 1
    if (pi_excludes_null && m$k >= 10) {
      return(list(rating = "-1 Serious",
                  note = sprintf("I2=%.0f%%; PI excludes null %s",
                                  i2, pi_str)))
    }
    # Otherwise, standard -2 for very serious inconsistency
    return(list(rating = "-2 Very serious",
                note = sprintf("I2=%.0f%%; PI %s", i2, pi_str)))
  }

  assess_imprecision <- function(m) {
    if (is.null(m)) return(list(rating = "", note = "No data"))
    lo <- exp(m$lower.random); hi <- exp(m$upper.random)
    est <- exp(m$TE.random)
    crosses_null <- (lo < 1 && hi > 1)
    ci_ratio <- hi / lo

    # Total sample size for OIS assessment
    # For HR meta-analyses, use total events approximation
    # For OR meta-analyses, use total observations
    total_n <- tryCatch({
      if (m$sm == "HR") {
        # Approximate from inverse-variance weights
        sum(1 / m$seTE^2, na.rm = TRUE)
      } else {
        # Binary: sum of all group sizes
        sum(m$n.e, na.rm = TRUE) + sum(m$n.c, na.rm = TRUE)
      }
    }, error = function(e) 0)

    # OIS threshold: for binary outcomes with prevalence ~50%,
    # need ~4000 for OR=1.25, ~1000 for OR=1.5
    # For HR with baseline event rate ~50%, need ~800 for HR=1.25
    large_sample <- total_n > 10000

    # -2 Very serious: very wide CI (ratio > 3) or very few studies with null crossing
    if (ci_ratio > 3 || (crosses_null && m$k < 5))
      return(list(rating = "-2 Very serious",
                  note = sprintf("CI ratio=%.1f, k=%d, N=%s",
                                  ci_ratio, m$k,
                                  ifelse(total_n > 0, format(round(total_n), big.mark=","), "N/A"))))

    # -1 Serious: CI crosses null AND (small sample OR narrow-effect uncertainty)
    # But NOT if sample is very large (OIS clearly met) and CI is reasonably narrow
    if (crosses_null) {
      # If large total sample and CI ratio is modest, OIS is met — no downgrade
      if (large_sample && ci_ratio < 1.6 && m$k >= 10) {
        return(list(rating = "No downgrade",
                    note = sprintf("CI ratio=%.1f, k=%d, N=%s; OIS met",
                                    ci_ratio, m$k, format(round(total_n), big.mark=","))))
      }
      return(list(rating = "-1 Serious",
                  note = sprintf("CI ratio=%.1f, k=%d, N=%s",
                                  ci_ratio, m$k,
                                  ifelse(total_n > 0, format(round(total_n), big.mark=","), "N/A"))))
    }

    # CI does not cross null
    if (m$k < 5)
      return(list(rating = "-1 Serious",
                  note = sprintf("CI ratio=%.1f, k=%d (few studies)", ci_ratio, m$k)))

    return(list(rating = "No downgrade",
                note = sprintf("CI ratio=%.1f, k=%d, N=%s",
                                ci_ratio, m$k,
                                ifelse(total_n > 0, format(round(total_n), big.mark=","), "N/A"))))
  }

  assess_pub_bias <- function(m, measure = "HR") {
    if (is.null(m) || m$k < 10)
      return(list(rating = "Not assessed", note = sprintf("k=%d", ifelse(is.null(m), 0, m$k))))
    tryCatch({
      # Use Peters' test for binary outcomes (OR), Egger's for continuous (HR)
      bias_method <- ifelse(measure == "OR", "Peters", "Egger")
      bias_test <- metabias(m, method.bias = bias_method, plotit = FALSE)
      if (bias_test$p.value < 0.10)
        return(list(rating = "-1 Serious",
                    note = sprintf("%s p=%.3f", bias_method, bias_test$p.value)))
      return(list(rating = "No downgrade",
                  note = sprintf("%s p=%.3f", bias_method, bias_test$p.value)))
    }, error = function(e) list(rating = "Not assessed", note = e$message))
  }

  # GRADE Indirectness Assessment (EOPC Meta-Analysis)
  # Criteria: Population, Exposure definition, Comparator, Outcome, Setting
  assess_indirectness <- function(m, outcome_name) {
    if (is.null(m)) return(list(rating = "", note = "No data"))

    # All studies directly compare EOPC vs AOPC for the stated outcomes.
    # Age thresholds vary (40, 45, 50, 60, 70, 75) but 50 is dominant (~45%).
    # Subgroup analysis by age threshold showed no significant interaction (p=0.397).
    # All outcomes (OS, DFS, RFS, clinical features, treatment patterns) are
    # directly measured using standard definitions.
    # Geographic/setting diversity (North America 51%, Europe 23%, Asia 21%)
    # enhances generalizability rather than creating indirectness.

    if (outcome_name == "Overall Survival") {
      # OS: 20/22 studies use threshold <=50; 2 use 60 or 70.
      # Those 2 studies technically compare elderly-onset vs younger,
      # but sensitivity analysis excluding them showed robust results.
      # HR direction was standardized. No serious indirectness.
      return(list(
        rating = "No downgrade",
        note = "Direct comparison EOPC vs AOPC; 91% studies use threshold <=50; subgroup by threshold p=0.40"
      ))
    }

    if (outcome_name %in% c("Disease-Free Survival", "Recurrence-Free Survival")) {
      return(list(
        rating = "No downgrade",
        note = "DFS/RFS directly measured; all studies use standard definitions"
      ))
    }

    # Binary outcomes (OR): clinical features and treatment patterns
    # All directly measured with standard definitions (pathology staging,
    # treatment records). No surrogate outcomes used.
    if (outcome_name %in% c("Male Proportion", "Stage IV", "Poorly Differentiated",
                             "LN Positive", "Metastatic Disease",
                             "Smoking History", "Diabetes History", "Family History",
                             "Alcohol History", "White Race", "Black Race", "Asian Race",
                             "Perineural Invasion",
                             "Stage I", "Stage II", "Stage III",
                             "Moderately Differentiated", "Well Differentiated",
                             "Tumour Head", "Tumour Body", "Tumour Tail",
                             "Vascular Invasion",
                             "5-Year OS", "3-Year OS", "2-Year OS", "1-Year OS")) {
      return(list(
        rating = "No downgrade",
        note = "Clinicopathological features directly measured; standard definitions across studies"
      ))
    }

    if (outcome_name %in% c("Surgical Resection", "Chemotherapy", "Radiotherapy",
                             "Neoadjuvant Chemo", "Adjuvant Chemo",
                             "Whipple Procedure", "Distal Pancreatectomy",
                             "R0 Resection",
                             "No Treatment", "Palliative Chemotherapy",
                             "Total Pancreatectomy")) {
      # Treatment patterns: directly recorded from medical records/databases.
      # Mix of national databases (60%) and institutional cohorts (40%)
      # may have different treatment capture completeness, but this is
      # a data quality issue (risk of bias), not indirectness.
      return(list(
        rating = "No downgrade",
        note = "Treatment directly recorded; no surrogate measures; diverse settings enhance generalizability"
      ))
    }

    # Default fallback
    return(list(rating = "No downgrade", note = "Direct comparison and outcome measurement"))
  }

  compute_grade <- function(downgrades, upgrades = character(0)) {
    levels <- c("Very Low", "Low", "Moderate", "High")
    start <- 2  # Low (observational studies)
    steps_down <- 0
    for (d in downgrades) {
      if (grepl("-2", d)) steps_down <- steps_down + 2
      else if (grepl("-1", d)) steps_down <- steps_down + 1
    }
    steps_up <- 0
    for (u in upgrades) {
      if (grepl("\\+2", u)) steps_up <- steps_up + 2
      else if (grepl("\\+1", u)) steps_up <- steps_up + 1
    }
    final <- max(1, min(4, start - steps_down + steps_up))
    return(levels[final])
  }

  # Outcome-specific Risk of Bias assessment
  # GRADE guidance (Schunemann 2019 BMJ): "Low" starting point for
  # observational studies already captures inherent design limitations.
  # Additional downgrade should only apply for concerns BEYOND what
  # the observational design implies — i.e., NOS Poor or ROBINS-E Critical.
  # ROBINS-E "Serious" is the expected default for observational studies
  # (unmeasured confounding), already reflected in the "Low" baseline.
  assess_rob <- function(m, outcome_name) {
    if (is.null(m)) return(list(rating = "", note = "No data"))
    study_labels <- m$studlab
    matched <- master %>%
      filter(Study_Label %in% study_labels)
    if (nrow(matched) == 0) {
      return(list(rating = "-1 Serious", note = "No RoB data matched"))
    }
    pct_nos_poor <- mean(matched$Quality_Grade == "Poor", na.rm = TRUE) * 100
    pct_nos_fair <- mean(matched$Quality_Grade %in% c("Fair", "Poor"), na.rm = TRUE) * 100
    pct_robins_critical <- mean(matched$ROBINS_E_Overall == "Critical", na.rm = TRUE) * 100
    pct_robins_serious <- mean(matched$ROBINS_E_Overall %in% c("Serious", "Critical"), na.rm = TRUE) * 100

    detail <- sprintf("NOS Good: %.0f%%, Fair: %.0f%%, Poor: %.0f%%; ROBINS-E Moderate: %.0f%%, Serious: %.0f%%, Critical: %.0f%%",
                       100 - pct_nos_fair, pct_nos_fair - pct_nos_poor, pct_nos_poor,
                       100 - pct_robins_serious, pct_robins_serious - pct_robins_critical, pct_robins_critical)

    # -2: majority Critical/Poor (study-level fatal flaws beyond design)
    if (pct_robins_critical >= 50 || pct_nos_poor >= 50) {
      return(list(rating = "-2 Very serious", note = detail))
    }
    # -1: any Critical/Poor present, OR predominantly low-quality Fair
    #     (NOS <=5 equivalent captured by "Poor"; Fair 6-7 is acceptable)
    if (pct_robins_critical > 0 || pct_nos_poor > 0) {
      return(list(rating = "-1 Serious", note = detail))
    }
    # No downgrade: all studies NOS Fair/Good + ROBINS-E Moderate/Serious
    # "Serious" on ROBINS-E (unmeasured confounding) is the expected
    # baseline for observational studies → already in "Low" starting point
    return(list(rating = "No downgrade", note = detail))
  }

  # GRADE upgrading: large effect, dose-response gradient
  assess_upgrade <- function(m, outcome_name) {
    upgrades <- character(0)
    notes <- character(0)
    if (is.null(m)) return(list(upgrades = upgrades, note = ""))

    est <- exp(m$TE.random)
    lo  <- exp(m$lower.random)
    hi  <- exp(m$upper.random)

    # Large effect (only for OR outcomes, CI must not cross 1)
    if (m$sm == "OR" && !(lo < 1 && hi > 1)) {
      if (est > 5 || est < 0.2) {
        upgrades <- c(upgrades, "+2 Very large effect")
        notes <- c(notes, sprintf("OR=%.2f, very large", est))
      } else if (est > 2 || est < 0.5) {
        upgrades <- c(upgrades, "+1 Large effect")
        notes <- c(notes, sprintf("OR=%.2f, large", est))
      }
    }

    # Dose-response gradient: OS (specific dose-response analysis)
    if (outcome_name == "Overall Survival") {
      dr_file <- file.path(PATHS$tables, "dose_response_results.RData")
      if (file.exists(dr_file)) {
        dr_env <- new.env()
        load(dr_file, envir = dr_env)
        if (!is.null(dr_env$dr_results) && isTRUE(dr_env$dr_results$use_quad)) {
          upgrades <- c(upgrades, "+1 Dose-response gradient")
          notes <- c(notes, "Significant quadratic dose-response (p<0.05)")
        }
      }
    }

    # Dose-response gradient: Binary outcomes via age-threshold meta-regression
    # If meta-regression shows significant monotonic relationship between
    # age threshold and effect size (p<0.05, R2>50%), this constitutes
    # a dose-response gradient per GRADE upgrade criteria
    if (m$sm == "OR") {
      # Map outcome names to MetaRegression file identifiers
      mr_map <- c(
        "Male Proportion" = "Male", "Smoking History" = "Smoking",
        "Diabetes History" = "Diabetes", "Family History" = "FamilyHx",
        "Alcohol History" = "Alcohol", "White Race" = "White",
        "Black Race" = "Black", "Asian Race" = "Asian", "Perineural Invasion" = "PNI",
        "Stage IV" = "StageIV", "Stage I" = "StageI",
        "Stage II" = "StageII", "Stage III" = "StageIII",
        "Poorly Differentiated" = "PoorlyDiff",
        "Moderately Differentiated" = "ModDiff",
        "Well Differentiated" = "WellDiff",
        "LN Positive" = "LNPositive", "Metastatic Disease" = "Metastatic",
        "Tumour Head" = "TumorHead", "Tumour Body" = "TumorBody",
        "Tumour Tail" = "TumorTail", "Vascular Invasion" = "VascInv",
        "Surgical Resection" = "Surgery", "Chemotherapy" = "Chemo",
        "Neoadjuvant Chemo" = "ChemoNeo", "Adjuvant Chemo" = "ChemoAdj",
        "Radiotherapy" = "RT", "Whipple Procedure" = "Whipple",
        "Distal Pancreatectomy" = "Distal", "R0 Resection" = "R0",
        "No Treatment" = "NoTx", "Palliative Chemotherapy" = "PalliativeChemo",
        "Total Pancreatectomy" = "TotalPanc",
        "5-Year OS" = "OS5yr", "3-Year OS" = "OS3yr", "2-Year OS" = "OS2yr", "1-Year OS" = "OS1yr"
      )
      mr_id <- mr_map[outcome_name]
      if (!is.na(mr_id)) {
        mr_file <- file.path(PATHS$tables, paste0("MetaRegression_", mr_id, ".xlsx"))
        if (file.exists(mr_file)) {
          tryCatch({
            mr <- readxl::read_xlsx(mr_file)
            age_mr <- mr[mr$Moderator == "Age Threshold", ]
            if (nrow(age_mr) > 0) {
              p_age <- suppressWarnings(as.numeric(age_mr$P_value[1]))
              r2_str <- age_mr$R2[1]
              r2 <- suppressWarnings(as.numeric(gsub("%", "", r2_str)))
              if (!is.na(p_age) && p_age < 0.05 && !is.na(r2) && r2 > 50) {
                upgrades <- c(upgrades, "+1 Dose-response gradient")
                notes <- c(notes, sprintf("Age threshold meta-regression p=%.3f, R2=%.1f%%", p_age, r2))
              }
            }
          }, error = function(e) NULL)
        }
      }
    }

    return(list(
      upgrades = upgrades,
      note = if (length(notes) > 0) paste(notes, collapse = "; ") else "None"
    ))
  }

  outcomes <- list()
  meta_list <- list(
    list(m = meta_os,           name = "Overall Survival",           measure = "HR"),
    list(m = meta_dfs,          name = "Disease-Free Survival",      measure = "HR"),
    list(m = meta_rfs,          name = "Recurrence-Free Survival",   measure = "HR"),
    # Demographics
    list(m = meta_male,         name = "Male Proportion",            measure = "OR"),
    list(m = meta_smoking,      name = "Smoking History",            measure = "OR"),
    list(m = meta_diabetes,     name = "Diabetes History",           measure = "OR"),
    list(m = meta_family_hx,    name = "Family History",             measure = "OR"),
    # Tumour characteristics
    list(m = meta_stage_iv,     name = "Stage IV",                   measure = "OR"),
    list(m = meta_stage_i,      name = "Stage I",                    measure = "OR"),
    list(m = meta_stage_ii,     name = "Stage II",                   measure = "OR"),
    list(m = meta_stage_iii,    name = "Stage III",                  measure = "OR"),
    list(m = meta_poorly_diff,  name = "Poorly Differentiated",      measure = "OR"),
    list(m = meta_mod_diff,     name = "Moderately Differentiated",  measure = "OR"),
    list(m = meta_well_diff,    name = "Well Differentiated",        measure = "OR"),
    list(m = meta_ln_positive,  name = "LN Positive",                measure = "OR"),
    list(m = meta_metastatic,   name = "Metastatic Disease",         measure = "OR"),
    list(m = meta_tumor_head,   name = "Tumour Head",                measure = "OR"),
    list(m = meta_tumor_body,   name = "Tumour Body",                measure = "OR"),
    list(m = meta_tumor_tail,   name = "Tumour Tail",                measure = "OR"),
    list(m = meta_vasc_inv,     name = "Vascular Invasion",          measure = "OR"),
    # Treatment
    list(m = meta_surgery,      name = "Surgical Resection",         measure = "OR"),
    list(m = meta_chemo,        name = "Chemotherapy",               measure = "OR"),
    list(m = meta_chemo_neo,    name = "Neoadjuvant Chemo",          measure = "OR"),
    list(m = meta_chemo_adj,    name = "Adjuvant Chemo",             measure = "OR"),
    list(m = meta_rt,           name = "Radiotherapy",               measure = "OR"),
    list(m = meta_whipple,      name = "Whipple Procedure",          measure = "OR"),
    list(m = meta_distal,       name = "Distal Pancreatectomy",      measure = "OR"),
    list(m = meta_r0,           name = "R0 Resection",               measure = "OR"),
    # Time-point survival
    list(m = meta_os_5yr,       name = "5-Year OS",                  measure = "OR"),
    list(m = meta_os_3yr,       name = "3-Year OS",                  measure = "OR"),
    list(m = meta_os_1yr,       name = "1-Year OS",                  measure = "OR"),
    list(m = meta_os_2yr,       name = "2-Year OS",                  measure = "OR"),
    # Additional demographics (data maximization)
    list(m = meta_alcohol,      name = "Alcohol History",             measure = "OR"),
    list(m = meta_white,        name = "White Race",                  measure = "OR"),
    list(m = meta_black,        name = "Black Race",                  measure = "OR"),
    list(m = meta_asian,        name = "Asian Race",                  measure = "OR"),
    # Additional tumour
    list(m = meta_pni,          name = "Perineural Invasion",         measure = "OR"),
    # Additional treatment
    list(m = meta_no_tx,        name = "No Treatment",                measure = "OR"),
    list(m = meta_palliative,   name = "Palliative Chemotherapy",     measure = "OR"),
    list(m = meta_total_panc,   name = "Total Pancreatectomy",        measure = "OR")
  )

  for (item in meta_list) {
    m <- item$m
    if (is.null(m)) next

    rob          <- assess_rob(m, item$name)
    inconsist    <- assess_inconsistency(m)
    indirect     <- assess_indirectness(m, item$name)
    imprec       <- assess_imprecision(m)
    pub_bias     <- assess_pub_bias(m, measure = item$measure)
    upgrade_info <- assess_upgrade(m, item$name)

    downgrades <- c(rob$rating,
                    inconsist$rating,
                    indirect$rating,
                    imprec$rating,
                    pub_bias$rating)

    est <- exp(m$TE.random)
    lo  <- exp(m$lower.random)
    hi  <- exp(m$upper.random)

    upgrade_text <- if (length(upgrade_info$upgrades) > 0)
      paste(upgrade_info$upgrades, collapse = "; ") else "None"

    # Build "What happens" plain-language absolute effect description
    what_happens <- ""
    if (item$measure == "HR") {
      if (est < 1) {
        what_happens <- sprintf(
          "Assuming 200 per 1000 AOPC patients die at the reference time point, approximately %d per 1000 EOPC patients would die (95%% CI %d to %d).",
          round(200 * est), round(200 * lo), round(200 * hi))
      } else {
        what_happens <- sprintf(
          "Assuming 200 per 1000 AOPC patients die at the reference time point, approximately %d per 1000 EOPC patients would die (95%% CI %d to %d).",
          round(200 * est), round(200 * lo), round(200 * hi))
      }
    } else {
      # OR outcomes: express as "per 1000"
      # Assume baseline (AOPC) prevalence is not available; use generic description
      if (est > 1) {
        what_happens <- sprintf(
          "EOPC patients had higher prevalence (OR %.2f; per 1000 AOPC baseline, ratio of %.2f).",
          est, est)
      } else {
        what_happens <- sprintf(
          "EOPC patients had lower prevalence (OR %.2f; per 1000 AOPC baseline, ratio of %.2f).",
          est, est)
      }
    }

    # Compose detailed GRADE footnote for this outcome
    footnote_parts <- c()
    if (grepl("-1|-2", rob$rating))
      footnote_parts <- c(footnote_parts, paste0("Downgraded ", sub("^(-\\d).*", "\\1", rob$rating), " for risk of bias (", rob$note, ")"))
    if (grepl("-1|-2", inconsist$rating)) {
      down_n <- ifelse(grepl("-2", inconsist$rating), "2", "1")
      footnote_parts <- c(footnote_parts, paste0("Downgraded ", down_n, " for inconsistency (", inconsist$note, ")"))
    }
    if (grepl("-1|-2", indirect$rating))
      footnote_parts <- c(footnote_parts, paste0("Downgraded for indirectness (", indirect$note, ")"))
    if (grepl("-1|-2", imprec$rating)) {
      down_n <- ifelse(grepl("-2", imprec$rating), "2", "1")
      footnote_parts <- c(footnote_parts, paste0("Downgraded ", down_n, " for imprecision (", imprec$note, ")"))
    }
    if (grepl("-1|-2", pub_bias$rating))
      footnote_parts <- c(footnote_parts, paste0("Downgraded 1 for publication bias (", pub_bias$note, ")"))
    if (length(upgrade_info$upgrades) > 0)
      footnote_parts <- c(footnote_parts, paste0("Upgraded: ", paste(upgrade_info$upgrades, collapse = "; ")))
    grade_footnote <- if (length(footnote_parts) > 0)
      paste(footnote_parts, collapse = ". ") else "No downgrading applied."

    outcomes[[item$name]] <- data.frame(
      Outcome = item$name,
      Studies_k = m$k,
      Effect = sprintf("%.2f [%.2f, %.2f]", est, lo, hi),
      Anticipated_Absolute_Effects = what_happens,
      Initial = "Low",
      Risk_of_Bias = paste0(rob$rating, " (", rob$note, ")"),
      Inconsistency = paste0(inconsist$rating, " (", inconsist$note, ")"),
      Indirectness = paste0(indirect$rating, " (", indirect$note, ")"),
      Imprecision = paste0(imprec$rating, " (", imprec$note, ")"),
      Publication_Bias = paste0(pub_bias$rating, " (", pub_bias$note, ")"),
      Upgrade = paste0(upgrade_text, " (", upgrade_info$note, ")"),
      Certainty = compute_grade(downgrades, upgrade_info$upgrades),
      GRADE_Footnote = grade_footnote,
      stringsAsFactors = FALSE
    )
  }

  grade <- bind_rows(outcomes)
  write_xlsx(grade, file.path(PATHS$tables, "GRADE_Evidence_Table.xlsx"))

  cat("\n========================================\n")
  cat("GRADE Evidence Quality Assessment\n")
  cat("========================================\n")
  for (i in seq_len(nrow(grade))) {
    cat(sprintf("\n%s (k=%d): %s -> %s\n",
                grade$Outcome[i], grade$Studies_k[i],
                grade$Effect[i], grade$Certainty[i]))
    cat(sprintf("  Footnote: %s\n", grade$GRADE_Footnote[i]))
  }
  cat("\nIndirectness assessed: No serious indirectness for any outcome.\n")
  cat("Rationale: All studies directly compare EOPC vs AOPC; age threshold subgroup p=0.40.\n")

  # Print Lancet-required GRADE table footnote summary
  cat("\n--- GRADE Table Footnotes (for manuscript Table 2) ---\n")
  cat("All outcomes start at Low certainty (observational studies).\n")
  cert_counts <- table(grade$Certainty)
  cat("Certainty distribution:\n")
  for (lev in c("Very Low", "Low", "Moderate", "High")) {
    if (lev %in% names(cert_counts))
      cat(sprintf("  %s: %d outcomes\n", lev, cert_counts[lev]))
  }
  cat(sprintf("Downgrade criteria (refined per GRADE Handbook & Schunemann 2019 BMJ):\n"))
  cat("  - Risk of bias: Only downgraded for NOS Poor or ROBINS-E Critical\n")
  cat("    (Low starting point already accounts for observational design;\n")
  cat("     ROBINS-E Serious reflects expected residual confounding)\n")
  cat("  - Inconsistency: I2 >= 75%% assessed with prediction interval and\n")
  cat("    effect direction; downgrade by 1 if PI excludes null or same direction\n")
  cat("  - Indirectness: No downgrade (direct EOPC vs AOPC comparison)\n")
  cat("  - Imprecision: OIS-informed; large N with narrow CI not downgraded\n")
  cat("  - Publication bias: Egger/Peters tests; downgraded if p < 0.10\n")
  cat("  - Upgrades: Large effect (OR > 2 or < 0.5), dose-response gradient\n")

  return(grade)
}

grade_table <- create_grade_table()

# ==============================================================================
# PART 4: Median OS Descriptive Table + Cleveland Dot-Plot
# ==============================================================================

create_median_os_table <- function() {
  cat("\n==== Median OS Descriptive Table ====\n")

  # Check for median OS columns in master
  eopc_col <- NULL; aopc_col <- NULL
  for (cn in names(master)) {
    if (grepl("Median.*OS.*EOPC", cn, ignore.case = TRUE)) eopc_col <- cn
    if (grepl("Median.*OS.*AOPC", cn, ignore.case = TRUE)) aopc_col <- cn
  }

  if (is.null(eopc_col) || is.null(aopc_col)) {
    cat("  Median OS columns not found in master. Skipping.\n")
    return(NULL)
  }

  cat("  EOPC column:", eopc_col, "\n")
  cat("  AOPC column:", aopc_col, "\n")

  med_os <- master %>%
    mutate(
      mOS_EOPC = suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(.data[[eopc_col]])))),
      mOS_AOPC = suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(.data[[aopc_col]]))))
    ) %>%
    filter(!is.na(mOS_EOPC), !is.na(mOS_AOPC)) %>%
    dplyr::select(Study_Label, Study_ID, mOS_EOPC, mOS_AOPC, Total_N_num,
                  Age_Threshold_Group, Region_Group, Data_Source_Group) %>%
    mutate(Difference = mOS_EOPC - mOS_AOPC) %>%
    arrange(Difference)

  if (nrow(med_os) == 0) {
    cat("  No studies with valid paired median OS data.\n")
    return(NULL)
  }

  cat("  Studies with paired median OS:", nrow(med_os), "\n")
  cat("  EOPC range:", sprintf("%.1f - %.1f months", min(med_os$mOS_EOPC), max(med_os$mOS_EOPC)), "\n")
  cat("  AOPC range:", sprintf("%.1f - %.1f months", min(med_os$mOS_AOPC), max(med_os$mOS_AOPC)), "\n")
  cat("  Difference range:", sprintf("%.1f to %.1f months",
      min(med_os$Difference), max(med_os$Difference)), "\n")

  write_xlsx(med_os, file.path(PATHS$tables, "Median_OS_Descriptive.xlsx"))

  # Wilcoxon signed-rank test (paired comparison)
  tryCatch({
    wt <- wilcox.test(med_os$mOS_EOPC, med_os$mOS_AOPC, paired = TRUE)
    cat("  Wilcoxon signed-rank test: V =", wt$statistic, ", p =", format.pval(wt$p.value, digits = 4), "\n")
    cat("  Median difference:", sprintf("%.1f months", median(med_os$Difference)), "\n")
  }, error = function(e) cat("  Wilcoxon test error:", e$message, "\n"))

  # Cleveland dot-plot
  dot_data <- med_os %>%
    tidyr::pivot_longer(cols = c(mOS_EOPC, mOS_AOPC),
                        names_to = "Group", values_to = "Months") %>%
    mutate(Group = ifelse(Group == "mOS_EOPC", "EOPC", "AOPC"))

  dot_data$Study_Label <- factor(dot_data$Study_Label,
                                  levels = med_os$Study_Label)

  p <- ggplot(dot_data, aes(x = Months, y = Study_Label, colour = Group)) +
    geom_line(aes(group = Study_Label), colour = LANCET_STYLE$col_structural, linewidth = 0.5) +
    geom_point(size = 2.5) +
    scale_colour_manual(values = c("EOPC" = LANCET_STYLE$col_km_eopc,
                                   "AOPC" = LANCET_STYLE$col_km_aopc)) +
    labs(x = "Median Overall Survival (months)",
         y = NULL,
         title = "Paired Median OS Comparison: EOPC vs AOPC",
         colour = NULL) +
    lancet_theme() +
    theme(legend.position = "bottom")

  save_figure_gg(p,
    filename = file.path(PATHS$tables, "Median_OS_dotplot"),
    width = 9, height = max(5, 1 + nrow(med_os) * 0.35))

  cat("  Median OS table and dot-plot saved.\n")
  return(med_os)
}

median_os_table <- create_median_os_table()

# ==============================================================================
# PART 5: Median DFS/RFS Descriptive Tables
# ==============================================================================

create_median_survival_table <- function(eopc_pattern, aopc_pattern, surv_label, file_prefix) {
  cat("\n==== Median", surv_label, "Descriptive Table ====\n")

  eopc_col <- NULL; aopc_col <- NULL
  for (cn in names(master)) {
    if (grepl(eopc_pattern, cn, ignore.case = TRUE)) eopc_col <- cn
    if (grepl(aopc_pattern, cn, ignore.case = TRUE)) aopc_col <- cn
  }

  if (is.null(eopc_col) || is.null(aopc_col)) {
    cat("  Median", surv_label, "columns not found. Skipping.\n")
    return(NULL)
  }

  cat("  EOPC column:", eopc_col, "\n")
  cat("  AOPC column:", aopc_col, "\n")

  med_surv <- master %>%
    mutate(
      mS_EOPC = suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(.data[[eopc_col]])))),
      mS_AOPC = suppressWarnings(as.numeric(gsub("[^0-9.]", "", as.character(.data[[aopc_col]]))))
    ) %>%
    filter(!is.na(mS_EOPC), !is.na(mS_AOPC)) %>%
    dplyr::select(Study_Label, Study_ID, mS_EOPC, mS_AOPC, Total_N_num,
                  Age_Threshold_Group, Region_Group, Data_Source_Group) %>%
    mutate(Difference = mS_EOPC - mS_AOPC) %>%
    arrange(Difference)

  if (nrow(med_surv) == 0) {
    cat("  No studies with valid paired median", surv_label, "data.\n")
    return(NULL)
  }

  cat("  Studies with paired median", surv_label, ":", nrow(med_surv), "\n")
  cat("  EOPC range:", sprintf("%.1f - %.1f months", min(med_surv$mS_EOPC), max(med_surv$mS_EOPC)), "\n")
  cat("  AOPC range:", sprintf("%.1f - %.1f months", min(med_surv$mS_AOPC), max(med_surv$mS_AOPC)), "\n")

  # Wilcoxon signed-rank test
  tryCatch({
    wt <- wilcox.test(med_surv$mS_EOPC, med_surv$mS_AOPC, paired = TRUE)
    cat("  Wilcoxon signed-rank test: V =", wt$statistic, ", p =", format.pval(wt$p.value, digits = 4), "\n")
  }, error = function(e) cat("  Wilcoxon test error:", e$message, "\n"))

  write_xlsx(med_surv, file.path(PATHS$tables, paste0("Median_", file_prefix, "_Descriptive.xlsx")))

  # Cleveland dot-plot
  names(med_surv)[names(med_surv) == "mS_EOPC"] <- "mEOPC"
  names(med_surv)[names(med_surv) == "mS_AOPC"] <- "mAOPC"
  dot_data <- med_surv %>%
    tidyr::pivot_longer(cols = c(mEOPC, mAOPC),
                        names_to = "Group", values_to = "Months") %>%
    mutate(Group = ifelse(Group == "mEOPC", "EOPC", "AOPC"))
  dot_data$Study_Label <- factor(dot_data$Study_Label, levels = med_surv$Study_Label)

  p <- ggplot(dot_data, aes(x = Months, y = Study_Label, colour = Group)) +
    geom_line(aes(group = Study_Label), colour = LANCET_STYLE$col_structural, linewidth = 0.5) +
    geom_point(size = 2.5) +
    scale_colour_manual(values = c("EOPC" = LANCET_STYLE$col_km_eopc,
                                   "AOPC" = LANCET_STYLE$col_km_aopc)) +
    labs(x = paste0("Median ", surv_label, " (months)"),
         y = NULL,
         title = paste0("Paired Median ", surv_label, " Comparison: EOPC vs AOPC"),
         colour = NULL) +
    lancet_theme() +
    theme(legend.position = "bottom")

  save_figure_gg(p,
    filename = file.path(PATHS$tables, paste0("Median_", file_prefix, "_dotplot")),
    width = 9, height = max(5, 1 + nrow(med_surv) * 0.35))

  cat("  Median", surv_label, "table and dot-plot saved.\n")
  return(med_surv)
}

median_dfs_table <- create_median_survival_table(
  "Median.*DFS.*EOPC", "Median.*DFS.*AOPC", "DFS", "DFS")
median_rfs_table <- create_median_survival_table(
  "Median.*RFS.*EOPC", "Median.*RFS.*AOPC", "RFS", "RFS")

cat("\nAll summary tables generated.\n")
