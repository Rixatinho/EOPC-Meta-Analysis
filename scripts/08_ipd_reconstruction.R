################################################################################
# EOPC vs AOPC Systematic Review and Meta-Analysis
# Script 08: IPD Reconstruction (Guyot 2012) + RMST Analysis
# Input:  prepared_data.RData, KM CSV files in data/km_digitized/
# Output: ipd_results.RData, RMST forest plots, PH diagnostics
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
# Check for KM digitized data
# ==============================================================================

km_files <- list.files(PATHS$km_data, pattern = "\\.csv$", full.names = TRUE)

if (length(km_files) == 0) {
  cat("No KM digitized CSV files found in:", PATHS$km_data, "\n")
  cat("To enable IPD reconstruction:\n")
  cat("  1. Digitize KM curves with WebPlotDigitizer\n")
  cat("  2. Save CSVs with 'time' and 'survival' columns\n")
  cat("  3. Place in:", PATHS$km_data, "\n")
  cat("Saving empty ipd_results.RData for downstream compatibility.\n")

  ipd_all <- NULL
  rmst_results <- NULL
  save(ipd_all, rmst_results,
       file = file.path(PATHS$tables, "ipd_results.RData"))
  cat("Done (skipped).\n")
  quit(save = "no", status = 0)
}

# ==============================================================================
# PART 1: Load and Validate KM CSV Files
# ==============================================================================

load_km_csv <- function(csv_path) {
  km <- tryCatch(read.csv(csv_path, stringsAsFactors = FALSE),
                 error = function(e) { cat("Error reading:", csv_path, "\n"); NULL })
  if (is.null(km)) return(NULL)

  names(km) <- tolower(names(km))
  if ("x" %in% names(km) && "y" %in% names(km)) {
    names(km)[names(km) == "x"] <- "time"
    names(km)[names(km) == "y"] <- "survival"
  }

  if (!all(c("time", "survival") %in% names(km))) {
    cat("  Missing time/survival columns in:", basename(csv_path), "\n")
    return(NULL)
  }

  km <- km %>%
    filter(!is.na(time), !is.na(survival)) %>%
    arrange(time)

  # Ensure survival in [0,1]
  if (max(km$survival, na.rm = TRUE) > 1.5) km$survival <- km$survival / 100
  km$survival <- pmin(pmax(km$survival, 0), 1)

  # Ensure starts at time=0, survival=1
  if (km$time[1] > 0) km <- rbind(data.frame(time = 0, survival = 1), km)

  cat("  Loaded:", basename(csv_path), "-", nrow(km), "points\n")
  return(km)
}

# ==============================================================================
# PART 2: IPD Reconstruction (Guyot Algorithm)
# ==============================================================================

reconstruct_ipd <- function(km_eopc, km_aopc, n_eopc, n_aopc, study_label,
                            trisk_eopc = NULL, nrisk_eopc = NULL,
                            trisk_aopc = NULL, nrisk_aopc = NULL) {
  cat("\nReconstructing IPD for:", study_label, "\n")

  ipd_list <- list()
  arms <- list(
    list(km = km_eopc, n = n_eopc, group = "EOPC",
         trisk = trisk_eopc, nrisk = nrisk_eopc),
    list(km = km_aopc, n = n_aopc, group = "AOPC",
         trisk = trisk_aopc, nrisk = nrisk_aopc)
  )

  for (arm in arms) {
    if (is.null(arm$km) || is.na(arm$n)) next

    # Use actual number-at-risk if provided, otherwise approximate
    if (!is.null(arm$trisk) && !is.null(arm$nrisk)) {
      tr <- arm$trisk
      nr <- arm$nrisk
    } else {
      tr <- seq(0, max(arm$km$time), length.out = min(10, nrow(arm$km)))
      nr <- rep(arm$n, length(tr))
    }

    ipd <- tryCatch({
      preprocess <- IPDfromKM::preprocess(
        dat     = arm$km,
        trisk   = tr,
        nrisk   = nr,
        maxy    = 1
      )
      IPDfromKM::getIPD(prep = preprocess, armID = 0, tot.events = NULL)
    }, error = function(e) {
      cat("  IPD reconstruction failed for", arm$group, ":", e$message, "\n")
      NULL
    })

    if (!is.null(ipd) && !is.null(ipd$IPD)) {
      df <- data.frame(
        time   = ipd$IPD$time,
        status = ipd$IPD$status,
        group  = arm$group,
        study  = study_label
      )
      ipd_list[[arm$group]] <- df
      cat("  ", arm$group, ": reconstructed", nrow(df), "patients\n")
    }
  }

  if (length(ipd_list) == 2) {
    combined <- bind_rows(ipd_list)
    return(combined)
  }
  return(NULL)
}

# ==============================================================================
# PART 3: RMST Analysis
# ==============================================================================

compute_rmst <- function(ipd_data, study_label, tau_values = SETTINGS$rmst_tau_months) {
  if (is.null(ipd_data)) return(NULL)

  results <- list()
  for (tau in tau_values) {
    if (max(ipd_data$time) < tau) {
      cat("  Max time < tau=", tau, "for", study_label, ". Skipping.\n")
      next
    }

    rmst_res <- tryCatch({
      group_numeric <- ifelse(ipd_data$group == "EOPC", 1, 0)
      survRM2::rmst2(
        time   = ipd_data$time,
        status = ipd_data$status,
        arm    = group_numeric,
        tau    = tau
      )
    }, error = function(e) {
      cat("  RMST error at tau=", tau, ":", e$message, "\n")
      NULL
    })

    if (!is.null(rmst_res)) {
      diff_row <- rmst_res$unadjusted.result[1, ]
      results[[as.character(tau)]] <- data.frame(
        Study = study_label,
        Tau_months = tau,
        RMST_Diff = diff_row[1],
        RMST_Lower = diff_row[2],
        RMST_Upper = diff_row[3],
        RMST_P = diff_row[4],
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(results) > 0) return(bind_rows(results))
  return(NULL)
}

# ==============================================================================
# PART 4: Process All Available KM Data
# ==============================================================================

cat("\nFound", length(km_files), "KM CSV files.\n")

# Load study info if available
study_info_file <- file.path(PATHS$km_data, "km_study_info.csv")
if (file.exists(study_info_file)) {
  study_info <- read.csv(study_info_file, stringsAsFactors = FALSE)
  cat("Loaded km_study_info.csv with", nrow(study_info), "rows\n")
} else {
  study_info <- NULL
}

# Process files (expecting paired files: StudyID_EOPC.csv, StudyID_AOPC.csv)
ipd_all <- list()
rmst_results <- list()

# Filter out non-KM CSV files (km_study_info.csv, etc.)
km_data_csvs <- km_files[grepl("_(EOPC|AOPC)\\.csv$", basename(km_files))]
unique_studies <- unique(gsub("_(EOPC|AOPC)\\.csv$", "", basename(km_data_csvs)))

cat("Studies with paired KM files:", length(unique_studies), "\n")

for (study in unique_studies) {
  eopc_file <- file.path(PATHS$km_data, paste0(study, "_EOPC.csv"))
  aopc_file <- file.path(PATHS$km_data, paste0(study, "_AOPC.csv"))

  if (!file.exists(eopc_file) || !file.exists(aopc_file)) {
    cat("Skipping", study, "- missing paired files\n")
    next
  }

  km_eopc <- load_km_csv(eopc_file)
  km_aopc <- load_km_csv(aopc_file)

  # Get sample sizes from master
  study_row <- master %>% filter(grepl(study, Study_ID, fixed = TRUE))
  if (nrow(study_row) == 0) {
    cat("  Study", study, "not found in master data\n")
    next
  }

  n_eopc <- study_row$EOPC_N_num[1]
  n_aopc <- study_row$AOPC_N_num[1]
  label  <- study_row$Study_Label[1]

  # Get number at risk from study_info if available
  trisk_eopc <- nrisk_eopc <- trisk_aopc <- nrisk_aopc <- NULL
  if (!is.null(study_info)) {
    si_eopc <- study_info[study_info$study_id == study & study_info$group == "EOPC", ]
    si_aopc <- study_info[study_info$study_id == study & study_info$group == "AOPC", ]
    if (nrow(si_eopc) > 0 && !is.na(si_eopc$nrisk_times[1]) && nchar(si_eopc$nrisk_times[1]) > 0) {
      trisk_eopc <- as.numeric(unlist(strsplit(si_eopc$nrisk_times[1], ";")))
      nrisk_eopc <- as.numeric(unlist(strsplit(si_eopc$nrisk_values[1], ";")))
      n_eopc <- si_eopc$n_patients[1]
    }
    if (nrow(si_aopc) > 0 && !is.na(si_aopc$nrisk_times[1]) && nchar(si_aopc$nrisk_times[1]) > 0) {
      trisk_aopc <- as.numeric(unlist(strsplit(si_aopc$nrisk_times[1], ";")))
      nrisk_aopc <- as.numeric(unlist(strsplit(si_aopc$nrisk_values[1], ";")))
      n_aopc <- si_aopc$n_patients[1]
    }
  }

  ipd <- reconstruct_ipd(km_eopc, km_aopc, n_eopc, n_aopc, label,
                          trisk_eopc, nrisk_eopc, trisk_aopc, nrisk_aopc)
  if (!is.null(ipd)) {
    ipd_all[[study]] <- ipd
    rmst_res <- compute_rmst(ipd, label)
    if (!is.null(rmst_res)) rmst_results[[study]] <- rmst_res

    # Proportional hazards assumption test
    tryCatch({
      cox_fit <- survival::coxph(
        survival::Surv(time, status) ~ group, data = ipd)
      ph_test <- survival::cox.zph(cox_fit)
      cat("  PH test (cox.zph):", sprintf("p = %.4f", ph_test$table["GLOBAL", "p"]),
          ifelse(ph_test$table["GLOBAL", "p"] < 0.05,
                 " -> PH violated, RMST preferred", " -> PH holds"), "\n")
    }, error = function(e) cat("  PH test error:", e$message, "\n"))

    # Save KM plot for verification (ggplot2 Lancet style)
    tryCatch({
      surv_obj <- survival::Surv(ipd$time, ipd$status)
      fit <- survival::survfit(surv_obj ~ group, data = ipd)

      # Build ggplot2 KM manually for Lancet aesthetics
      km_df <- data.frame()
      for (grp in c("AOPC", "EOPC")) {
        g_idx <- which(names(fit$strata) == paste0("group=", grp))
        n_t <- fit$strata[g_idx]
        if (g_idx == 1) {
          s_idx <- 1:n_t
        } else {
          s_start <- sum(fit$strata[1:(g_idx-1)]) + 1
          s_idx <- s_start:(s_start + n_t - 1)
        }
        km_df <- rbind(km_df,
          data.frame(time = c(0, fit$time[s_idx]),
                     surv = c(1, fit$surv[s_idx]),
                     upper = c(1, fit$upper[s_idx]),
                     lower = c(1, fit$lower[s_idx]),
                     group = grp, stringsAsFactors = FALSE))
      }

      p_km <- ggplot(km_df, aes(x = time, y = surv, colour = group)) +
        geom_step(linewidth = 0.9) +
        geom_ribbon(aes(ymin = lower, ymax = upper, fill = group),
                    stat = "identity", alpha = 0.1, colour = NA, show.legend = FALSE) +
        scale_colour_manual(values = c("AOPC" = LANCET_STYLE$col_km_aopc,
                                        "EOPC" = LANCET_STYLE$col_km_eopc),
                            name = NULL) +
        scale_fill_manual(values = c("AOPC" = LANCET_STYLE$col_km_aopc,
                                      "EOPC" = LANCET_STYLE$col_km_eopc)) +
        scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
        labs(x = "Time (months)", y = "Survival Probability",
             title = paste("Reconstructed IPD:", label)) +
        lancet_theme(base_size = 10) +
        theme(legend.position = c(0.80, 0.85),
              legend.background = element_rect(fill = "white", colour = NA),
              plot.title = element_text(size = 11, face = "bold"))

      # Number-at-risk table
      risk_times <- pretty(c(0, max(km_df$time)), n = 5)
      risk_times <- risk_times[risk_times <= max(km_df$time)]
      risk_rows <- list()
      for (grp in c("EOPC", "AOPC")) {
        grp_df <- km_df[km_df$group == grp, ]
        for (rt in risk_times) {
          surv_at <- grp_df$surv[max(which(grp_df$time <= rt))]
          n_at <- if (!is.null(study_info)) {
            si <- study_info[study_info$study_id == study & study_info$group == grp, ]
            if (nrow(si) > 0 && !is.na(si$n_patients[1])) {
              round(si$n_patients[1] * surv_at)
            } else {
              sr <- if (grp == "EOPC") n_eopc else n_aopc
              round(sr * surv_at)
            }
          } else {
            sr <- if (grp == "EOPC") n_eopc else n_aopc
            round(sr * surv_at)
          }
          risk_rows[[length(risk_rows) + 1]] <- data.frame(
            time = rt, group = grp, n = n_at, stringsAsFactors = FALSE)
        }
      }
      risk_df <- do.call(rbind, risk_rows)

      p_risk <- ggplot(risk_df, aes(x = time, y = group, label = n)) +
        geom_text(size = 3, colour = LANCET_STYLE$col_emphasis) +
        scale_x_continuous(limits = range(ggplot_build(p_km)$layout$panel_scales_x[[1]]$range$range),
                           breaks = risk_times) +
        labs(x = NULL, y = NULL) +
        lancet_theme(base_size = 10) +
        theme(axis.text.x = element_blank(),
              axis.ticks = element_blank(),
              axis.line = element_blank(),
              panel.grid = element_blank(),
              plot.margin = margin(0, 5, 5, 5))

      p_combined <- p_km / p_risk + plot_layout(heights = c(5, 1))

      plot_file <- file.path(PATHS$influence, paste0(study, "_ipd_km"))
      save_figure_gg(p_combined, plot_file, width = 8, height = 7)
    }, error = function(e) cat("  KM plot error:", e$message, "\n"))
  }
}

# Combine results
if (length(rmst_results) > 0) {
  rmst_combined <- bind_rows(rmst_results)
  write_xlsx(rmst_combined, file.path(PATHS$tables, "RMST_Results.xlsx"))
  cat("\nRMST results saved.\n")
}

# ==============================================================================
# PART 5: Pooled RMST Meta-Analysis (when k >= 2 studies per tau)
# ==============================================================================

pool_rmst <- function(rmst_combined, tau_values = SETTINGS$rmst_tau_months) {
  if (is.null(rmst_combined) || nrow(rmst_combined) == 0) {
    cat("\nNo RMST data available for pooling.\n")
    return(NULL)
  }

  cat("\n==== Pooled RMST Meta-Analysis ====\n")
  pooled_results <- list()

  for (tau in tau_values) {
    sub <- rmst_combined %>% filter(Tau_months == tau)
    if (nrow(sub) < 2) {
      cat(sprintf("  tau=%d months: k=%d, need >= 2 for pooling. Skipping.\n",
                  tau, nrow(sub)))
      next
    }

    cat(sprintf("\n--- tau = %d months (k = %d) ---\n", tau, nrow(sub)))

    # RMST difference and SE from CI
    sub <- sub %>% mutate(
      se_diff = (RMST_Upper - RMST_Lower) / (2 * qnorm(0.975))
    )

    m_rmst <- tryCatch({
      metagen(
        TE      = sub$RMST_Diff,
        seTE    = sub$se_diff,
        studlab = sub$Study,
        sm      = "MD",
        method.tau = SETTINGS$method_tau,
        method.random.ci = SETTINGS$method_ci,
        adhoc.hakn.ci = SETTINGS$adhoc_hakn,
        prediction = SETTINGS$prediction,
        title   = sprintf("RMST Difference at %d months: EOPC vs AOPC", tau)
      )
    }, error = function(e) {
      cat("  Meta-analysis error:", e$message, "\n")
      NULL
    })

    if (!is.null(m_rmst)) {
      print(summary(m_rmst))

      pooled_results[[as.character(tau)]] <- data.frame(
        Tau_months = tau,
        k = m_rmst$k,
        RMST_Diff_Pooled = sprintf("%.2f", m_rmst$TE.random),
        CI_95 = sprintf("[%.2f, %.2f]", m_rmst$lower.random, m_rmst$upper.random),
        P = format_pval(m_rmst$pval.random),
        I2 = sprintf("%.1f%%", m_rmst$I2 * 100),
        stringsAsFactors = FALSE
      )

      # Forest plot (forestploter table-based)
      rmst_forest_file <- file.path(PATHS$ipd_rmst,
                                     paste0("RMST_pooled_tau", tau))
      fp <- tryCatch(
        lancet_fp_forest(m_rmst,
                          outcome_label = sprintf("RMST Difference at %d months", tau)),
        error = function(e) {
          cat("  forestploter RMST error:", e$message, " - skipping\n")
          NULL
        }
      )
      if (!is.null(fp)) {
        save_fp_figure(fp, rmst_forest_file)
      }
    }
  }

  if (length(pooled_results) > 0) {
    pooled_table <- bind_rows(pooled_results)
    write_xlsx(pooled_table, file.path(PATHS$tables, "RMST_Pooled.xlsx"))
    cat("\nPooled RMST results saved.\n")
    print(pooled_table)
    return(pooled_table)
  }
  return(NULL)
}

rmst_pooled <- NULL
if (length(rmst_results) > 0) {
  rmst_pooled <- pool_rmst(bind_rows(rmst_results))
}

save(ipd_all, rmst_results, rmst_pooled,
     file = file.path(PATHS$tables, "ipd_results.RData"))

cat("\nIPD reconstruction and RMST analysis complete.\n")
cat("Studies processed:", length(ipd_all), "\n")
if (!is.null(rmst_pooled)) {
  cat("Pooled RMST computed for", nrow(rmst_pooled), "time-points.\n")
} else {
  cat("Pooled RMST: insufficient studies (need k>=2 per tau).\n")
  cat("To add more studies: digitize KM curves and place paired CSVs in:\n  ",
      PATHS$km_data, "\n")
}
