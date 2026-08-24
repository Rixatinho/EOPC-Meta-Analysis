# EOPC vs AOPC Meta-Analysis: Variable Dictionary

## Study-Level Variables (from Data_Extraction_Master_Filled_v2.xlsx)

| Variable | Source Sheet | Type | Description |
|----------|-------------|------|-------------|
| Study_ID | Part_A | string | Unique identifier: FirstAuthor_Year |
| Country | Part_A | string | Country of data origin |
| Region | Part_A | string | Geographic region (North America/Europe/Asia/Other) |
| Data_Source | Part_A | string | Original data source (SEER, NCDB, institutional, etc.) |
| Study_Period | Part_A | string | Data collection period (start year-end year) |
| Age_Threshold | Part_A | string | EOPC age cutoff definition (e.g., "<=50", "50") |
| Total_N | Part_A | numeric | Total sample size |
| EOPC_N | Part_A | numeric | Number of EOPC patients |
| AOPC_N | Part_A | numeric | Number of AOPC patients |
| OS_HR | Part_E | numeric | Overall survival hazard ratio (EOPC vs AOPC) |
| OS_HR_LCI | Part_E | numeric | OS HR lower 95% CI |
| OS_HR_UCI | Part_E | numeric | OS HR upper 95% CI |
| OS_HR_Source | Part_E | string | HR derivation method (Multivariable/Univariable/IPD) |

## Computed Moderator Variables (from 01_data_import.R)

| Variable | Computation | Type | Description |
|----------|------------|------|-------------|
| First_Author | sub from Study_ID | string | First author surname |
| Year | sub from Study_ID | integer | Publication year |
| Study_Label | paste0(First_Author, " (", Year, ")") | string | Display label |
| Age_Threshold_Num | normalize_age_threshold() | numeric | Numeric age cutoff (40/45/50/55/60) |
| Age_Threshold_Group | case_when from Age_Threshold_Num | categorical | <=40, <=45, <=50, <=55, Other |
| Region_Group | case_when from Region | categorical | North America/Europe/Asia/Other |
| Data_Source_Group | case_when from Data_Source | categorical | Database/Institution |
| Database_Cluster | case_when from Data_Source | categorical | SEER/NCDB/Other (overlap handling) |
| NOS_Quality_Group | ifelse from Total_Stars | categorical | Good (>=7)/Fair (<7); no Poor studies |
| ROBINS_E_Group | ifelse from ROBINS_E_Overall | categorical | Low/Moderate/Serious |
| Total_N_num | as.numeric from Total_N | numeric | Numeric total sample size |
| EOPC_N_num | as.numeric from EOPC_N | numeric | Numeric EOPC count |
| AOPC_N_num | as.numeric from AOPC_N | numeric | Numeric AOPC count |
| EOPC_Proportion | EOPC_N_num / Total_N_num | numeric | Proportion of EOPC in total sample |
| Publication_Era | ifelse from Year | categorical | Pre-2020/2020+ |
| Study_Period_Mid | parsed from Study_Period | numeric | Midpoint of data collection period |
| HR_Source_Group | case_when from HR source | categorical | Multivariable/Univariable/IPD/Other |

## Statistical Analysis Output Variables

| Variable | Source | Type | Description |
|----------|--------|------|-------------|
| logHR | log(OS_HR) | numeric | Log-transformed hazard ratio |
| seLogHR | (log(UCI)-log(LCI))/(2*1.96) | numeric | Standard error of log HR |
| TE / seTE | meta package | numeric | Effect size / standard error (log scale) |
| w.random | meta package | numeric | Random-effects weight per study |
| tau2 | REML estimator | numeric | Between-study variance |
| I2 | meta package | numeric | Heterogeneity percentage |
| Q / pval.Q | meta package | numeric/float | Cochran's Q test / P-value |
| R2 | metafor rma | numeric | Variance explained by moderators (%) |

## R Environment & Package Versions

- R version 4.5.3 (R Foundation for Statistical Computing)
- meta 7.0-0, metafor 4.8-0, metasens, ggplot2, dplyr, readxl, writexl
- Full package list: see 00_config.R `required_packages`
