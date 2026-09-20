# EOPC Meta-Analysis: Early-Onset Pancreatic Cancer Paradox

## Overview

This repository contains the complete analytic code and data for the systematic review and meta-analysis:

**Title**: The Early-Onset Pancreatic Cancer Paradox: A Systematic Review and Meta-Analysis

**Manuscript**: JNCI-26-0532 (Journal of the National Cancer Institute) - accepted 2026-09-20

**PROSPERO**: CRD420261346604

## Contents

- `scripts/`: 16 R analysis scripts + master execution script (`run_all.R`)
- `data/`: data extraction workbook (xlsx) and prepared analysis dataset (RData)
- `Variable_Dictionary.md`: definitions of all extracted fields and computed variables
- `search_strategies.md`: complete search strategies for all 5 databases (PubMed, Embase, Web of Science, Cochrane Library, Scopus)
- `SAP_v2.0.pdf`: Statistical Analysis Plan v2.0 (study protocol)

## Execution

```bash
# Requires R >= 4.5.0 with packages: meta, metafor, metasens, dmetar,
# IPDfromKM, survival, survminer, survRM2, dosresmeta, rms, ggplot2,
# forestplot, forestploter, robvis, patchwork, readxl, writexl, openxlsx,
# dplyr, tidyr, stringr, purrr, tibble, officer, flextable

# Run the complete pipeline:
Rscript scripts/run_all.R
```

The pipeline auto-detects the project root from the script location and generates all outputs in `output/` subdirectories.

## Script Execution Order

```
scripts/00_config.R              # Configuration and path setup
scripts/01_data_import.R         # Data import and variable computation
scripts/02_primary_meta.R        # Primary meta-analysis (HR/OR pooling)
scripts/03_sensitivity_pub_bias.R # Sensitivity analyses and publication bias
scripts/04_subgroup_metareg.R    # Subgroup analyses and meta-regression
scripts/04b_heterogeneity_decomposition.R  # Systematic tau-squared decomposition
scripts/05_rob_visualization.R   # Risk of bias visualization
scripts/06_summary_tables.R      # Summary tables generation
scripts/07_prisma_flowchart.R    # PRISMA 2020 flow diagram
scripts/08_ipd_reconstruction.R  # IPD reconstruction (Guyot algorithm) + RMST
scripts/09_dose_response.R       # Exploratory dose-response meta-regression
scripts/10_influence_cumulative.R # Influence diagnostics and cumulative meta-analysis
scripts/11_figure3_summary.R     # Binary outcome summary forest plot
scripts/12_lancet_tables.R       # Main tables (Table 1, Table 3 GRADE, Table S16)
scripts/14_supplementary_etables.R # Supplementary eTables 1-5 (search strategies, excluded studies, NOS, ROBINS-E, characteristics)
scripts/13_assemble_submission.R # Submission package assembly (run last)
```

## Key Analytic Features

- **39 outcomes** derived from 9 PROSPERO domains (1 HR-OS, 4 HR-RFS, 32 binary OR, 4 time-point OS)
- **48 studies** encompassing 1,659,432 patients (non-overlapping estimate: 695,936)
- **Dual-track quality assessment**: NOS + ROBINS-E
- **7 moderators** for heterogeneity decomposition (including study period midpoint)
- **8 sensitivity analyses** for overall survival
- **GRADE assessment** for all 39 outcomes

## Supplementary Item Renumbering Crosswalk

During Major Revision, supplementary items were renumbered by order of first
mention in the manuscript (AMA style). If an output file or older script
comment refers to a previous supplementary number, map it as follows:

| Item (original) | Item (revised) | Content |
|---|---|---|
| Table S1 | Table S1 | Characteristics of included studies |
| Table S2 | Table S2 | Extracted data summary |
| Table S3 | Table S4 | Studies excluded at full-text assessment |
| Table S4 | Table S6 | Newcastle-Ottawa Scale assessment |
| Table S5 | Table S7 | ROBINS-E assessment |
| Table S6 | Table S13 | Meta-regression for overall survival |
| Table S7 | Table S12 | Publication bias assessment |
| Table S8 | Table S8 | Binary outcome results summary |
| Table S9a-c | Table S11a-c | Median survival descriptive comparison |
| Table S10 | Table S9 | SEER/NCDB overlap sensitivity analysis |
| Table S11 | Table S10 | Binary outcome subgroup interactions |
| Table S12 | Table S15 | Molecular and genomic characteristics |
| Table S13a-c | Table S14a-c | Heterogeneity decomposition |
| Table S14 | Table S16 | Stage-specific hazard ratios |
| Table S15 | Table S5 | SEER/NCDB overlap resolution |
| Table S16a-b | Table S3a-b | Hazard ratio source stratification |
| Figure S1 | Figure S1 | Newcastle-Ottawa Scale scores |
| Figure S2 | Figure S4 | Influence diagnostics |
| Figure S3 | Figure S5 | Leave-one-out and cumulative meta-analysis |
| Figure S4 | Figure S3 | Subgroup forest plots |
| Figure S5 | Figure S8 | Funnel plots |
| Figure S6 | Figure S7 | Cleveland dot-plots of median survival |
| Figure S7 | Figure S6 | IPD-reconstructed Kaplan-Meier curves |
| Figure S8 | Figure S2 | Individual binary outcome forest plots |

## Citation

Accepted for publication in the Journal of the National Cancer Institute (JNCI: Journal of the National Cancer Institute), MS# JNCI-26-0532R1, 2026. A DOI and full citation will be added here once the article is published online.

## License

MIT License

Copyright (c) 2026 Rexiati Ruze et al.
