# Transplant-Free Fontan Survival Project

[![R](https://img.shields.io/badge/R-4.4.2-blue.svg)](https://www.r-project.org/)
[![Quarto](https://img.shields.io/badge/Quarto-1.4-green.svg)](https://quarto.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview

This repository contains the code and analysis for a retrospective cohort study comparing long-term outcomes between **Lateral Tunnel (LT)** and **Extracardiac Conduit (ECC)** Fontan procedures using data from the International Fontan Registry (IFR).

## Key Findings

| Outcome | Result |
|---------|--------|
| Primary composite (death/transplant/takedown) | No significant difference (p = 0.84) |
| Lymphatic complications | Significantly higher in LT (p = 0.036) |
| Arrhythmia | No significant difference (p = 0.81) |
| Thromboembolic events | Trend toward higher in ECC (p = 0.16) |
| Age at Fontan | Significant predictor (HR 1.23/year, p = 0.008) |

**Bottom line:** Both contemporary Fontan techniques demonstrate excellent 5-year outcomes (event-free survival >92%), with age at surgery being the strongest predictor of adverse outcomes.

## Repository Structure

```
├── Fontan_Survival_Analysis.qmd   # Main Quarto document (reproducible report)
├── Fontan_Survival.R              # Complete R analysis script
├── IFR_outcomes.xlsx              # Dataset (International Fontan Registry)
├── README.md                      # This file
└── figures/                       # Generated figures
    ├── KM_Primary_Outcome.png
    ├── KM_Arrhythmia.png
    ├── KM_Thromboembolism.png
    ├── KM_Lymphatic.png
    └── Forest_Plot_Primary_Model.png
```

## Requirements

### R Packages

```r
install.packages(c(
  "readxl",      # Read Excel files
  "dplyr",       # Data manipulation
  "tidyr",       # Data tidying
  "survival",    # Survival analysis
  "survminer",   # Survival plots
  "ggplot2",     # Visualization
  "tableone",    # Baseline tables
  "broom",       # Tidy model outputs
  "knitr",       # Report generation
  "kableExtra"   # Table formatting
))
```

### Software

- R ≥ 4.4.0
- RStudio ≥ 2025.05.0 (optional but recommended)
- Quarto ≥ 1.4 (for rendering the .qmd file)

## Quick Start

### Option 1: Run the R Script

```r
# Update the file path in line 47 to your local data location
source("Fontan_Survival.R")
```

### Option 2: Render the Quarto Document

```bash
quarto render Fontan_Survival_Analysis.qmd
```

Or in RStudio: Open the `.qmd` file and click "Render"

## Methods Summary

### Study Design
- **Design:** Retrospective cohort analysis
- **Data Source:** International Fontan Registry (IFR)
- **Population:** Pediatric patients (<18 years) undergoing contemporary TCPC

### Statistical Analysis
- **Descriptive:** TableOne for baseline characteristics
- **Survival:** Kaplan-Meier curves with log-rank tests
- **Regression:** Cox proportional hazards models
- **Approach:** Dual modeling strategy due to limited events (EPV = 8.0 for primary model)

### Outcomes
| Outcome Type | Definition |
|--------------|------------|
| **Primary** | Composite of death, heart transplant, or Fontan takedown |
| **Secondary** | Arrhythmia, thromboembolic events, lymphatic complications |

## Cohort Selection

```
Initial cohort: 477 patients
    ↓
Exclusions:
  - Missing Fontan type assignment
  - Age ≥18 years at Fontan
  - Missing follow-up data
  - Implausible follow-up times (>360 months)
    ↓
Final analytical cohort: 469 patients
  - Extracardiac: 328 (69.9%)
  - Lateral Tunnel: 141 (30.1%)
```

## Results Highlights

### Primary Outcome
- 16 patients experienced the composite outcome
- No significant difference between ECC and LT (adjusted HR: 1.04, 95% CI: 0.28-3.81, p = 0.96)
- 5-year event-free survival: ECC ~93%, LT ~97%

### Age Effect
- Each 1-year increase in age at Fontan associated with 22.6% increased hazard (HR: 1.23, p = 0.008)
- Supports optimal timing of Fontan completion in the 2-4 year age range

### Lymphatic Complications
- Significantly higher in Lateral Tunnel group
- 60-month freedom from lymphatic complications: ECC 94.5% vs LT 84.1% (p = 0.036)
- Absolute difference: 10.4% favoring ECC

## Limitations

1. Limited events (n=16) constrained statistical power
2. Differential follow-up duration between groups
3. Registry design precluded standardized surveillance protocols
4. Unmeasured confounders (surgical era, institutional practices)
5. Median follow-up of 30 months limits long-term conclusions

## Author

**Carlos Carhuas**  
MS Biostatistics Candidate  
George Washington University  
December 2025

## Citation

If you use this code or analysis, please cite:

```
Carhuas C. (2025). Transplant-Free Survival Following Contemporary Fontan 
Procedures: A Comparative Analysis of Lateral Tunnel vs Extracardiac 
Conduit Techniques. GitHub repository.
```

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- International Fontan Registry for data access
- GW Biostatistics faculty for guidance
