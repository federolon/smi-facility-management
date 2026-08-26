# Facility management and antenatal care quality: evidence from the Salud Mesoamerica Initiative

### Data and scripts underlying the facility management analysis

This repository contains the processed data and R scripts required to reproduce the facility management analysis conducted using data from the Salud Mesoamerica Initiative (SMI). The analysis evaluates whether management-related practices at the health facility level are associated with the probability that antenatal care records meet the study quality criterion.

## Overview

The project is structured as a reproducible workflow for the manuscript analysis. Using secondary health facility survey data and antenatal care record data, the pipeline performs:

- Construction of a four-domain facility management exposure based on management-related questions from the Health Facility Surveys.
- Harmonization of facility type into a two-level facility complexity variable, because original facility type codes are not directly comparable across countries.
- Mixed-effects logistic regression models with a random intercept for facility, accounting for clustering of antenatal care records within health facilities.
- Sensitivity analyses treating pharmacy availability and pharmacy administration arrangement separately from the primary four-domain management count.
- Export of manuscript-ready tables and figures.

All analyses were implemented using the R programming language.

## Repository Structure

The repository is organized so that figures and tables can be traced back to the analysis code:

```text
.
|-- Data/
|   |-- datosq4.rda
|   `-- dataPool.rda
|-- Scripts/
|   `-- 01_facility_management_models.R
|-- Outputs/
|   |-- Figures/
|   `-- Tables/
|-- RUN_PROCESS.R
|-- README.md
`-- smi-facility-management.Rproj
```

## Data

The analysis uses two processed data files:

- `Data/datosq4.rda`: record-level analytic dataset for antenatal care quality.
- `Data/dataPool.rda`: health facility survey dataset used to reconstruct management-related domains, pharmacy variables, and harmonized facility complexity.

The datasets are included as processed analysis files to support reproducibility of the published results.

## Management Domains

The primary exploratory exposure is a facility-level count of four positive management-related domains:

1. Documented routine administrative meetings.
2. Documented medical meetings.
3. Cultural training.
4. Human resources evaluation.

Pharmacy-related characteristics are not included in the primary domain count. They are analyzed separately as structural or operational facility characteristics.

## Statistical Models

The primary model is a mixed-effects logistic regression model:

```text
quality antenatal care ~ management domain count + country + (1 | facility_id)
```

The term `(1 | facility_id)` specifies a random intercept for each facility. This accounts for the fact that multiple antenatal care records may come from the same health facility and are therefore not fully independent.

Sensitivity models additionally include:

- harmonized facility complexity;
- pharmacy availability;
- pharmacy administration arrangement among facilities reporting a pharmacy.

## Instructions for Reproduction

To replicate the study results:

1. Clone or download this repository.
2. Open the project in R or RStudio.
3. Install the required R packages.
4. From the repository root, run:

```r
source("RUN_PROCESS.R")
```

The script recreates all generated files in:

- `Outputs/Figures/`
- `Outputs/Tables/`

## Required R Packages

Install the required packages from R with:

```r
install.packages(c("lme4", "knitr", "MASS"))
```

The workflow was prepared for R 4.x.

## Generated Tables

The workflow exports CSV and Markdown tables, including:

- `table01_domain_dictionary`
- `domain_count_distribution`
- `facility_complexity_by_country`
- `model_sample_summaries`
- `mixed_model_management_domain_count4`
- `mixed_model_management_domain_count4_facility_complexity`
- `mixed_model_management_domain_count4_pharmacy_complexity`
- `mixed_model_pharmacy_management_arrangement`

## Generated Figures

The workflow exports PNG figures:

- `figure01_domain_count_distribution.png`
- `figure02_quality_by_country.png`
- `figure03_predicted_quality_by_management_domains.png`

## Citation

If you use these materials, please cite the associated publication once available.
