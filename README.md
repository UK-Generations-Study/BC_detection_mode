# Temporal Trends in Behavioural Risk Factors for Early-Onset Cancer in England

  Code and publicly available data for:

  > Johns L, Brayley M, Frost R, Coulson P, Jones M, Berrington de Gonzalez A, García-Closas M.
  > *Breast density and risk factors for interval and screen-detected breast cancers: a case-case analysis of the Breast Cancer Now Generations Cohort, UK.*

  ## Overview

  This repository contains the code for a case-case analysis examining differences in aetiology between interval and screen-detected breast cancers utilising Breast Cancer Now Generations Study (BGS) data. The analysis looks at the risk factors associated with mammographic breast density, using linear regression, and then uses logistic regression to calculate odds ratios (OR) for interval vs screen-detected breast cancers, adjusting for breast density and tumour characteristics.

  ## Key Files
  ### [🔗 detection_mode_project/code/tables_and_figures_v10.html](https://uk-generations-study.github.io/BC_detection_mode/detection_mode_project/code/tables_and_figures_v10.html)
  * Reproduction of manuscript figures and tables

  ## Repository Structure

```text
BC_detection_mode
├── detection_mode_DD/        # Data dictionaries for detection mode
│
├── detection_mode_project/   
│   ├── code/                 # Code to reproduce results from BGS data
│   ├── density_selection/    # Process to select breast density measurements for each participant
│   └── outputs/              # Output tables and figures
│
└── flowchart/                # Flowchart of BGS participants for inclusion in case/case analysis
```

## Software Requirements

* R (version 4.4.1 or later recommended)

## Citation

If you use this repository in your work, please cite:

  > Johns L, Brayley M, Frost R, Coulson P, Jones M, Berrington de Gonzalez A, García-Closas M.
  > *BC_detection_mode*.
  > (2026).

## License

This project is licensed under the MIT License.

