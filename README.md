# BC_detection_mode
Analytical project using Generations and Screening data to explore differences between interval vs screen detected breast cancers.

## Project Background 
Initial work done by Louise. The analytical dataset was initially collated in Safe Haven, then transferred in the same format to RDS where analysis was carried out using Stata. To ensure reproducibility and consistency with current projects, this project is now to be translated into R using RDS datasets. The code is stored in GitHub. 

## Data preparation
Done using R scripts (not Quarto) as these can be linked in the master file. Hence these are not available as HTML outputs but can be reviewed in the code folder. Alternatively, they could be ran through Quarto file with printing the output as html. 

Data preparation was done in 7 scripts to separate different parts and stages. These are all connected in the Master script. By running the Master script the analytical dataset is generated (an_df). 
This is the summary of the 7 parts: 
1. SET UP --------------------------------------
2. PREPARE RISK FACTORS ---------------------------------
3. PREPARE CASUMMARY ---------------------------------
4. PREPARE DETECTION MODE -----------------------------
5. PREPARE MAMMODENSITY -------------------------------
6. CREATE MAMMO DENSITY VARIABLE ------------------------
7. COMPRISE ANALYTICAL DATASET  ------------------------

There are 2 version of script 7 varying by how they handle missing values. For analysis, v3 is used. (v1 is archived)

V2 - missing values treated as error codes 888 - good for exploratory analysis and checking reason for missingness

V3 - Missing values are treated as NAs in v3 - better for futher analysis

### Breast density selection decision tree
[diagram](https://expert-dollop-n8m7kve.pages.github.io/detection_mode_project/density_selection/breast_density_selection.drawio.html)  

### Breast density descriptions 
preliminary - for checks against Louise's density
[density descriptions](https://expert-dollop-n8m7kve.pages.github.io/detection_mode_project/code/density_descriptives.html)

### Mode of detection - analytical dataset descriptives 
work in progress
[dataset descriptions](https://expert-dollop-n8m7kve.pages.github.io/detection_mode_project/code/screening_descriptives.html) 

### Analysis 
with detailed code
[analysis](https://expert-dollop-n8m7kve.pages.github.io/detection_mode_project/code/analysis.html) 

### Publication table and figures
code hidden, outputs only 
[tables and figures](https://expert-dollop-n8m7kve.pages.github.io/detection_mode_project/code/tables_and_figures.html)
