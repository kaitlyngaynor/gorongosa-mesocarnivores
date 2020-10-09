# gorongosa-mesocarnivores

## FUll guild model
**01-generate-model-input.R** generates the model input.

**02-occupancy-model-Rich.R** runs the full guild, no-interactions occupancy model.

**03-occupancy-plot.R** 

**04-species-covariate-plot.R**

**05-richness-covariate-plots.R**

**06-richness-bysite.R**

## Species interaction model
### 1. Data prep

**01-prep-data-for-occupancy-analysis.R**: creates detection histories for species of interest and covariate tables for use in occupancy models

### 2. Occupancy data analysis

Species-specific scripts for mesocarnivores (for example, 01a-civet-exploration.R) used to determine which covariates had the biggest impact on occupancy for that species

**01a-civet-exploration.R**

**01b-genet-exploration.R**

**01c-honeybadger-exploration.R**

**01d-marshmongoose-exploration.R**

**01e-lion-exploration.R** used to explore different measures of lion presence from camera trap and GPS data

**01f-civet-genet-exploration.R**

**02-multi-species-occupancy-analysis-GNP.Rmd** runs occupancy models using occuMulti, for models with no species interactions, those with constant species interactions, and those with interaction covariates. 

**03-multi-species-occupancy-plots.R**

## Temporal analysis

**01-temporal-overlap-calculation.R** Calculates pairwise temporal (24-hr) overlap between all pairs of species, using overlap package.

**02-temporal-figure-functions.R** plots the temporal analysis results for 1-4 species. 