# gorongosa-mesocarnivores

## Full guild model
**01-generate-model-input.R** generates the model input.

**02-occupancy-model-Rich.R** runs the full guild, no-interactions occupancy model.

**03-occupancy-plot.R** generates plot of species occupancies with 95% CI intervals.

**04-species-covariate-plot.R** generates plots of mean estimates, with 95% credibility interval, of species-specific effects of each covariate on the probability that mesocarnivore species used the area surveyed.

**05-richness-covariate-plots.R** generates plots of modeled mesocarnivore richness with 95% CI intervals at camera trap sites relative to tree cover, distance to lake, and termite mound density.

**06-richness-bysite.R** counts total detections of each species during a specific time period (currently includes many lines of scrap code and needs to be cleaned up).

## Species interaction model
### 1. Data prep

**01-prep-data-for-occupancy-analysis.R**: creates detection histories for species of interest and covariate tables for use in species interaction occupancy models

### 2. Occupancy data analysis

Species-specific scripts for mesocarnivores (for example, 01a-civet-exploration.R) used to determine which covariates had the biggest impact on occupancy for that species

**01a-civet-exploration.R** used to determine which environmental covariates had the greatest effect on civet occupancy by testing models using each covariate individually and all possible pairs of covariates and comparing them using AIC. 

**01b-genet-exploration.R** same as above, for genets.

**01c-honeybadger-exploration.R** same as above, for honey badgers.

**01d-marshmongoose-exploration.R** same as above, for marsh mongooses.

**01e-lion-exploration.R** used to explore different measures of lion presence from camera trap and GPS data (comparing data using only dry season vs full-year, for example).

**01f-civet-genet-exploration.R** used to explore potential effects on civet-genet interactions more closely. 

**02-multi-species-occupancy-analysis-GNP.Rmd** runs occupancy models using occuMulti, for models with no species interactions, those with constant species interactions, and those with interaction covariates. 

**03-multi-species-occupancy-plots.R**

## Temporal analysis

**01-temporal-overlap-calculation.R** Calculates pairwise temporal (24-hr) overlap between all pairs of species, using overlap package.

**02-temporal-figure-functions.R** plots the temporal analysis results for 1-4 species. 