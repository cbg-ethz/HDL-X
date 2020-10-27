# HDL-X

This repository contains all R Markdown files used for the data analysis presented in [Cardner *et al.* (2020)](https://insight.jci.org/articles/view/131491), [doi: 10.1172/jci.insight.131491](https://doi.org/10.1172/jci.insight.131491).

## Main document structure

For reasons of data privacy, this repository contains no actual data, but merely the code used to analyse it. The raw data was collated and stored as [tidy data](https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html) in RDS files under `computations/` (not committed). In general, the data frames were stored in a long format, with a column named `key` coding for the feature (clinical, protein, lipid) and a column named `value` recording the measurement. A column named `Proband ID` contained an anonymised subject code.

- Normalisation
  - Proteomics and lipidomics in `OmicsNormalisation.Rmd`
  - NMR spectroscopy in `Nightingale.Rmd`
  - Functional bioassays in `FunctionalNormalisation.Rmd`
  - Clinical imputation in `ClinicalImputation.Rmd`
- Mixed-effects modelling in `MixedEffects.Rmd`
- Logistic regression in `glmnet.Rmd`
- PLS regression in `PLS.Rmd`
- Graphical lasso in `HugeNPN.Rmd`
- Helper functions
  - `helper_functions.R`
  - `helper_normalisation.R`
  - `helper_volcanoes.R`

## Additional analyses

In addition to the main documents, there are auxiliary R Markdown files for various purposes. Importantly, the `JointGGM.Rmd` explores the joint graphical lasso. The corresponding setup for running it on Euler are available in the `FGL` directory (not committed).
