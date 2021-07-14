[![R-CMD-check](https://github.com/resplab/PLCOm2012/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/resplab/PLCOm2012/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/resplab/PLCOm2012/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/resplab/PLCOm2012/actions/workflows/test-coverage.yaml)
[![codecov](https://codecov.io/gh/resplab/PLCOm2012/branch/main/graph/badge.svg?token=H9S9DM629T)](https://codecov.io/gh/resplab/PLCOm2012)
<!-- README.md is generated from README.Rmd. Please edit that file -->

# PLCOm2012

<!-- badges: start -->

<!-- badges: end -->

The goal of PLCOm2012 is to predict 6-year probability of lung cancer in an individual person.

## Installation

The development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("resplab/PLCOm2012")
```

## Example

This is a basic example which shows the the 6-year probability of lung cancer in an individual person. The example is an individual who is 62 years old, white, had some college (level 4) education, with body-mass index 27, no chronic obstructive pulmonary disease, no personal history of cancer, no family history of lung cancer, former smoker, smokes 80 cigarettes per day, smokes for 27 years and quits smoking for 10 years. He/she would have 1.75% probability of getting lung cancer over the next 6 years.

``` r
library(PLCOm2012)
plcom2012(age=62, race='White', education=4, bmi=27, copd=0, 
          cancer_hist=0, family_hist_lung_cancer=0, smoking_status=0, 
          smoking_intensity=80, duration_smoking=27, smoking_quit_time=10)
 #> 0.01750922
```
