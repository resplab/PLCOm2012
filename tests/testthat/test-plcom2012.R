library(tidyverse)
library(testthat)

test_input <- function() {
  test_that("Error raising failed", {
    expect_error(
      plcom2012(
        age = 120,
        race = 'White',
        education = 4,
        bmi = 27,
        copd = 0,
        cancer_hist = 0,
        family_hist_lung_cancer = 0,
        smoking_status = 0,
        smoking_intensity = 80,
        duration_smoking = 27,
        smoking_quit_time = 10
      ),
      "Your input data for age should be greater than 0 and less than 100."
    )
    expect_error(
      plcom2012(
        age = 60,
        race = 'indian',
        education = 4,
        bmi = 27,
        copd = 0,
        cancer_hist = 0,
        family_hist_lung_cancer = 0,
        smoking_status = 0,
        smoking_intensity = 80,
        duration_smoking = 27,
        smoking_quit_time = 10
      ),
      "Your input data for race should come from White, American Indian, Alaskan Native, Black, Hispanic, Asian, Native Hawaiian, Pacific Islander."
    )
    expect_error(
      plcom2012(
        age = 62,
        race = 'White',
        education = 8,
        bmi = 27,
        copd = 0,
        cancer_hist = 0,
        family_hist_lung_cancer = 0,
        smoking_status = 0,
        smoking_intensity = 80,
        duration_smoking = 27,
        smoking_quit_time = 10
      ),
      "Your input data for education should be 1 or 2 or 3 or 4 or 5 or 6."
    )
    expect_error(
      plcom2012(
        age = 62,
        race = 'White',
        education = 4,
        bmi = -27,
        copd = 0,
        cancer_hist = 0,
        family_hist_lung_cancer = 0,
        smoking_status = 0,
        smoking_intensity = 80,
        duration_smoking = 27,
        smoking_quit_time = 10
      ),
      "Your input data for bmi should be a positive number."
    )
    expect_error(
      plcom2012(
        age = 62,
        race = 'White',
        education = 4,
        bmi = 27,
        copd = 0,
        cancer_hist = 2,
        family_hist_lung_cancer = 0,
        smoking_status = 0,
        smoking_intensity = 80,
        duration_smoking = 27,
        smoking_quit_time = 10
      ),
      "Your input data for personal history of cancer should be either 1 or 0. 1 as yes and 0 as no."
    )
  })
}

test_input()

test_output <- function() {
  test_that('each condition gives the expected output', {
    expect_equal(
      plcom2012(
        age = 62,
        race = 'White',
        education = 4,
        bmi = 27,
        copd = 0,
        cancer_hist = 0,
        family_hist_lung_cancer = 0,
        smoking_status = 0,
        smoking_intensity = 10/0.4021541613,
        duration_smoking = 27,
        smoking_quit_time = 10
      )
      ,
      exp(-4.532506)/(1 + exp(-4.532506))
    )
    expect_equal(
      plcom2012(
        age = 62,
        race = 'black',
        education = 4,
        bmi = 27,
        copd = 0,
        cancer_hist = 0,
        family_hist_lung_cancer = 0,
        smoking_status = 0,
        smoking_intensity = 10/0.4021541613,
        duration_smoking = 27,
        smoking_quit_time = 10
      )
      ,
      exp(- 4.532506 + 0.3944778)/(1 + exp(- 4.532506 + 0.3944778))
    )
  })
}

test_output()
