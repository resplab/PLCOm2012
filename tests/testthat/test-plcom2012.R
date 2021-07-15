library(testthat)

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
        smoking_intensity = 10 / 0.4021541613,
        duration_smoking = 27,
        smoking_quit_time = 10
      )
      ,
      exp(-4.532506) / (1 + exp(-4.532506))
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
        smoking_intensity = 10 / 0.4021541613,
        duration_smoking = 27,
        smoking_quit_time = 10
      )
      ,
      exp(-4.532506 + 0.3944778) / (1 + exp(-4.532506 + 0.3944778))
    )
    expect_equal(
      plcom2012(
        age = 62,
        race = 'hispanic',
        education = 4,
        bmi = 27,
        copd = 0,
        cancer_hist = 0,
        family_hist_lung_cancer = 0,
        smoking_status = 0,
        smoking_intensity = 10 / 0.4021541613,
        duration_smoking = 27,
        smoking_quit_time = 10
      )
      ,
      exp(-4.532506 - 0.7434744) / (1 + exp(-4.532506 - 0.7434744))
    )
    expect_equal(
      plcom2012(
        age = 62,
        race = 'asian',
        education = 4,
        bmi = 27,
        copd = 0,
        cancer_hist = 0,
        family_hist_lung_cancer = 0,
        smoking_status = 0,
        smoking_intensity = 10 / 0.4021541613,
        duration_smoking = 27,
        smoking_quit_time = 10
      )
      ,
      exp(-4.532506 - 0.466585) / (1 + exp(-4.532506 - 0.466585))
    )
    expect_equal(
      plcom2012(
        age = 62,
        race = 'pacific islander',
        education = 4,
        bmi = 27,
        copd = 0,
        cancer_hist = 0,
        family_hist_lung_cancer = 0,
        smoking_status = 0,
        smoking_intensity = 10 / 0.4021541613,
        duration_smoking = 27,
        smoking_quit_time = 10
      )
      ,
      exp(-4.532506 + 1.027152) / (1 + exp(-4.532506 + 1.027152))
    )
  })
}

test_output()
