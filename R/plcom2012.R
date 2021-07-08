#' PLCO_M2012 risk prediction model
#'
#' @return
#' @export
#'
#' @examples

plcom2012 <-
  function(age,
           race,
           education,
           bmi,
           copd,
           cancer_hist,
           family_hist_lung_cancer,
           smoking_status,
           smoking_intensity,
           duration_smoking,
           smoking_quit_time) {
    if (race == 'White' |
        race == 'American Indian' | race == 'Alaskan Native') {
      model <-
        0.0778868 * age - 0.0812744 * education - 0.0274194 * bmi + 0.3553063 *
        copd + 0.4589971 * cancer_hist + 0.587185 * family_hist_lung_cancer + 0.2597431 *
        smoking_status - 1.822606 * smoking_intensity + 0.0317321 * duration_smoking -
        0.0308572 * smoking_quit_time - 4.532506
    }

    if (race == 'Black') {
      model <-
        0.0778868 * age - 0.0812744 * education - 0.0274194 * bmi + 0.3553063 *
        copd + 0.4589971 * cancer_hist + 0.587185 * family_hist_lung_cancer + 0.2597431 *
        smoking_status - 1.822606 * smoking_intensity + 0.0317321 * duration_smoking -
        0.0308572 * smoking_quit_time - 4.532506 + 0.3944778
    }

    if (race == 'Hispanic') {
      model <-
        0.0778868 * age - 0.0812744 * education - 0.0274194 * bmi + 0.3553063 *
        copd + 0.4589971 * cancer_hist + 0.587185 * family_hist_lung_cancer + 0.2597431 *
        smoking_status - 1.822606 * smoking_intensity + 0.0317321 * duration_smoking -
        0.0308572 * smoking_quit_time - 4.532506 - 0.7434744
    }

    if (race == 'Asian') {
      model <-
        0.0778868 * age - 0.0812744 * education - 0.0274194 * bmi + 0.3553063 *
        copd + 0.4589971 * cancer_hist + 0.587185 * family_hist_lung_cancer + 0.2597431 *
        smoking_status - 1.822606 * smoking_intensity + 0.0317321 * duration_smoking -
        0.0308572 * smoking_quit_time - 4.532506 − 0.466585
    }

    if (race == 'Native Hawaiian' | race == 'Pacific Islander') {
      model <-
        0.0778868 * age - 0.0812744 * education - 0.0274194 * bmi + 0.3553063 *
        copd + 0.4589971 * cancer_hist + 0.587185 * family_hist_lung_cancer + 0.2597431 *
        smoking_status - 1.822606 * smoking_intensity + 0.0317321 * duration_smoking -
        0.0308572 * smoking_quit_time - 4.532506 + 1.027152
    }

    prob <- exp(model) / (1 + exp(model))

    return(prob)

  }
