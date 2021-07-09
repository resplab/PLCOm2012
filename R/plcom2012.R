#' PLCOm2012 risk prediction model
#'
#' @param age a vector of patient's age
#' @param race categorical variable of patient's race or ethnic group (White, Black, Hispanic,
#' Asian, American Indian, Alaskan Native, Native Hawaiian, Pacific Islander)
#' @param education education was measured in six ordinal levels: less than high-school graduate (level 1),
#' high-school graduate (level 2), some training after high school (level 3), some college (level 4),
#' college graduate (level 5), and postgraduate or professional degree (level 6)
#' @param bmi a vector of patient's body mass index, per 1 unit of increase
#' @param copd binary variable of chronic obstructive pulmonary disease (yes as 1 or no as 0)
#' @param cancer_hist binary variable of patient's cancer history (yes as 1 or no as 0)
#' @param family_hist_lung_cancer binary variable of patient's family history of lung cancer (yes as 1 or no as 0)
#' @param smoking_status binary variable of patient's smoking status (current as 1 or former as 0)
#' @param smoking_intensity a vector of the number cigarettes patient smokes per day
#' @param duration_smoking a vector of patient's duration of smoking, per 1-yr increase
#' @param smoking_quit_time a vector of patient's smoking quit time, per 1-yr increase
#'
#' @return prob patient’s 6-year probability of lung-cancer
#' @export
#'
#' @examples
#' plcom2012(age=57, race="Black", education=6, bmi=24, copd=1,
#' cancer_hist=1, family_hist_lung_cancer=1, smoking_status=1,
#' smoking_intensity=3, duration_smoking=2, smoking_quit_time=3
#' )
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

    if (typeof(age) != 'double') {
      stop('Your input data for age should be a number.')
    }
    if (age <= 0 | age > 100) {
      stop('Your input data for age should be greater than 0 and less than 100.')
    }

    %notin% <- Negate(%in%)
    if (race %notin% c('White', 'American Indian', 'Alaskan Native', 'Black', 'Hispanic', 'Asian', 'Native Hawaiian', 'Pacific Islander')) {
      stop('Your input data for race should come from White, American Indian, Alaskan Native, Black, Hispanic, Asian, Native Hawaiian, Pacific Islander.')
    }

    if (education %notin% c(1, 2, 3, 4, 5, 6)) {
      stop('Your input data for education should be 1 or 2 or 3 or 4 or 5 or 6.')
    }

    if (typeof(bmi) != 'double') {
      stop('Your input data for bmi should be a number.')
    }

    if (bmi <= 0) {
      stop('Your input data for bmi should be a positive number.')
    }

    if (copd %notin% c(1, 0)) {
      stop('Your input data for copd should be either 1 or 0. 1 as yes and 0 as no.')
    }

    if (cancer_hist %notin% c(1, 0)) {
      stop('Your input data for personal history of cancer should be either 1 or 0. 1 as yes and 0 as no.')
    }

    #Eric's tests...


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
