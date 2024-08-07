validate_regression <- function(reproduced_dat_exp_1,
                                reproduced_dat_exp_2)
{
  # TODO
  get_regression_coefficients(reproduced_dat_exp_1)

  # Individual correlation tests yield the same picture.

  # zung (Depression) & anxiety (Social Anxiety) - Anxious-Depression (AD)
  # ocir (Obsessive-Compulsive Inventory-Revised) - Compulsivness (C)
  # liebowitz (Social Anxiety) - Social Withdrawal (SW)
  cor.test(reproduced_dat_exp_1$zung     , reproduced_dat_exp_1$accuracy) # p = .99
  cor.test(reproduced_dat_exp_1$anxiety  , reproduced_dat_exp_1$accuracy) # p = .94
  cor.test(reproduced_dat_exp_1$ocir     , reproduced_dat_exp_1$accuracy) # p = .73
  cor.test(reproduced_dat_exp_1$liebowitz, reproduced_dat_exp_1$accuracy) # p = .52

  cor.test(reproduced_dat_exp_1$zung     , reproduced_dat_exp_1$mean_confidence) # p = .01 -> -.13
  cor.test(reproduced_dat_exp_1$anxiety  , reproduced_dat_exp_1$mean_confidence) # p = .01 -> -.11
  cor.test(reproduced_dat_exp_1$ocir     , reproduced_dat_exp_1$mean_confidence) # p = .77
  cor.test(reproduced_dat_exp_1$liebowitz, reproduced_dat_exp_1$mean_confidence) # p = .01 -> -.12

  cor.test(reproduced_dat_exp_1$zung     , reproduced_dat_exp_1$m_ratio) # p = .03 -> -.09
  cor.test(reproduced_dat_exp_1$anxiety  , reproduced_dat_exp_1$m_ratio) # p = .08
  cor.test(reproduced_dat_exp_1$ocir     , reproduced_dat_exp_1$m_ratio) # p = .74
  cor.test(reproduced_dat_exp_1$liebowitz, reproduced_dat_exp_1$m_ratio) # p = .49

  # In Experiment 2, we indeed find the reported pattern. AD is associated
  # with lower mean confidences and C with higher confidences.
  get_regression_coefficients(reproduced_dat_exp_2)

  cor.test(reproduced_dat_exp_2$zung     , reproduced_dat_exp_2$accuracy) # p = .61
  cor.test(reproduced_dat_exp_2$anxiety  , reproduced_dat_exp_2$accuracy) # p = .35
  cor.test(reproduced_dat_exp_2$ocir     , reproduced_dat_exp_2$accuracy) # p = .88
  cor.test(reproduced_dat_exp_2$liebowitz, reproduced_dat_exp_2$accuracy) # p = .66

  cor.test(reproduced_dat_exp_2$zung     , reproduced_dat_exp_2$mean_confidence) # p = .64
  cor.test(reproduced_dat_exp_2$anxiety  , reproduced_dat_exp_2$mean_confidence) # p = .07 -> r = -.08
  cor.test(reproduced_dat_exp_2$ocir     , reproduced_dat_exp_2$mean_confidence) # p = .03 -> r = +.09
  cor.test(reproduced_dat_exp_2$liebowitz, reproduced_dat_exp_2$mean_confidence) # p = .36

  cor.test(reproduced_dat_exp_2$zung     , reproduced_dat_exp_2$m_ratio) # p = .93
  cor.test(reproduced_dat_exp_2$anxiety  , reproduced_dat_exp_2$m_ratio) # p = .26
  cor.test(reproduced_dat_exp_2$ocir     , reproduced_dat_exp_2$m_ratio) # p = .46
  cor.test(reproduced_dat_exp_2$liebowitz, reproduced_dat_exp_2$m_ratio) # p = .88

  # There may also be a problem with regard to the factor solution, which
  # could be less stable than the authors assumed. I perform a
  # crossvalidation analysis in which part of the data serves for
  # constructing the factors and another part for evaluating the regression.
  # We find the same results as before: Experiment 1 points towards a
  # negative association between AD and mean confidence, SW and confidence,
  # and if anything a negative trend for C and mean confidence. Experiment 2
  # reflects the original results: negative association between AD and mean
  # confidence as well as a positive association between C and mean
  # confidence.
  reference_loadings <- reproduce_factor_analysis_results(reproduced_dat = reproduced_dat_exp_2,
                                                          should_return_loadings = TRUE)
  validate_factor_analysis_overfit(reproduced_dat = reproduced_dat_exp_2,
                                   reference_loadings)

  cat("Factor Analysis and Regression 

  For Experiment 2, the results seem stable even after applying cross
  validation (compute factors from a training set, then compute factor scores
  and regression coefficients from a separate test set).

  However for Experiment 1, the results look different. Even after matching
  the factors to those of Experiment 2, we only find partially the same
  results. Consistently, factor Anxious-Depression (AD) has a negative
  association with mean confidence. But there is no positive association
  between factor Compulsivity (C) and mean confidence. Moreover,
  Social-Withdrawal (SW) also has a clera negative association with mean
  confidence. This also shows up in the cross validation analysis.

  Additionally, correlation analyses based on single questionnaires seem to
  support these findings. (But note that the regression analyses controlled
  for covariates gender, age, and IQ which is not reflected in these
  correlations.)
  
  ")
}
