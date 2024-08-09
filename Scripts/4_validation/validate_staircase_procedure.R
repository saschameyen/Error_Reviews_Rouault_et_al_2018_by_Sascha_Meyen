# I validated whether the staircase procedure obscured confounds with M-Ratio
validate_staircase_procedure <- function(original_dat_exp_2,
                                         reproduced_dat_exp_2)
{
  # There were no correlations with the symptom clusters
  cor.test(reproduced_dat_exp_2$var_stimdevi, reproduced_dat_exp_2$AD)
  cor.test(reproduced_dat_exp_2$var_stimdevi, reproduced_dat_exp_2$Compul)
  cor.test(reproduced_dat_exp_2$var_stimdevi, reproduced_dat_exp_2$SW)

  # There were no correlations with M-Ratio, neither the original values nor
  # my reproduced values
  cor.test(reproduced_dat_exp_2$var_stimdevi, reproduced_dat_exp_2$m_ratio)
  plot(reproduced_dat_exp_2$var_stimdevi, reproduced_dat_exp_2$m_ratio)

  cor.test(reproduced_dat_exp_2$var_stimdevi, original_dat_exp_2$mRatio)
  plot(reproduced_dat_exp_2$var_stimdevi, original_dat_exp_2$mRatio)

  # Taken together, the staircase procedure problem did not manifest in the
  # data

  dev.off()

  cat("Problem With Staircase Procedure and M-Ratio

  Although I hypothesized that there is a confound with the variability of the
  staircase procedure influencing the range of presented difficulties and
  thereby the difficulty how giving appropriate confidence ratings, there
  seems to be no problem here. There were no noteworthy difference in the
  variability of the staircases between participants.

  \n")
}