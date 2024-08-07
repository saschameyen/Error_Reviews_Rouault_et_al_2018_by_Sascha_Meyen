get_regression_coefficients <- function(dat)
{
  # Z-Transform relevant variables
  dat$z_gender          <- z_transform(dat$gender)
  dat$z_age             <- z_transform(dat$age)
  dat$z_iq              <- z_transform(dat$iq)
  dat$z_AD              <- z_transform(dat$AD)
  dat$z_Compul          <- z_transform(dat$Compul)
  dat$z_SW              <- z_transform(dat$SW)
  dat$z_accuracy        <- z_transform(dat$accuracy)
  dat$z_mean_confidence <- z_transform(dat$mean_confidence)
  dat$z_m_ratio         <- z_transform(dat$m_ratio)

  # Go through all dependent and independent variable combinations and
  # construct a regression model with covariates gender, age, and IQ. Extract
  # the slope coefficient for the respective independent variable.
  regrassands <- c("z_accuracy", "z_mean_confidence", "z_m_ratio")
  regressors  <- c("z_AD", "z_Compul", "z_SW")

  coef_dat <- data.frame()
  for (regressand in regrassands)
  {
    for (regressor in regressors)
    {
      model <- lm(dat[, regressand] ~ dat$z_gender + 
                                      dat$z_age + 
                                      dat$z_iq + 
                                      dat[, regressor])
      coefficients <- summary(model)$coefficients
      coef_dat <- rbind(coef_dat,
                        data.frame(regressor   = regressor         ,
                                   regressand  = regressand        ,
                                   coefficient = coefficients[5, 1],
                                   se          = coefficients[5, 2]))
    }
  }
  rownames(coef_dat) <- NULL
  coef_dat
}