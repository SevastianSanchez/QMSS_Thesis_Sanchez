plm_aic_bic <- function(model) {
  res <- residuals(model)
  n <- length(res)
  k <- length(coef(model))  # Number of estimated coefficients (excluding fixed effects)
  sigma_sq <- sum(res^2) / n
  ll <- -n/2 * log(2 * pi * sigma_sq) - (1/(2 * sigma_sq)) * sum(res^2)
  aic <- -2 * ll + 2 * k
  bic <- -2 * ll + log(n) * k
  return(list(AIC = aic, BIC = bic))
}