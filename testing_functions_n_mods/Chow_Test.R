# Chow Test (Proper Use of the Group Variable)
# Description: Code performs Chow Test to check for structural breaks in regression models
# Example from: https://bookdown.org/mike/data_analysis/model-specification-tests.html#sec-chow-test 

# Pooled model (all data)
pooled_model <- lm(y ~ x1 + x2 + x3)

# Separate models for Group 0 and Group 1
model_group0 <- lm(y[group == 0] ~ x1[group == 0] + x2[group == 0] + x3[group == 0])
model_group1 <- lm(y[group == 1] ~ x1[group == 1] + x2[group == 1] + x3[group == 1])

# Calculating SSRs
SSR_pooled <- sum(residuals(pooled_model)^2)
SSR_group0 <- sum(residuals(model_group0)^2)
SSR_group1 <- sum(residuals(model_group1)^2)

# Chow Test formula
k_chow <- length(coef(pooled_model))  # Number of parameters (including intercept)
n0 <- sum(group == 0)                 # Sample size for Group 0
n1 <- sum(group == 1)                 # Sample size for Group 1

F_chow <- ((SSR_pooled - (SSR_group0 + SSR_group1)) / k_chow) /
  ((SSR_group0 + SSR_group1) / (n0 + n1 - 2 * k_chow))

# Corresponding p-value
p_value_chow <-
  pf(
    F_chow,
    df1 = k_chow,
    df2 = (n0 + n1 - 2 * k_chow),
    lower.tail = FALSE
  )

cat("Chow Test F-statistic:", F_chow, "\n")
#> Chow Test F-statistic: 0.3551197
cat("P-value:", p_value_chow, "\n")
#> P-value: 0.8398657