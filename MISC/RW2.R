setwd("~/Documents/GitHub/QMSS_Thesis_Sanchez")

#load libraries/packages
source("packages.R")

# load data
source("Comp2_panel_wrangling.R")

# Extract the data actually used in the model
ols_spi_di <- lm(spi_comp ~ dplyr::lag(di_score, 1) + log_gdppc + factor(year), data = panel_data) # regular linear model
model_data1 <- model.frame(ols_spi_di)

# Residuals vs Fitted Values plot for ols_spi_di
plot(ols_spi_di, which = 1,
     main = "Residuals vs Fitted Model", caption = "ols_spi_di [Stage 1]", pch = 1, cex = 0.35, col = "#595959")
abline(h = 0, col = "black", lty = 2, lwd = 1)
fitted_vals <- fitted(ols_spi_di)
lines(lowess(fitted_vals, resid(ols_spi_di)), col = "red", lwd =  1.5)

# Residuals vs di_score plot
plot(model_data1$dplyr::lag(di_score, 1), resid(ols_spi_di),
     xlab = "di_score",  ylab = "", yaxt = "n", main = "Residuals vs di_score", pch = 1, cex = 0.35, col = "darkred")
abline(h = 0, col = "black", lty = 2, lwd = 1)
lines(lowess(model_data1$di_score, resid(ols_spi_di)), col = "red", lwd = 1.5)

# Residuals vs log_gdppc plot
plot(model_data1$log_gdppc, resid(ols_spi_di),
     xlab = "log_gdppc",  ylab = "", yaxt = "n", main = "Residuals vs log_gdppc", pch = 1, cex = 0.35, col = "darkgreen")
abline(h = 0, col = "black", lty = 2, lwd = 1)
lines(lowess(model_data1$log_gdppc, resid(ols_spi_di)), col = "red", lwd =  1.5)

