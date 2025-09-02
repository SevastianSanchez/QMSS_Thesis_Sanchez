# Two goals 
#1) which lag specification is being used in the POLS model of SPI ~ DI
#2) which robust standard errors function to use 

# set working directory
setwd("~/Documents/GitHub/QMSS_Thesis_Sanchez")

#load libraries/packages
source("packages.R")

# load data
source("Comp2_panel_wrangling.R")

panel_data <- panel_data %>%
  dplyr::select(country_name, country_code, year, spi_comp, spi_comp_lag1, 
                spi_comp_lag2, di_score, di_score_lag1, di_score_lag2, 
                log_gdppc, income_level_recoded)

# Conditions: no pdata.frame created

# dplyr premade lags 
pols_premade_lags <- plm(
  formula = spi_comp ~ di_score + di_score_lag1 + log_gdppc + income_level_recoded + factor(year), 
  index = c("country_code", "year"),
  model = "pooling", 
  data = panel_data)
summary(pols_premade_lags)

# dplyr lags
pols_dplyr_lags <- plm(
  formula = spi_comp ~ di_score + dplyr::lag(di_score, n=1) + log_gdppc + income_level_recoded + factor(year), 
  index = c("country_code", "year"),
  model = "pooling", 
  data = panel_data)
summary(pols_dplyr_lags)

# plm lags
pols_plm_lags <- plm(
  formula = spi_comp ~ di_score + plm::lag(di_score, n=1) + log_gdppc + income_level_recoded + factor(year), 
  index = c("country_code", "year"),
  model = "pooling", 
  data = panel_data)
summary(pols_plm_lags)


# FOR FIGURING OUT GOAL 2
# coeftest() is different from summary(data, vcov = vcovHC())
summary(pols_premade_lags, vcov = vcovHC(pols_premade_lags, cluster = "group", type = "HC1"))


coeftest(pols_premade_lags)

# Show heteroskedasticity consistent coefficients
coeftest(pols_premade_lags, vcovHC)

coeftest(pols_premade_lags, vcovHC(pols_premade_lags, cluster = "group", type = "HC1"))
