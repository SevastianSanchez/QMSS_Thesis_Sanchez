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
  dplyr::select(country_name, country_code, year, spi_comp, di_score, di_score_lag1, di_score_lag2, 
                log_gdppc, spi_comp_lag1, spi_comp_lag2, income_level_recoded) %>% 
  dplyr::arrange(country_code, year)

head(panel_data)

pdata <- pdata.frame(panel_data, index = c("country_code", "year"))

# Conditions: no pdata.frame created

# dplyr premade lags 1
fe_premade_lags <- plm(
  formula = spi_comp ~ di_score + di_score_lag1 + log_gdppc + factor(year) + factor(income_level_recoded), 
  model = "within", 
  index = c("country_code", "year"),
  data = panel_data)
summary(fe_premade_lags)

# plm lags
fe_plm_lags <- plm(
  formula = spi_comp ~ di_score + lag(di_score, 1) + log_gdppc + factor(year) + factor(income_level_recoded), 
  index = c("country_code", "year"),
  model = "within", 
  data = pdata)
summary(fe_plm_lags)

# dplyr lags [NOT CORRECT - DON'T USE dplyr IN PLM!!!!]
#CONCLUSION: DONT USE DPLYR LAGS IN PLM!!! EITHER USE PLM LAGS OR PREMADE LAGS! 
# (SO LONG AS YOU ENSURE PREMADE ARE REPRESENT THE YEAR PRIOR TO THE CURRENT OBSERVATION)

# USE PREMADE LAGGED VARIABLES OF DI (di_score)!!!

# FOR FIGURING OUT CORRECT Robust SE - GOAL 2

summary(pols_premade_lags)
coeftest(pols_premade_lags) # Same as summary(pols_premade_lags)

coeftest(pols_premade_lags, vcovHC) # Defaults to HC0 

coeftest(pols_premade_lags, vcovHC(pols_premade_lags, cluster = "group", type = "HC1")) # Same as below
summary(pols_premade_lags, vcov = vcovHC(pols_premade_lags, cluster = "group", type = "HC1"))
