# set working directory
setwd("~/Documents/GitHub/QMSS_Thesis_Sanchez")

#load libraries/packages
source("packages.R")

#load function 
#source("df_years2.0_Function.R")

#load df_years() function: 2015-present
#all_data <- df_years2.0(2004, 2023)

#load data 
all_data <- read_csv("data/Main CSV Outputs/merged_final_df.csv")

# selecting vars
panel_data <- all_data %>% 
  dplyr::select(country_name, country_code, year, sdg_overall, spi_comp, di_score, log_gdppc, income_level, aut_ep, dem_ep, regch_event, regime_type_2, regime_type_4, elect_dem, lib_dem, part_dem, delib_dem, egal_dem, academ_free, goal1:goal17, p1_use, p2_services, p3_products, p4_sources, p5_infra) %>% 
  arrange(country_code, year) %>%  # Critical for correct lagging
  filter(year >= 2016)

# Adding has_aut_ep and has_dem_ep for regressing regimes (experienced atleast 1 aut_ep/dem_ep)
panel_data <- panel_data %>%
  # Group by country to check for any event
  group_by(country_code) %>%
  mutate(has_aut_ep = any(aut_ep == 1, na.rm = TRUE)) %>%
  mutate(has_dem_ep = any(dem_ep == 1, na.rm = TRUE)) %>% 
  ungroup() 

# Transforming variables: Centering >> Lagging >> Squaring & Cubing Terms (for polynomial terms)
panel_data <- panel_data %>%
  group_by(country_code) %>%
  arrange(year) %>%  # Ensure data is sorted by year within each country
  mutate(
    # Transforming SPI
    cen_spi_comp = spi_comp - mean(spi_comp, na.rm = TRUE),
    cen_spi_comp_lag1 = dplyr::lag(cen_spi_comp, n = 1),
    cen_spi_comp_lag1_sq = cen_spi_comp_lag1^2,
    cen_spi_comp_lag1_cub = cen_spi_comp_lag1^3,
    cen_spi_comp_lag2 = dplyr::lag(cen_spi_comp, n = 2),
    cen_spi_comp_lag2_sq = cen_spi_comp_lag2^2,
    cen_spi_comp_lag2_cub = cen_spi_comp_lag2^3,
    
    # Transforming DI
    cen_di_score = di_score - mean(di_score, na.rm = TRUE),
    cen_di_score_lag1 = dplyr::lag(cen_di_score, n = 1),
    cen_di_score_lag1_sq = cen_di_score_lag1^2,
    cen_di_score_lag1_cub = cen_di_score_lag1^3,
    cen_di_score_lag2 = dplyr::lag(cen_di_score, n = 2),
    cen_di_score_lag2_sq = cen_di_score_lag2^2,
    cen_di_score_lag2_cub = cen_di_score_lag2^3,
    
    # Transforming log GDP per capita
    cen_log_gdppc = log_gdppc - mean(log_gdppc, na.rm = TRUE),
    cen_log_gdppc_sq = cen_log_gdppc^2,
    cen_log_gdppc_cub = cen_log_gdppc^3
  ) %>%
  ungroup()

# Creating first and second order lags for di_score, spi_comp, and log_gdppc
panel_data <- panel_data %>%
  group_by(country_code) %>%
  arrange(year) %>%  # Ensure data is sorted by year within each country
  mutate(
    di_score_lag1 = dplyr::lag(di_score, n = 1),
    di_score_lag2 = dplyr::lag(di_score, n = 2),
    spi_comp_lag1 = dplyr::lag(spi_comp, n = 1),
    spi_comp_lag2 = dplyr::lag(spi_comp, n = 2),
    log_gdppc_lag1 = dplyr::lag(log_gdppc, n = 1),
    log_gdppc_lag2 = dplyr::lag(log_gdppc, n = 2)
  ) %>%
  ungroup()

panel_data <- panel_data %>% 
  # Recoding income_level, split income_level into dummy variables using case_when()
  # Everything on the left of ~ is the condition, and everything on the right 
  # is the value to return if the condition is true
  mutate(income_level_recoded = case_when(
    income_level == "L" ~ 0, # Low-Income
    income_level == "LM" ~ 1, # Lower-Middle-Income
    income_level == "UM" ~ 2, # Upper-Middle-Income
    income_level == "H" ~ 3, # High-Income
    TRUE ~ NA_real_  # Handle any other cases
  )) %>% 
  mutate(income_level_recoded = as.factor(income_level_recoded)) %>% 
  # recoding/factorizing regime_type_2: 0 = Autocracy; 1 = Democracy 
  mutate(regime_type_binary = as.factor(regime_type_2)) %>% 
  # Recoding regime_type_2, split regime_type_2 into two dummy variables using case_when()
  mutate(
    autocracy = case_when(
      regime_type_2 == 0 ~ 1, # Autocracy
      regime_type_2 == 1 ~ 0, # Democracy
      TRUE ~ NA_real_ # Handle any other cases
    ),
    democracy = case_when(
      regime_type_2 == 0 ~ 0, # Autocracy
      regime_type_2 == 1 ~ 1, # Democracy
      TRUE ~ NA_real_ # Handle any other cases
    )
  ) %>%
  mutate(autocracy = as.factor(autocracy), # autocracy dummy
         democracy = as.factor(democracy)) %>% # democracy dummy
  # recoding regime_type_4: 0 = Closed Autocracy; 1 = Electoral Autocracy; 2 = Electoral Democracy; 3 = Full Democracy
  mutate(regime_type_categ = as.factor(regime_type_4)) %>% 
  mutate(aut_ep = as.factor(aut_ep), # autocratization episode
         dem_ep = as.factor(dem_ep)) # democratization episode

# year-year lags for FD Models - DF
fd_data <- panel_data %>%
  select(country_code, year, sdg_overall, di_score, spi_comp, log_gdppc, income_level, aut_ep, dem_ep, income_level_recoded, regch_event, di_score_lag1, di_score_lag2, spi_comp_lag1, spi_comp_lag2, log_gdppc_lag1, log_gdppc_lag2) %>%
  filter(!is.na(di_score) & !is.na(spi_comp) | !is.na(spi_comp) & !is.na(sdg_overall)) %>% 
  group_by(country_code) %>%
  arrange(year) %>%  # Ensure data is sorted by year within each country
  mutate(
    # first differences for selected variables
    sdg_diff = sdg_overall - dplyr::lag(sdg_overall, n=1),
    di_diff = di_score - di_score_lag1,,
    spi_diff = spi_comp - spi_comp_lag1,
    log_gdppc_diff = log_gdppc - log_gdppc_lag1,
    # lagged first differences 
    di_diff_lag1 = dplyr::lag(di_diff, n=1),
    di_diff_lag2 = dplyr::lag(di_diff, n=2),
    spi_diff_lag1 = dplyr::lag(spi_diff, n=1),
    spi_diff_lag2 = dplyr::lag(spi_diff, n=2),
    log_gdppc_diff_lag1 = dplyr::lag(log_gdppc_diff, n=1),
    log_gdppc_diff_lag1 = dplyr::lag(log_gdppc_diff, n=2)
  ) %>%
  ungroup()

# View(panel_data)
# View(fd_data)