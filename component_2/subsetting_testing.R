# set working directory
setwd("~/Documents/GitHub/QMSS_Thesis_Sanchez")

#load libraries/packages
source("packages.R")

# load data
source("Comp2_panel_wrangling.R")
panel_data1 <- panel_data %>% 
  dplyr::select(country_name, country_code, year, spi_comp, di_score, 
                log_gdppc, sdg_overall, income_level_recoded, starts_with("goal")) %>%
  dplyr::arrange(country_code, year)

## subsetting by regime types and stability ##

panel_data_test <- panel_data1 %>%
  arrange(country_code, year) %>%
  group_by(country_code) %>%
  mutate(
    regime_type_eiu = case_when(
      di_score >= 8.0 ~ "Full democracy",
      di_score >= 6.0 & di_score < 8.0 ~ "Flawed democracy",
      di_score >= 4.0 & di_score < 6.0 ~ "Hybrid regime",
      di_score < 4.0 ~ "Authoritarian",
      TRUE ~ NA_character_
    ),
    regime_transition_eiu = regime_type_eiu != dplyr::lag(regime_type_eiu),  # Did regime change?
    regime_stable_eiu = !regime_transition_eiu | is.na(regime_transition_eiu)  # Stable years
  ) %>%
  ungroup()

# Function that counts number of unique countries by regime type
get_regime_counts <- function(regime_type, stable = NULL, 
                                   transition = NULL, return_names = FALSE) {
  
  result <- panel_data_test
  
  # Filter by regime type
  result <- result %>%
    filter(regime_type_eiu == regime_type)
  
  # Filter by stability if specified
  if (!is.null(stable)) {
    result <- result %>%
      filter(regime_stable_eiu == stable)
  }
  
  # Filter by transition if specified
  if (!is.null(transition)) {
    result <- result %>%
      filter(regime_transition_eiu == transition)
  }
  
  # Get distinct countries
  countries <- result %>%
    distinct(country_code) %>%
    pull(country_code)
  
  # Return based on argument
  if (return_names) {
    list(
      count = length(countries),
      countries = countries
    )
  } else {
    length(countries)
  }
}
# Example usage:
countries_testing <- get_regime_counts("Full democracy", stable = FALSE, return_names = TRUE)

# New function: focuses on subsetting
get_regime_data <- function(regime_type, stable = NULL, transition = NULL) {
  
  # Reuse the original function to get countries
  matching <- get_regime_counts(
    regime_type, 
    stable = stable, 
    transition = transition, 
    return_names = TRUE
  )$countries
  
  # Return filtered data
  panel_data_test %>%
    filter(country_code %in% matching)
}

# Example usage:
regime_data_testing <- get_regime_data(c("Full democracy", "Flawed democracy", "Hybrid regime", "Authoritarian"), transition = TRUE)

### DIFFERENT VARIATIONS ###
# Democracy - Full democracies only
# regime_data <- get_regime_data("Full democracy")

# Flawed democracies only
# regime_data <- get_regime_data("Flawed democracy")

# Hybrid regimes only
# regime_data <- get_regime_data("Hybrid regime")

# Authoritarian regimes only
# regime_data <- get_regime_data("Authoritarian")

# Democracies with regime transitions only
# regime_data <- get_regime_data("Full democracy", stable = FALSE)

# Flawed democracies with regime transitions only
# regime_data <- get_regime_data("Flawed democracy", stable = FALSE)

# Hybrid regimes with regime transitions only
# regime_data <- get_regime_data("Hybrid regime", stable = FALSE)

# Authoritarian regimes with regime transitions only
# regime_data <- get_regime_data("Authoritarian", stable = FALSE)


## MISC TESTING: MULTIPLE DEPENDENT VARIABLES WITH fixest PACKAGE ##
library(fixest)

panel_data_test_1 <- panel(regime_data_testing, ~country_code + year)

# Multiple dependent variables with same predictors
models <- feols(c(goal1, goal2, goal3, goal4, goal5, goal6, goal7, goal8, goal9, 
                  goal10, goal11, goal12, goal13, goal14, goal15, goal16, goal17) ~ 
                  l(di_score, 0:2) 
                + l(spi_comp, 0:2) 
                + log_gdppc | country_code + year,
                data = panel_data_test_1)

# View all results
etable(models, cluster = ~country_code + year)


