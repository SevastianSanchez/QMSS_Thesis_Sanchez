# Create regime change variables based on EIU Democracy Index following V-Dem ERT methodology
eiu_ert_variables <- function(data) {
  # Validate input data
  required_cols <- c("country_code", "year", "di_score", "eiu_regime_type")
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Create a new dataframe 
  eiu_consistent <- data %>%
    group_by(country_code) %>%
    arrange(year) %>%
    
    # Create lagged variables for regime type and democracy score
    mutate(
      di_score_lag1 = lag(di_score, 1),
      di_score_lag2 = lag(di_score, 2),
      eiu_regime_type_lag1 = lag(eiu_regime_type, 1),
      
      # Calculate year-to-year changes
      di_score_change = di_score - di_score_lag1,
      di_score_pct_change = (di_score - di_score_lag1) / di_score_lag1 * 100,
      
      # Calculate moving average changes (for identifying episodes)
      di_score_change_3yr = di_score - lag(di_score, 3),
      
      #------------------------------------------------------------
      # 1. REGIME TRANSITIONS - Following V-Dem methodology
      #------------------------------------------------------------
      # Regime transition occurs when a country crosses the democracy threshold
      # eiu_regime_type: 0 = autocracy, 1 = democracy
      
      eiu_regime_transition = case_when(
        # Autocratization: democracy to autocracy transition
        eiu_regime_type == 0 & eiu_regime_type_lag1 == 1 ~ -1,  
        # Democratization: autocracy to democracy transition
        eiu_regime_type == 1 & eiu_regime_type_lag1 == 0 ~ 1,   
        # No transition (regime stability)
        !is.na(eiu_regime_type) & !is.na(eiu_regime_type_lag1) ~ 0,  
        TRUE ~ NA_integer_
      ),
      
      #------------------------------------------------------------
      # 2. EPISODES OF AUTOCRATIZATION/DEMOCRATIZATION
      #------------------------------------------------------------
      # Adapting V-Dem's methodology to the EIU 0-10 scale
      
      eiu_dem_ep = case_when(
        # Substantial positive change in democracy score
        di_score_change >= 0.75 ~ 1,
        # Sustained positive change over 3 years
        di_score_change_3yr >= 1.25 & di_score_change > 0 ~ 1,
        TRUE ~ 0
      ),
      
      eiu_aut_ep = case_when(
        # Substantial negative change in democracy score
        di_score_change <= -0.75 ~ 1,
        # Sustained negative change over 3 years
        di_score_change_3yr <= -1.25 & di_score_change < 0 ~ 1, 
        TRUE ~ 0
      ),
      
      #------------------------------------------------------------
      # 3. EVENT TIME VARIABLE (for event history analysis)
      #------------------------------------------------------------
      # Identify transition years
      transition_year = ifelse(eiu_regime_transition != 0, year, NA),
      
      # For each country, find the most recent transition
      last_transition_year = lag(transition_year, default = NA),
      
      # Calculate years since most recent transition
      years_since_transition = ifelse(!is.na(last_transition_year), 
                                      year - last_transition_year, 
                                      NA)
    ) %>%
    ungroup() %>%
    
    #------------------------------------------------------------
    # 4. COUNTRY-LEVEL IDENTIFIER VARIABLES
    #------------------------------------------------------------
    # Group by country to create country-level identifiers
    group_by(country_code) %>%
    mutate(
      # Episode identifiers
      eiu_has_aut_ep = ifelse(any(eiu_aut_ep == 1, na.rm = TRUE), 1, 0),
      eiu_has_dem_ep = ifelse(any(eiu_dem_ep == 1, na.rm = TRUE), 1, 0),
      eiu_has_neither = ifelse(!any(eiu_aut_ep == 1 | eiu_dem_ep == 1, na.rm = TRUE), 1, 0),
      eiu_total_aut_ep = sum(eiu_aut_ep, na.rm = TRUE),
      eiu_total_dem_ep = sum(eiu_dem_ep, na.rm = TRUE),
      
      # Transition identifiers
      eiu_democratized = ifelse(any(eiu_regime_transition == 1, na.rm = TRUE), 1, 0),
      eiu_autocratized = ifelse(any(eiu_regime_transition == -1, na.rm = TRUE), 1, 0),
      eiu_stable = ifelse(!any(eiu_regime_transition == 1 | eiu_regime_transition == -1, na.rm = TRUE), 1, 0),
      
      # For event history analysis - create event time variables
      # Years before and after democratization (negative = before, positive = after)
      eiu_dem_event_time = case_when(
        eiu_democratized == 1 ~ year - min(year[eiu_regime_transition == 1], na.rm = TRUE),
        TRUE ~ NA_integer_
      ),
      
      # Years before and after autocratization (negative = before, positive = after)
      eiu_aut_event_time = case_when(
        eiu_autocratized == 1 ~ year - min(year[eiu_regime_transition == -1], na.rm = TRUE),
        TRUE ~ NA_integer_
      )
    ) %>%
    ungroup() %>%
    
    # Convert to factors for categorical analysis
    mutate(
      eiu_dem_ep = as.factor(eiu_dem_ep),
      eiu_aut_ep = as.factor(eiu_aut_ep),
      eiu_regime_transition = as.factor(eiu_regime_transition),
      eiu_has_aut_ep = as.factor(eiu_has_aut_ep),
      eiu_has_dem_ep = as.factor(eiu_has_dem_ep),
      eiu_has_neither = as.factor(eiu_has_neither),
      eiu_democratized = as.factor(eiu_democratized),
      eiu_autocratized = as.factor(eiu_autocratized),
      eiu_stable = as.factor(eiu_stable)
    )
  
  return(eiu_consistent)
}

# Example usage:
# panel_data_ert <- eiu_ert_variables(panel_data)
# 
# # Get list of countries that experienced democratization
# democratized_countries <- panel_data_ert %>%
#   filter(eiu_democratized == 1) %>%
#   distinct(country_name) %>%
#   pull(country_name)
# 
# # Get list of countries that experienced autocratization
# autocratized_countries <- panel_data_ert %>%
#   filter(eiu_autocratized == 1) %>%
#   distinct(country_name) %>%
#   pull(country_name)
# 
# # Create event history dataset focused on years around transitions
# dem_transitions <- panel_data_ert %>%
#   filter(!is.na(eiu_dem_event_time)) %>%
#   filter(eiu_dem_event_time >= -5 & eiu_dem_event_time <= 5)