# Create regime change variables based on EIU Democracy Index following V-Dem ERT methodology
eiu_ert_variables <- function(data) {
  
  # Validate all columns exist in DF
  required_cols <- c("country_code", "year", "di_score")
  # check for missing required columns
  missing_cols <- setdiff(required_cols, names(data)) 
  # stop if any required columns are missing
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Create a new dataframe 
  eiu_consistent <- data %>%
    group_by(country_code) %>%
    arrange(year) %>%
    
    # Creating regime type variable for EIU Democracy Index (di_score)
    mutate(
      eiu_regime_type = case_when(
        di_score < 5 ~ 0,  # Autocracy
        di_score >= 5 ~ 1,  # Democracy
        TRUE ~ NA_integer_
      )) %>% 
    
    # Create lagged variables for democracy score
    mutate(
      di_score_lag1 = dplyr::lag(di_score, 1),
      di_score_lag2 = dplyr::lag(di_score, 2),
      eiu_regime_type_lag1 = dplyr::lag(eiu_regime_type, 1),
      
      #------------------------------------------------------------
      # 1. REGIME TRANSITIONS - Following V-Dem methodology
      #------------------------------------------------------------
      # Regime transition occurs when a country crosses the democracy threshold
      # eiu_regime_type: 0 = autocracy, 1 = democracy
      
      eiu_regch_event = case_when(
        
        # Autocratization: democracy to autocracy transition (first year of autocracy)
        eiu_regime_type == 0 & eiu_regime_type_lag1 == 1 ~ -1,  
        
        # Democratization: autocracy to democracy transition (first year of democracy)
        eiu_regime_type == 1 & eiu_regime_type_lag1 == 0 ~ 1,   
        
        # No transition (regime stability)
        !is.na(eiu_regime_type) & !is.na(eiu_regime_type_lag1) ~ 0,  
        TRUE ~ NA_integer_
      ),
      
      #------------------------------------------------------------
      # 2. EPISODES OF AUTOCRATIZATION/DEMOCRATIZATION
      #------------------------------------------------------------
      # Adapting V-Dem's methodology to the EIU 0-10 scale
      
      # Calculate year-to-year changes
      di_score_diff = di_score - di_score_lag1,
      di_score_pct_diff = (di_score - di_score_lag1) / di_score_lag1 * 100,
      
      # Calculate 3yr changes: moving average changes (for identifying episodes)
      di_score_diff_3yr = di_score - dplyr::lag(di_score, 3),
      di_score_pct_diff_3yr = (di_score - dplyr::lag(di_score, 3)) / dplyr::lag(di_score, 3) * 100,
      
      # Identifying episodes based on thresholds
      eiu_dem_ep = case_when(
        # Substantial positive change in democracy score
        di_score_diff >= 0.75 ~ 1,
        # Sustained positive change over 3 years
        di_score_diff_3yr >= 1.25 & di_score_diff > 0 ~ 1,
        TRUE ~ 0
      ),
      
      eiu_aut_ep = case_when(
        # Substantial negative change in democracy score
        di_score_diff <= -0.75 ~ 1,
        # Sustained negative change over 3 years
        di_score_diff_3yr <= -1.25 & di_score_diff < 0 ~ 1, 
        TRUE ~ 0
      )
    ) %>%
    ungroup() %>%
    
     #------------------------------------------------------------
     # 3. COUNTRY-LEVEL IDENTIFIER VARIABLES
     #------------------------------------------------------------
     # Creating variables for country selection based on episodes and transitions
    
  group_by(country_code) %>%
    mutate(
      # Episode identifiers
      eiu_has_aut_ep = ifelse(any(eiu_aut_ep == 1, na.rm = TRUE), 1, 0),
      eiu_has_dem_ep = ifelse(any(eiu_dem_ep == 1, na.rm = TRUE), 1, 0),
      eiu_has_both = ifelse(any(eiu_aut_ep == 1 & eiu_dem_ep == 1, na.rm = TRUE), 1, 0),
      eiu_has_neither = ifelse(!any(eiu_aut_ep == 1 | eiu_dem_ep == 1, na.rm = TRUE), 1, 0),
      
      # Total episode counts
      eiu_total_aut_ep = sum(eiu_aut_ep, na.rm = TRUE),
      eiu_total_dem_ep = sum(eiu_dem_ep, na.rm = TRUE),
      
      # Transition identifiers
      eiu_democratized = ifelse(any(eiu_regch_event == 1, na.rm = TRUE), 1, 0),
      eiu_autocratized = ifelse(any(eiu_regch_event == -1, na.rm = TRUE), 1, 0),
      eiu_stable = ifelse(!any(eiu_regch_event == 1 | eiu_regch_event == -1, na.rm = TRUE), 1, 0)
    ) %>%
      
      # Create a separate object to avoid warnings with min()
      # First identify the transition years for each country
      mutate(
        # Find first year of democratization (if it exists)
        first_dem_year = if(any(eiu_regch_event == 1, na.rm = TRUE)) {
          min(year[eiu_regch_event == 1], na.rm = TRUE)
        } else {
          NA_integer_
        },
        
        # Find first year of autocratization (if it exists)
        first_aut_year = if(any(eiu_regch_event == -1, na.rm = TRUE)) {
          min(year[eiu_regch_event == -1], na.rm = TRUE)
        } else {
          NA_integer_
        },
      ) %>%
        
        # Now calculate event times safely
        mutate(
          # Years since democratization event
          eiu_dem_event_time = if_else(!is.na(first_dem_year), year - first_dem_year, 
                                       NA_integer_),
          
          # Years since autocratization event
          eiu_aut_event_time = if_else(!is.na(first_aut_year), year - first_aut_year, 
                                       NA_integer_)
        ) %>%
       
        # Clean up temporary variables
        select(-first_dem_year, -first_aut_year) %>%
        ungroup() %>%
    #------------------------------------------------------------
    # 4. CATEGORICAL CONVERSION
    #------------------------------------------------------------
    
    # Convert to factors for categorical analysis
    mutate(
      eiu_dem_ep = as.factor(eiu_dem_ep),
      eiu_aut_ep = as.factor(eiu_aut_ep),
      eiu_regch_event = as.factor(eiu_regch_event),
      eiu_has_aut_ep = as.factor(eiu_has_aut_ep),
      eiu_has_dem_ep = as.factor(eiu_has_dem_ep),
      eiu_has_both = as.factor(eiu_has_both),
      eiu_has_neither = as.factor(eiu_has_neither),
      eiu_democratized = as.factor(eiu_democratized),
      eiu_autocratized = as.factor(eiu_autocratized),
      eiu_stable = as.factor(eiu_stable)
    )
  
  return(eiu_consistent)
}

# unit test for the function
#if (interactive()) {
#  # Example usage with a sample dataframe
#  sample_data <- data.frame(
#    country_code = rep("COUNTRY", 10),
#    year = 2000:2009,
#    di_score = c(4.5, 4.6, 4.7, 4.8, 5.0, 5.1, 5.2, 5.3, 5.4, 5.5)
#  )
#  
#  result <- eiu_ert_variables(sample_data)
#  print(result)
#} 
#   eiu_regime_type, eiu_regch_event, eiu_dem_ep, eiu_aut_ep,
#   eiu_has_aut_ep, eiu_has_dem_ep, eiu_has_both, eiu_has_neither,
#   eiu_total_aut_ep, eiu_total_dem_ep, eiu_democratized,
#   eiu_autocratized, eiu_stable, eiu_dem_event_time, eiu_aut_event_time
