#Set Directory
#setwd("~/Documents/GitHub/QMSS_Thesis_Sanchez/data/misc")

#vdem = country_text_id, country_name, [COWcode]
#spi = iso3c
#sdg = country_code, country_name
#sci = country_code 
#ert = country_text_id, country_name, country_id,
#gdppc = country_code
#infocap = country_id
#income class = country_code
#di = country_code
#gini = Code

country_colnames <- c(
  "country", "country_code", "ccodecow", "COWcode", "cowcode", "iso3", "iso3c", 
  "country_name", "country_id", "countryname", "Entity", "Code", "country_text_id", "countryid"
)

# Define forward mapping (country names/aliases to ISO3 codes)
custom_match_forward <- c(
  "Kosovo" = "XKX",
  "Somaliland" = "SOL",
  "Zanzibar" = "ZNZ",
  "Channel Islands" = "CHI",
  "Saint Martin (French part)" = "MAF",
  "Curaçao" = "CUW",
  "West Bank and Gaza" = "PSE",
  "Congo (Kinshasa)" = "COD",
  "Myanmar (Burma)" = "MMR",
  "Iran, Islamic Rep." = "IRN",
  "Kyrgyz Republic" = "KGZ",
  "Bahamas" = "BHS",
  "Belize" = "BLZ",
  "Brunei" = "BRN",
  "South Sudan" = "SSD"
)

# Define reverse mapping (ISO3 codes to country names)
custom_match_reverse <- c(
  "XKX" = "Kosovo",
  "SOL" = "Somaliland",
  "ZNZ" = "Zanzibar",
  "CHI" = "Channel Islands",
  "MAF" = "Saint Martin (French part)",
  "CUW" = "Curaçao",
  "PSE" = "West Bank and Gaza",
  "COD" = "Congo (Kinshasa)",
  "MMR" = "Myanmar (Burma)",
  "IRN" = "Iran, Islamic Rep.",
  "KGZ" = "Kyrgyz Republic", 
  "BHS" = "Bahamas",
  "BLZ" = "Belize",
  "BRN" = "Brunei",
  "SSD" = "South Sudan"
)

standardize_country_codes <- function(df) {
  # Identify country columns
  country_cols <- names(df)[tolower(names(df)) %in% 
                              tolower(country_colnames)]
  
  if(length(country_cols) > 0) {
    for(col in country_cols) {
      try({
        # custom matching
        df$iso3_standardized <- countrycode(
          df[[col]],
          origin = case_when(
            tolower(col) == "iso2" ~ "iso2c",
            tolower(col) == "iso3" ~ "iso3c",
            tolower(col) == "country_code" ~ "iso3c",
            TRUE ~ "country.name"
          ),
          destination = "iso3c",
          # Add custom matches here
          custom_match = c(custom_match_forward
          )
        )
        break
      }, silent = TRUE)
    }
  }
  
  if ("iso3_standardized" %in% names(df)) {
    # Use countrycode's built-in mappings for standard ISO3 codes
    df$country_name <- countrycode::countrycode(
      sourcevar = df$iso3_standardized,
      origin = "iso3c",
      destination = "country.name",
      # Only override for non-standard codes (e.g., Kosovo)
      custom_match = custom_match_reverse
    )
  }
  if(!"iso3_standardized" %in% names(df)) {
    warning("No suitable country code column found in dataset")
  }
  
  return(df)
}


#Processing datasets for merging 
process_datasets <- function(df_lists) {
  merged_data <- list()
  
  for(i in seq_along(df_lists)) {
    df <- df_lists[[i]]
    
    # Standardize year column naming
    year_col <- names(df)[tolower(names(df)) %in% c("year")]
    if (length(year_col) == 0) {
      year_col <- names(df)[grepl("year", tolower(names(df)))]
    }
    if (length(year_col) > 0 && year_col != "year") {
      names(df)[names(df) == year_col[1]] <- "year"
    }
    if ("year" %in% names(df)) {
      df <- df %>%
        mutate(year = suppressWarnings(as.integer(year))) %>%
        filter(!is.na(year))
    }
    df <- standardize_country_codes(df)
    
    # Identify original country columns
    orig_cols <- names(df)[tolower(names(df)) %in% tolower(country_colnames)]
    new_orig_names <- character(0)
    if(length(orig_cols) > 0) {
      new_orig_names <- paste0("orig_", orig_cols, "_ds", i)
      names(df)[match(orig_cols, names(df))] <- new_orig_names
    }
    
    # Identify non-key columns for suffixing
    #protected_cols <- c("iso3_standardized", "year", new_orig_names)
    #non_key_cols <- setdiff(names(df), protected_cols)
    #if(length(non_key_cols) > 0) {
    #  new_names <- paste0(non_key_cols, "_ds", i)
    #  names(df)[match(non_key_cols, names(df))] <- new_names
    #}
    
    # Skip datasets without required columns
    if(!"iso3_standardized" %in% names(df)) {
      warning(paste("Skipping dataset", i, "- no country codes found"))
      next
    }
    if(!"year" %in% names(df)) {
      warning(paste("Dataset", i, "missing year column"))
      next
    }
    
    merged_data[[i]] <- df
  }
  
  # DEDUPLICATE HERE:
  merged_data <- lapply(merged_data, function(df) {
    if(all(c("iso3_standardized", "year") %in% names(df))) {
      df %>%
        group_by(iso3_standardized, year) %>%
        slice(1) %>%
        ungroup()
    } else {
      df
    }
  })
  
  # Merge with reduced memory usage
  final_df <- merged_data %>%
    reduce(
      function(x, y) {
        full_join(x, y, by = c("iso3_standardized", "year"))
      },
      .init = tibble(iso3_standardized = character(), year = numeric())
    )
  
  return(final_df)
  
}
    
#importing data function 
import_data <- function(year1, year2) {
  source("years_filter.R")
  new_list_df <- years_filter(start_yr = year1, end_yr = year2)
  return(new_list_df)
}

#Loading Necessary Data 
df_years2.0 <- function(x, y) {
  list_for_dfs <- import_data(x, y)
  processed <- process_datasets(list_for_dfs)
  data <- processed %>% 
    dplyr::mutate(year_fct = as.factor(year)) %>% 
    dplyr::mutate(across(c(income_level, regime_type_2, regime_type_4, regime_type_10, income_spi, region_spi), as.factor),
                  across(c(sci_overall, sci_method, sci_periodicity, sci_source), as.numeric)) %>% 
    dplyr::select(orig_country_name_ds1, iso3_standardized, year, year_fct, income_level, sdg_overall,
                  spi_comp, sci_overall, di_score, di_reg_type_2, log_gdppc, population, gini_score, everything()) %>% 
    rename(country_name = orig_country_name_ds1,
           country_code = iso3_standardized)
  return(data)
}

#testing2.0 <- df_years2.0(2004, 2023)

