# Rough work 

# control/constant 
test_lm1 <- lm(sdg_overall ~ spi_comp + aut_ep, data = event_data)
summary(test_lm1) 

# interaction
test_lm2 <- lm(sdg_overall ~ spi_comp*aut_ep, data = event_data)
summary(test_lm2) 

# same as test_lm2
test_lm3 <- lm(sdg_overall ~ spi_comp + spi_comp*aut_ep, data = event_data)
summary(test_lm3) 

# same as test_lm2
test_lm3 <- lm(sdg_overall ~ spi_comp + aut_ep + spi_comp*aut_ep, data = event_data)
summary(test_lm3) 

# doesn't make sense - perplexity says remove 
test_lm4 <- lm(sdg_overall ~ spi_comp:aut_ep, data = event_data)
summary(test_lm4) 

# spi_comp * aut_ep is the same as spi_comp + aut_ep + spi_comp:aut_ep
test_lm5 <- lm(sdg_overall ~ spi_comp + aut_ep + spi_comp:aut_ep, data = event_data)
summary(test_lm5) 

# Perplexity: When aut_ep is binary (0/1), the coefficient shows the difference between the two groups. 
# This is how binary variables always work in regression—they compare one group to the reference group.
# Yes, it absolutely makes sense to interpret it as SPI's effect in different groups
# That's the whole point of the interaction model! Here's how to read your results:

# multiple categories - controls 
test_ctrl <- lm(sdg_overall ~ spi_comp + income_level, data = merged_2015)
summary(test_ctrl)

# multiple categories - interaction 
test_intr <- lm(sdg_overall ~ spi_comp*income_level, data = merged_2015)
summary(test_intr)

# New df with income classifications 
read_csv("~/Documents/GitHub/QMSS_Thesis_Sanchez/Output_CSVs /world_bank_income_classifications.csv")
merged_inc <- merged %>% 
  left_join(income_class, by = c("country_code", "year"))


merged_inc_test <- merged_inc %>% 
  select(income_spi, income_level, population_spi, population, everything())

testing_df <- df_years(yr1 = 2005)

vdemdata::find_var("gdp") #finds variables based on key words 


#Distribution of all countries that experienced democratization from 1900-2021
hist(ert$year[df$reg_trans==1])
#Distribution of all countries that experienced democratic backsliding from 1900-2021
hist(df$year[df$reg_trans==-1])
#Distribution of all countries that have not experienced change  1900-2021
hist(df$year[df$reg_trans==0])

#frequency/proportion table: instances of backsliding, stability, democratization from 1900-2021
table(df$reg_trans) # frequency
prop.table(table(df$reg_trans)) # proportions 

# Merger varaibles 
vdem = country_text_id, country_name, [COWcode]
spi = iso3c
sdg = country_code, country_name
sci = country_code 
ert = country_text_id, country_name, country_id,
gdppc = country_code
infocap = country_id
income class = country_code
di = country_code
gini = Code

# Validate 

data("codelist", package = "countrycode")
invalid_codes1 <- spi$iso3c[!spi$iso3c %in% codelist$iso3c]

data("codelist")

# Check for ISO3C match
all(vdem$country_text_id %in% codelist$iso3c)

# Check for ISO2C match
all(vdem$country_text_id %in% codelist$iso2c)

# Check for UN numeric match
all(vdem$country_text_id %in% codelist$un)

# Check for ccow numeric match
all(vdem$country_text_id %in% codelist$un)

# COW
all(vdem$country_text_id %in% codelist$cown)




######################################## new cleaning function work 
#Call country code function 
source("data/misc/country_code_merge.R")


file_list <- c(name1, name2, name3, name4, name5, name6, name7, name8, name9, name10)
test_merged <- process_datasets(file_list)

return(test_merged)


#reloading data 
vdem_perp <- testing$vdem
spi_perp <- testing$spi
sdg_perp <- testing$spi
sci_df_perp <- testing$sci_df
ert_perp <- testing$ert
gdppc_df_perp <- testing$sci_df
info_cap_perp <- testing$info_cap
gni_class_perp <- testing$gni_class
di_perp <- testing$di
gini_perp <- testing$gini

return(list(vdem = name1, spi = name2, sdg = name3, 
            sci_df = name4, ert = name5, gdppc_df = name6, 
            info_cap = name7, gni_class = name8, di = name9, 
            gini = name10))




standardize_country_codes <- function(df) {
  # [Existing code] Identify country columns
  country_cols <- names(df)[tolower(names(df)) %in% 
                              tolower(country_colnames)]
  
  if(length(country_cols) > 0) {
    for(col in country_cols) {
      try({
        # Modified code with custom matching
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
          custom_match = c(
            "Timor-Leste" = "TLS",
            "Congo (Kinshasa)" = "COD",
            "Myanmar (Burma)" = "MMR",
            "Iran, Islamic Rep." = "IRN",  # Based on your GDP data
            "Kyrgyz Republic" = "KGZ",      # From your GDP data
            "St. Martin French part" = "MAF",
            "Kosovo" = "XKX",
            "Somaliland" = "SOL",  # Not ISO-standard but needed for your data
            "Zanzibar" = "ZNZ",    # Not ISO-standard but needed
            "Channel Islands" = "CHI",
            "West Bank and Gaza" = "PSE",
            "Curaçao" = "CUW"
          )
        )
        break
      }, silent = TRUE)
    }
  }
  
  if(!"iso3_standardized" %in% names(df)) {
    warning("No suitable country code column found in dataset")
  }
  
  return(df)
}



test_copyfunc <- df_years_test()
#rm duplicate col names 
#colnames(namex) <- make.unique(colnames(namex)) # Make column names unique

#data type changes & reordering 
namex$year <- as.integer(namex$year)
name <- namex %>% 
  # Convert specific columns to factor and numeric
  dplyr::mutate(year_fct = as.factor(year)) %>% 
  dplyr::mutate(across(c(income_level, regime_type_2, regime_type_4, regime_type_10, income_spi, region_spi), as.factor), # Convert col3 and col4 to numeric
                across(c(sci_overall, sci_method, sci_periodicity, sci_source), as.numeric)) %>% 
  dplyr::select(country_name.x, country_code, country_id, year, year_fct, income_level, sdg_overall, spi_comp, di_score, everything())
