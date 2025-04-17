library(tidyverse)
source("data/data_sources.R")

#function to extract data from specified years 
df_years <- function(df1=vdem, yr1=2000, #vdem data ONLY
                     df2=spi, spi_yr=yr1, #spi data ONLY
                     df3=sdg, sdg_yr=yr1, #sdg data ONLY
                     df4=sci_df, sci_yr=yr1, #sci data ONLY
                     df5=ert, ert_yr=yr1, #ert data ONLY
                     df6=gdppc_df, gdppc_yr=yr1, #, #gdppc_dta data ONLY
                     df7=info_cap, info_cap_yr=yr1, #info_cap data ONLY
                     df8=gni_class, gni_yr=yr1, #gni_class data ONLY
                     df9=di, di_yr=yr1, 
                     df10 = gini, gini_yr=yr1 # di data ONLY 
                     #df10=odin, odin_yr=yr1 #odin data ONLY
                     ){ 
  
  #VDEM DATASET
  
  name1 <- df1 %>%
    dplyr::select(country_name, country_text_id, year, v2x_regime, v2x_regime_amb, v2x_polyarchy, 
                  v2x_libdem, v2x_partipdem, v2x_delibdem, v2x_egaldem, v2xel_frefair, v2x_accountability, 
                  v2x_veracc, v2x_horacc, v2x_diagacc, v2xca_academ, v2x_freexp_altinf, 
                  e_wb_pop) %>%
    filter(year >= yr1) %>% #Only 1999 and up 
    rename(country_code = country_text_id, #renaming country code (new_name = old_name)
           regime_type_4 = v2x_regime, 
           regime_type_10 = v2x_regime_amb,
           elect_dem = v2x_polyarchy,
           lib_dem = v2x_libdem,
           part_dem = v2x_partipdem,
           delib_dem = v2x_delibdem,
           egal_dem = v2x_egaldem,
           freefair = v2xel_frefair,
           #freefair_ord = e_v2xel_frefair,
           accountability = v2x_accountability,
           vt_account = v2x_veracc,
           hz_account = v2x_horacc,
           diag_account = v2x_diagacc,
           academ_free = v2xca_academ,
           freexp_altinfo = v2x_freexp_altinf,
           population = e_wb_pop)
  
  #SPI DATASET
  name2 <- df2 %>% 
    dplyr::select(iso3c, date, SPI.INDEX, SPI.INDEX.PIL1, SPI.INDEX.PIL2, SPI.INDEX.PIL3, 
                  SPI.INDEX.PIL4, SPI.INDEX.PIL5, income, region, weights) %>% 
    rename(country_code = iso3c) %>% 
    rename(year = date) %>% 
    rename(spi_comp = SPI.INDEX) %>% #SPI composite score, average of p1-5
    rename(p1_use = SPI.INDEX.PIL1) %>% #SPI pillar 1, Data use 
    rename(p2_services = SPI.INDEX.PIL2) %>% #SPI pillar, 2 Data services 
    rename(p3_products = SPI.INDEX.PIL3) %>% #SPI pillar, 3 Data products  
    rename(p4_sources = SPI.INDEX.PIL4) %>% #SPI pillar, 4 Data sources 
    rename(p5_infra = SPI.INDEX.PIL5) %>% #SPI pillar 5, Data infrastructure
    rename(income_spi = income) %>%
    rename(region_spi = region) %>%
    rename(weights_spi = weights) %>%
    filter(year >= spi_yr)
  
  #SDG DATASET
  name3 <- df3 %>% 
    dplyr::select(country_name, country_code, year, sdg_overall, goal1, goal2, goal3, goal4, goal5, goal6, goal7, goal8, goal9, goal10, goal11, goal12, goal13, goal14, goal15, goal16, goal17) %>% 
    filter(year >= sdg_yr)
  
  #SCI DATASET
  name4 <- df4 %>% 
    dplyr::select(country_code, Year, IQ.SCI.OVRL, IQ.SCI.MTHD, IQ.SCI.PRDC, IQ.SCI.SRCE) %>% 
    filter(Year >= sci_yr) %>% 
    rename(year = Year,
           sci_overall = IQ.SCI.OVRL, 
           sci_method = IQ.SCI.MTHD, 
           sci_periodicity = IQ.SCI.PRDC,
           sci_source = IQ.SCI.SRCE)
  
  name4$year <- as.numeric(name4$year)
  
  #ERT DATASET 
  name5 <- df5 %>%
    dplyr::select(country_name, country_id, country_text_id, year, reg_type, v2x_polyarchy, 
                  row_regch_event, reg_trans, dem_ep, dem_pre_ep_year, dem_ep_start_year, 
                  dem_ep_end_year, aut_ep, aut_pre_ep_year, aut_ep_start_year, aut_ep_end_year) %>%
    filter(year >= ert_yr) %>% 
    rename(country_code = country_text_id, 
           regime_type_2 = reg_type,
           elect_dem_ert = v2x_polyarchy,
           regch_event = row_regch_event,
           regch_genuine = reg_trans,
           dem_ep_pre_yr = dem_pre_ep_year,
           dem_ep_start_yr = dem_ep_start_year,
           dem_ep_end_yr = dem_ep_end_year,
           aut_ep_pre_yr = aut_pre_ep_year,
           aut_ep_start_yr = aut_ep_start_year,
           aut_ep_end_yr = aut_ep_end_year)
  
  #GDP Per Capita
  name6 <- df6 %>% 
    dplyr::select(country_code, year, gdp_pc) %>% 
    rename(gdppc = gdp_pc) %>% 
    filter(year >= gdppc_yr)
   
  name6$year <- as.numeric(name6$year) 
  
  #INFO CAPACITY
  name7 <- df7 %>% 
    dplyr::select(country_id, year, infcap_irt, infcap_pca, everything()) %>% 
    filter(year >= info_cap_yr)
  
 #WB Income Classifications 
  name8 <- df8 %>% 
    dplyr::mutate(income_level = na_if(income_level, "..")) %>% 
    dplyr::mutate(income_level_lab = factor(income_level, 
      levels = c("H", "UM", "LM", "L"),  # Desired order
      labels = c("High Income Countries", 
                 "Upper-Middle Income Countries", 
                 "Lower-Middle Income Countries", 
                 "Low Income Countries")  # Full descriptive labels
    )) %>% 
    filter(year >= gni_yr)
  
  #EIU Democracy Index  
  name9 <- df9 %>% 
    filter(year >= di_yr)
  
  #GINI Coefficient 
  name10 <- df10 %>% 
    dplyr::select(Entity, Code, Year, gini) %>% 
    rename(country_code = Code, 
           year = Year,
           country_name = Entity) %>% 
    dplyr::filter(year >= gini_yr)
  
  #MERGING spi, sci, sdg, vdem data 
  namex <- left_join(name3, name2, by = c("year", "country_code")) 
  namex <- left_join(namex, name4, by = c("country_code", "year"))
  namex <- left_join(namex, name1, by = c("country_code", "year"))
  namex <- left_join(namex, name5, by = c("country_code", "year"))
  namex <- left_join(namex, name6, by = c("country_code", "year"))
  namex <- left_join(namex, name7, by = c("country_id", "year"))
  namex <- left_join(namex, name8, by = c("country_code", "year"))
  namex <- left_join(namex, name9, by = c("country_code", "year"))
  namex <- left_join(namex, name10, by = c("country_code", "year"))
  
  #rm duplicate col names 
  colnames(namex) <- make.unique(colnames(namex)) # Make column names unique
  
  #data type changes & reordering 
  namex$year <- as.integer(namex$year)
  name <- namex %>% 
  # Convert specific columns to factor and numeric
    dplyr::mutate(year_fct = as.factor(year)) %>% 
    dplyr::mutate(across(c(income_level, regime_type_2, regime_type_4, regime_type_10, income_spi, region_spi), as.factor), # Convert col3 and col4 to numeric
      across(c(sci_overall, sci_method, sci_periodicity, sci_source), as.numeric)) %>% 
    dplyr::select(country_name.x, country_code, country_id, year, year_fct, income_level, sdg_overall, spi_comp, di_score, everything())
  
  return(name)
}
