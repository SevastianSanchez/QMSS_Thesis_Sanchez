
#function to extract data from specified years 
df_years_test <- function(df1=vdem, yr1=2000, #vdem data ONLY
                     df2=spi, spi_yr=yr1, #spi data ONLY
                     df3=sdg, sdg_yr=yr1, #sdg data ONLY
                     df4=sci_df, sci_yr=yr1, #sci data ONLY
                     df5=ert, ert_yr=yr1, #ert data ONLY
                     df6=info_cap, info_cap_yr=yr1, #info_cap data ONLY
                     df7=gdppc_dta, gdppc_dta=yr1, #gdppc_dta data ONLY
                     df8=odin, odin_yr=yr1){ #odin data ONLY
  
  #VDEM DATASET
  
  name1 <- df1 %>%
    dplyr::select(country_name, country_text_id, year, v2x_regime, v2x_regime_amb, v2x_polyarchy, 
                  v2x_libdem, v2x_partipdem, v2x_delibdem, v2x_egaldem, v2xel_frefair, v2x_accountability, 
                  v2x_veracc, v2x_horacc, v2x_diagacc, v2xca_academ, v2x_freexp_altinf, e_gdp, e_gdppc, 
                  e_wb_pop, v3ststybcov, v3ststybpub, v3stcensus, v3ststatag) %>%
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
           gdp = e_gdp,
           gdp_pc = e_gdppc,
           population = e_wb_pop,
           stat_agency = v3ststatag,
           stat_yb_cov = v3ststybcov,
           stat_yb_pub = v3ststybpub,
           census = v3stcensus)
  
  #SPI DATASET
  name2 <- df2 %>% 
    select(country, iso3c, date, SPI.INDEX, SPI.INDEX.PIL1, SPI.INDEX.PIL2, SPI.INDEX.PIL3, SPI.INDEX.PIL4, SPI.INDEX.PIL5) %>% 
    rename(country_name = country) %>% 
    rename(country_code = iso3c) %>% 
    rename(year = date) %>% 
    rename(spi_comp = SPI.INDEX) %>% #SPI composite score, average of p1-5
    rename(p1_use = SPI.INDEX.PIL1) %>% #SPI pillar 1, Data use 
    rename(p2_services = SPI.INDEX.PIL2) %>% #SPI pillar, 2 Data services 
    rename(p3_products = SPI.INDEX.PIL3) %>% #SPI pillar, 3 Data products  
    rename(p4_sources = SPI.INDEX.PIL4) %>% #SPI pillar, 4 Data sources 
    rename(p5_infra = SPI.INDEX.PIL5) %>% #SPI pillar 5, Data infrastructure
    filter(year >= spi_yr)
  
  #SDG DATASET
  name3 <- df3 %>% 
    dplyr::select("country_name", "country_code", "year", "sdg_overall", "goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal3", "goal14", "goal15", "goal16", "goal17") %>% 
    filter(year >= sdg_yr)
  
  #SCI DATASET
  name4 <- df4 %>% 
    dplyr::select(country_name, country_code, Year, IQ.SCI.OVRL, IQ.SCI.MTHD, IQ.SCI.PRDC, IQ.SCI.SRCE) %>% 
    filter(Year >= sci_yr) %>% 
    rename(year = Year,
           sci_overall = IQ.SCI.OVRL, 
           sci_method = IQ.SCI.MTHD, 
           sci_periodicity = IQ.SCI.PRDC,
           sci_source = IQ.SCI.SRCE)
  
  name4$year <- as.numeric(name4$year)
  
  #ERT DATASET 
  name5 <- df5 %>%
    dplyr::select(country_name, country_text_id, year, reg_type, v2x_polyarchy, 
                  row_regch_event, reg_trans, dem_ep, dem_pre_ep_year, dem_ep_start_year, 
                  dem_ep_end_year, aut_ep, aut_pre_ep_year, aut_ep_start_year, aut_ep_end_year, everything()) %>%
    filter(year >= ert_yr) %>% #default only 1999 and up 
    rename(country_code = country_text_id, #renaming country code (new_name = old_name)
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
  
  #MERGING spi, sci, sdg and vdem data 
  namex <- left_join(name3, name2, by = c("year", "country_code")) 
  namex <- left_join(namex, name4, by = c("country_code", "year"))
  namex <- left_join(namex, name1, by = c("country_code", "year"))
  namex <- left_join(namex, name5, by = c("country_code", "year"))
  
  #rm duplicate col names 
  colnames(namex) <- make.unique(colnames(namex)) # Make column names unique
  
  #data type changes 
  namex$year <- as.integer(namex$year)
  name <- namex %>% 
    dplyr::mutate_at(c("spi_comp", "p1_use", "p2_services", "p3_products", "p4_sources", "p5_infra", "sci_overall", "sci_method", "sci_periodicity", "sci_source"), as.numeric)
  
  return(name)
}
testingg <- df_years_test()
