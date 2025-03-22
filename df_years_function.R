
#function to extract data from specified years 
df_years <- function(name, df1=vdem, yr1=1999, #vdem data ONLY
                     df2=spi, yr2=yr1, #spi data ONLY
                     df3=sdg, yr3=yr1) #sdg data ONLY 
  { 
  
  #VDEM DATASET
  
  name1 <- df1 %>%
    dplyr::select(country_name, country_text_id, year, v2x_regime, v2x_regime_amb, v2x_polyarchy, v2x_libdem, v2x_partipdem, v2x_delibdem, v2x_egaldem, v2xel_frefair, v2x_accountability, v2x_veracc, v2x_horacc, v2x_diagacc, v2xca_academ, v2x_freexp_altinf, e_gdp, e_gdppc, e_wb_pop, v3ststybcov, v3ststybpub, v3stcensus, v3ststatag) %>%
    filter(year > yr1) %>% #Only 1999 and up 
    rename(country_code = country_text_id, #renaming country code (new_name = old_name)
           regime_type = v2x_regime, 
           regime_type_amb = v2x_regime_amb,
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
    filter(year > yr2)
  
  #SDG DATASET
  
  name3 <- df3 %>% 
    dplyr::select("country_name", "country_code", "year", "sdg_overall", "goal1", "goal2", "goal3", "goal4", "goal5", "goal6", "goal7", "goal8", "goal9", "goal10", "goal11", "goal12", "goal3", "goal14", "goal15", "goal16", "goal17") %>% 
    filter(year > yr3)
   
  #merge spi, sci, sdg and vdem data 
  namex <- left_join(name3, name2, by = c("year", "country_code")) 
  namex <- left_join(namex, name1, by = c("country_code", "year"))
  
  #rm duplicate col names 
  colnames(namex) <- make.unique(colnames(namex)) # Make column names unique
  
  #data type changes 
  namex$year <- as.integer(namex$year)
  name <- namex %>% 
    mutate_at(c("spi_comp", "p1_use", "p2_services", "p3_products", "p4_sources", "p5_infra"), as.numeric)
  
  return(name)
}

df_name <- df_years(df, yr1 = 1999)

