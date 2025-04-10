# Rough work 



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
