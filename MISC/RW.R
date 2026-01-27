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



