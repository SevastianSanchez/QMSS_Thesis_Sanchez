# Rough work 

testing_df <- df_years(yr1 = 2005)

vdemdata::find_var("gdp") #finds variables based on key words 


region_info <- vdemdata::var_info("e_regionpol_6C") #info on variables 
region_info$responses

#backsliding:
dem_breakdown_info <- vdemdata::var_info("e_democracy_breakdowns")

#Democratization:
dem_transition_info <- vdemdata::var_info("e_democracy_trans")

