gdppc_df <- read_csv("~/Desktop/OneDrive/OneDrive/QMSS Thesis/Datasets/gdppc.csv")

gdppc_df <- gdppc_df %>%  #pivot back to long format
  pivot_longer(3:66, #use any way to select all columns (he used range)
               names_to = "year", # new var using cols 2:13 as values
               values_to = "gdp_pc") #new var containing all values within cols 2:13 as values 
View(gdppc_df)

write.table(gdppc_df, file = "gdppc_df_long.csv", row.names=F, sep = ",") 