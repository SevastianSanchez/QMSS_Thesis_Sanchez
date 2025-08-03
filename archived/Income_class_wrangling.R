library(tidyverse)
library(readxl)

# Read WDI classification sheet
income_data <- read_excel("data/WDI_IncomeClass_Hist.xlsx", 
                          sheet = "Country Analytical History", 
                          skip = 5) # Skip the header rows

# Clean 
income_data_clean <- income_data %>%
  # Remove rows with NA in the first column (these are notes at the bottom)
  filter(!is.na(.[[1]])) %>%
  # Select only the country code column and the data columns (FY89-FY25)
  select(1, 3:39)

# Rename first column to "country_code"
colnames(income_data_clean)[1] <- "country_code"

# Extract the years from column names
years <- as.numeric(str_extract(colnames(income_data_clean)[2:38], "\\d{4}"))
colnames(income_data_clean)[2:38] <- years

# Convert to long format
income_data_long <- income_data_clean %>%
  pivot_longer(
    cols = -country_code,
    names_to = "year",
    values_to = "income_level"
  ) %>%
  # Convert year to numeric
  mutate(year = as.numeric(year)) %>%
  # Arrange by country and year
  arrange(country_code, year)

# View the first few rows
head(income_data_long)

# Save to CSV if needed
write.csv(income_data_long, "data/world_bank_income_classifications.csv", row.names = FALSE)
