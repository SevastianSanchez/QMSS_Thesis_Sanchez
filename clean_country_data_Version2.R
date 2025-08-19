# Script to clean the country dataset by removing redundant columns and standardizing variable names

# Load the data
data <- read.csv("testing_df_years2.0.csv", stringsAsFactors = FALSE)

# 1. Identify all country name/code columns to remove (except the main ones)
country_pattern <- "country_name_|country_code_|country_text_id_|iso3c_|original_country_name"
country_cols <- grep(country_pattern, names(data), value = TRUE)
# Keep only the main country_name and country_code columns
country_cols_to_remove <- country_cols[!country_cols %in% c("country_name", "country_code")]

# 2. Get the base variable names by removing _ds# suffix pattern
base_names <- gsub("_ds[0-9]+$", "", names(data))
unique_base_names <- unique(base_names)

# 3. For each unique base name, decide which column to keep
cols_to_keep <- c("country_name", "country_code", "year", "year_fct")
for (base_name in unique_base_names) {
  # Skip country columns we already decided on
  if (base_name %in% c("country_name", "country_code", "year", "year_fct") || 
      base_name %in% gsub("_ds[0-9]+$", "", country_cols_to_remove)) {
    next
  }
  
  # Find all columns with this base name
  cols_with_base <- names(data)[base_names == base_name]
  
  # If there's a column with the exact base name, keep that one
  if (base_name %in% cols_with_base) {
    cols_to_keep <- c(cols_to_keep, base_name)
  } else {
    # Otherwise keep the first one with a suffix
    cols_to_keep <- c(cols_to_keep, cols_with_base[1])
  }
}

# 4. Keep only the selected columns
data_cleaned <- data[, cols_to_keep]

# 5. Rename columns to remove _ds# suffix
names(data_cleaned) <- gsub("_ds[0-9]+$", "", names(data_cleaned))

# 6. Write the cleaned data to a new CSV
write.csv(data_cleaned, "cleaned_country_data.csv", row.names = FALSE)

# Print summary of what was done
cat("Cleaning complete:\n")
cat("- Original columns:", ncol(data), "\n")
cat("- Cleaned columns:", ncol(data_cleaned), "\n")
cat("- Columns removed:", ncol(data) - ncol(data_cleaned), "\n")
cat("- Output saved to cleaned_country_data.csv\n")