extract_pgrang_results <- function(...) {
  test_results <- list(...)
  test_names <- as.list(substitute(list(...)))[-1L]
  test_names <- sapply(test_names, deparse)
  
  results_df <- data.frame(
    direction = character(),
    cause = character(),
    effect = character(),
    lag_order = integer(),
    zbar = numeric(),
    p_value = character(),
    has_warning = logical(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(test_results)) {
    test <- test_results[[i]]
    obj_name <- test_names[i]
    
    # Extract lag order from object name (look for _order1 or _order2 or trailing 1/2)
    lag_order <- NA
    lag_match <- regmatches(obj_name, regexpr("order[0-9]+", obj_name))
    if (length(lag_match) > 0) {
      lag_order <- as.integer(gsub("order", "", lag_match))
    } else {
      # Try to extract trailing digit
      lag_match2 <- regmatches(obj_name, regexpr("[0-9]+$", obj_name))
      if (length(lag_match2) > 0) {
        lag_order <- as.integer(lag_match2)
      }
    }
    
    # Extract text output
    output <- capture.output(print(test))
    
    # Get variables (from "data:" line)
    data_line <- grep("^data:", output, value = TRUE)
    direction <- effect_var <- cause_var <- "unknown"
    if (length(data_line) > 0) {
      formula_parts <- strsplit(trimws(sub("^data:", "", data_line)), "~")[[1]]
      if (length(formula_parts) >= 2) {
        effect_var <- trimws(formula_parts[1])
        cause_var <- trimws(formula_parts[2])
        direction <- paste(effect_var, "~", cause_var)
      }
    }
    
    # Extract Zbar statistic and p-value
    zbar <- NA
    p_value <- NA
    zbar_line <- grep("^Zbar =", output, value = TRUE)
    if (length(zbar_line) > 0) {
      zbar_parts <- strsplit(zbar_line, ",")[[1]]
      zbar_str <- trimws(sub("^Zbar =", "", zbar_parts[1]))
      zbar <- suppressWarnings(as.numeric(zbar_str))
      if (length(zbar_parts) > 1) {
        p_value <- trimws(sub("^p-value", "", zbar_parts[2]))
      }
    }
    
    # Check for warnings in the output
    has_warning <- any(grepl("perfect fit|warning", paste(output, collapse = " "), ignore.case = TRUE))
    
    # Add to results dataframe
    new_row <- data.frame(
      direction = direction,
      cause = cause_var,
      effect = effect_var,
      lag_order = lag_order,
      zbar = zbar,
      p_value = p_value,
      has_warning = has_warning,
      stringsAsFactors = FALSE
    )
    results_df <- rbind(results_df, new_row)
  }
  return(results_df)
}
