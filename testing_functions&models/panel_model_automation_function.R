panel_model_automation <- function(
    dep_var,
    indep_vars,
    lag_vars = NULL,
    n_lags = 0,
    poly_vars = NULL,
    poly_degree = NULL,
    model_type = c("pooling", "fd", "within"),
    data = panel_data,
    index = c("country_code", "year"),
    effect = "twoways",
    robust = FALSE,
    cluster = "group",
    vcov_type = "HC1"
) {
  # Helper to add lag terms
  add_lags <- function(vars, n_lags) {
    out <- vars
    if (!is.null(vars) & n_lags > 0) {
      for (var in vars) {
        for (lag in 1:n_lags) {
          out <- c(out, paste0("dplyr::lag(", var, ", n=", lag, ")"))
        }
      }
    }
    out
  }
  
  # Helper to add polynomial terms
  add_polynomials <- function(vars, degree) {
    out <- character(0)
    if (!is.null(vars) & !is.null(degree) & degree > 1) {
      for (var in vars) {
        # Add the linear term
        out <- c(out, var)
        # Add polynomial terms (squared, cubed)
        for (d in 2:degree) {
          if (d == 2) {
            term_suffix <- "_sq"
          } else if (d == 3) {
            term_suffix <- "_cub"
          } else {
            term_suffix <- paste0("_", d)
          }
          # Use I() to properly handle polynomial terms in formulas
          out <- c(out, paste0("I(", var, "^", d, ") # ", var, term_suffix))
        }
      }
    }
    out
  }
  
  # Build formula
  # Start with independent variables not in polynomial specification
  non_poly_vars <- indep_vars[!indep_vars %in% poly_vars]
  
  # Add polynomial terms
  poly_terms <- character(0)
  if (!is.null(poly_vars) && !is.null(poly_degree) && poly_degree > 1) {
    poly_terms <- add_polynomials(poly_vars, poly_degree)
  }
  
  # Add lag terms (excluding polynomial terms that will be handled separately)
  lag_terms <- character(0)
  if (!is.null(lag_vars) && n_lags > 0) {
    lag_vars_no_poly <- lag_vars[!lag_vars %in% poly_vars]
    lag_terms <- add_lags(lag_vars_no_poly, n_lags)
    
    # Handle lagged polynomial terms if a variable needs both lag and polynomial
    if (!is.null(poly_vars) && !is.null(poly_degree) && poly_degree > 1) {
      lag_poly_vars <- lag_vars[lag_vars %in% poly_vars]
      if (length(lag_poly_vars) > 0) {
        for (var in lag_poly_vars) {
          # Add regular lags
          for (lag in 1:n_lags) {
            # Linear lagged term
            lag_terms <- c(lag_terms, paste0("dplyr::lag(", var, ", n=", lag, ")"))
            
            # Polynomial lagged terms
            for (d in 2:poly_degree) {
              if (d == 2) {
                term_suffix <- "_sq"
              } else if (d == 3) {
                term_suffix <- "_cub"
              } else {
                term_suffix <- paste0("_", d)
              }
              lag_terms <- c(lag_terms, 
                             paste0("I(dplyr::lag(", var, ", n=", lag, ")^", d, ") # ", 
                                    var, "_lag", lag, term_suffix))
            }
          }
        }
      }
    }
  }
  
  # Combine all terms
  all_terms <- c(non_poly_vars, poly_terms, lag_terms)
  all_terms <- all_terms[!duplicated(all_terms)]
  
  # Create formula
  formula_str <- paste(dep_var, "~", paste(all_terms, collapse = " + "))
  formula_obj <- as.formula(formula_str)
  
  # Choose model type
  model_type <- match.arg(model_type)
  plm_args <- list(
    formula = formula_obj,
    data = data,
    index = index,
    model = model_type
  )
  
  # If FE, add effect argument
  if (model_type == "within") {
    plm_args$effect <- effect
  }
  
  mod <- do.call(plm::plm, plm_args)
  
  # Return results
  if (robust) {
    # Return model with robust standard errors
    return(list(
      model = mod,
      robust_summary = summary(mod, vcov = vcovHC(mod, cluster = cluster, type = vcov_type)),
      robust_coefs = coeftest(mod, vcov = vcovHC(mod, cluster = cluster, type = vcov_type))
    ))
  } else {
    # Return just the model
    return(mod)
  }
}

# Example: Combined lags AND polynomials
#lag_poly_model <- panel_model_automation(
#  dep_var = "spi_comp",
#  indep_vars = c("di_score", "log_gdppc", "income_level_recoded"),
#  lag_vars = "di_score",   # Lag di_score
#  n_lags = 2,              # Two lags
#  poly_vars = "di_score",  # Also apply polynomial to di_score
#  poly_degree = 2,         # Quadratic
#  model_type = "within"
#)