## P-value function for DFs 

# Defining function for significance stars based on p-values
sig_stars <- function(p_value) {
  if (p_value <= 0.001) {
    return("***")
  } else if (p_value <= 0.01) {
    return("**")
  } else if (p_value <= 0.05) {
    return("*")
  } else if (p_value <= 0.1) {
    return(".")
  } else {
    return("")
  }
}

