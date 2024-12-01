# Minimizes influce of residuals (optimization)
f.calc_tukey_loss <- function(residuals, threshold) {
  u <- residuals / threshold
  s <- threshold^2 / 6
  
  loss <- ifelse(
    abs(u) <= 1, 
    s * (1 - (1 - u^2)^3), 
    s
  )
  return(loss)
}


# Indicates the level of influence of each observation
f.calc_tukey_weight <- function(residuals, threshold) {
  u <- residuals / threshold
  
  weight <- ifelse(
    abs(u) <= 1, 
    (1 - u^2)^2,
    0
  )
  return(weight)
}

