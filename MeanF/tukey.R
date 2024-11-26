f.calc_tukey <- (
  tukey_biweight_loss <- function(residuals, threshold) {
    u <- residuals / threshold
    s <- threshold^2 / 6
    
    loss <- ifelse(abs(u) <= 1, 
                   s * (1 - (1 - u^2)^3), 
                   s)
    
    return(loss)
  }
)

