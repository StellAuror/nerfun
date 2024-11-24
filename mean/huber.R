f.calc_huber <- (
  huber_loss <- function(residuals, threshold) {
    loss <- ifelse(abs(residuals) <= threshold,
                   0.5 * residuals^2,
                   threshold * (abs(residuals) - 0.5 * threshold))
    return(loss)
  }
)

