f.calc_mfamily <- function(ae, f, actual = NULL) {
  switch(f,
         mse = mean(ae^2, na.rm = TRUE),
         rmse = sqrt(mean(ae^2, na.rm = TRUE)),
         mae = mean(abs(ae), na.rm = TRUE),
         me = mean(ae, na.rm = TRUE),
         mape = if (!is.null(actual)) mean(abs(ae / (actual + 1e-10)) * 100, na.rm = TRUE) else NA,
         stop("Unknown function: ", f)
  )
}
