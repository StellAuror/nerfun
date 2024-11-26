f.pred_txtstats <- function(actual, predicted, c) {
  residuals <- actual - predicted
  ae <- abs(residuals)
  
  stats <- list(
    "Mean Error" = f.calc_mfamily(ae, "me") |> f.format_metric(),
    "Mean Squared Error" = f.calc_mfamily(ae, "mse") |> f.format_metric(),
    "Standard Dev. Absolute Error" = sd(ae, na.rm = TRUE) |> f.format_metric(),
    "Mean Absolute Error" = f.calc_mfamily(ae, "mae") |> f.format_metric(),
    "Root Mean Squared Error" = f.calc_mfamily(ae, "rmse") |> f.format_metric(),
    "Mean Absolute %age Error" = f.calc_mfamily(ae, "mape", actual) |> f.format_metric("%", r = 0),
    "Huber Loss" = f.calc_huber(residuals, c) |> mean(na.rm = TRUE) |> f.format_metric(),
    "Tukey Loss" = f.calc_tukey(residuals, c) |> mean(na.rm = TRUE) |> f.format_metric()
  )
  
  f.get_color <- function(name) {
    if (name %in% c("Mean Absolute Error", "Standard Dev. Absolute Error")) {
      "color: #2585f4;" 
    } else {
      "color: black;" 
    }
  }
  
  lapply(names(stats), function(name) {
    list(
      shiny::tags$dt(class = "col-sm-6", style = f.get_color(name), name),
      shiny::tags$dd(class = "col-sm-6", style = f.get_color(name), stats[[name]])
    )
  }) |> unlist(recursive = FALSE)
}
