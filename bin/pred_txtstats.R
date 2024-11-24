f.pred_txtstats <- function(actual, predicted, c) {
  residuals <- actual - predicted
  ae <- abs(residuals)
  paste(
    sep = "\n",
    me = f.calc_mfamily(ae, "me") |> f.format_metric("Me"),
    mse = f.calc_mfamily(ae, "mse") |> f.format_metric("MSe"),
    sdae = sd(ae, na.rm = TRUE) |> f.format_metric("SDae"),
    mae = f.calc_mfamily(ae, "mae") |> f.format_metric("Mae"),
    rmse = f.calc_mfamily(ae, "rmse") |> f.format_metric("RMSe"),
    mape = f.calc_mfamily(ae, "mape", actual) |> f.format_metric("MAPe %", r = 0),
    huber = f.calc_huber(residuals, c) |> mean(na.rm = TRUE) |> f.format_metric("Huber"),
    tukey = f.calc_tukey(residuals, c) |> mean(na.rm = TRUE) |> f.format_metric("Tukey")
  )
}


