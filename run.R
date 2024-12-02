print("run.R Initiated")
 
 ### Load primary packages
pacman::p_load(
  shiny,
  bslib,
  htmlwidgets,
  shinyWidgets,
  tidyverse,
  rstudioapi,
  viridis,
  gghalves,
  plotly
)

### Loading Local Scripts

 ####### DEVELOP A UNICERSAL WAY TO GET THE PATH
 
# Loading .R scripts (local makes var declarations temporary)
local({
  # Directory of the current script
  dp <- dirname("B:\\Coding\\Nerfun\\run.R")
  
  # Find subdirectories
  folders <- list.dirs(path = dp, full.names = TRUE, recursive = FALSE)
  
  # Iterate over each folder to load .R scripts
  for (folder in folders) {
    # List all .R files in the current folder
    scripts <- list.files(path = folder, pattern = "\\.[rR]$", full.names = TRUE)
    if (length(scripts) > 0) {
      # Safely source each script
      for (script in scripts) {
        tryCatch(
          source(script),
          error = function(e) {
            message(sprintf("Error sourcing script %s: %s", script, e$message))
          }
        )
      }
    }
  }
})


 
  # Predict
  ldfRQ <- datasets::airquality |> f.split_data(.4, 1)

  model <- lm(Ozone ~ Solar.R + Wind + Temp, data = ldfRQ$Learn)
  ldfRQ$Test$Prediction <- predict(model, ldfRQ$Test)
