### Load primary packages
pacman::p_load(
  tidyverse,
  rstudioapi,
  viridis,
  gghalves
)

### Loading Local Scripts

# Function to get the script's path
f.script.path <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  match <- grep(file_arg, cmdArgs)
  if (length(match) > 0) {
    # Script is run with Rscript
    return(normalizePath(sub(file_arg, "", cmdArgs[match])))
  } else if (!is.null(sys.frames()[[1]]$ofile)) {
    # Script is sourced via R console
    return(normalizePath(sys.frames()[[1]]$ofile))
  } else {
    # RStudio or interactive environment
    return(normalizePath(rstudioapi::getActiveDocumentContext()$path))
  }
}

# Loading .R scripts (local makes var declarations temporary)
local({
  # Directory of the current script
  dp <- dirname(f.script.path())
  
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
