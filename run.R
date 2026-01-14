
# Script to set up and run the Shiny app
# This script will:
# 1. Install renv if not already installed
# 2. Restore all required packages from renv.lock
# 3. Launch the Shiny app

# Function to print messages with timestamp
log_message <- function(msg) {
  cat(paste0("[", Sys.time(), "] ", msg, "\n"))
}

# Check if renv is installed, if not install it
log_message("Checking for renv package...")
if (!requireNamespace("renv", quietly = TRUE)) {
  log_message("renv not found. Installing renv...")
  install.packages("renv")
} else {
  log_message("renv is already installed.")
}

# Load renv
library(renv)

# Check if renv.lock exists
if (!file.exists("renv.lock")) {
  stop("Error: renv.lock file not found in the current directory. 
       Please make sure you are running this script from the app's root directory.")
}

# Restore packages from renv.lock
log_message("Restoring packages from renv.lock...")
log_message("This may take several minutes on first run...")
renv::restore(prompt = FALSE)

log_message("Package restoration complete!")

# Load the Golem app package
log_message("Loading the app...")

# Get the package name from DESCRIPTION file

devtools::load_all()
run_app()
