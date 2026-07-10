# =============================================================================
# Signals of famine: citizen science tracking of adult weight in 
# the Gaza Strip, 2025
# =============================================================================

# =============================================================================
# R script to call other scripts, clean data and generate analysis
# =============================================================================

# Set project directory
dir_path <- paste(dirname(rstudioapi::getActiveDocumentContext()$path  )
  , "/", sep = "")
dir_path <- gsub("/R/", "/", dir_path)
setwd(dir_path)

# Pre-process datasets
source(paste0(dir_path, "R/clean.R"))

# Visualise data
source(paste0(dir_path, "R/visualise.R"))



