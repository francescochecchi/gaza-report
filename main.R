# =============================================================================
# Signals of famine: citizen science tracking of adult weight in 
# the Gaza Strip, 2025
# =============================================================================

# =============================================================================
# R script to call other scripts, clean data and generate analysis
# =============================================================================

pacman::p_load(here)


# Pre-process datasets
source(here::here("R", "clean.R"))

# Visualise data
source(here::here("R", "visualise.R"))



