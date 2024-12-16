# Program Name: compile-cart-sim.R
# Description:  Compiles results into a single file

# Load Packages -----------------------------------------------------------
library(here)


# Compile Results ---------------------------------------------------------

# Read all result files
files <- list.files(here('cart-sim','Results','temp'), full.names = TRUE)
results <- lapply(files, readRDS)

# Export compiled results as one file
saveRDS(results, here('cart-sim','Results','cart-sim-results.rds'))