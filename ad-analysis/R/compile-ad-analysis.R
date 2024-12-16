# Program Name: compile-ad-analysis.R
# Description:  Compiles results into one smaller file

# Load Packages -----------------------------------------------------------
library(here)
library(tidyverse)


# Compile files in the temp folder ----------------------------------------

# Read all result files
files <- list.files(here('ad-analysis','Results','temp'), full.names = TRUE)

# Drop memory-heavy elements when compiling into one file
results <- list()
for (i in 1:length(files)) {
  fit <- readRDS(files[i])
  fit$fit$lambda <- NULL
  # fit$fit$forest <- NULL
  # fit$fit$split_probs <- NULL
  # fit$fit$split_counts <- NULL
  results[[i]] <- fit
}

# Export ------------------------------------------------------------------

# Write compiled (and condensed) results to dated file
saveRDS(results, here('ad-analysis','Results','ad-results.rds'))
