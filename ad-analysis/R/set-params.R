# Program Name: set-params.R
# Description:  Set hyperparameters for real data analysis.

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(here)


# Specify Parameters ------------------------------------------------------

# Subset
race_eth_cat <- c('All','HISP','NHW','NHB','NHAPI','NHO')

# Model Parameters
num_tree  <- c(5, 10, 25, 50, 100)
k         <- c(0.1, 0.5, 1)
base      <- c(0.95, 0.5)
power     <- c(2, 3)

update_sigma_mu   <- TRUE
update_s          <- TRUE
update_alpha      <- TRUE
store_lambda      <- TRUE

# MCMC parameters
num_burn    <- 5000
num_thin    <- 5
num_save    <- 1000
num_chains  <- 1

# Compile
params <- crossing(race_eth_cat,
                   num_tree, k, 
                   nesting(base, power),
                   update_sigma_mu,
                   nesting(update_s, update_alpha),
                   store_lambda,
                   num_burn, num_thin, num_save, num_chains) |>
  mutate(key = row_number()) |>
  select(key, everything())


# Export
write_csv(params, here('ad-analysis','Params','params.csv'))
