# Program Name: run-analysis-short.R
# Description:  Fit CL-BART models to simulated application data.
# This script is serves as a demo of the real data analysis. For the paper,
# the dataset was much larger and more MCMC iterations were used, so it was
# not practical to run locally.The fake data do not include a signal since
# the cases were randomly allocated, so the figures and tables are not as
# informative as in the paper! This script just serves as an example for
# how to perform the analyses described in the manuscript.

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(here)
library(clbart)
library(splines)
library(pdpd)
library(rpart)


# Run shortened analysis --------------------------------------------------

# Identify parameter settings used in the text
# - M = 25 for stratified analysis, M = 100 for pooled analysis
params <- read_csv(here('ad-analysis','Params','params.csv'))
keys <- filter(params, 
               (race_eth_cat != 'All' & k == 1 & base == 0.95 & update_s & num_tree == 25) |
                 (race_eth_cat == 'All' & k == 1 & base == 0.95 & update_s & num_tree == 100))$key

# Loop through keys
race_eth_cat <- params$race_eth_cat[keys]
results <- list()
for (i in 1:length(keys)) {
  
  print(paste('Start', race_eth_cat[i]))
  
  # Read in cleaned case-crossover dataset and filter based on race/ethnicity
  cco <- read_csv(here('ad-analysis','Data','Clean','cco.csv'), show_col_types = FALSE)
  
  # Define moderators
  if (race_eth_cat[i] != 'All') {
    
    # Filter based on race/ethnicity
    cco <- filter(cco, RACE_ETH == race_eth_cat[i])
    
    # Don't include race/ethnicity indicators among moderators
    w <- cco |>
      select(FEMALE, AGE, 
             CKD, COPD, DEPRESSION, HT, CHF, DIAB, HYPERLIP)
    
  } else {
    
    # Do include race/ethnicity indicators among moderators as one-hot
    # encoded predictors
    w <- cco |>
      mutate(value = 1) |>
      pivot_wider(names_from = RACE_ETH, values_from = value, values_fill = 0) |>
      select(FEMALE, AGE, HISP, NHAPI, NHB, NHO, NHW,
             CKD, COPD, DEPRESSION, HT, CHF, DIAB, HYPERLIP)
    
  }
  
  # Create confounder model matrix
  x <- cbind(cco$HOLIDAY,
             splines::ns(cco$DP3DMA, 4),
             splines::ns(cco$AVG3DMA, 4))
  colnames(x) <- c('HOLIDAY', paste0('DP3DMAns', 1:4), paste0('AVG3DMAns', 1:4))
  
  # Outcome, primary exposure, and strata
  y <- cco$OUTCOME
  z <- cco$AVGHW
  strata <- match(cco$ID, unique(cco$ID))
  
  # Set options and hyperparameters
  opts <- Opts(num_burn = 1000,
               num_save = 1000,
               num_thin = 1,
               update_sigma_mu = TRUE, 
               update_s = TRUE, update_alpha = TRUE,
               store_lambda = FALSE,
               seed = 1)
  
  # Using 5 trees here to make model run faster...
  hypers <- Hypers(num_tree = 5, base = 0.95, power = 2, k = 1)
  
  # Fit model
  fit <- clbart(w, x, y, z, strata, hypers, opts)
  
  # Marginal partial dependence
  print(paste('Computing marginal pd for:', race_eth_cat[i]))
  f_hat <- function (w) predict(fit, list(w = w), posterior = TRUE)
  f_hat_exp <- function (w) exp(predict(fit, list(w = w), posterior = TRUE))
  firsts <- match(unique(strata), strata)
  w <- w[firsts,]
  
  # Don't compute for AGE since it is not binary
  bin_vars <- setdiff(colnames(w), c('AGE'))
  
  marg_pd <- lapply(bin_vars, \(v) bayes_pd(w, f_hat_exp, vars = v, k = 2)) |>
    lapply(\(x) {
      x$var <- colnames(x)[1]
      colnames(x)[1] <- 'x'
      x
    }) |>
    bind_rows() |>
    mutate(key = keys[i])
  
  # Marginal effects via partial dependence
  pd_diff <- function (x, f_hat, var, fun = I) {
    x <- as.data.frame(x)
    
    x0 <- x1 <- x
    
    x0[[var]] <- 0
    x1[[var]] <- 1
    
    yhat0 <- f_hat(x0)
    yhat1 <- f_hat(x1)
    
    delta_avg <- colMeans(fun(f_hat(x1) - f_hat(x0)))
    
    est <- mean(delta_avg)
    lcl <- quantile(delta_avg, 0.025)
    ucl <- quantile(delta_avg, 0.975)
    
    return(data.frame(var, est, lcl, ucl))
  }
  
  # Don't compute marginal effect for AGE since it is not binary
  print(paste('Computing marginal pd diff for:', race_eth_cat[i]))
  marg_pd_diff <- lapply(bin_vars, \(v) pd_diff(w, f_hat, v, exp)) |>
    bind_rows() |>
    mutate(key = keys[i])
  
  # Lower dimensional CART summary
  print(paste('Computing CART pd for:', race_eth_cat[i]))
  # Create CART data
  cart_data <- data.frame(w, tau = fit$lambda_est[firsts]) |>
    select(CKD:HYPERLIP, tau) |>
    mutate(across(CKD:HYPERLIP, \(x) factor(x, levels = 0:1, labels = c('N','Y'))))
  
  # Fit CART model
  cart <- rpart(tau ~ ., data = cart_data, control = rpart.control(maxdepth = 3))
  
  # Compute R-squared for lower-dimensional summary
  cart_R2 <- 1 - (sum((cart_data$tau - predict(cart))^2) / sum((cart_data$tau - mean(cart_data$tau))^2))
  
  # Group observations by CART leaf node
  cart_groups <- cbind(cart_data, node = as.numeric(row.names(cart$frame)[cart$where])) |>
    mutate(across(CKD:HYPERLIP, \(x) as.numeric(x) - 1)) |>
    arrange(node) |>
    select(-tau) |>
    group_by(node) |>
    group_split(.keep = FALSE) |>
    lapply(\(df) unique(Filter(\(col) length(unique(col)) == 1, df)))
  
  # Compute partial dependence within each leaf node
  # Marginal effects via partial dependence
  pd_cart <- function (x, f_hat, vars, vals, fun = I) {
    x <- as.data.frame(x)
    
    for (i in 1:length(vars)) x[[vars[i]]] <- vals[i]
    
    yhat <- fun(f_hat(x))
    
    yhat_avg <- colMeans(yhat)
    
    est <- mean(yhat_avg)
    lcl <- stats::quantile(yhat_avg, 0.025)
    ucl <- stats::quantile(yhat_avg, 0.975)
    Pr <- mean(yhat_avg > fun(0))
    
    df <- matrix(c(vals, est, lcl, ucl, Pr), nrow = 1)
    colnames(df) <- c(vars, 'est','lcl','ucl','Pr')
    
    return(data.frame(df))
  }
  cart_pd <- lapply(cart_groups, \(g) pd_cart(w, f_hat, names(g), as.numeric(g), exp)) |>
    bind_rows()
  cart_pd$node <- as.numeric(names(table(as.numeric(row.names(cart$frame)[cart$where]))))
  cart_pd$R2 <- cart_R2
  cart_pd$key <- keys[i]
  
  # Reorder columns
  fixed_cols <- c('est','lcl','ucl','Pr','node','R2','key')
  cart_pd <- cart_pd[,c(setdiff(names(cart_pd), fixed_cols), fixed_cols)]
  
  
  # Output Results ----------------------------------------------------------
  
  # Compile and return results
  fit$race_eth_cat <- race_eth_cat[i]
  results[[keys[i]]] <- list(key = keys[i], fit = fit,
                             marg_pd = marg_pd, 
                             marg_pd_diff = marg_pd_diff,
                             cart = cart, 
                             cart_vars = setdiff(colnames(cart_pd), fixed_cols), 
                             cart_pd = cart_pd)
}

saveRDS(results, here('ad-analysis','Results','ad-results.rds'))
