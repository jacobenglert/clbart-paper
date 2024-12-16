# Program Name: run-ad-analysis.R
# Description:  Fit CL-BART models to AD data using heatwave as exposure

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(here)
library(splines)
library(clbart)
library(pdpd)
library(rpart)


# Get Parameters ----------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
key <- as.numeric(args[1])
params <- read_csv(here('ad-analysis','Params','params.csv'))
for (param in colnames(params)) {
  assign(param, unlist(params[key, param, drop = TRUE]))
}


# Load and Format Data ----------------------------------------------------

# Read in cleaned case-crossover dataset
cco <- na.omit(read_csv(here('ad-analysis','Data','Clean','cco.csv')))

# Define moderators
if (race_eth_cat != 'All') {
  
  # Filter based on race/ethnicity
  cco <- filter(cco, RACE_ETH == race_eth_cat)
  
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
x <- cbind(cco$HOLIDAY, ns(cco$DP3DMA, 4), ns(cco$AVG3DMA, 4))
colnames(x) <- c('HOLIDAY', paste0('DP3DMAns', 1:4), paste0('AVG3DMAns', 1:4))


# Outcome, primary exposure, and strata
y <- cco$OUTCOME
z <- cco$AVGHW
strata <- match(cco$ID, unique(cco$ID))
rm(cco)


# Fit CL-BART Model -------------------------------------------------------

# Set options and hyperparameters
opts <- Opts(num_burn = num_burn,
             num_save = num_save,
             num_thin = num_thin,
             num_chains = num_chains,
             update_sigma_mu = update_sigma_mu, 
             update_s = update_s, update_alpha = update_alpha,
             store_lambda = store_lambda,
             seed = 1)

hypers <- Hypers(num_tree = num_tree, base = base, power = power, k = k)

# Fit model
fit <- clbart(w, x, y, z, strata, hypers, opts)
fname <- paste0(sprintf('%04d', key),'.rds')
saveRDS(fit, here('ad-analysis','Results','models', fname))


# Analyses ----------------------------------------------------------------

# Marginal partial dependence
f_hat <- function (x) get_forest_posterior_predictions(fit$forest, x)
f_hat_exp <- function (x) exp(get_forest_posterior_predictions(fit$forest, x))
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
  mutate(key = key)

# Marginal effects via partial dependence
pd_diff <- function (x, f_hat, var, fun = I) {
  x <- as.data.frame(x)
  
  x0 <- x1 <- x
  
  x0[[var]] <- 0
  x1[[var]] <- 1

  delta_avg <- colMeans(fun(f_hat(x1) - f_hat(x0)))
  
  est <- mean(delta_avg)
  lcl <- quantile(delta_avg, 0.025)
  ucl <- quantile(delta_avg, 0.975)
  
  return(data.frame(var, est, lcl, ucl))
}

marg_pd_diff <- lapply(bin_vars, \(v) pd_diff(w, f_hat, v, exp)) |>
  bind_rows() |>
  mutate(key = key)

# Lower dimensional CART summary
# Create CART data
cart_data <- data.frame(w, tau = fit$lambda_est[firsts]) |>
  select(CKD:HYPERLIP, tau) |>
  mutate(across(CKD:HYPERLIP, \(x) factor(x, levels = 0:1, labels = c('N','Y'))))

# Fit CART model
cart <- rpart(tau ~ ., data = cart_data, control = rpart.control(maxdepth = 3))

# Compute R-squared for lower-dimensional summary
cart_R2 <- 1 - (sum((cart_data$tau - predict(cart))^2) / sum((cart_data$tau - mean(cart_data$tau))^2))

# # Identify important chronic conditions
# cart_vars <- names(cart$variable.importance)[1:3]
# cart_vars <- cart_vars[!is.na(cart_vars)]
# 
# cart_pd <- bayes_pd(w[firsts,], f_hat, vars = cart_vars, k = 2) |>
#   mutate(key = key)

# Group observations by CART leaf node
cart_groups <- cbind(cart_data, node = as.numeric(row.names(cart$frame)[cart$where])) |>
  mutate(across(CKD:HYPERLIP, \(x) as.numeric(x) - 1)) |>
  arrange(node) |>
  select(-tau) |>
  group_by(node) |>
  group_split(.keep = FALSE) |>
  lapply(\(df) unique(Filter(\(col) length(unique(col)) == 1, df)))

# Compute partial dependence within each leaf node
pd_cart <- function (x, f_hat, vars, vals, fun = I) {
  x <- as.data.frame(x)
  
  for (i in 1:length(vars)) x[[vars[i]]] <- vals[i]
  
  yhat_avg <- colMeans(fun(f_hat(x)))
  
  est <- mean(yhat_avg)
  lcl <- quantile(yhat_avg, 0.025)
  ucl <- quantile(yhat_avg, 0.975)
  Pr <- mean(yhat_avg > fun(0))
  
  df <- matrix(c(vals, est, lcl, ucl, Pr), nrow = 1)
  colnames(df) <- c(vars, 'est','lcl','ucl','Pr')
  
  return(data.frame(df))
}
cart_pd <- lapply(cart_groups, \(g) pd_cart(w, f_hat, names(g), as.numeric(g), exp)) |>
  bind_rows()
cart_pd$node <- as.numeric(names(table(as.numeric(row.names(cart$frame)[cart$where]))))
cart_pd$R2 <- cart_R2
cart_pd$key <- key

# Reorder columns
fixed_cols <- c('est','lcl','ucl','Pr','node','R2','key')
cart_pd <- cart_pd[,c(setdiff(names(cart_pd), fixed_cols), fixed_cols)]


# Output Results ----------------------------------------------------------

# Compile and return results
fit$race_eth_cat <- race_eth_cat

results <- list(key = key, fit = fit,
                marg_pd = marg_pd, marg_pd_diff = marg_pd_diff,
                cart = cart, cart_vars = setdiff(colnames(cart_pd), fixed_cols), cart_pd = cart_pd)

fname <- paste0(sprintf('%04d', key),'.rds')
saveRDS(results, here('ad-analysis','Results','temp', fname))
