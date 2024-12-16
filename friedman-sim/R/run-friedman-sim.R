# Program Name: run-friedman-sim.R
# Description:  Simulate log odds-ratios from the standardized Friedman 
#               benchmark function and attempt to recover using CL-BART.

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(here)
library(clbart)
library(survival)
library(pdpd)


# Get Parameters ----------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
key <- as.numeric(args[1])
seed <- as.numeric(args[2])
params <- read_csv(here('friedman-sim','Params','friedman-sim-params.csv'))
for (param in colnames(params)) {
  assign(param, unlist(params[key, param, drop = TRUE]))
}


# Simulate Data -----------------------------------------------------------

# Setup
set.seed(seed)
n       <- n
start   <- as.Date("2020-01-01")
end     <- as.Date("2022-12-31")
dates   <- seq.Date(start, end, by = 'day')
t       <- length(dates)
p_w     <- 10
p_x     <- 5

# Exposure
z <- rnorm(t, sin(seq(0, 2 * pi * 3, length.out = t)))

# Confounders
x <- replicate(p_x, runif(t))
colnames(x) <- paste0('X', 1:p_x)

# Modifiers
w <- replicate(p_w, runif(n))
colnames(w) <- paste0('W', 1:p_w)

# Data frame of time-varying variables
xz <- data.frame(x, Z = z, date = dates) |>
  mutate(year = year(date), month = month(date), dow = wday(date))

# Data frame of all individuals at all times
data <- cross_join(data.frame(strata = 1:n, w), xz)

# Heterogeneous exposure log odds-ratios
f <- function(x) ((10 * sin(pi*x[,1]*x[,2]) + 20*(x[,3]-.5)^2 + 10*x[,4] + 5*x[,5]) - 14) / 15
tau <- f(data[,colnames(w)])

# Fixed effects
alpha <- -8 # intercept
beta <- log(c(0.5, 0.8, 1.0, 1.2, 2.0)) # confounder log odds-ratios

# Calculate probabilities
expit <- function(x) exp(x) / (1 + exp(x))
p <- expit(alpha + as.matrix(data[,colnames(x)]) %*% beta + tau * data$Z)[,1]

# Store probabilities and log-odds in dataset
data$tau <- tau
data$p <- p

# Generate outcome
y <- rbinom(n * t, 1, data$p)

# Implement time-stratified case-crossover design
cco <- data[which(y == 1),] |>  # subset cases
  mutate(strata = row_number()) |>  # assign unique ID (strata) to each case
  select(-all_of(c(colnames(x), 'Z'))) |>
  left_join(xz, by = c('year', 'month', 'dow'), relationship = 'many-to-many') |>
  mutate(Y = ifelse(date.x == date.y, 1, 0)) |> # create case variable
  select(-date.x) |>
  rename(date = date.y)

rm(data, xz)

# Prepare CCO data for model
tau <- cco$tau
w <- cco[colnames(w)]
x <- cco[colnames(x)]
z <- cco$Z
y <- cco$Y
strata <- cco$strata

rm(cco)


# Fit Model ---------------------------------------------------------------

# Set options and hyperparameters
opts <- Opts(num_burn = num_burn,
             num_save = num_save,
             num_thin = num_thin,
             num_chains = num_chains,
             update_mu_mu = update_mu_mu,
             update_sigma_mu = update_sigma_mu, 
             update_s = update_s, update_alpha = update_alpha,
             store_lambda = store_lambda,
             seed = seed)

hypers <- Hypers(num_tree = num_tree, base = base, power = power, k = k)

# Fit model
fit <- clbart(w, x, y, z, strata, hypers, opts)


# Calculate Statistics ----------------------------------------------------

colQuants <- function (x, q) apply(x, 2, quantile, probs = q)

# Point and interval estimates for BART predictions
tau_est <- colMeans(fit$lambda)
tau_lcl <- colQuants(fit$lambda, 0.025)
tau_ucl <- colQuants(fit$lambda, 0.975)

# Bias, rmse, coverage and interval width
avg_bias      <- mean(tau_est - tau)
rmse          <- sqrt(mean((tau_est - tau)^2))
avg_coverage  <- mean(between(tau, tau_lcl, tau_ucl))
avg_width     <- mean(tau_ucl - tau_lcl)

bart_stats <- data.frame(type = 'clbart',
                         key = key, seed = seed, 
                         avg_bias, rmse, avg_coverage, avg_width)

# Point and intervals estimates for confounders
beta_est <- colMeans(fit$beta)
beta_lcl <- colQuants(fit$beta, 0.025)
beta_ucl <- colQuants(fit$beta, 0.975)

beta_bias <- beta_est - beta
beta_coverage <- as.numeric(between(beta, beta_lcl, beta_ucl))

beta_stats <- data.frame(type = 'clbart',
                         key = key, seed = seed, var = colnames(x),
                         bias = beta_bias, coverage = beta_coverage)

# Variable importance
split_props <- prop.table(fit$split_counts, margin = 1)
var_imp <- data.frame(key = key, seed = seed, 
                      var = colnames(w),
                      
                      # Observed split proportions
                      prop_est = colQuants(split_props, 0.5),
                      prop_lower = colQuants(split_props, 0.025),
                      prop_upper = colQuants(split_props, 0.975),
                      
                      # Split probabilities
                      prob_est = colQuants(fit$split_probs, 0.5),
                      prob_lower = colQuants(fit$split_probs, 0.025),
                      prob_upper = colQuants(fit$split_probs, 0.975),
                      
                      # Posterior inclusion probabilities
                      pip = colMeans(fit$split_counts > 0))
                      

# Partial dependence
f_hat <- function (x) get_forest_posterior_predictions(fit$forest, x)
firsts <- match(unique(strata), strata)
marg_pd <- lapply(colnames(w), 
                  \(v) bayes_pd(w[firsts,], f_hat, vars = v, k = K, f = f)) |>
  lapply(\(x) {
    x$var <- colnames(x)[1]
    colnames(x)[1] <- 'x'
    x
  }) |>
  bind_rows() |>
  mutate(key = key, seed = seed)


# Fit and Calculate Statistics for Oracle Model ---------------------------

# True moderator design matrix
w_true <- cbind(rep(1, nrow(w)), sin(pi*w[,1]*w[,2]), (w[,3] - 0.5)^2, w[,4], w[,5])

# Oracle fit
oracle <- clogit(y ~ as.matrix(x) + w_true : z + strata(strata))

coef_idx <- (p_x + 1):(p_x + ncol(w_true))
oracle_pt <- oracle$coefficients[coef_idx]
oracle_se <- sqrt(diag((w_true) %*% vcov(oracle)[coef_idx, coef_idx] %*% t(w_true)))

# Point and interval estimates for BART predictions
tau_est_oracle <- as.numeric((w_true) %*% oracle_pt)
tau_lcl_oracle <- as.numeric(tau_est_oracle - 1.96*oracle_se)
tau_ucl_oracle <- as.numeric(tau_est_oracle + 1.96*oracle_se)

# Bias, rmse, coverage and interval width
avg_bias_oracle      <- mean(tau_est_oracle - tau)
rmse_oracle          <- sqrt(mean((tau_est_oracle - tau)^2))
avg_coverage_oracle  <- mean(between(tau, tau_lcl_oracle, tau_ucl_oracle))
avg_width_oracle     <- mean(tau_ucl_oracle - tau_lcl_oracle)

bart_stats_oracle <- data.frame(type = 'oracle',
                                key = key, seed = seed, 
                                avg_bias = avg_bias_oracle, 
                                rmse = rmse_oracle, 
                                avg_coverage = avg_coverage_oracle, 
                                avg_width = avg_width_oracle)

# Point and intervals estimates for confounders
beta_est_oracle <- as.numeric(oracle$coefficients[1:p_x])
beta_lcl_oracle <- as.numeric(confint(oracle)[1:p_x,1])
beta_ucl_oracle <- as.numeric(confint(oracle)[1:p_x,2])

beta_bias_oracle <- beta_est_oracle - beta
beta_coverage_oracle <- as.numeric(between(beta, beta_lcl_oracle, beta_ucl_oracle))

beta_stats_oracle <- data.frame(type = 'oracle',
                                key = key, seed = seed, var = colnames(x),
                                bias = beta_bias_oracle, 
                                coverage = beta_coverage_oracle)


# Compile and return results ----------------------------------------------
results <- list(key = key, seed = seed, waic = fit$WAIC, time = fit$time, 
                beta_stats = rbind(beta_stats, beta_stats_oracle), 
                bart_stats = rbind(bart_stats, bart_stats_oracle),
                var_imp = var_imp,
                marg_pd = marg_pd)
ID <- paste0(sprintf('%03d', key), '-', sprintf('%03d', seed))
saveRDS(results, here('friedman-sim','Results','temp', paste0(ID, '.rds')))
