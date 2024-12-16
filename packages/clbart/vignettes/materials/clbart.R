## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
fit <- readr::read_rds(here::here('vignettes','fit.rds'))
marg_ale <- readr::read_rds(here::here('vignettes','marg_ale.rds'))
joint_ale <- readr::read_rds(here::here('vignettes','joint_ale.rds'))

## ----setup, message = FALSE---------------------------------------------------
library(clbart)
library(tidyverse)

## -----------------------------------------------------------------------------
# Setup
set.seed(1)
n       <- 10000
start   <- as.Date("2023-01-01")
end     <- as.Date("2023-12-31")
dates   <- seq.Date(start, end, by = 'day')
t       <- length(dates)
p_w     <- 10
p_x     <- 5

# Simulate shared exposure time-series
z <- rnorm(t, sin(seq(0, 2 * pi, length.out = t)))

# Simulate independent time-varying confounders
x <- replicate(p_x, runif(t))
colnames(x) <- paste0('X', 1:p_x)

# Simulate independent time-invariant moderators
w <- replicate(p_w, runif(n))
colnames(w) <- paste0('W', 1:p_w)

# Construct data frame of time-varying variables
xz <- data.frame(x, Z = z, date = dates) |>
  mutate(year = year(date), month = month(date), dow = wday(date))

# Data frame of all individuals at all times
data <- cross_join(data.frame(strata = 1:n, w), xz)

## -----------------------------------------------------------------------------
# Heterogeneous exposure log odds-ratios
f0 <- function(x) 10 * sin(pi*x[,1]*x[,2]) + 20*(x[,3]-.5)^2 + 10*x[,4] + 5*x[,5]
f <- function(x) (f0(x) - 14) / 15
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

## -----------------------------------------------------------------------------
# Implement time-stratified case-crossover design
cco <- data[which(y == 1),] |>  # subset cases
  mutate(strata = row_number()) |>  # assign unique ID (strata) to each case
  select(-all_of(c(colnames(x), 'Z'))) |>
  left_join(xz, by = c('year', 'month', 'dow'), relationship = 'many-to-many') |>
  mutate(Y = ifelse(date.x == date.y, 1, 0)) |> # create case variable
  select(-date.x) |>
  rename(date = date.y)

## -----------------------------------------------------------------------------
# Prepare CCO data for model
w <- cco[colnames(w)]
x <- cco[colnames(x)]
z <- cco$Z
y <- cco$Y
strata <- cco$strata

## ----eval = FALSE-------------------------------------------------------------
#  # Specify hyperparameters for fitting clbart model
#  hypers <- Hypers(num_tree = 20, k = 1)
#  opts <- Opts(update_s = TRUE, update_alpha = TRUE,
#               num_burn = 2000, num_thin = 1, num_save = 500)
#
#  # Fit clbart model
#  fit <- clbart(w, x, y, z, strata, hypers, opts)

## -----------------------------------------------------------------------------
# Obtain a brief summary of the model fit
fit_sum <- summary(fit)

# Check beta (confounder) estimates
fit_sum$beta_stats
beta

## ----fig.width=6, fig.height = 4, fig.align='center', fig.cap = 'CL-BART Predictions vs. Truth'----
# Plot individual BART predictions versus the true association
plot(fit$lambda_est ~ cco$tau,
     xlab = 'Truth', ylab = 'Prediction')
abline(a = 0, b = 1, col = 'red', lty = 2)

## -----------------------------------------------------------------------------
# Check variable importance measures
fit_sum$var_imp

## ----eval = FALSE-------------------------------------------------------------
#  library(pdpd)
#  f_hat <- function(w) predict(fit, list(w = w), type = "bart", posterior = TRUE)
#  firsts <- match(unique(strata), strata)
#
#  # Compute ALE
#  marg_ale <- lapply(colnames(w),
#                     \(v) bayes_ale(w[firsts,], f_hat, vars = v, k = 40, f = f))

## ----fig.cap='Accumulated Local Effects Plots for Effect Moderators'----------
marg_ale |>
  bind_rows() |>
  ggplot(aes(x = x, y = est, ymin = lcl, ymax = ucl)) +
  geom_line() +
  geom_line(aes(y = truth), col = 'red', lty = 2) +
  geom_ribbon(alpha = 0.2) +
  facet_wrap(~factor(var, colnames(w)), nrow = 2) +
  theme_bw() +
  labs(x = 'Covariate Value',
       y = 'Posterior Mean ALE w/ 95% Credible Interval')

## ----fig.cap='Trace Plot of Average BART Prediction', fig.align='center'------
plot(fit$lambda_mean_overall, type = 'l',
     xlab = 'Posterior Sample Index', ylab = 'Average Prediction')

## -----------------------------------------------------------------------------
fit$WAIC

