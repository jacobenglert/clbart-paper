# Program Name: summarize-cart-sim.R
# Description:  Summarize results of CART simulation.

# Load Packages -----------------------------------------------------------
library(here)
library(tidyverse)
library(knitr)
library(kableExtra)
options(knitr.kable.NA = '')


# Helper Functions --------------------------------------------------------

# Formatting for tables
fmt_est_se <- function(est, se, est_dec = 3, se_dec = 3) {
  est <- format(round(est, est_dec), nsmall = est_dec)
  se <- format(round(se, se_dec), nsmall = se_dec)
  paste0(est, ' (', se, ')')
}

# Label formatting for Greek letters
base_power_labeller <- label_bquote(
  group("(", list(gamma, xi), ")") ~ ":" ~ .(base_power), k:.(k)
)


# Load Results and Parameter Settings -------------------------------------

# Load results
results <- readRDS(here('cart-sim','Results','cart-sim-results.rds'))

# Load parameter settings
params <- read_csv(here('cart-sim','Params','cart-sim-params.csv')) |>
  mutate(base_power = paste0('(', base, ', ', power, ')'))

# Main text parameter settings
keys <- filter(params, k == 1 & base == 0.95 & update_s)$key


# Variable Importance Measures --------------------------------------------

# Load variable importance results
var_imp <- bind_rows(lapply(results, '[[', 'var_imp'))
  
# Posterior split proportions

# All settings
var_imp |>
  summarise(mean = mean(prop_est), 
            se = sd(prop_est) / sqrt(n()),
            .by = c(key, var)) |>
  left_join(params, by = join_by(key)) |>
  filter(update_s) |>
  ggplot(aes(x = parse_number(var), 
             y = mean, ymin = mean - 1.96 * se, ymax = mean + 1.96 * se,
             color = factor(num_tree),
             group = paste(k, base_power, num_tree))) +
  geom_hline(yintercept = 0.1, lty = 2, color = 'gray') +
  geom_line() +
  geom_pointrange(size = 0.1) +
  facet_grid(base_power ~ k, labeller = base_power_labeller) +
  scale_x_continuous(breaks = seq(1,10,1)) +
  theme_bw() +
  theme(legend.position = 'bottom',
        text = element_text(size = 13)) +
  labs(x = 'Covariate Index',
       y = 'Proportion of Splits',
       color = 'Number of Trees')
ggsave(here('cart-sim','Figures','ext-cart-sim-var-imp.png'), 
       width = 6.5, height = 4.5)

# Main text settings only
var_imp |>
  filter(key %in% keys) |>
  summarise(mean = mean(prop_est), 
            se = sd(prop_est) / sqrt(n()),
            .by = c(key, var)) |>
  left_join(params, by = join_by(key)) |>
  ggplot(aes(x = parse_number(var), 
             y = mean, ymin = mean - 1.96 * se, ymax = mean + 1.96 * se,
             color = factor(num_tree),
             group = paste(num_tree))) +
  geom_hline(yintercept = 0.1, lty = 2, color = 'gray') +
  geom_line() +
  geom_pointrange(size = 0.3) +
  scale_x_continuous(breaks = seq(1,10,1)) +
  theme_bw() +
  theme(legend.position = 'inside',
        legend.position.inside = c(.95,.95),
        legend.justification = c(1,1),
        text = element_text(size = 13),
        legend.background = element_rect(color = 'black')) +
  labs(x = 'Covariate Index',
       y = 'Proportion of Splits',
       color = '# of Trees')
ggsave(here('cart-sim','Figures','cart-sim-var-imp.png'), 
       width = 3.5, height = 3.5)

# Posterior split probabilities
var_imp |>
  summarise(mean = mean(prob_est), 
            se = sd(prob_est) / sqrt(n()),
            .by = c(key, var)) |>
  left_join(params, by = join_by(key)) |>
  filter(update_s) |>
  ggplot(aes(x = parse_number(var),
             y = mean, ymin = mean - 1.96 * se, ymax = mean + 1.96 * se,
             color = factor(num_tree),
             group = paste(k, base, num_tree))) +
  geom_pointrange(size = 0.1) +
  geom_line() +
  geom_hline(yintercept = 0.1, lty = 2, color = 'gray') +
  facet_grid(base_power ~ k, labeller = base_power_labeller) +
  scale_x_continuous(breaks = seq(1,10,1)) +
  theme_bw() +
  theme(legend.position = 'bottom',
        text = element_text(size = 13)) +
  labs(x = 'Covariate',
       y = 'Posterior Mean Splitting Probability',
       color = 'Number of Trees')
ggsave(here('cart-sim','Figures','ext-cart-sim-psp.png'),
       width = 6.5, height = 4.5)

# Posterior inclusion probabilities
var_imp |>
  summarise(mean = mean(pip), 
            se = sd(pip) / sqrt(n()),
            .by = c(key, var)) |>
  left_join(params, by = join_by(key)) |>
  filter(update_s) |>
  ggplot(aes(x = parse_number(var),
             y = mean, ymin = mean - 1.96 * se, ymax = mean + 1.96 * se,
             color = factor(num_tree),
             group = paste(k, base, num_tree))) +
  geom_pointrange(size = 0.1) +
  geom_line() +
  geom_hline(yintercept = 0.1, lty = 2, color = 'gray') +
  facet_grid(base_power ~ k, labeller = base_power_labeller) +
  scale_x_continuous(breaks = seq(1,10,1)) +
  theme_bw() +
  theme(legend.position = 'bottom',
        text = element_text(size = 13)) +
  labs(x = 'Covariate Index',
       y = 'Posterior Inclusion Probability (PIP)',
       color = 'Number of Trees')
ggsave(here('cart-sim','Figures','ext-cart-sim-pip.png'), 
       width = 6.5, height = 4.5)


# BART Predictions --------------------------------------------------------

# Load BART prediction results
bart_stats <- bind_rows(lapply(results, '[[', 'bart_stats'))

# Format results
bart_stats_summary <- bart_stats |>
  summarise(across(avg_bias:avg_width, mean, .names = "mean_{.col}"),
            across(c(avg_bias, rmse, avg_width), \(x) sd(x) / sqrt(n()), .names = "se_{.col}"),
            se_avg_coverage = sqrt(mean(avg_coverage) * (1 - mean(avg_coverage)) / n()),
            .by = c(type, key)) |>
  left_join(params, by = join_by(key)) |>
  filter((type == 'oracle' & key == 1) | (type == 'clbart' & update_s)) |>
  mutate(across(c(num_tree, k, base_power), \(x) ifelse(type == 'oracle', NA, x))) |>
  select(key, type, num_tree, k, base_power, 
         ends_with(c('bias','rmse','coverage','width'))) |>
  arrange(desc(type), num_tree, k) |>
  mutate(bias = fmt_est_se(mean_avg_bias, se_avg_bias),
         rmse = fmt_est_se(mean_rmse, se_rmse),
         coverage = fmt_est_se(mean_avg_coverage, se_avg_coverage),
         width = fmt_est_se(mean_avg_width, se_avg_width)) |>
  select(key, type:base_power, bias, rmse, coverage, width)

# All settings
bart_stats_tbl <- bart_stats_summary[,2:9] |>
  kbl(format = 'latex', booktabs = TRUE, linesep = '', escape = FALSE,
      align = c('c'), label = 'ext-cart-sim-bart-stats',
      caption = 'Extended CART Simulation Results - BART Predictions',
      col.names = c('Type',
                    paste0('$M$', footnote_marker_alphabet(1, 'latex')),
                    paste0('$k$', footnote_marker_alphabet(2, 'latex')),
                    paste0('$(\\gamma, \\xi)$', footnote_marker_alphabet(3, 'latex')),
                    paste0('Bias', footnote_marker_alphabet(4, 'latex')),
                    paste0('RMSE', footnote_marker_alphabet(4, 'latex')),
                    paste0('Coverage', footnote_marker_alphabet(4, 'latex')),
                    paste0('Width', footnote_marker_alphabet(4, 'latex')))) |>
  kable_styling(latex_options = 'hold_position', font_size = 11) |>
  footnote(alphabet = c('$M$: Number of trees.',
                        '$k$: Numerator of scale parameter for half-Cauchy hyper-prior.',
                        '$(\\\\gamma, \\\\xi)$: Hyperparameters for tree depth prior.',
                        'Monte Carlo mean and standard errors across 200 simulations reported.'),
           escape = FALSE)

row_breaks <- bart_stats_summary |> 
  summarise(last_row = n(), .by = num_tree) |>
  mutate(last_row = cumsum(last_row)) |>
  pull(last_row)

for (row in row_breaks[1:(length(row_breaks) - 1)]) {
  bart_stats_tbl <- bart_stats_tbl |> row_spec(row, hline_after = TRUE, extra_latex_after = '\\addlinespace[2pt]')
}

# Write to file
write_lines(bart_stats_tbl, 
            here('cart-sim','Tables','ext-cart-sim-bart-stats.txt'))

# Main text settings only
bart_stats_tbl_sub <- bart_stats_summary |>
  filter(key %in% keys | type == 'oracle') |>
  select(type, num_tree, bias:width) |>
  kbl(format = 'latex', booktabs = TRUE, linesep = '', escape = FALSE,
      align = c('c'), label = 'cart-sim-bart-stats',
      caption = 'CART Simulation Results - BART Predictions',
      col.names = c('Type',
                    paste0('$M$', footnote_marker_alphabet(1, 'latex')),
                    paste0('Bias', footnote_marker_alphabet(2, 'latex')),
                    paste0('RMSE', footnote_marker_alphabet(2, 'latex')),
                    paste0('Coverage', footnote_marker_alphabet(2, 'latex')),
                    paste0('Width', footnote_marker_alphabet(2, 'latex')))) |>
  kable_styling(latex_options = 'h') |>
  footnote(alphabet = c('$M$: Number of trees.',
                        'Monte Carlo mean and standard errors across 200 simulations reported.'),
           escape = FALSE) |>
  row_spec(1, hline_after = TRUE)

# Write to file
write_lines(bart_stats_tbl_sub, 
            here('cart-sim','Tables','cart-sim-bart-stats.txt'))


# Confounder Statistics ---------------------------------------------------

# Load beta estimation results
beta_stats <- bind_rows(lapply(results, '[[', 'beta_stats'))

# Format results
beta_stats_summary <- beta_stats |>
  mutate(bias = bias * 1000) |>
  summarise(bias = fmt_est_se(mean(bias), sd(bias) / sqrt(n()), 2, 2),
            coverage = format(round(mean(coverage), 2), nsmall = 2),
            .by = c(key, type, var)) |>
  left_join(params, by = join_by(key)) |>
  filter((type == 'oracle' & key == 1) | (type == 'clbart' & update_s)) |>
  mutate(across(c(num_tree, k, base_power), \(x) ifelse(type == 'oracle', NA, x))) |>
  arrange(desc(type), num_tree, k) |>
  select(type, num_tree, k, base_power, var, bias, coverage) |>
  pivot_wider(id_cols = type:base_power, names_from = var, values_from = c(bias, coverage))

# All settings - Bias
beta_stats_tbl_bias <- beta_stats_summary |>
  select(type:base_power, starts_with('bias')) |>
  kbl(format = 'latex', booktabs = TRUE, linesep = '', escape = FALSE,
      align = c('c'), label = 'ext-cart-sim-beta-bias',
      caption = 'Extended CART Simulation Results - Confounder Estimates (Bias)',
      col.names = c('Type',
                    paste0('$M$', footnote_marker_alphabet(1, 'latex')),
                    paste0('$k$', footnote_marker_alphabet(2, 'latex')),
                    paste0('$(\\gamma, \\xi)$', footnote_marker_alphabet(3, 'latex')),
                    paste0('$\\beta_',1:5,'$'))) |>
  kable_styling(latex_options = 'h', font_size = 11) |>
  add_header_above(c(' '= 4, 
                     'Bias $\\\\times$ 1,000\\\\textsuperscript{d}' = 5), escape = FALSE) |>
  footnote(alphabet = c('$M$: Number of trees.',
                        '$k$: Numerator of scale parameter for half-Cauchy hyper-prior.',
                        '$(\\\\gamma, \\\\xi)$: Hyperparameters for tree depth prior.',
                        'Monte Carlo mean and standard errors across 200 simulations reported.'),
           escape = FALSE)

# All settings - Coverage
beta_stats_tbl_coverage <- beta_stats_summary |>
  select(type:base_power, starts_with('coverage')) |>
  kbl(format = 'latex', booktabs = TRUE, linesep = '', escape = FALSE,
      align = c('c'), label = 'ext-cart-sim-beta-coverage',
      caption = 'Extended CART Simulation Results - Confounder Estimates (Coverage)',
      col.names = c('Type',
                    paste0('$M$', footnote_marker_alphabet(1, 'latex')),
                    paste0('$k$', footnote_marker_alphabet(2, 'latex')),
                    paste0('$(\\gamma, \\xi)$', footnote_marker_alphabet(3, 'latex')),
                    paste0('$\\beta_',1:5,'$'))) |>
  kable_styling(latex_options = 'h', font_size = 11) |>
  add_header_above(c(' '= 4, 
                     'Coverage\\\\textsuperscript{d}' = 5), escape = FALSE) |>
  footnote(alphabet = c('$M$: Number of trees.',
                        '$k$: Numerator of scale parameter for half-Cauchy hyper-prior.',
                        '$(\\\\gamma, \\\\xi)$: Hyperparameters for tree depth prior.',
                        '95\\\\% credible interval coverage across 200 simulations.'),
           escape = FALSE)

row_breaks <- beta_stats_summary |> 
  summarise(last_row = n(), .by = num_tree) |>
  mutate(last_row = cumsum(last_row)) |>
  pull(last_row)

for (row in row_breaks[1:(length(row_breaks) - 1)]) {
  beta_stats_tbl_bias <- beta_stats_tbl_bias |> row_spec(row, hline_after = TRUE, extra_latex_after = '\\addlinespace[2pt]')
  beta_stats_tbl_coverage <- beta_stats_tbl_coverage |> row_spec(row, hline_after = TRUE, extra_latex_after = '\\addlinespace[2pt]')
}

# Write to files
write_lines(beta_stats_tbl_bias, 
            here('cart-sim','Tables','ext-cart-sim-beta-bias.txt'))
write_lines(beta_stats_tbl_coverage, 
            here('cart-sim','Tables','ext-cart-sim-beta-coverage.txt'))


# WAIC --------------------------------------------------------------------

# Extract WAIC from results and calculate relative WAIC for each random seed
waic <- data.frame(key = sapply(results, '[[', 'key'),
                   waic = sapply(results, '[[', 'waic')) |>
  mutate(seed = row_number(), .by = key) |>
  mutate(rel_waic = waic / min(waic), .by = c(seed)) |>
  summarise(mean = mean(rel_waic), 
            se = sd(rel_waic) / sqrt(n()), 
            .by = key) |>
  left_join(params, by = join_by(key))

# Plot relative WAIC
waic |>
  mutate(type = ifelse(update_s, 'DART', 'BART')) |>
  ggplot(aes(x = factor(num_tree), 
             y = mean, ymin = mean - 1.96 * se, ymax = mean + 1.96 * se,
             group = paste(base_power, k, update_s), 
             lty = type, shape = type)) +
  geom_pointrange(position = position_dodge(0.5), size = .3) +
  geom_line(position = position_dodge(0.5)) +
  geom_hline(yintercept = 1, lty = 2, color = 'lightgray') +
  theme_bw() +
  theme(legend.position = 'inside',
        legend.position.inside = c(.9,.9),
        legend.title = element_blank(),
        text = element_text(size = 13),
        legend.background = element_rect(color = 'black')) +
  facet_grid(base_power ~ k, labeller = base_power_labeller) +
  labs(x = 'Number of Trees',
       y = 'Relative WAIC')
ggsave(here('cart-sim','Figures','ext-cart-sim-waic.png'), 
       width = 6.5, height = 4.5)


# Timing ------------------------------------------------------------------

# Extract run times
time <- data.frame(key = sapply(results, '[[', 'key'),
                   time = sapply(results, '[[', 'time') / 60) |>
  summarise(mean = mean(time), 
            se = sd(time) / sqrt(n()), 
            .by = key) |>
  left_join(params, by = join_by(key))

# Plot average run times
time |>
  filter(update_s) |>
  mutate(type = ifelse(update_s, 'DART', 'BART')) |>
  ggplot(aes(x = num_tree, 
             y = mean, ymin = mean - 1.96 * se, ymax = mean + 1.96 * se,
             group = paste(base_power, k, update_s)#, lty = type, shape = type
             )) +
  geom_pointrange(position = position_dodge(0.5), size = .3) +
  geom_line(position = position_dodge(0.5)) +
  theme_bw() +
  theme(legend.position = 'inside',
        legend.position.inside = c(.9,.1),
        legend.title = element_blank(),
        text = element_text(size = 13),
        legend.background = element_rect(color = 'black')) +
  facet_grid(base_power ~ k, labeller = base_power_labeller) +
  labs(x = 'Number of Trees',
       y = 'Mean Runtime (Hours)')
ggsave(here('cart-sim','Figures','ext-cart-sim-time.png'), 
       width = 6.5, height = 4.5)
