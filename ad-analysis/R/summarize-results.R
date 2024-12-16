# Program Name: summarize-results.R
# Description:  Summarize results of AD application

# Load Packages -----------------------------------------------------------
library(here)
library(tidyverse)
library(patchwork)
library(knitr)
library(kableExtra)
library(rpart.plot)
library(clbart)


# Load Parameter Settings -------------------------------------------------

# Parameter table
params <- read_csv(here('ad-analysis','Params','params.csv'))

# Identify parameter settings used in the text
# - M = 25 for stratified analysis, M = 100 for pooled analysis
keys <- filter(params, 
               (race_eth_cat != 'All' & k == 1 & base == 0.95 & update_s & num_tree == 25) |
               (race_eth_cat == 'All' & k == 1 & base == 0.95 & update_s & num_tree == 100))$key


# Load Results ------------------------------------------------------------

# Read in results only for parameter settings used in the text
results <- readRDS(here('ad-analysis','Results','ad-results.rds'))[keys]


# Create Labels -----------------------------------------------------------

# Raw, short, and long versions of moderators
var_raw   <- c('FEMALE','AGE','CHF','CKD','COPD','DEPRESSION','DIAB','HT','HYPERLIP')
var_short <- c('Female','Age','CHF','CKD','COPD','DEP','DIAB','HT','HLD')
var_long  <- c('Female','Age','CHF','CKD','COPD','Depression','Diabetes','Hypertension','Hyperlipidemia')

# Raw/short, and long versions of race/ethnicity levels
race_eth_raw    <- c('All','HISP','NHW','NHB','NHAPI','NHO')
race_eth_short  <- c('Overall','HISP','NHW','NHB','NHAPI','NHO')
race_eth_long   <- c('Overall',
                     'Hispanic',
                     'Non-Hispanic White',
                     'Non-Hispanic Black',
                     'Non-Hispanic Asian and Pacific Islander',
                     'Non-Hispanic Other')


# Helper Functions --------------------------------------------------------

colQuants <- function (x, q) apply(x, 2, \(col) quantile(col, probs = q, na.rm = TRUE))
rowQuants <- function (x, q) apply(x, 1, \(row) quantile(row, probs = q, na.rm = TRUE))


# Variable Importance Measures --------------------------------------------

# Summarize variable importance statistics for all models
var_imp <- lapply(results, \(x) 
       data.frame(key = x$key,
                  var = colnames(x$fit$split_counts),
                  prop_est = colMeans(x$fit$split_counts / rowSums(x$fit$split_counts)),
                  prop_lcl = colQuants(x$fit$split_counts / rowSums(x$fit$split_counts), 0.025),
                  prop_ucl = colQuants(x$fit$split_counts / rowSums(x$fit$split_counts), 0.975),
                  prob_est = colMeans(x$fit$split_probs),
                  pip = colMeans(x$fit$split_probs > 0))) |>
  bind_rows()

# Posterior Split Proportions (selected models)
#   - excluding race/ethnicity variables in the overall model
var_imp_plt <- var_imp |>
  left_join(params, by = join_by(key)) |>
  filter(key %in% keys) |>
  mutate(var = factor(var, levels = var_raw, labels = var_short),
         race_eth_cat = factor(race_eth_cat, race_eth_raw, race_eth_long)) |>
  na.omit() |>
  mutate(prop_est = prop_est / sum(prop_est), .by = key) |> # re-normalize
  ggplot(aes(x = var, y = prop_est, color = race_eth_cat, group = race_eth_cat)) +
  geom_hline(yintercept = 1 / 9, lty = 2, color = 'gray') +
  geom_point() +
  geom_line() +
  theme_bw() +
  theme(text = element_text(size = 13),
        legend.position = 'none') +
  labs(x = 'Covariate',
       y = 'Proportion of Splits',
       color = 'Subgroup')

# Posterior Split Probabilities (all models)
var_imp |>
  left_join(params, by = join_by(key)) |>
  mutate(var = factor(var, levels = var_raw, labels = var_short),
         race_eth_cat = factor(race_eth_cat, race_eth_raw)) |>
  na.omit() |>
  ggplot(aes(x = var, y = prob_est, color = race_eth_cat)) +
  geom_point() +
  facet_grid(update_s~k) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 270))

# Posterior Inclusion Probabilities (all models)
var_imp |>
  left_join(params, by = join_by(key)) |>
  mutate(var = factor(var, levels = var_raw, labels = var_short),
         race_eth_cat = factor(race_eth_cat, race_eth_raw)) |>
  na.omit() |>
  ggplot(aes(x = var, y = pip, color = race_eth_cat)) +
  geom_point() +
  facet_grid(update_s~k) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 270))


# Marginal Partial Dependence for Binary Variables ------------------------

# Estimated marginal partial dependence functions for binary moderators
#   - excluding race/ethnicity variables in the overall model
marg_sum <- bind_rows(lapply(results, '[[', 'marg_pd_diff')) |>
  left_join(params, by = join_by(key)) |>
  filter(key %in% keys)

marg_pd_diff_plt <- marg_sum |>
  mutate(race_eth_cat = factor(race_eth_cat, race_eth_raw, race_eth_long),
         var = factor(var, levels = var_raw, labels = var_short)) |>
  na.omit() |>
  ggplot(aes(y = est, ymin = lcl, ymax = ucl, x = var, color = race_eth_cat)) +
  geom_hline(yintercept = 1, color = 'gray', lty = 2) +
  geom_pointrange(size = .2, position = position_dodge(.5)) +
  theme_bw() +
  theme(legend.position = 'bottom', text = element_text(size = 13)) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
  labs(x = 'Covariate', 
       y = expression(bar(tau)[partial] * ' Difference'),
       color = 'Subgroup')

var_imp_plt / marg_pd_diff_plt + plot_annotation(tag_levels = 'A')
ggsave(here('ad-analysis','Figures','app-var-imp-and-marg-pd-diff.png'), 
       width = 6.5, height = 7)

# Check percentage of splits in Overall model based on race/ethnicity
var_imp |>
  left_join(params, by = join_by(key)) |>
  filter(race_eth_cat == 'All' & var %in% race_eth_raw) |>
  pull(prop_est) |>
  sum()

# Forest plot of marginal effects
marg_sum |>
  mutate(race_eth_cat = factor(race_eth_cat, race_eth_raw, race_eth_short),
         var = factor(var, levels = var_raw, labels = var_short)) |>
  na.omit() |>
  mutate(flag = case_when(ucl < 1 ~ 'Protective',
                          lcl > 1 ~ 'Harmful',
                          .default = 'Neutral')) |>
  ggplot(aes(x = est, xmin = lcl, xmax = ucl, y = var, color = flag)) +
  geom_vline(xintercept = 1, color = 'gray', lty = 2) +
  geom_pointrange(show.legend = FALSE) +
  scale_color_manual(values = c('Protective' = 'blue','Neutral' = 'black', 'Harmful' = 'red')) +
  theme_bw() +
  theme(text = element_text(size = 13)) +
  facet_wrap(~race_eth_cat)+#, scales = 'free_x') +
  labs(x = 'Marginal pACLOR Difference (w/ 95% Credible Interval)',
       y = 'Covariate')
ggsave(here('ad-analysis','Figures','app-marg-pd-diff.png'), 
       width = 6.5, height = 5)

# Table (Stratified Analysis)
marg_sum_tbl_strat <- marg_sum |>
  mutate(race_eth_cat = factor(race_eth_cat, race_eth_raw, race_eth_long),
         var = factor(var, levels = var_raw, labels = var_long)) |>
  na.omit() |>
  filter(race_eth_cat != 'Overall') |>
  mutate(val = paste0(format(round(est, 2), nsmall = 2), ' (',
                      format(round(lcl, 2), nsmall = 2), ', ', 
                      format(round(ucl, 2), nsmall = 2), ')')) |>
  select(race_eth_cat, var, val) |>
  arrange(race_eth_cat, var) |>
  kbl(format = 'latex', booktabs = TRUE, escape = FALSE,
      digits = 2, align = c('llc'),
      label = 'app-marg-pd-tbl-stratified',
      caption = 'Marginal Partial Dependence Estimates - Stratified Analysis',
      col.names = c('Subgroup','Covariate','$\\exp{\\left( \\bar{\\tau}_{diff} \\right)}^a$')) |>
  kable_styling(font_size = 11) |>
  footnote(alphabet = c('$\\\\bar{\\\\tau}_{diff}$: Difference in marginal partial average exposure effects.'),
           general = c('Posterior mean and 95\\\\% credible interval presented.'),
           footnote_order = c('alphabet','general'),
           general_title = '',
           escape = FALSE) |>
  collapse_rows(1, latex_hline = 'major', valign = 'middle')

# Write to file
write_lines(marg_sum_tbl_strat, 
            here('ad-analysis','Tables','app-marg-pd-tbl-stratified.txt'))

# Table (Overall)
marg_sum_tbl_overall <- marg_sum |>
  filter(race_eth_cat == 'All') |>
  mutate(var = factor(var, levels = c(race_eth_raw, var_raw), 
                      labels = c(race_eth_long, var_long))) |>
  mutate(val = paste0(format(round(est, 2), nsmall = 2), ' (',
                      format(round(lcl, 2), nsmall = 2), ', ', 
                      format(round(ucl, 2), nsmall = 2), ')')) |>
  select(var, val) |>
  arrange(var) |>
  kbl(format = 'latex', booktabs = TRUE, escape = FALSE, linesep = '',
      digits = 2, align = c('lc'),
      label = 'app-marg-pd-tbl-overall',
      caption = 'Marginal Partial Dependence Estimates - Overall',
      col.names = c('Covariate','$\\exp{\\left( \\bar{\\tau}_{diff} \\right)}^a$')) |>
  kable_styling(font_size = 11) |>
  row_spec(5, extra_latex_after = "\\addlinespace") |>
  footnote(alphabet = c('$\\\\bar{\\\\tau}_{diff}$: Difference in marginal partial average exposure effects.'),
           general = c('Posterior mean and 95\\\\% credible interval presented.',
                       'Race/ethnicity covariates are mutually exclusive and one-hot encoded.'),
           footnote_order = c('alphabet','general'),
           general_title = '',
           escape = FALSE)

# Write to file
write_lines(marg_sum_tbl_overall, 
            here('ad-analysis','Tables','app-marg-pd-tbl-overall.txt'))


# CART Partial Dependence -------------------------------------------------

# Plot lower-dimensional summaries
png(here('ad-analysis','Figures','app-cart-summaries.png'),
    width = 9, height = 11, units = 'in', res = 600)
par(mfrow = c(3,2))
for (r in results) {
  
  rpart.plot(r$cart, 
             digits = 2, tweak = 1.1,
             roundint = FALSE,
             type = 5, fallen.leaves = FALSE,
             extra = 1, branch = 0.2, #under = TRUE, under.cex = 1.2,
             leaf.round = .9,
             box.palette = 'BuRd',
             main = factor(r$fit$race_eth_cat, race_eth_raw, race_eth_long))
  
}

dev.off()

# Forest plot of all partial dependence estimates
cart_forest_plot <- function (x) {
  
  cart_groups <- paste0("interaction(", paste(x$cart_vars, collapse = ", "), ", sep = ' / ', lex.order = TRUE)")
  cart_labels <- var_short[match(x$cart_vars, var_raw)]
  
  lcl <- floor(min(x$cart_pd$lcl) / 0.05) * 0.05
  ucl <- ceiling(max(x$cart_pd$ucl) / 0.05) * 0.05
  x_breaks <- round(c(lcl, (lcl + ucl) / 2, ucl), 2)
  
  plt <- x$cart_pd |>
    mutate(across(x$cart_vars, \(v) case_when(v == 0 ~ 'N',
                                              v == 1 ~ 'Y',
                                              is.na(v) ~ '\u2013'))) |>
    mutate(cat = case_when(ucl < 1 ~ '-1', lcl > 1 ~ '1', .default = '0')) |>
    ggplot(aes(y = !!rlang::parse_expr(cart_groups), color = cat)) +
    geom_vline(xintercept = 1, lty = 2, color = 'gray') +
    geom_pointrange(aes(x = est, xmin = lcl, xmax = ucl),
                    size = .2,
                    show.legend = FALSE) +
    scale_color_manual(values = list('-1' = 'blue', '0' = 'black', '1' = 'red')) +
    scale_x_continuous(breaks = x_breaks, limits = c(lcl - .01, ucl + .01)) +
    theme_bw() +
    theme(axis.title.y = element_text(size = 13),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 13, hjust = 0.45)) +
    labs(color = '',
         title = gsub('Asian and Pacific Islander', 'API', 
                      factor(x$fit$race_eth_cat, race_eth_raw, race_eth_long)),
         y = paste(cart_labels, collapse = ' / '))
}

cart_plts <- lapply(results, cart_forest_plot)

# Combine plots
wrap_plots(cart_plts, ncol = 3)
ggsave(here('ad-analysis','Figures','app-cart-pd.png'), width = 8.5, height = 6)


# Table of partial dependence estimates
cart_sum <- bind_rows(lapply(results, '[[', 'cart_pd')) |>
  left_join(params, by = join_by(key)) |>
  filter(key %in% keys)

# Manually add columns for any conditions that did not make the cut
for (c in var_raw[3:length(var_raw)]) {
  if (!(c %in% colnames(cart_sum))) cart_sum[[c]] <- NA
}

cart_sum <- cart_sum |>
  select(race_eth_cat, any_of(var_raw), est, lcl, ucl, Pr, R2) |>
  mutate(across(any_of(var_raw), \(v) factor(v, levels = c(NA, 0, 1), 
                                             labels = c('', '-', '+'), exclude = NULL))) |>
  mutate(race_eth_cat = factor(race_eth_cat, race_eth_raw, race_eth_short)) |>
  mutate(val = paste0(format(round(est, 2), nsmall = 2), ' (',
                      format(round(lcl, 2), nsmall = 2), ', ', 
                      format(round(ucl, 2), nsmall = 2), ')')) |>
  select(race_eth_cat, any_of(var_raw), val, Pr, R2) |>
  arrange(race_eth_cat)
  
# LaTeX code
cart_sum_tbl <- cart_sum |>
  mutate(race_eth_cat = cell_spec(race_eth_cat, 'latex', angle = 90)) |>
  kbl(format = 'latex', booktabs = TRUE, escape = FALSE,
      digits = 2, align = c('cccccccccc'),
      label = 'app-cart-pd-tbl',
      caption = 'Lower Dimensional CART Summary Partial Dependence Estimates',
      col.names = c('Subgroup',
                    var_short[3:length(var_short)],
                    '$\\exp{\\left( \\bar{\\tau} \\right)}^a$',
                    '$\\Pr\\left(\\bar{\\tau} > 0 \\right)$',
                    '$R^2$ $^b$')) |>
  kable_styling(font_size = 9) |>
  column_spec(1, border_right = TRUE) |>
  collapse_rows(1, latex_hline = 'major') |>
  footnote(alphabet = c('$\\\\bar{\\\\tau}$: Partial average exposure effect. Posterior mean and 95\\\\% credible interval presented.',
                        'Summary $R^2$.'),
           general = c('HISP: Hispanic, NHW: Non-Hispanic White, NHB: Non-Hispanic Black, NHAPI: Non-Hispanic Asian and ',
                       'Pacific Islander, NHO: Non-Hispanic Other.'),
           general_title = '',
           footnote_order = c('alphabet','general'),
           escape = FALSE)

# Write to file
write_lines(cart_sum_tbl, here('ad-analysis','Tables','app-cart-pd-tbl.txt'))


# Distribution of Point Estimate Predictions ------------------------------

lapply(results, \(x) data.frame(key = x$key, est = x$fit$lambda_est)) |>
  bind_rows() |>
  left_join(params, by = join_by(key)) |>
  mutate(race_eth_cat = factor(race_eth_cat, race_eth_raw, race_eth_long)) |>
  filter(key %in% keys) |>
  ggplot(aes(x = est, y = after_stat(density), 
             fill = race_eth_cat, color = race_eth_cat,
             group = race_eth_cat)) +
  geom_density(alpha = 0.2, position = 'identity', adjust = 3) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) +
  labs(x = 'Posterior Mean Individual Exposure Effect',
       color = 'Subgroup',
       fill = 'Subgroup')
ggsave(here('ad-analysis','Figures','app-preds-density.png'), 
       width = 6, height = 4)


# Assess Convergence ------------------------------------------------------

# Trace Plots for select parameters
lapply(results, \(x) data.frame(key = x$key, 
                                sigma_mu = x$fit$sigma_mu,
                                loglik = x$fit$loglik,
                                avg_prediction = x$fit$lambda_mean_overall,
                                avg_num_nodes = x$fit$avg_num_nodes)) |>
  bind_rows() |>
  mutate(iter = row_number(), .by = key) |>
  left_join(params, by = join_by(key)) |>
  filter(key %in% keys) |>
  pivot_longer(cols = c(sigma_mu, avg_prediction, avg_num_nodes),
               names_to = 'Parameter', values_to = 'Value') |>
  mutate(Parameter = factor(Parameter, levels = c('sigma_mu','avg_prediction','avg_num_nodes'),
                            labels = c('sigma[mu]',
                                       'bar(tau)',
                                       'avg_num_nodes')),
         race_eth_long = factor(race_eth_cat, race_eth_raw, race_eth_long),
         race_eth_cat = factor(race_eth_cat, race_eth_raw, race_eth_short)) |>
  ggplot(aes(x = iter, y = Value, color = race_eth_long)) +
  geom_line() +
  facet_grid(Parameter~race_eth_cat, scales = 'free', 
             labeller = labeller(Parameter = label_parsed, race_eth_cat = label_value)) +
  scale_x_continuous(breaks = c(0, 500, 1000)) +
  theme_bw() +
  theme(legend.position = 'bottom',
        text = element_text(size = 13)) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
  labs(x = 'Posterior Sample Index',
       color = 'Subgroup')
ggsave(here('ad-analysis','Figures','app-diag-trace.png'), 
       width = 9, height = 7)


# Compare to Standard Conditional Logistic Regression ---------------------

# Load the data
cco <- read_csv(here('ad-analysis','Data','Clean','cco.csv'))

# Loop through models and compare the average exposure effect from CL-BART
# to the estimated homogeneous OR from standard conditional logistic regression
for (i in 1:length(results)) {
  
  r <- results[[i]]
  
  if (r$fit$race_eth_cat != 'All') {
    cco_sub <- filter(cco, RACE_ETH == r$fit$race_eth_cat)
  } else {
    cco_sub <- cco
  }
  
  # Create confounder model matrix
  x <- cbind(cco_sub$HOLIDAY,
             splines::ns(cco_sub$DP3DMA, 4),
             splines::ns(cco_sub$AVG3DMA, 4))
  colnames(x) <- c('HOLIDAY', paste0('DP3DMAns', 1:4), paste0('AVG3DMAns', 1:4))
  
  # Outcome, primary exposure, and strata
  y <- cco_sub$OUTCOME
  z <- cco_sub$AVGHW
  strata <- match(cco_sub$ID, unique(cco_sub$ID))
  
  clogit_fit <- bayes_clr(cbind(x, z), y, strata, num_burn = 5000, num_save = 1000, num_thin = 5)
  clogit_est <- colMeans(clogit_fit$beta)['z']
  clogit_lcl <- colQuants(clogit_fit$beta, 0.025)['z']
  clogit_ucl <- colQuants(clogit_fit$beta, 0.975)['z']

  clbart_est <- r$fit$lambda_mean_overall
  
  results[[i]]$comp <- data.frame(
    key = r$key,
    type = c('clbart','clogit'),
    est = c(median(clbart_est), clogit_est),
    lcl = c(quantile(clbart_est, 0.025), clogit_lcl),
    ucl = c(quantile(clbart_est, 0.975), clogit_ucl),
    waic = c(r$fit$WAIC, clogit_fit$WAIC)
    )
}

# Create table
clr_v_clbart <- bind_rows(lapply(results, '[[', 'comp')) |>
  left_join(params, by = join_by(key)) |>
  mutate(val = paste0(format(round(exp(est), 2), nsmall = 2), ' (',
                      format(round(exp(lcl), 2), nsmall = 2), ', ', 
                      format(round(exp(ucl), 2), nsmall = 2), ')'),
         waic = format(round(waic), big.mark = ',', scientific = FALSE)) |>
  select(race_eth_cat, type, val, waic) |>
  pivot_wider(id_cols = race_eth_cat, names_from = type, values_from = c(val, waic)) |>
  select(race_eth_cat, val_clogit, waic_clogit, val_clbart, waic_clbart) |>
  mutate(race_eth_cat = factor(race_eth_cat, race_eth_raw, race_eth_long)) |>
  mutate(race_eth_cat = gsub('Asian and Pacific Islander','API', race_eth_cat)) |>
  kbl(format = 'latex', booktabs = TRUE, linesep = '', escape = FALSE,
      digits = 0, align = c('lcrcr'),
      label = 'app-clr-comparison',
      caption = 'Homogeneous vs. Average Heterogeneous Estimate for Heat Wave Effect',
      col.names = c('Subgroup',
                    '$\\exp{ \\left( \\hat{\\tau} \\right)}^a$ (95\\% CI)','WAIC',
                    '$\\exp{ \\left( \\bar{\\tau} \\right)}^b$ (95\\% CrI)','WAIC')) |>
  kable_styling(font_size = 12) |>
  add_header_above(c(' ' = 1, 'CLR' = 2, 'CL-BART' = 2)) |>
  footnote(general = c('API: Asian and Pacific Islander.',
                       'CLR: Conditional Logistic Regression.',
                       'CI: Confidence Interval. CrI: Posterior Credible Interval.'),
           alphabet = c('Estimated odds-ratio from frequentist CLR with no effect moderators.',
                        'Average exposure effect from CL-BART model.'),
           general_title = '')

# Write to file
write_lines(clr_v_clbart, here('ad-analysis','Tables','app-clr-comparison.txt')) 


# Timing ------------------------------------------------------------------

key   <- sapply(results, \(x) x$key)
time  <- sapply(results, \(x) x$fit$time) / 60 # hours
n     <- sapply(results, \(x) length(x$cart$y))

# Timing
time_tbl <- data.frame(key, n, time) |>
  left_join(params, by = join_by(key)) |>
  filter(key %in% keys) |>
  mutate(race_eth_cat = factor(race_eth_cat, race_eth_raw, race_eth_long)) |>
  select(race_eth_cat, n, time) |>
  arrange(race_eth_cat) |>
  kbl(format = 'latex', booktabs = TRUE, digits = 2, escape = FALSE,
      caption = 'Application Model Runtime', label = 'app-timing', linesep = '',
      col.names = c('Subgroup','Sample Size','Runtime$^a$')) |>
  kable_styling(font_size = 12) |>
  footnote(alphabet = c('Time taken to run one chain of a 25-tree CL-BART model (subgroups) or 100-tree CL-BART model (overall), in hours. All models were run using 1 CPU on the high performance computing cluster at the Rollins School of Public Health, Emory University.'),
           threeparttable = TRUE)

# Write to file
write_lines(time_tbl, here('ad-analysis','Tables','app-timing.txt'))
