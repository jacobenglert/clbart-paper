# Program Name: describe-data.R
# Description:  Create descriptive tables for manuscript.


# Load Packages -----------------------------------------------------------
library(tidyverse)
library(here)
library(gtsummary)
library(knitr)
library(kableExtra)
options(knitr.kable.NA = '')


# Create Labels -----------------------------------------------------------

# Raw variable names
var_raw <- c('CHF','CKD','COPD','DEPRESSION','DIAB','HT','HYPERLIP')
var_short <- c('CHF','CKD','COPD','DEP','DIAB','HT','HLD')
var_long <- c('CHF','CKD','COPD','Depression','Diabetes','Hypertension','Hyperlipidemia')

# Raw race/ethnicity levels
race_eth_raw <- c('HISP','NHW','NHB','NHAPI','NHO')
race_eth_long <- c('Hispanic',
                   'Non-Hispanic White',
                   'Non-Hispanic Black',
                   'Non-Hispanic Asian and Pacific Islander',
                   'Non-Hispanic Other')


# Load Data ---------------------------------------------------------------

# Remove records with missing data
# Remove individuals who did not experience a heat wave
# Add variable counting number of comorbid conditions
cco <- read_csv(here('ad-analysis','Data','Clean','cco.csv')) |>
  na.omit() |>
  filter(var(AVGHW) > 0, .by = ID) |>
  filter(row_number() == 1, .by = ID) |>
  mutate(n_conditions = sum(across(all_of(var_raw))), .by = ID) |>
  mutate(n = 1)

cco$sex_cat <- factor(cco$FEMALE, 0:1, c('Male','Female'))
  

# Descriptive Statistics for Sample - Overall -----------------------------

t1 <- cco |>
  mutate(race_eth_cat = factor(RACE_ETH, race_eth_raw, race_eth_long)) |>
  tbl_summary(include = c(n, race_eth_cat, sex_cat, AGE, n_conditions, all_of(var_raw)),
              label = list(n = 'N',
                           race_eth_cat = 'Race/Ethnicity',
                           sex_cat = 'Sex',
                           AGE = 'Age, yrs',
                           n_conditions = 'Number of Comorbid Conditions',
                           CHF = 'Congestive Heart Failure (CHF)',
                           CKD = 'Chronic Kidney Disease (CKD)',
                           COPD = 'Chronic Obstructive Pulmonary Disease (COPD)',
                           DEPRESSION = 'Depression (DEP)',
                           DIAB = 'Diabetes (DIAB)',
                           HYPERLIP = 'Hyperlipidemia (HLD)',
                           HT = 'Hypertension (HT)'),
              statistic = n ~ "{N_obs}",
              type = list(n ~ 'continuous', n_conditions ~ 'continuous'),
              digits = list(n ~ 0,
                            AGE ~ 0,
                            n_conditions ~ 0,
                            all_categorical() ~ c(0,1))) |>
  modify_header(all_stat_cols() ~ "**{level}**")

# LaTeX code
row_breaks <- t1$table_body |> 
  summarise(last_row = n(), .by = variable) |>
  mutate(last_row = cumsum(last_row)) |>
  pull(last_row)

t1_kbl <- t1$table_body |>
  mutate(label = case_when((row_type == 'label') & (label %in% c('Age, yrs','Number of Comorbid Conditions')) ~ paste0(label,'$^c$'),
                           (row_type == 'label') & (label == 'N') ~ paste0(label,'$^a$'),
                           row_type == 'label' ~ paste0(label,'$^b$'),
                           row_type == 'level' ~ label)) |>
  mutate(stat_0 = gsub('%','\\\\%', stat_0)) |>
  select(label, stat_0) |>
  kbl(format = 'latex', booktabs = TRUE, escape = FALSE,
      align = c('lc'), linesep = '',
      label = 'app-overall-descriptives',
      caption = 'Descriptive Statistics for AD ED Patients, CA 2005-2015',
      col.names = c('Characteristic', 'Overall')) |>
  kable_styling(font_size = 12) |>
  footnote(alphabet = c('N; ','N (%); ','Median (IQR).'),
           footnote_as_chunk = TRUE) |>
  add_indent(positions = c(3:7,9:10))

for (row in row_breaks) t1_kbl <- t1_kbl |> row_spec(row, extra_latex_after = "\\addlinespace[2pt]")

write_lines(t1_kbl, here('ad-analysis','Tables','app-overall-descriptives.txt')) 


# Descriptive Statistics for Sample - Subgroups ---------------------------

t1_groups <- cco |>
  mutate(race_eth_cat = factor(RACE_ETH, race_eth_raw)) |>
  tbl_summary(by = race_eth_cat,
              include = c(n, sex_cat, AGE, n_conditions, all_of(var_raw)),
              label = list(sex_cat = 'Sex',
                           AGE = 'Age, yrs',
                           n_conditions = 'Number of Comorbid Conditions',
                           CHF = 'CHF',
                           CKD = 'CKD',
                           COPD = 'COPD',
                           DEPRESSION = 'Depression',
                           DIAB = 'Diabetes',
                           HYPERLIP = 'Hyperlipidemia',
                           HT = 'Hypertension'),
              statistic = n ~ "{N_obs}",
              type = list(n ~ 'continuous', n_conditions ~ 'continuous'),
              digits = list(n ~ 0,
                            AGE ~ 0,
                            n_conditions ~ 0,
                            all_categorical() ~ c(0,1))) |>
  modify_header(all_stat_cols() ~ "**{level}**")

# LaTeX Code
row_breaks <- t1_groups$table_body |> 
  summarise(last_row = n(), .by = variable) |>
  mutate(last_row = cumsum(last_row)) |>
  pull(last_row)

options(knitr.kable.NA = '')
t1_groups_kbl <- t1_groups |>
  kbl(format = 'latex',
      booktabs = TRUE,
      align = c('lrrrrr'),
      linesep = '',
      label = 'app-subgroup-descriptives',
      caption = 'Descriptive Statistics for AD ED Patients by Subgroup, CA 2005-2015',
      col.names = c('Characteristic', race_eth_raw)) |>
  kable_styling(font_size = 12) |>
  landscape() |>
  footnote(general = c('n: sample size.',
                       'Median (IQR) reported for age. number of conditions.',
                       'HISP: Hispanic, NHW: Non-Hispanic White, NHB: Non-Hispanic Black, NHAPI: Non-Hispanic Asian and ',
                       'Pacific Islander, NHO: Non-Hispanic Other.',
                       'CHF: Congestive Heart Failure, CKD: Chronic Kidney Disease, COPD: Chronic Obstructive Pulmonary Disease.'),
           general_title = '') |>
  add_indent(c(3:4))

for (row in row_breaks) t1_groups_kbl <- t1_groups_kbl |> row_spec(row, extra_latex_after = "\\addlinespace[2pt]")

write_lines(t1_groups_kbl, here('ad-analysis','Tables','app-subgroup-descriptives.txt'))


# Most common pairings ----------------------------------------------------

# Look at two-way frequencies
contingency_tables <- list()

# Create all possible combinations of two variables
combinations <- combn(names(cco[setdiff(var_raw,c('FEMALE','AGE'))]), 2)

# Loop through each combination and create a contingency table
for (i in 1:ncol(combinations)) {
  var1 <- combinations[1, i]
  var2 <- combinations[2, i]
  contingency_table <- table(cco[[var1]], cco[[var2]]) / nrow(cco)
  contingency_tables[[paste(var1, var2, sep = "_")]] <- contingency_table
}

lapply(contingency_tables, \(x) x[2,2]) |>
  bind_rows(.id = 'pair') |>
  pivot_longer(cols = everything()) |>
  arrange(desc(value))
