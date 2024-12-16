
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Reproducibility Materials for CL-BART Manuscript

The purpose of this repository is to share how the results presented
within the manuscript were obtained. Specifically, this repository
includes three sub-folders, each pertaining to a different component of
the manuscript:

- `cart-sim`: contains R and shell scripts to run the CART scenario
  simulation on an HPC Cluster, and an R script to generate the
  corresponding tables and figures.
- `friedman-sim`: contains R and shell scripts to run the Friedman
  scenario simulation on an HPC Cluster, and an R script to generate the
  corresponding tables and figures.
- `ad-analysis`: contains an R script to generate data to mimic the
  health data, as well as more R and shell scripts to run the analyses
  on an HPC Cluster, and an R script to generate the corresponding
  tables and figures.

Both of the simulation studies and the application were run entirely on
the High Performance Computing (HPC) Cluster at \[redacted\] using a
Slurm Workload Manager. The exact number of compute nodes used varied
based on availability. If you do attempt to reproduce all of the
results, any shell (.sh) scripts in this repository should be updated to
work within the framework of the HPC cluster available to you.

Alternatively, one can run a single iteration of either of the
simulation studies, or a single model using synthetic data, by directly
using the `run-\*.R` scripts located within the R/ sub-folders. This
will require one to specify a random seed and the desired parameter
settings (see details for Option 1 below).

## Software

The main CL-BART models are fit using the `clbart` R package. When many
trees are specified or the dataset is very large, the model make take a
while to fit on a personal computer. The work also makes use of the
`pdpd` R package. These packages may be installed from this repository
using the following code:

``` r
# install.packages("devtools")
devtools::install_local("packages/clbart", build_vignettes = TRUE)
devtools::install_local("packages/pdpd")
```

Other packages used throughout this repository include `zoo`, `splines`,
and `rpart` for the application, as well as `tidyverse`, `here`,
`knitr`, `kableExtra`, and `patchwork` for various data management and
figure/table creation tasks. All of these are available on CRAN.

## Simulation Study

### Scenario 1: CART Simulation

#### Option 1: Run a single iteration of one parameter setting

1.  Open the `cart-sim/R/run-cart-sim.R` file and comment out the
    following lines at the top of the script:

    ``` r
    # args <- commandArgs(trailingOnly = TRUE)
    # key <- as.numeric(args[1])
    # seed <- as.numeric(args[2])
    ```

<!-- -->

2.  Manually set the `key` and `seed` objects. The seed is passed to
    `set.seed()`. The `key` determines which parameter settings will be
    used. To look up a key, refer to the table at
    `cart-sim/Params/cart-sim-params.csv`. Each key corresponds to a
    particular parameter setting used within the
    manuscript/supplementary material, including values for $M$
    (`num_tree`), $k$, $\gamma$ (`base`), and $\xi$ (`power`). The key
    also determines whether or not to update splitting probabilities
    using the Dirichlet prior (requires both `update_s` and
    `update_alpha`), and some additional information such as the number
    of MCMC iterations to run the model for. The R script will
    automatically load the parameter settings into the environment when
    a valid `key` from the table is specified.

3.  Run the rest of the script.

#### Option 2: Reproduce tables and figures in the manuscript

1.  Open and run the `cart-sim/R/summarize-cart-sim.R` file. This script
    reads in the compiled results of the CART simulation stored in
    `cart-sim/Results/cart-sim-results.rds` and produces the following
    tables ($\LaTeX$ code) and figures:
    - Main Text: Table 1, Figure 1A
    - Supplementary Material: Table S.2, Table S.3, Table S.4, Figure
      S.1, Figure S.2, Figure S.3

#### Option 3: Reproduce the entire simulation study (HPC cluster required)

To reproduce the results of the CART simulation from the paper, run the
following (all within the `cart-sim` sub-folder):

1.  `R/set-cart-sim-params.R`
    - Generates the data frame of simulation parameter settings, assigns
      each setting a key, and stores it in `Params/cart-sim-params.csv`.
2.  `HPC/run-cart-sim-study.sh`
    - Loops through each simulation parameter setting (“key”) and calls
      `HPC/run-cart-sim.sh` for each one. `HPC/run-cart-sim.sh` launches
      an array job of 200 simulations for the given setting by passing
      the key and seed to `R/run-cart-sim.R`. Results are stored in
      `Results/temp/key-seed.rds`.
    - Upon completion of all simulation iterations, calls
      `HPC/compile-cart-sim.sh`, which calls `R/compile-cart-sim.R`,
      which combines and condenses the results into a single R object
      stored in `Results/cart-sim-results.rds`.
3.  `R/summarize-cart-sim.R`
    - Generates the figures and $\LaTeX$ code for tables as described in
      Option 2.

### Scenario 2: Friedman Simulation

#### Option 1: Run a single iteration of one parameter setting

1.  Open the `friedman-sim/R/run-friedman-sim.R` file and comment out
    the following lines at the top of the script:

    ``` r
    # args <- commandArgs(trailingOnly = TRUE)
    # key <- as.numeric(args[1])
    # seed <- as.numeric(args[2])
    ```

<!-- -->

2.  Manually set the `key` and `seed` objects. The seed is passed to
    `set.seed()`. The `key` determines which parameter settings will be
    used. To look up a key, open the table at
    `friedman-sim/Params/friedman-sim-params.csv`. Each key corresponds
    to a particular parameter setting used within the
    manuscript/supplementary material, including values for $M$
    (`num_tree`), $k$, $\gamma$ (`base`), and $\xi$ (`power`). The key
    also determines whether or not to update splitting probabilities
    using the Dirichlet prior (requires both `update_s` and
    `update_alpha`), and some additional information such as the number
    of MCMC iterations to run the model for. The R script will
    automatically load the parameter settings into the environment when
    a valid key from the table is specified.

3.  Run the rest of the script.

#### Option 2: Reproduce tables and figures in the manuscript

1.  Open and run the `friedman-sim/R/summarize-friedman-sim.R` file.
    This script reads in the compiled results of the Friedman simulation
    stored in `friedman-sim/Results/friedman-sim-results.rds` and
    produces the following tables ($\LaTeX$ code) and figures:
    - Main Text: Table 2, Figure 1B
    - Supplementary Material: Table S.5, Table S.6, Table S.7, Figure
      S.4, Figure S.5, Figure S.6, Figures S.7

#### Option 3: Reproduce the entire simulation study (HPC cluster required)

To reproduce the results of the Friedman simulation from the paper, run
the following (all within the `friedman-sim` sub-folder):

1.  `R/set-friedman-sim-params.R`
    - Generates the data frame of simulation parameter settings, assigns
      each setting a key, and stores it in
      `Params/friedman-sim-params.csv`.
2.  `HPC/run-friedman-sim-study.sh`
    - Loops through each simulation parameter setting (“key”) and calls
      `HPC/run-friedman-sim.sh` for each one. `HPC/run-friedman-sim.sh`
      launches an array job of 200 simulations for the given setting by
      passing the key and seed to `R/run-friedman-sim.R`. Results are
      stored in `Results/temp/key-seed.rds`.
    - Upon completion of all simulation iterations, calls
      `HPC/compile-friedman-sim.sh`, which calls
      `R/compile-friedman-sim.R`, which combines and condenses the
      results into a single R object stored in
      `Results/friedman-sim-results.rds`.
3.  `R/summarize-friedman-sim.R`
    - Generates the figures and $\LaTeX$ code for tables as described in
      Option 2.

## Application

### Data

#### Notes on Data Availability

- Health Data: The application involves analyzing emergency department
  visits from the state of California. We do not have permission to
  share this data due to the Data Use Agreement, but it may be requested
  from the [California Office of Statewide Health Planning and
  Development](https://hcai.ca.gov/data/request-data/research-data-request-information/).
  We provide code to simulate health data which mimics this.
- Exposure Data: The meteorology data is derived from [Daymet version
  4.1](), which is publicly available. We use a spatially aggregated
  version of this data product at the ZIP code level in our analyses. A
  subset of the processed data has been made available.

#### Description of Synthetic Data

While the real data are unavailable, it is possible to see how the
analyses are performed using synthetic data. A synthetic dataset
mimicking a case-crossover design is created by
`ad-analysis/R/create-data.R`. The steps for creating this dataset are
as follows:

1.  Simulates demographic and comorbid condition covariates for 1,000
    individuals based on the descriptive statistics presented in Table 3
    of the manuscript.
2.  Load external external datasets from the `ad-analysis/Data/Raw`
    sub-folder:
    - `exposure.csv`: contains ZIP code, calendar date (2013-2015),
      average temperature (°C), and dew-point temperature (°C). This
      dataset has been filtered to only include one ZIP code (90011 -
      Los Angeles, CA).
    - `holiday.csv`: contains calendar date (2013-2015) and a federal
      holiday indicator variable.
3.  Calculate moving averages for the temperature data and define a new
    binary heat wave indicator variable as described in the manuscript.
    1.  Note: there is only one ZIP code included in the exposure data
        for simplicity. In practice, these calculations are
        location-specific.
4.  Randomly assign each individual to an informative referent window
    under the time-stratified case-crossover design (e.g., a set of
    dates sharing the same day-of-week and month). Informative referent
    windows include those which contain at least one heat wave day and
    one non-heat wave day.
    1.  Note: in practice, individuals not within an informative
        referent window are excluded from the analysis because they do
        not contribute to the estimation of the exposure association.
5.  Within each referent window, randomly select a single day to be the
    “case”.
6.  Export the resulting dataset to `ad-analysis/Data/Clean/cco.csv`.

### Running the Analysis

#### Descriptive Statistics

Run `ad-analysis/R/describe-data.R` to generate descriptive statistics
similar to Table 3 and Table S.8, but based on the synthetic data.

#### Option 1: Local Demo

The easiest way to experiment with/run the code used in the real data
analysis is to run the `ad-analysis/R/run-analysis-short.R` script. This
script does the following:

1.  Reads in the synthetic dataset stored at
    `ad-analysis/Data/Clean/cco.csv`.
2.  Fits one CL-BART model to each subgroup, and one model overall,
    using only the hyperparameter settings used in the main text (with
    the exception that the number of trees has been reduced to 5 by
    default to run faster).
3.  For each model, compute 1) the marginal partial dependence for each
    level of each condition, 2) the difference in marginal partial
    dependence for each condition (e.g. log odds ratio for CKD vs. no
    CKD), and 3) the lower-dimensional CART summary informed partial
    dependence.
4.  Exports the results to `ad-analysis/Results/ad-results.rds`.

The figures and tables ($\LaTeX$ code) from the manuscript are included
by default in `ad-analysis/Figures/` and `ad-analysis/Tables`. To see
how these were generated, run the `ad-analysis/R/summarize-results.R`
script. This script reads in the `ad-analysis/Results/ad-results.rds`
file and generates the following:

- Main Text: Table 4, Figure 3, Figure 4

- Supplementary Material: Table S.9, Table S.10, Table S.11, Table S.12,
  Figure S.8, Figure S.9, Figure S.10

#### Option 2: Reproduce analysis results for all hyperparameter settings (HPC cluster required)

This section outlines how to run all hyperparameter settings included in
`ad-analysis/Params/params.csv` on the synthetic data (i.e., not just
those reported in the paper) via an HPC cluster. To reproduce these
results, run the following (all within the `ad-analysis` sub-folder):

1.  `R/set-params.R`
    - Generates a data frame of different hyperparameter settings and
      subgroup analyses to be conducted and stores the result in
      `ad-analysis/Params/params.csv`. While we tried all settings, in
      the paper we only report one hyperparameter setting for each
      subgroup: $M = 25, k= 1, \gamma = 0.95$, and $\xi = 2$ (except for
      the overall model which uses $M = 100$).

<!-- -->

1.  `HPC/run-ad-analysis.sh`
    - Loops through each hyperparameter/subgroup setting (“key”) and
      calls `R/run-ad-analysis.R` for each one. This script fits all
      models and computes all partial dependence statistics and
      lower-dimensional CART summaries. The results for each model are
      stored in `Results/temp/key.rds`.
2.  `HPC/compile-ad-analysis.sh`
    - Once all of the jobs have ran, this script calls
      `R/compile-ad-analysis.R`, which combines and condenses the
      results into a single R object stored in `Results/ad-results.rds`.
3.  `R/summarize-results.R`
    - Generates the figures and $\LaTeX$ code for tables listed in
      Option 1 (stored in `Figures/*.png` and `Tables/*.txt`,
      respectively).
