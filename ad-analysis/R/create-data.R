# Program Name: create-data.R
# Description:  Simulate health data from a case-crossover design using the
#               supplied exposure data.

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(here)
library(zoo)


# Load Reference Data -----------------------------------------------------

# Load exposure data and compute moving averages and heat wave indicators.
# Note: This simulated data only uses data from a single ZIP code (90011 - Los 
#       Angeles) for the sake of simplicity. In practice, moving averages are 
#       calculated within ZIP code and heat waves are defined relative to the 
#       ZIP code specific 95th percentile of temperature.
e_data <- read_csv(here('ad-analysis','Data','Raw','exposure.csv')) |>
  mutate(Year = year(DATE), 
         Month = month(DATE),
         DoW = weekdays(DATE),
         DoY = yday(DATE)) |>
  arrange(DATE) |>
  
  # Identify "hot days"
  mutate(AVG_HOTDAY = ifelse(AVG > quantile(AVG, 0.95, na.rm = TRUE), 1, 0)) |>
  
  # Create indicator for heat waves and compute moving averages
  mutate(AVGHW = rollsumr(AVG_HOTDAY, 2, fill = NA),
         AVG3DMA = rollmeanr(AVG, 3, fill = NA),
         DP3DMA = rollmeanr(DP, 3, fill = NA)) |>
  mutate(AVGHW = ifelse(AVGHW == 2, 1, 0)) |>
  
  # Only include time strata having at least one heat wave day and one not.
  # Time strata without this are not informative of the heat wave effect.
  filter(var(AVGHW) > 0, .by = c(Year, Month, DoW)) |>
  
  # Arrange by time strata
  arrange(Year, Month, DoW)

# Created variables:
#   - AVG3DMA:  3-day moving average for average daily temperature
#   - DP3DMA:   3-day moving average for daily dew point
#   - AVGHW:    heatwave indicator (two or more consecutive days above the 
#               95th percentile of zip-code specific average temperature)

# Load federal holiday indicators
t_data <- read_csv(here('ad-analysis','Data','Raw','holiday.csv'))


# Simulate Patient Data ---------------------------------------------------

# Set seed for reproducibility
set.seed(1)

# Number of cases to simulate
n <- 1000

# Patient characteristics
FEMALE    <- rbinom(n, 1, 0.5)
AGE       <- round(rnorm(n, 80, 5))
RACE_ETH  <- sample(c('HISP','NHW','NHB','NHAPI','NHO'), n, replace = TRUE,
                    prob = c(0.17, 0.64, 0.08, 0.08, 0.03))
CHF       <- rbinom(n, 1, 0.16)
CKD       <- rbinom(n, 1, 0.25)
COPD      <- rbinom(n, 1, 0.12)
DEPRESSION  <- rbinom(n, 1, 0.13)
DIAB      <- rbinom(n, 1, 0.25)
HT        <- rbinom(n, 1, 0.65)
HYPERLIP  <- rbinom(n, 1, 0.30)

# All dates contained within an informative time strata
dates <- unique(e_data$DATE)

# Find referent windows for all dates
find_window <- function (d) {
  year_matches  <- year(dates) == year(d)
  month_matches <- month(dates) == month(d)
  wday_matches  <- weekdays(dates) == weekdays(d)
  dates[year_matches & month_matches & wday_matches]
}
windows <- sapply(dates, find_window)

# Randomly assign each case a referent window
window <- sample(windows, n, replace = TRUE)
n_i <- sapply(window, length)

# Create health data frame
h_data <- data.frame(ID = 1:n, FEMALE, AGE, RACE_ETH, 
                     CHF, CKD, COPD, DEPRESSION, DIAB, HT, HYPERLIP)

# Match each observation to itself n_i times using its referent window
h_data <- h_data[rep(1:n, times = n_i),]
h_data$DATE <- as.Date(unlist(window))


# Create Case-Crossover Dataset -------------------------------------------

# Start with health dataset
cco <- h_data |>
  
  # 1) Merge in exposure information and federal holiday indicators
  left_join(e_data, by = join_by('DATE')) |>
  left_join(t_data, by = join_by('DATE')) |>
  
  # 2) Set probability of each day being the case within each strata.
  #     (all equally likely in simulated dataset, i.e., no signal)
  mutate(case_prob = 1 / max(row_number()), .by = ID) |>

  # 3) Select and rename variables
  select(ID, FEMALE, AGE, RACE_ETH, DATE,
         AVG3DMA, DP3DMA, AVGHW, HOLIDAY, 
         CHF, CKD, COPD, DEPRESSION, DIAB, HT, HYPERLIP,
         case_prob)

# Simulate outcome
cco$OUTCOME <- unlist(tapply(cco$case_prob, cco$ID, rmultinom, n = 1, size = 1))

# Export
write_csv(cco, here('ad-analysis','Data','Clean','cco.csv'))
