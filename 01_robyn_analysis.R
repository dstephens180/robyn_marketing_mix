# ROBYN ANALYSIS ----
# **** ----

# LIBRARIES ----

# *Robyn Dependencies ----
library(data.table) 
library(stringr) 
library(lubridate) 
library(doFuture)
library(doRNG)
library(foreach) 
library(glmnet) 
library(car) 
library(StanHeaders)
library(prophet)
library(rstan)
library(ggplot2)
library(gridExtra)
library(grid)
library(ggpubr)
library(see)
library(PerformanceAnalytics)
library(nloptr)
library(minpack.lm)
library(rPref)
library(reticulate)
library(rstudioapi)

# * Tidyverse ----
library(tidyverse)
library(janitor)
library(timetk)

options(scipen = 999)

# PYTHON ENV ----
Sys.setenv(RETICULATE_PYTHON = "C:\\Users\\DavidStephens\\anaconda3\\envs\\robyn_marketing_mix/python.exe")
use_condaenv('robyn_marketing_mix', required = TRUE)

py_discover_config()

# SOURCED SCRIPTS ---- 

source("robyn_source/fb_robyn.func.R")
source("robyn_source/fb_robyn.optm.R")

# STEP 1: DATA PREP & VISUALIZATION ----

# 1.1 DATA ----

# * Germany Adstock Simulation ----
simulated_data_tbl <- read_csv(
  file = 'robyn_source/de_simulated_data.csv',
  name_repair = make_clean_names
) %>%
  mutate(date = date + years(4),
         date = ceiling_date(date, unit = "week", week_start = 1)) %>%
  mutate(across(where(is.numeric), ~./10)) %>%
  filter(date <= '2023-05-31') %>%
  select(-tv_s, -ooh_s)
  

  

simulated_data_tbl %>% glimpse()

# * Holidays Supporing Data ----

holidays_tbl <- read_csv("robyn_source/holidays.csv")

holidays_tbl


# 1.2 VISUALIZATION ----

# * Visualize Time Series ----
simulated_data_tbl %>% 
  plot_time_series(
    date, revenue, .smooth = F
  )

# * TS Regression (All Features, No Date) ----
simulated_data_tbl %>%
  plot_time_series_regression(
    date, 
    .formula = revenue ~ . - date,
    .show_summary = TRUE
  )

# * Cross-Correlation ----
simulated_data_tbl %>%
  plot_acf_diagnostics(
    date, revenue, 
    .ccf_vars = c(competitor_sales_b)
  )

# STEP 2: MODELING -----

# **** ROBYN **** ----

# 2.1 MODELING PARAMETERS -----

# * SET INPUT VARIABLES ----

set_country        <- "DE" # Note - These are the alpha-2 country codes.

set_dateVarName    <- "date"

set_depVarName     <- "revenue"

activate_prophet   <- TRUE
set_prophet        <- c("trend", "season", "holiday")
set_prophetVarSign <- c("default", "default", "default")

# MEDIA IMPRESSION/SPEND NOTES:
# - I = Impression data. If don't have impressions, use spend for VarName. 
# - S = Spend data. The amount the company spent in the time frame being analyzed (e.g. weekly)
# - Sign: Positive. We expect all variables to have a positive impact on revenue.
set_mediaVarName   <- c("print_s", "facebook_i", "search_clicks_p")
set_mediaVarSign   <- c("positive", "positive", "positive")
set_mediaSpendName <- c("print_s", "facebook_s", "search_s")

# BASELINE NOTES:
# - B = Baseline. 
# - can be weather data, promotions, external data, etc.
activate_baseline  <- TRUE
set_baseVarName    <- c("competitor_sales_b")
set_baseVarSign    <- c("negative")

# CATEGORICAL DATA:
# - If any data is categorical, list names here. We don't have any. 
# - Could be a category like Promotions, with "promotion 1" "promotion 2"
set_factorVarName  <- c()




# * SET MODEL PARAMETERS -----
# - Recommended for production: 500 iter / 40 trials (we use 1 for time during lab)
# - Experimentation: Use lower to speed up modeling 

set_trainStartDate <- "2019-11-25"
adstock            <- "geometric" # geometric or weibull.
set_iter           <- 500
set_hyperOptimAlgo <- "DiscreteOnePlusOne"
set_trial          <- 1

# * Adstock Curves ----
#   - Describes the latent effect of ads (Customers may not purchase same day of ad) 
#   - Decay rates may vary by channel

f.plotAdstockCurves(T)
f.plotResponseCurves(T)

# * SET HYPER PARAMETERS ----

local_name <- f.getHyperNames()
local_name

set_hyperBoundLocal <- list(
  facebook_i_alphas       = c(0.5, 3) # example bounds for alpha
  ,facebook_i_gammas      = c(0.3, 1) # example bounds for gamma
  ,facebook_i_thetas      = c(0, 0.3) # example bounds for theta
  
  # ,ooh_s_alphas           = c(0.5, 3)
  # ,ooh_s_gammas           = c(0.3, 1)
  # ,ooh_s_thetas           = c(0.1, 0.4)
  
  ,print_s_alphas         = c(0.5, 3)
  ,print_s_gammas         = c(0.3, 1)
  ,print_s_thetas         = c(0.1, 0.4)
  
  # ,tv_s_alphas            = c(0.5, 3)
  # ,tv_s_gammas            = c(0.3, 1)
  # ,tv_s_thetas            = c(0.3, 0.8)
  
  ,search_clicks_p_alphas = c(0.5, 3)  
  ,search_clicks_p_gammas = c(0.3, 1)
  ,search_clicks_p_thetas = c(0, 0.3)
)

# 2.2 CALIBRATION ----
# - If we have data to calibrate against, we can use to validate the model

activate_calibration <- FALSE

# 2.3 MODELING ----

# * PARALLEL PROCESSING ----

set_cores <- 6 

# * PREPARE INPUT ----
# must name it dt_name and it has to use data.table

dt_input    <- as.data.table(simulated_data_tbl)
dt_holidays <- as.data.table(holidays_tbl)

dt_mod <- f.inputWrangling()
dt_mod %>% as_tibble() 

# * RUN MODELS ----
# - Long running script!!!

model_output_collect <- f.robyn(
  set_hyperBoundLocal = set_hyperBoundLocal,
  optimizer_name      = set_hyperOptimAlgo,
  set_trial           = set_trial,
  set_cores           = set_cores
)


# * SAVE WORK ----
model_output_collect %>% write_rds(
  str_glue("{model_output_collect$folder_path}/model_output.rds")
)

model_output_collect



# 2.4 MODEL EVALUATION ----

model_output_collect$allSolutions

model_output_collect

model_output_collect$resultHypParam %>% glimpse()

best_model_nrmse_tbl <- model_output_collect$resultHypParam %>%
  as_tibble() %>%
  slice_min(nrmse)

best_model_nrmse_tbl %>% glimpse()

best_model_nrmse_tbl$solID

# * REVIEW THE PLOT ----

# TL: Waterfall Chart - Indicates the Effect on Response
# TR: Actual vs Predicted Response
# ML: Share of Spend vs Effect: If Effect > Spend, channel is not saturated
# MR: Identifies point of diminishing returns
# BL: Adstock Decay Rates. Hi Pct = Longer Decay Rate (Lasts Longer)
# BR: Residuals. Make sure centered around 0.

# 2.5 BUDGET ALLOCATION -----

optim_result <- f.budgetAllocator(
  modID    = best_model_nrmse_tbl$solID, 
  
  scenario = "max_historical_response", # c(max_historical_response, max_response_expected_spend)
  
  # scenario = "max_response_expected_spend",                          
  # expected_spend = 100000, # specify future spend volume. only applies when scenario = "max_response_expected_spend"
  # expected_spend_days = 90, # specify period for the future spend volumne in days. only applies when scenario = "max_response_expected_spend"
  
  channel_constr_low = c(0.60, 0.8, 0.65), # must be between 0.01-1 and has same length and order as set_mediaVarName
  channel_constr_up  = c(1.5, 2, 1.5) # not recommended to 'exaggerate' upper bounds. 1.5 means channel budget can increase to 150% of current level
)

optim_result

# 2.6 REVIEW BUDGET RESULTS -----

# TL: Initial vs Optimal Budget Allocation
# BL: Shows how the response will change if update based on TL Budget Allocation

# STEP 3: SHINY APP ----
# - LLPRO BONUS | MMM AUTOMATION APP

# LEARNING MORE ----
# Transforming your career in 3-steps
#   Step 1: ANALYST 
#   Step 2: DATA SCIENTIST
#   Step 3: SENIOR DATA SCIENTIST 



