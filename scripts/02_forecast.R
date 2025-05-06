#' ---
#' title: "Forecast Saxony and Thuringia"
#' date: " 2024"
#' ---


# Script Preparation ===

###########################
# Prepare environment
###########################


source("_auxilary/packages.R")
source("_auxilary/functions.R")


# Data ===

# Load Data
load("data/forecast_data.RData")
load("data/full_data.RData")

# Load Model Estimates ===

res <- readRDS("output/models/01_model_est_bayes.RDS")


# Get Forecasts  ============  

# Two Days Lead
fcst_ci_days <- do.call("rbind",lapply(c("sn_2024-09-01", "th_2024-09-01", "bb_2024-09-22"), function(elc) 
                                         {generate_forecast_data(election_id = elc,  model=res[["lr"]][["days_all"]])}))  

# Two Weeks Lead
fcst_ci_weeks <- do.call("rbind",lapply(c("sn_2024-09-01", "th_2024-09-01", "bb_2024-09-22"), function(elc) 
                                          {generate_forecast_data(election_id = elc,  model= res[["lr"]][["weeks_all"]])}))

# Two Months Lead
fcst_ci_months <- do.call("rbind",lapply(c("sn_2024-09-01", "th_2024-09-01", "bb_2024-09-22"), function(elc) 
                                          {generate_forecast_data(election_id = elc,  model= res[["lr"]][["months_all"]])}))


# Two Days Lead
fcstpoll_ci_days <- do.call("rbind",lapply(c("sn_2024-09-01", "th_2024-09-01", "bb_2024-09-22"), function(elc) 
                                             {generate_forecast_data(election_id = elc,  model=res[["lr"]][["days"]])}))  

# Two Weeks Lead
fcstpoll_ci_weeks <- do.call("rbind",lapply(c("sn_2024-09-01", "th_2024-09-01", "bb_2024-09-22"), function(elc) 
                                              {generate_forecast_data(election_id = elc,  model= res[["lr"]][["weeks"]])}))
# Two Months Lead
fcstpoll_ci_months <- do.call("rbind",lapply(c("sn_2024-09-01", "th_2024-09-01", "bb_2024-09-22"), function(elc) 
                                              {generate_forecast_data(election_id = elc,  model= res[["lr"]][["months"]])}))

# Two Days Lead
fcstfund_ci_days <- do.call("rbind",lapply(c("sn_2024-09-01", "th_2024-09-01", "bb_2024-09-22"), function(elc) 
                                             {generate_forecast_data(election_id = elc,  model=res[["lr"]][["days_fund"]])}))  

# Two Weeks Lead
fcstfund_ci_weeks <- do.call("rbind",lapply(c("sn_2024-09-01", "th_2024-09-01", "bb_2024-09-22"), function(elc) 
                                              {generate_forecast_data(election_id = elc,  model= res[["lr"]][["weeks_fund"]])}))
# Two Months Lead
fcstfund_ci_months <- do.call("rbind",lapply(c("sn_2024-09-01", "th_2024-09-01", "bb_2024-09-22"), function(elc) 
                                              {generate_forecast_data(election_id = elc,  model= res[["lr"]][["months_fund"]])}))


# Linear Model draws 2 Days Lead
fcst_draws_days <- do.call("rbind",lapply(c("sn_2024-09-01", "th_2024-09-01", "bb_2024-09-22"), function(elc) 
                                            {get_posterior_draws(election_id = elc,  model=res[["lr"]][["days_all"]])})) 

# Linear Model draws 2 Weeks Lead
fcst_draws_weeks <- do.call("rbind",lapply(c("sn_2024-09-01", "th_2024-09-01", "bb_2024-09-22"), function(elc) 
                                             {get_posterior_draws(election_id = elc,  model=res[["lr"]][["weeks_all"]])}))

# Save =====

save(fcst_ci_days,fcst_ci_weeks, fcst_ci_months,
     fcstpoll_ci_days,fcstpoll_ci_weeks, fcstpoll_ci_months,
     fcstfund_ci_days,fcstfund_ci_weeks, fcstfund_ci_months,
     fcst_draws_days, fcst_draws_weeks,
     file="output/forecast/02_fcst_state.Rdata")

  