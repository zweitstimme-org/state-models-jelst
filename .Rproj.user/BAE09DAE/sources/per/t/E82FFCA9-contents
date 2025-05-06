#' ---
#' title: "Estimation of Models"
#' date: " 2024"
#' ---

###########################
# Setup and Data Preparation
###########################

# Load required packages and functions
source("_auxilary/packages.R")
source("_auxilary/functions.R")

# Load data
load("data/full_data.RData")

# Configure parallel processing
options(mc.cores = detectCores()-2)
plan(multisession, workers = 6)

# Prepare evaluation dataset
elec_eval <- full_data %>% 
  filter(year > 2010, pollsNA_weeks==0, pollsNA_days==0, pollsNA_months==0) %>% 
  select(elec_ind) %>%
  distinct() %>% 
  pull()

# Filter data excluding "other" party
data_filter = filter(full_data, party != "oth")

###########################
# Bayesian Model Estimation
###########################

# Log Ratio Model
res_lr <- est_election_bayesstan(
  dat = data_filter, 
  dv = "votesharelr",
  filter_basedon = list(
    "days" = "pollsNA_days==0",
    "weeks" = "pollsNA_weeks==0",
    "months" = "pollsNA_months==0",
    "days_fund" = "pollsNA_days==0",
    "weeks_fund" = "pollsNA_weeks==0",
    "months_fund" = "pollsNA_months==0",
    "days_all" = "pollsNA_days==0",
    "weeks_all" = "pollsNA_weeks==0",
    "months_all" = "pollsNA_months==0"
  ),
  predictor_sets = list(
    "months" = c("pollslr_months"),
    "weeks" = c("pollslr_weeks"),
    "days" = c("pollslr_days"),
    "days_fund" = c("fed_trends_lr_days", "votesharelr_l1", "pm", "gov", "new_party"),
    "weeks_fund" = c("fed_trends_lr_weeks", "votesharelr_l1", "pm", "gov", "new_party"),
    "months_fund" = c("fed_trends_lr_months", "votesharelr_l1", "pm", "gov", "new_party"),
    "days_all"= c("pollslr_days","fed_trends_lr_days","votesharelr_l1", "pm", "gov", "new_party"),
    "weeks_all"= c("pollslr_weeks","fed_trends_lr_weeks","votesharelr_l1", "pm", "gov", "new_party"),
    "months_all"= c("pollslr_months","fed_trends_lr_months","votesharelr_l1", "pm", "gov", "new_party")
  )
)

# Linear Model
res_lm <- est_election_bayesstan(
  dat = data_filter, 
  dv="voteshare",
  filter_basedon = list(
    "days" = "pollsNA_days==0",
    "weeks" = "pollsNA_weeks==0",
    "months" = "pollsNA_months==0",
    "days_fund" = "pollsNA_days==0",
    "weeks_fund" = "pollsNA_weeks==0",
    "months_fund" = "pollsNA_months==0",
    "days_all" = "pollsNA_days==0",
    "weeks_all" = "pollsNA_weeks==0",
    "months_all" = "pollsNA_months==0"
  ),
  predictor_sets = list(
    "months" = c("polls_months"),
    "weeks" = c("polls_weeks"),
    "days" = c("polls_days"),
    "days_fund" = c("fed_trend_days", "polls_months", "voteshare_l1", "pm", "gov", "new_party"),
    "weeks_fund" = c("fed_trend_weeks", "voteshare_l1", "pm", "gov", "new_party"),
    "months_fund" = c("fed_trend_months", "voteshare_l1", "pm", "gov", "new_party"),
    "days_all" = c("polls_days", "fed_trend_days", "voteshare_l1", "pm", "gov", "new_party"),
    "weeks_all" = c("polls_weeks", "fed_trend_weeks", "voteshare_l1", "pm", "gov", "new_party"),
    "months_all" = c("polls_months", "fed_trend_months", "voteshare_l1", "pm", "gov", "new_party")
  )
)

# Save model results
saveRDS(list("lr" = res_lr, "lm" = res_lm), file = "output/models/01_model_est_bayes.RDS")

###########################
# Model Evaluation
###########################

# Linear Model Evaluation
all_res_lm <- do.call(rbind, future_lapply(elec_eval, function(elec) {
  process_election_bayesstan(
    elec, 
    dat = data_filter, 
    dv = "voteshare",
    filter_basedon = list(
      "days" = "pollsNA_days==0",
      "weeks" = "pollsNA_weeks==0",
      "months" = "pollsNA_months==0",
      "days_fund" = "pollsNA_days==0",
      "weeks_fund" = "pollsNA_weeks==0",
      "months_fund" = "pollsNA_months==0",
      "days_all" = "pollsNA_days==0",
      "weeks_all" = "pollsNA_weeks==0",
      "months_all" = "pollsNA_months==0"
    ),
    predictor_sets = list(
      "months" = c("polls_months"),
      "weeks" = c("polls_weeks"),
      "days" = c("polls_days"),
      "days_fund" = c("fed_trend_days", "polls_months", "voteshare_l1", "pm", "gov", "new_party"),
      "weeks_fund" = c("fed_trend_weeks", "voteshare_l1", "pm", "gov", "new_party"),
      "months_fund" = c("fed_trend_months", "voteshare_l1", "pm", "gov", "new_party"),
      "days_all" = c("polls_days", "fed_trend_days", "voteshare_l1", "pm", "gov", "new_party"),
      "weeks_all" = c("polls_weeks", "fed_trend_weeks", "voteshare_l1", "pm", "gov", "new_party"),
      "months_all" = c("polls_months", "fed_trend_months", "voteshare_l1", "pm", "gov", "new_party")
    )
  )
}, future.seed = TRUE))

# Calculate evaluation metrics for linear model
res_eval_agg <- all_res_lm %>%
  group_by(model_id) %>%
  dplyr::summarise(
    mse = mean(abs(voteshare*100 - fit*100),na.rm = T),
    rmse = sqrt((mean((voteshare*100 - fit*100)^2,na.rm = T))),
    bias=mean(voteshare*100 - fit*100, na.rm=T),
    coverage = mean(lwr < voteshare & voteshare < upr,na.rm=T)
  ) %>%
  arrange(rmse)

# Log-ratio Model Evaluation
all_res_lr <- do.call(rbind, future_lapply(elec_eval, function(elec) {
  process_election_bayesstan(
    elec, 
    dat = data_filter, 
    dv="votesharelr",
    dv_inv = function(x) {exp(x)/(1+exp(x)) },
    filter_basedon = list(
      "days" = "pollsNA_days==0",
      "weeks" = "pollsNA_weeks==0",
      "months" = "pollsNA_months==0",
      "days_fund" = "pollsNA_days==0",
      "weeks_fund" = "pollsNA_weeks==0",
      "months_fund" = "pollsNA_months==0",
      "days_all" = "pollsNA_days==0",
      "weeks_all" = "pollsNA_weeks==0",
      "months_all" = "pollsNA_months==0"
    ),
    predictor_sets = list(
      "months" = c("pollslr_months"),
      "weeks" = c("pollslr_weeks"),
      "days" = c("pollslr_days"),
      "days_fund" = c("fed_trends_lr_days", "pollslr_months", "voteshare_l1", "pm", "gov", "new_party"),
      "weeks_fund" = c("fed_trends_lr_weeks", "voteshare_l1", "pm", "gov", "new_party"),
      "months_fund" = c("fed_trends_lr_months", "voteshare_l1", "pm", "gov", "new_party"),
      "days_all"= c("pollslr_days","fed_trends_lr_days","votesharelr_l1", "pm", "gov", "new_party"),
      "weeks_all"= c("pollslr_weeks","fed_trends_lr_weeks","votesharelr_l1", "pm", "gov", "new_party"),
      "months_all"= c("pollslr_months","fed_trends_lr_months","votesharelr_l1", "pm", "gov", "new_party")
    )
  )
}, future.seed = TRUE))

# Calculate evaluation metrics for log-ratio model
res_eval_agg_lr <- all_res_lr %>%
  group_by(model_id) %>%
  dplyr::summarise(
    mse = mean(abs(voteshare*100 - fit*100),na.rm = T),
    rmse = sqrt((mean((voteshare*100 - fit*100)^2,na.rm = T))),
    bias=mean(voteshare*100 - fit*100, na.rm=T),
    coverage = mean(lwr < voteshare & voteshare < upr,na.rm=T)
  ) %>%
  arrange(rmse)

###########################
# Combine and Process Results
###########################

# Process log-ratio results
res_eval_agg_lr <- res_eval_agg_lr %>%
  separate(model_id, c(NA,"lead","predictors")) %>% 
  mutate(
    predictors = ifelse(is.na(predictors),"polls",predictors),
    model_type = "logratio"
  )

# Process linear model results
res_eval_agg <- res_eval_agg %>%
  separate(model_id, c(NA,"lead","predictors")) %>% 
  mutate(
    predictors = ifelse(is.na(predictors),"polls",predictors),
    model_type = "linear"
  )

# Combine results
res_eval_agg <- bind_rows(res_eval_agg, res_eval_agg_lr) %>% 
  arrange(rmse)

# Print results
print(res_eval_agg)

# Save evaluation results
saveRDS(res_eval_agg, file="output/models/01_model_est_bayes_eval.RDS")

###########################
# Error Analysis
###########################

# Calculate errors for log-ratio model
error_lr <- all_res_lr %>%
  mutate(
    voteshare = exp(votesharelr)/(1 + exp(votesharelr)),
    "fit_lr"= exp(fit)/(1 + exp(fit)),
    "ae_lr" = abs(voteshare - fit_lr)
  ) %>%
  select(party, elec_ind, model_id, fit_lr, ae_lr)

# Calculate errors for linear model
error_lm <- all_res_lm %>%
  mutate("ae_lm" = abs(voteshare - fit)) %>%
  select(party, elec_ind, "fit_lm"=fit, model_id, voteshare, ae_lm)

# Combine error metrics
df_error <- left_join(error_lr, error_lm, by=c("party","elec_ind","model_id")) %>% 
  pivot_longer(cols = c("fit_lr", "fit_lm", "ae_lr", "ae_lm")) %>%
  separate(name, c("var","dv")) %>%
  pivot_wider(names_from = "var", values_from = "value") %>%
  separate(model_id, c(NA,"lead","predictors")) %>% 
  mutate(predictors = ifelse(is.na(predictors),"polls",predictors))

# Save error analysis results
saveRDS(df_error, file="output/models/01_model_est_bayes_errors.RDS")

###########################
# Linear Models (Non-Bayesian)
###########################

# Estimate linear models for all elections
res_eval_lm <- do.call("rbind", lapply(elec_eval, process_election_lm,
  dat = data_filter,
  dv = "voteshare",
  filter_basedon = list(
    "days" = "pollsNA_days==0",
    "weeks" = "pollsNA_weeks==0",
    "months" = "pollsNA_months==0",
    "days_fund" = "pollsNA_days==0",
    "weeks_fund" = "pollsNA_weeks==0",
    "months_fund" = "pollsNA_months==0",
    "days_all" = "pollsNA_days==0",
    "weeks_all" = "pollsNA_weeks==0",
    "months_all" = "pollsNA_months==0"
  ),
  predictor_sets = list(
    "months" = c("pollslr_months"),
    "weeks" = c("pollslr_weeks"),
    "days" = c("pollslr_days"),
    "days_fund" = c("fed_trends_lr_days", "voteshare_l1", "pm", "gov", "new_party"),
    "weeks_fund" = c("fed_trends_lr_weeks", "voteshare_l1", "pm", "gov", "new_party"),
    "months_fund" = c("fed_trends_lr_months", "voteshare_l1", "pm", "gov", "new_party"),
    "days_all"= c("pollslr_days","fed_trends_lr_days","votesharelr_l1", "pm", "gov", "new_party"),
    "weeks_all"= c("pollslr_weeks","fed_trends_lr_weeks","votesharelr_l1", "pm", "gov", "new_party"),
    "months_all"= c("pollslr_months","fed_trends_lr_months","votesharelr_l1", "pm", "gov", "new_party")
  )
))

# Calculate evaluation metrics for linear models
res_eval_agg <- res_eval_lm %>%
  group_by(model_id) %>%
  dplyr::summarise(
    mse = mean(abs(voteshare*100 - fit*100),na.rm = T),
    rmse = sqrt((mean((voteshare*100 - fit*100)^2,na.rm = T))),
    bias=mean(voteshare*100 - fit*100, na.rm=T),
    coverage = mean(lwr < voteshare & voteshare < upr,na.rm=T)
  ) %>%
  arrange(rmse)

# Log-ratio linear model estimation
res_eval_lr <- do.call("rbind", lapply(elec_eval, process_election_lm,
  dat = data_filter,
  dv = "votesharelr",
  filter_basedon = list(
    "days" = "pollsNA_days==0",
    "weeks" = "pollsNA_weeks==0",
    "months" = "pollsNA_months==0",
    "days_fund" = "pollsNA_days==0",
    "weeks_fund" = "pollsNA_weeks==0",
    "months_fund" = "pollsNA_months==0",
    "days_all" = "pollsNA_days==0",
    "weeks_all" = "pollsNA_weeks==0",
    "months_all" = "pollsNA_months==0"
  ),
  predictor_sets = list(
    "months" = c("pollslr_months"),
    "weeks" = c("pollslr_weeks"),
    "days" = c("pollslr_days"),
    "days_fund" = c("fed_trends_lr_days", "polls_months", "voteshare_l1", "pm", "gov", "new_party"),
    "weeks_fund" = c("fed_trends_lr_weeks", "voteshare_l1", "pm", "gov", "new_party"),
    "months_fund" = c("fed_trends_lr_months", "voteshare_l1", "pm", "gov", "new_party"),
    "days_all"= c("pollslr_days","fed_trends_lr_days","votesharelr_l1", "pm", "gov", "new_party"),
    "weeks_all"= c("pollslr_weeks","fed_trends_lr_weeks","votesharelr_l1", "pm", "gov", "new_party"),
    "months_all"= c("pollslr_months","fed_trends_lr_months","votesharelr_l1", "pm", "gov", "new_party")
  )
))

# Calculate evaluation metrics for log-ratio linear models
res_eval_agg_lr <- res_eval_lr %>%
  mutate(
    voteshare = exp(votesharelr)/(1 + exp(votesharelr)),
    fit = exp(fit)/(1 + exp(fit))
  ) %>%
  group_by(model_id) %>%
  dplyr::summarise(
    mse = mean(abs(voteshare*100 - fit*100),na.rm = T),
    rmse = sqrt((mean((voteshare*100 - fit*100)^2,na.rm = T))),
    bias=mean(voteshare*100 - fit*100, na.rm=T),
    coverage = mean(lwr < votesharelr & votesharelr < upr,na.rm=T)
  ) %>%
  arrange(rmse)

###########################
# Process and Save Linear Model Results
###########################

# Process log-ratio results
res_eval_agg_lr <- res_eval_agg_lr %>%
  separate(model_id, c(NA,"lead","predictors")) %>% 
  mutate(
    predictors = ifelse(is.na(predictors),"polls",predictors),
    mutate = "logratio"
  )

# Process linear model results
res_eval_agg <- res_eval_agg %>%
  separate(model_id, c(NA,"lead","predictors")) %>% 
  mutate(
    predictors = ifelse(is.na(predictors),"polls",predictors),
    mutate = "linear"
  )

# Combine results
res_eval_agg <- bind_rows(res_eval_agg, res_eval_agg_lr) %>% 
  arrange(rmse)

# Print results
print(res_eval_agg)

# Save evaluation results
saveRDS(res_eval_agg, file="output/models/01_model_est_lr_eval.RDS")

###########################
# Error Analysis for Linear Models
###########################

# Calculate errors for log-ratio model
error_lr <- res_eval_lr %>%
  mutate(
    voteshare = exp(votesharelr)/(1 + exp(votesharelr)),
    "fit_lr"= exp(fit)/(1 + exp(fit)),
    "ae_lr" = abs(voteshare - fit_lr)
  ) %>%
  select(party, elec_ind, model_id, fit_lr, ae_lr)

# Calculate errors for linear model
error_lm <- res_eval_lm %>%
  mutate("ae_lm" = abs(voteshare - fit)) %>%
  select(party, elec_ind, "fit_lm"=fit, model_id, voteshare, ae_lm)

# Combine error metrics
df_error <- left_join(error_lr, error_lm, by=c("party","elec_ind","model_id")) %>% 
  pivot_longer(cols = c("fit_lr", "fit_lm", "ae_lr", "ae_lm")) %>%
  separate(name, c("var","dv")) %>%
  pivot_wider(names_from = "var", values_from = "value") %>%
  separate(model_id, c(NA,"lead","predictors")) %>% 
  mutate(predictors = ifelse(is.na(predictors),"polls",predictors))

# Save error analysis results
saveRDS(df_error, file="output/models/01_model_est_lr_errors.RDS")


###########################
# Additional Models with Growth
###########################

# Estimate Bayesian models with growth variables
res_lr_growth <- est_election_bayesstan(
  dat = data_filter,
  dv = "votesharelr",
  filter_basedon = list(
    "days" = "pollsNA_days==0",
    "weeks" = "pollsNA_weeks==0",
    "months" = "pollsNA_months==0",
    "days_fund" = "pollsNA_days==0",
    "weeks_fund" = "pollsNA_weeks==0",
    "months_fund" = "pollsNA_months==0",
    "days_all" = "pollsNA_days==0",
    "weeks_all" = "pollsNA_weeks==0",
    "months_all" = "pollsNA_months==0"
  ),
  predictor_sets = list(
    "days_all"= c("pollslr_days","fed_trends_lr_days","votesharelr_l1", "pm", "gov", "new_party", "growth", "growthXgov"),
    "weeks_all"= c("pollslr_weeks","fed_trends_lr_weeks","votesharelr_l1", "pm", "gov", "new_party", "growth", "growthXgov"),
    "months_all"= c("pollslr_months","fed_trends_lr_months","votesharelr_l1", "pm", "gov", "new_party", "growth", "growthXgov")
  )
)

# Evaluate growth models
all_res_lr_growth <- do.call(rbind, future_lapply(elec_eval, function(elec) {
  process_election_bayesstan(
    elec, 
    dat = data_filter, 
    dv = "votesharelr",
    dv_inv = function(x) {exp(x)/(1+exp(x)) },
    filter_basedon = list(
      "days" = "pollsNA_days==0",
      "weeks" = "pollsNA_weeks==0",
      "months" = "pollsNA_months==0",
      "days_fund" = "pollsNA_days==0",
      "weeks_fund" = "pollsNA_weeks==0",
      "months_fund" = "pollsNA_months==0",
      "days_all" = "pollsNA_days==0",
      "weeks_all" = "pollsNA_weeks==0",
      "months_all" = "pollsNA_months==0"
    ),
    predictor_sets = list(
      "months" = c("pollslr_months"),
      "weeks" = c("pollslr_weeks"),
      "days" = c("pollslr_days"),
      "days_fund" = c("fed_trends_lr_days", "polls_months", "voteshare_l1", "pm", "gov", "new_party", "growth", "growthXgov"),
      "weeks_fund" = c("fed_trends_lr_weeks", "voteshare_l1", "pm", "gov", "new_party", "growth", "growthXgov"),
      "months_fund" = c("fed_trends_lr_months", "voteshare_l1", "pm", "gov", "new_party", "growth", "growthXgov"),
      "days_all"= c("pollslr_days","fed_trends_lr_days","votesharelr_l1", "pm", "gov", "new_party", "growth", "growthXgov"),
      "weeks_all"= c("pollslr_weeks","fed_trends_lr_weeks","votesharelr_l1", "pm", "gov", "new_party", "growth", "growthXgov"),
      "months_all"= c("pollslr_months","fed_trends_lr_months","votesharelr_l1", "pm", "gov", "new_party", "growth", "growthXgov")
    )
  )
}, future.seed = TRUE))

# Calculate evaluation metrics for growth models
res_eval_agg_growth <- all_res_lr_growth %>%
  group_by(model_id) %>%
  dplyr::summarise(
    mse = mean(abs(voteshare*100 - fit*100),na.rm = T),
    rmse = sqrt((mean((voteshare*100 - fit*100)^2,na.rm = T))),
    bias=mean(voteshare*100 - fit*100, na.rm=T),
    coverage = mean(lwr < voteshare & voteshare < upr,na.rm=T)
  ) %>%
  arrange(rmse)

# Save evaluation results
saveRDS(res_eval_agg_growth, file="output/models/01_model_est_bayes_growth.RDS")

# Generate LaTeX table for growth model results
res_eval_agg_growth %>% 
  mutate(
    mse = round(mse,2),
    rmse = round(rmse,2),
    bias = round(bias,2),
    coverage = round(coverage,2)
  ) %>% 
  rename(mae = mse) %>% 
  select(-c(bias, coverage)) %>% 
  stargazer(type="latex", digits=2, summary=FALSE, out = "output/tables/01_model_est_bayes_growth.tex")

###########################
# Visualization
###########################

# Function to extract posterior draws
extract_posterior_draws_long <- function(res_list) {
  draws_list <- list()
  
  for (name in names(res_list)) {
    draws_df <- as.data.frame(res_list[[name]])
    draws_df <- draws_df %>% mutate(model = name)
    draws_long <- draws_df %>%
      pivot_longer(cols = -model, names_to = "parameter", values_to = "value")
    draws_list[[name]] <- draws_long
  }
  
  return(bind_rows(draws_list))
}

# Process posterior draws
posterior_draws_long_df <- extract_posterior_draws_long(res_lr_growth) %>%
  separate(model, c("lead","model")) %>% 
  mutate(
    model = ifelse(is.na(model),"polls",model),
    parameter = factor(case_when(
      parameter %in% c("pollslr_months", "pollslr_days","pollslr_weeks") ~ "Latent Support",
      parameter %in% c("fed_trends_lr_weeks", "fed_trends_lr_days","fed_trends_lr_months") ~ "Trend Federal Polls",
      parameter %in% c("votesharelr_l1") ~ "Vote Share last election",
      parameter %in% c("pm") ~ "Prime Minister",
      parameter %in% c("gov") ~ "Government Party",
      parameter %in% c("new_party") ~ "New Party",
      parameter %in% c("growth") ~ "Economic Growth",
      parameter %in% c("growthXgov") ~ "Economic Growth X Government Party",
      parameter %in% c("(Intercept)") ~ "Intercept",
      parameter %in% c("sigma") ~ "Sigma",
      TRUE ~ parameter
    ),
    levels = rev(c(
      "Latent Support","Intercept","Vote Share last election",
      "Prime Minister", "Government Party", "New Party",
      "Trend Federal Polls", "Economic Growth", 
      "Economic Growth X Government Party", "Sigma"
    ))),
    type = factor(case_when(
      parameter == "Latent Support" ~ "Polls",
      TRUE~ "Fundamentals"
    ), levels = c("Polls", "Fundamentals")),
    lead = factor(paste("2",lead), level = c("2 months","2 weeks","2 days")),
    model = factor(model, level = c("all","polls", "fund"), 
                  labels = c("Polls + Fund.", "Polls", "Fund."))
  )

# Calculate summary statistics
summary_stats <- posterior_draws_long_df %>%
  group_by(model, lead, parameter, type) %>%
  summarize(
    lower = quantile(value, 0.01),
    upper = quantile(value, 0.99),
    lower2 = quantile(value, 1/6),
    upper2 = quantile(value, (1-1/6)),
    median = median(value),
    .groups = 'drop'
  )

# Create visualization
ggplot(posterior_draws_long_df, aes(x = parameter, y = value, fill = lead)) +
  geom_violin(trim = TRUE, alpha = 0.1, position = position_dodge(width = 0.9), scale = "width") +
  geom_linerange(data = summary_stats, 
                 aes(y = median, ymin = lower, ymax = upper, color = lead),
                 position = position_dodge(width = 0.9), linewidth = 0.8) +
  geom_linerange(data = summary_stats, 
                 aes(y = median, ymin = lower2, ymax = upper2, color = lead),
                 position = position_dodge(width = 0.9), linewidth = 2) +
  geom_point(data = summary_stats, 
             aes(y = median, color = lead),
             position = position_dodge(width = 0.9), size = 2) +
  coord_flip() +
  facet_grid(~ model, scales = "free_y") + 
  theme_minimal() +
  scale_fill_grey() +
  scale_color_grey() +
  theme(
    text = element_text(family = "Helvetica", size = 12, color = "black"),
    axis.text = element_text(size = 12),
    axis.text.y = element_text(margin = margin(r = 10)),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.minor.x = element_line(color = "gray", size = 0.25),
    panel.grid.major.x = element_line(color = "gray", size = 0.25),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    panel.border = element_rect(color = "gray", fill = NA, size = 0.5),
    legend.position = "bottom"
  ) +
  labs(
    title = "Posterior Distributions with 95% Credible Intervals by Model",
    x = "",
    y = "Posterior Distribution",
    fill = "Lead Time",
    color = "Lead Time"
  )

# Save visualization
ggsave(filename = "output/figures/fig_eval_bayes_par_growth.pdf", width=12, height=6)
