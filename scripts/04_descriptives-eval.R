#' ---
#' title: "Descriptives of Evaluation"
#' date: " 2024"
#' ---

# Load required packages and functions
source("_auxilary/packages.R")
source("_auxilary/functions.R")

# Load model results and evaluation data
res <- readRDS("output/models/01_model_est_bayes.RDS")
eval_bayes <- readRDS("output/models/01_model_est_bayes_eval.RDS")

# 1. Posterior Distribution Plot ====
# Extract log-ratio results and prepare data for visualization
res_lr <- res[["lr"]]

# Transform posterior draws into long format and prepare for plotting
posterior_draws_long_df <- extract_posterior_draws_long(res_lr) %>%
  separate(model, c("lead","model")) %>% 
  mutate(model = ifelse(is.na(model),"polls",model)) %>% 
  mutate(
    parameter = factor(case_when(
      parameter %in% c("pollslr_months", "pollslr_days","pollslr_weeks") ~ "Latent Support",
      parameter %in% c("fed_trends_lr_weeks", "fed_trends_lr_days","fed_trends_lr_months") ~ "Trend Federal Polls",
      parameter %in% c("votesharelr_l1") ~ "Vote Share last election",
      parameter %in% c("pm") ~ "Prime Minister",
      parameter %in% c("gov") ~ "Government Party",
      parameter %in% c("new_party") ~ "New Party",
      parameter %in% c("(Intercept)") ~ "Intercept",
      parameter %in% c("sigma") ~ "Sigma",
      TRUE ~ parameter),
      levels = rev(c("Latent Support","Intercept","Vote Share last election",
                    "Prime Minister", "Government Party", "New Party","Trend Federal Polls", "Sigma"))),
    type = factor(case_when(
      parameter == "Latent Support" ~ "Polls",
      TRUE~ "Fundamentals"), levels =c("Polls", "Fundamentals")),
    lead = factor(paste("2",lead), level = c("2 months","2 weeks","2 days")),
    model = factor(model, level = c("all","polls", "fund"), labels = c("Polls + Fund.", "Polls", "Fund.") )
  )

# Calculate summary statistics for credible intervals
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

# Create and save posterior distribution plot
p <- ggplot(posterior_draws_long_df, aes(x = parameter, y = value, fill = lead)) +
  geom_violin(trim = TRUE, alpha = 0.1, position = position_dodge(width = 0.9), scale = "width") +
  geom_linerange(data = summary_stats, aes(y = median, ymin = lower, ymax = upper, color = lead),
                 position = position_dodge(width = 0.9), linewidth = 0.8) +
  geom_linerange(data = summary_stats, aes(y = median, ymin = lower2, ymax = upper2, color = lead),
                 position = position_dodge(width = 0.9), linewidth = 2) +
  geom_point(data = summary_stats, aes(y = median, color = lead),
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
save(posterior_draws_long_df, file = "output/models/posterior_draws_long_df.RData")

p

ggsave(filename = "output/figures/fig_eval_bayes_par.pdf", width = 12, height = 6)

# 2. Forecast Evaluation Plots ====
# Prepare evaluation data
eval_bayes <- eval_bayes %>% 
  mutate(predictors = factor(predictors, level = c("all","polls", "fund"), 
                            labels = c("Polls + Fund.", "Polls", "Fund.")))

plot_data <- filter(eval_bayes, mutate == "logratio") %>% 
  mutate(lead = paste("2", lead),
         lead = factor(lead, levels = c("2 months", "2 weeks", "2 days")))

# Create and save MAE plot
ggplot(plot_data, aes(x = mse, y = lead, col = predictors, shape = predictors)) +
  geom_point(size = 5, alpha = 0.8) +
  geom_text_repel(aes(label = paste(round(mse, 2))), 
                  nudge_y = 0.4,   
                  nudge_x = 0,    
                  size = 4,        
                  arrow = arrow(length = unit(0.001, "npc")),  
                  point.padding = 0.5) +  
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica", size = 12, color = "black"),
    axis.text = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.minor.y = element_line(color = "gray", size = 0.1),
    panel.grid.major.y = element_line(color = "gray", size = 0.5),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_color_grey() + 
  scale_x_continuous(breaks = seq(0, max(plot_data$mse), by = 1),
                    minor_breaks = seq(0, max(plot_data$mse), by = 0.5)) +
  coord_flip() +
  labs(
    title = "Forecast Evaluation for State Elections 2024",
    subtitle = "Forecast based on Bayesian Linear Regression",
    x = "Mean Absolute Error",
    y = "Lead Time"
  )

ggsave(filename = "output/figures/fig_eval_bayes_mae.pdf", height = 6, width = 6*1.5)

# Create and save RMSE plot
ggplot(plot_data, aes(x = rmse, y = lead, col = predictors, shape = predictors)) +
  geom_point(size = 5, alpha = 0.8) +
  geom_text_repel(aes(label = paste(round(rmse, 2))), 
                  nudge_y = 0.4,   
                  nudge_x = 0,    
                  size = 4,        
                  arrow = arrow(length = unit(0.001, "npc")),  
                  point.padding = 0.5) +  
  theme_minimal() +
  theme(
    text = element_text(family = "Helvetica", size = 12, color = "black"),
    axis.text = element_text(size = 14),
    panel.grid.minor = element_blank(),
    panel.grid.minor.y = element_line(color = "gray", size = 0.1),
    panel.grid.major.y = element_line(color = "gray", size = 0.5),
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  scale_color_grey() + 
  scale_x_continuous(breaks = seq(0, max(plot_data$rmse), by = 1),
                    minor_breaks = seq(0, max(plot_data$rmse), by = 0.5)) +
  coord_flip() +
  labs(
    title = "Forecast Evaluation for State Elections 2024",
    subtitle = "Forecast based on Bayesian Linear Regression",
    x = "Root Mean Square Error",
    y = "Lead Time"
  )

ggsave(filename = "output/figures/fig_eval_bayes_rmse.pdf", height = 6, width = 6*1.5)
  