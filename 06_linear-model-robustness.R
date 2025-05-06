#' ---
#' title: "Linear Model"
#' date: " 2025"
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

# Function to estimate linear models for different specifications
parameters_election_lm <- function(elec, dat=data_structural,dv="voteshare",impute_polls=T,
                                filter_basedon=list("days"="pollsNA_days==0",
                                                    "weeks"="pollsNA_weeks==0",
                                                    "months"="pollsNA_months==0"),
                                predictor_sets = list(
                                  "months" = c("polls_months", "voteshare_l1", "pm", "gov", "new_party"),
                                  "weeks" = c("polls_weeks", "voteshare_l1", "pm", "gov", "new_party"),
                                  "days" = c("polls_days", "voteshare_l1", "pm", "gov", "new_party")
                                )) {
  
  # Step 1: Mask the selected election results and prepare the data for processing
  dat_list <- mask_and_select(sel = elec,dat = dat, dv= dv)
  dat_masked <- dat_list$dat_masked
  
  # Step 2: Impute missing poll data in the masked dataset
  if(impute_polls){
    dat_masked <- impute_poll(dat_masked)
  }
  
  # Step 3: Initialize an empty list to store results from each predictive model
  all_res_dat <- list()
  
  print(elec)
  
  # Step 4: Loop through each set of predictors (e.g., months, weeks, days)
  for (name in names(predictor_sets)) {
    
    # Step 4a: Create a linear model formula based on the current set of predictors
    lm_form <- as.formula(paste(dv, "~", paste(predictor_sets[[name]], collapse = " + ")))
    
    # filter
    if(!is.null(filter_basedon[[name]])){
      dat_masked_filter <-  dat_masked %>% filter(eval(parse(text = filter_basedon[[name]])))
    } else{
      dat_masked_filter <-  dat_masked
    }
    
    # Step 4b: Fit the linear model using the masked data
    mdl <- lm(lm_form, data = dat_masked_filter)
    
    
    
    # Extract parameters and confidence intervals
    mdl_dat <- summary(mdl)$coefficients %>% as.data.frame()
    names(mdl_dat) <- c("est","se","t","p")
    mdl_dat$parameter <- rownames(mdl_dat)
    
    # Step 4e: Add an identifier to the results indicating which predictor set was used
    mdl_dat$model_id <- paste0("model_", name)
    
    # Step 4f: Store the results in the list
    all_res_dat[[name]] <- mdl_dat
  }
  
  # Step 5: Combine the results from all models into a single data frame
  combined_res_dat <- do.call(rbind, all_res_dat)
  
  # Step 6: Return the combined results
  return(combined_res_dat)
}

# Running function
lm_params <- parameters_election_lm(elec=elec_eval,
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



lm_params <- lm_params %>%
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
    lead = str_extract(model_id, "(days)|(months)|(weeks)"),
    lead = factor(paste("2",lead), level = c("2 months","2 weeks","2 days")),
    model = str_extract(model_id, "(all)|(polls)|(fund)"),
    model = ifelse(is.na(model), "polls", model),
    model = factor(model, level = c("all","polls", "fund"), labels = c("Polls + Fund.", "Polls", "Fund.") 
    ),
    lower = est - 1.96 * se, 
    upper = est + 1.96 * se,
    lower2 = est - 2.576 * se,
    upper2 = est + 2.576 * se
)


load("output/models/posterior_draws_long_df.RData")

posterior_draws_long_df <- merge(posterior_draws_long_df, lm_params, by = c("lead", "model", "parameter", "type"))

# Plot together with posterior distributions
ggplot(posterior_draws_long_df, aes(x = parameter, y = value, fill = lead)) +
  geom_violin(trim = TRUE, alpha = 0.1, position = position_dodge(width = 0.9), scale = "width") +
  # geom_linerange(data = summary_stats, aes(y = median, ymin = lower, ymax = upper, color = lead),
  #                position = position_dodge(width = 0.9), linewidth = 0.8) +
  # geom_linerange(data = summary_stats, aes(y = median, ymin = lower2, ymax = upper2, color = lead),
  #                position = position_dodge(width = 0.9), linewidth = 2) +
  # geom_point(data = summary_stats, aes(y = median, color = lead),
  #            position = position_dodge(width = 0.9), size = 2) +
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
    title = "Posterior Distributions and Linear Model estimates with 95% Confidence Intervals by Model",
    x = "",
    y = "Posterior Distribution",
    fill = "Lead Time",
    color = "Lead Time"
  ) +
  geom_point(aes(y = est, color = lead),
             position = position_dodge(width = 0.9), size = 2) +
  geom_linerange(aes(y = est, ymin = lower, ymax = upper, color = lead),
                 position = position_dodge(width = 0.9), linewidth = 0.8)

ggsave(filename = "output/figures/fig_eval_bayes_par_vs_lm.pdf", width = 12, height = 6)

  