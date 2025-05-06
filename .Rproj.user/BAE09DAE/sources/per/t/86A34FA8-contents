# Define the general function
generate_forecast_data <- function(data = forecast_data, election_id, model, 
                                   inv_function = function(x) {exp(x)/(1 + exp(x))}, 
                                   alpha = 1/6, party_colors, caption_text) {
  
  # Step 1: Set up Prediction Data for the specified election
  pred_dat <- data %>% 
    filter(elec_ind == election_id, party != "oth")
  
  # Step 2: Extract relevant predictors based on the model's coefficients
  pred_weeks <- pred_dat %>% 
    select(names(coef(model))[-1])
  
  # Step 3: Posterior Prediction
  pred_sim <- posterior_predict(model, pred_weeks)
  
  # Step 4: Apply the inverse function to transform the dependent variable
  if(!is.null(inv_function)){
    pred_sim <- apply(pred_sim, 2, inv_function)
  }
  
  # Step 5: Calculate summary statistics (mean, lower bound, upper bound)
  pred_mean <- apply(pred_sim, 2, mean)
  pred_lwr <- apply(pred_sim, 2, quantile, probs = alpha / 2)
  pred_upr <- apply(pred_sim, 2, quantile, probs = 1 - (alpha / 2))
  
  # Step 6: Combine prediction results with the original data
  pred_dat <- cbind(pred_dat, fit = pred_mean, lwr = pred_lwr, upr = pred_upr)
  
  
  return(pred_dat)
}


# Function to get posterior draws in long format
get_posterior_draws <- function(data = forecast_data, election_id, model, 
                                inv_function= function(x) {exp(x)/(1 + exp(x))}) {
  
  # Step 1: Set up Prediction Data for the specified election
  pred_dat <- data  %>% 
    filter(elec_ind == election_id, party != "oth")
  
  # Step 2: Extract relevant predictors based on the model's coefficients
  pred_weeks <- pred_dat %>% 
    select(names(coef(model))[-1])
  
  # Step 3: Posterior Prediction
  pred_sim <- posterior_predict(model, pred_weeks)
  
  # Step 4: Apply the inverse function to transform the dependent variable
  if(!is.null(inv_function)){
    pred_sim <- apply(pred_sim, 2, inv_function)
  }
  
  # Step 5: Convert the posterior draws to a long format
  pred_long <- as.data.frame(pred_sim) %>%
    mutate(draw = row_number()) %>%
    pivot_longer(cols = -draw, names_to = "party", values_to = "posterior_draw") %>%
    mutate(party = pred_dat$party[as.numeric(gsub("V", "", party))],
           elec_ind = election_id,
           state = unique(pred_dat$state),
           date = unique(pred_dat$electiondate))
  
  return(pred_long)
}

# Define the function to map party codes to party names
map_party_names <- function(party_codes) {
  party_labels <- case_when(
    party_codes == "afd" ~ "AfD",
    party_codes == "bsw" ~ "BSW",
    party_codes == "cdu" ~ "CDU",
    party_codes == "gru" ~ "Greens",
    party_codes == "spd" ~ "SPD",
    party_codes == "lin" ~ "Linke",
    party_codes == "fdp" ~ "FDP",
    TRUE ~ "Other"  # Default label for any unspecified party code
  )
  
  return(party_labels)
}

# Define party colors
party_colors <- c(
  AfD = "#009DE0",      # AfD (Blue)
  BSW = "#800080",      # BSW (Yellow)
  CDU = "#151518",      # CDU (Black)
  FDP = "#FFED00",      # FDP (Yellow)
  FW = "#FF8000",       # FW (Orange)
  Greens = "#409A3C",   # Greens (Green)
  Linke = "#BE3075",    # Linke (Dark Red)
  Other = "#808080",    # Other (Grey)
  SPD = "#E3000F"       # SPD (Red)
)



# Define the function to map state codes to state names
map_state_names <- function(state_codes) {
  state_labels <- case_when(
    state_codes == "bb" ~ "Brandenburg",
    state_codes == "be" ~ "Berlin",
    state_codes == "bw" ~ "Baden-Württemberg",
    state_codes == "by" ~ "Bavaria",
    state_codes == "hb" ~ "Bremen",
    state_codes == "he" ~ "Hesse",
    state_codes == "hh" ~ "Hamburg",
    state_codes == "mv" ~ "Mecklenburg-Vorpommern",
    state_codes == "ni" ~ "Lower Saxony",
    state_codes == "nw" ~ "North Rhine-Westphalia",
    state_codes == "rp" ~ "Rhineland-Palatinate",
    state_codes == "sh" ~ "Schleswig-Holstein",
    state_codes == "sl" ~ "Saarland",
    state_codes == "sn" ~ "Saxony",
    state_codes == "st" ~ "Saxony-Anhalt",
    state_codes == "th" ~ "Thuringia",
    
    TRUE ~ "Other"  # Default label for any unspecified party code
  )
  
  return(state_labels)
}


# Define the function
  extract_posterior_draws_long <- function(res_list) {
    # Initialize an empty list to store the data frames
    draws_list <- list()
    
    # Loop through each item in the input list
    for (name in names(res_list)) {
      # Extract the posterior draws as a data frame
      draws_df <- as.data.frame(res_list[[name]])
      
      # Add a column for the model name
      draws_df <- draws_df %>%
        mutate(model = name)
      
      # Convert to long format
      draws_long <- draws_df %>%
        pivot_longer(cols = -model, names_to = "parameter", values_to = "value")
      
      # Append the long data frame to the list
      draws_list[[name]] <- draws_long
    }
    
    # Combine all long data frames into one large data frame
    combined_draws_long <- bind_rows(draws_list)
    
    # Return the combined long-format data frame
    return(combined_draws_long)
  }
  

# Function to impute missing poll data
impute_poll <- function(dat = data_structural){
  
  # Step 1: Impute missing data for 'polls_days' using linear regression
  m_days <- lm(polls_days ~ voteshare_l1, filter(dat, pollsNA_days == 0)) 
  dat$polls_days[dat$pollsNA_days == 1] <- predict(m_days, filter(dat, pollsNA_days == 1) %>% select(voteshare_l1)) + 
    rnorm(sum(dat$pollsNA_days == 1), mean = 0, sd = sigma(m_days))
  
  # Step 2: Impute missing data for 'polls_weeks' using linear regression
  m_weeks <- lm(polls_weeks ~ voteshare_l1, filter(dat, pollsNA_weeks == 0)) 
  dat$polls_weeks[dat$pollsNA_weeks == 1] <- predict(m_weeks, filter(dat, pollsNA_weeks == 1) %>% select(voteshare_l1)) + 
    rnorm(sum(dat$pollsNA_weeks == 1), mean = 0, sd = sigma(m_weeks))
  
  # Step 3: Impute missing data for 'polls_months' using linear regression
  m_mnts <- lm(polls_months ~ voteshare_l1, filter(dat, pollsNA_months == 0)) 
  dat$polls_months[dat$pollsNA_months == 1] <- predict(m_mnts, filter(dat, pollsNA_months == 1) %>% select(voteshare_l1)) + 
    rnorm(sum(dat$pollsNA_months == 1), mean = 0, sd = sigma(m_mnts))
  
  # Step 4: Return the dataset with imputed poll data
  return(dat)
}


# Function to mask results and select
mask_and_select <- function(sel = "nw_2022-05-15", dv="voteshare", dat = full_data){
  
  # Step 1: Extract the election date of the selected election
  date <- dat %>% filter(elec_ind == sel) %>% pull(electiondate) %>% unique()
  
  # Step 2: Filter the data to include only elections that occurred before the selected election date
  # and include the selected election itself
  dat <- dat %>% filter(electiondate < date | elec_ind == sel)
  
  # Step 3: Extract the vote share data and relevant columns (party, state, election index, and year)
  dat_voterest <- dat %>% 
    filter(elec_ind == sel) %>% 
    dplyr::select(!!dv,voteshare, party, state, elec_ind, year)
  
  # Step 4: Assign a unique identifier (pid) to each row in the extracted data
  dat_voterest$pid <- 1:nrow(dat_voterest)
  
  # Step 5: Mask the vote share results of the selected election by replacing them with NA
  dat <- dat %>% mutate(
    !!dv := case_when(elec_ind == sel ~ NA_real_, TRUE ~ eval(parse(text = dv)))
  )
  
  # Step 6: Return a list containing the masked data and the extracted vote share data
  return(list("dat_masked" = dat, "dat_results" = dat_voterest))
  
}

# Function to run one iteration of the loop for processing election data with Bayesian linear regression using bayesglm
process_election_bayesstan <- function(elec, dat=data_structural, dv="voteshare", impute_polls=T,
                                      filter_basedon=list("days"="pollsNA_days==0",
                                                          "weeks"="pollsNA_weeks==0",
                                                          "months"="pollsNA_months==0"),
                                      predictor_sets = list(
                                        "months" = c("polls_months", "voteshare_l1", "pm", "gov", "new_party"),
                                        "weeks" = c("polls_weeks", "voteshare_l1", "pm", "gov", "new_party"),
                                        "days" = c("polls_days", "voteshare_l1", "pm", "gov", "new_party")
                                      ), alpha=0.1,
                                      dv_inv  = NULL) {  # Specify the number of posterior samples
  
  # Step 1: Mask the selected election results and prepare the data for processing
  dat_list <- mask_and_select(sel = elec, dat = dat, dv= dv)
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
    
    # Step 4a: Create a Bayesian linear model formula based on the current set of predictors
    bayes_lm_form <- as.formula(paste(dv, "~", paste(predictor_sets[[name]], collapse = " + ")))
    
    # Step 4b: Filter the data based on the current condition
    if(!is.null(filter_basedon[[name]])){
      dat_masked_filter <-  dat_masked %>% filter(eval(parse(text = filter_basedon[[name]])))
    } else{
      dat_masked_filter <-  dat_masked
    }
    
    # Bayes Model
    mdl <-  stan_glm(bayes_lm_form, data = dat_masked_filter,
                     family = gaussian(),
                     prior = normal(0,5),
                     prior_intercept = normal(0,5),
                     prior_aux =exponential(rate = 1),
                     # for speed of example only
                     chains = 2, iter = 3000,
                     seed = 238523862)

    # Step 4e: Prepare the prediction data by filtering for the selected election and selecting the relevant predictors
    pred_dat <- dat_masked_filter %>% filter(elec_ind == elec)
    pred_x <- pred_dat %>% select(predictor_sets[[name]])
    
    # Step 4f: Generate predictions by applying the simulated coefficients to the predictor matrix
    pred_sim <- posterior_predict(mdl,pred_x)
    
    # Transform dv with inverse function
    if(!is.null(dv_inv)){
      pred_sim <- dv_inv(pred_sim)
    } 
    
    # Step 4h: Calculate summary statistics (mean, lower bound, upper bound)
    pred_mean <- apply(pred_sim, 2, mean)
    pred_lwr <- apply(pred_sim, 2, quantile, probs = alpha/2)
    pred_upr <- apply(pred_sim, 2, quantile, probs = 1-(alpha/2))
    
    # Combine predictions with the original data
    pred_dat <- cbind(pred_dat, fit = pred_mean, lwr = pred_lwr, upr = pred_upr)
    res_dat <- pred_dat %>% select(party, fit, lwr, upr) %>%
      left_join(dat_list$dat_results, ., by = join_by("party"))
    
    # Step 4i: Add an identifier to the results indicating which predictor set was used
    res_dat$model_id <- paste0("model_", name)
    
    # Step 4j: Store the results in the list
    all_res_dat[[name]] <- res_dat
  }
  
  # Step 5: Combine the results from all models into a single data frame
  combined_res_dat <- do.call(rbind, all_res_dat)
  
  # Step 6: Return the combined results
  return(combined_res_dat)
}

# Function to run one iteration of the loop for processing election data
process_election_lm <- function(elec, dat=data_structural,dv="voteshare",impute_polls=T,
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
    
    # Step 4c: Prepare the prediction data by filtering for the selected election and selecting the relevant predictors
    pred_dat <- dat_masked_filter %>% filter(elec_ind == elec) 
    pred_x <- pred_dat %>% select(predictor_sets[[name]])
    
    # Step 4d: Predict the vote share using the fitted model
    pred_dat <- cbind(pred_dat,  as.matrix(predict(mdl, pred_x, interval = "prediction")))
    res_dat <- pred_dat %>% select(party,fit, lwr,upr) %>%
      left_join(dat_list$dat_results,.,by = join_by("party"))
    
    # Step 4e: Add an identifier to the results indicating which predictor set was used
    res_dat$model_id <- paste0("model_", name)
    
    # Step 4f: Store the results in the list
    all_res_dat[[name]] <- res_dat
  }
  
  # Step 5: Combine the results from all models into a single data frame
  combined_res_dat <- do.call(rbind, all_res_dat)
  
  # Step 6: Return the combined results
  return(combined_res_dat)
}

# Function to run one iteration of the loop for processing election data with Bayesian linear regression using bayesglm
est_election_bayesstan <- function(dat=data_structural, dv="voteshare", impute_polls=T,
                                       filter_basedon=list("days"="pollsNA_days==0",
                                                           "weeks"="pollsNA_weeks==0",
                                                           "months"="pollsNA_months==0"),
                                       predictor_sets = list(
                                         "months" = c("polls_months", "voteshare_l1", "pm", "gov", "new_party"),
                                         "weeks" = c("polls_weeks", "voteshare_l1", "pm", "gov", "new_party"),
                                         "days" = c("polls_days", "voteshare_l1", "pm", "gov", "new_party")
                                       )) {  # Specify the number of posterior samples
  
  
  # Step 2: Impute missing poll data in the masked dataset
  if(impute_polls){
    dat <- impute_poll(dat)
  }
  
  # Step 3: Initialize an empty list to store results from each predictive model
  res_mdl <- list()
  
  # Step 4: Loop through each set of predictors (e.g., months, weeks, days)
  for (name in names(predictor_sets)) {
    
    # Step 4a: Create a Bayesian linear model formula based on the current set of predictors
    bayes_lm_form <- as.formula(paste(dv, "~", paste(predictor_sets[[name]], collapse = " + ")))
    
    # Step 4b: Filter the data based on the current condition
    if(!is.null(filter_basedon[[name]])){
      dat_filter <-  dat %>% filter(eval(parse(text = filter_basedon[[name]])))
    } else{
      dat_filter <-  dat
    }
    
    # Bayes Model
    mdl <-  stan_glm(bayes_lm_form, data = dat_filter,
                     family = gaussian(),
                     prior = normal(0,5),
                     prior_intercept = normal(0,5),
                     prior_aux =exponential(rate = 1),
                     # for speed of example only
                     chains = 2, iter = 3000,
                     seed = 238523862)
    
    res_mdl[[name]] <- mdl
  }
  
  
  # Step 5: Return the combined results
  return(res_mdl)
}

# Function to run one iteration of the loop for processing election data with Bayesian linear regression using bayesglm
est_election_lm <- function(dat=data_structural, dv="voteshare", impute_polls=T,
                                   filter_basedon=list("days"="pollsNA_days==0",
                                                       "weeks"="pollsNA_weeks==0",
                                                       "months"="pollsNA_months==0"),
                                   predictor_sets = list(
                                     "months" = c("polls_months", "voteshare_l1", "pm", "gov", "new_party"),
                                     "weeks" = c("polls_weeks", "voteshare_l1", "pm", "gov", "new_party"),
                                     "days" = c("polls_days", "voteshare_l1", "pm", "gov", "new_party")
                                   )) {  # Specify the number of posterior samples
  
  
  # Step 2: Impute missing poll data in the masked dataset
  if(impute_polls){
    dat <- impute_poll(dat)
  }
  
  # Step 3: Initialize an empty list to store results from each predictive model
  res_mdl <- list()
  
  # Step 4: Loop through each set of predictors (e.g., months, weeks, days)
  for (name in names(predictor_sets)) {
    
    # Step 4a: Create a linear model formula based on the current set of predictors
    lm_form <- as.formula(paste(dv, "~", paste(predictor_sets[[name]], collapse = " + ")))
    
    # Step 4b: Filter the data based on the current condition
    if(!is.null(filter_basedon[[name]])){
      dat_filter <-  dat %>% filter(eval(parse(text = filter_basedon[[name]])))
    } else{
      dat_filter <-  dat
    }
    
    # Linear Model
    mdl <-  lm(lm_form, data = dat_filter)
    
    res_mdl[[name]] <- mdl
  }
  
  
  # Step 5: Return the combined results
  return(res_mdl)
}

# Function to run one iteration of the loop for processing election data
process_election_lm <- function(elec, 
                                predictor_sets = list(
                                  "months" = c("polls_months", "voteshare_l1", "pm", "gov", "new_party"),
                                  "weeks" = c("polls_weeks", "voteshare_l1", "pm", "gov", "new_party"),
                                  "days" = c("polls_days", "voteshare_l1", "pm", "gov", "new_party")
                                )) {
  
  # Step 1: Mask the selected election results and prepare the data for processing
  dat_list <- mask_and_select(sel = elec)
  dat_masked <- dat_list$dat_masked
  
  # Step 2: Impute missing poll data in the masked dataset
  dat_masked <- imput_poll(dat_masked)
  
  # Step 3: Initialize an empty list to store results from each predictive model
  all_res_dat <- list()
  
  # Step 4: Loop through each set of predictors (e.g., months, weeks, days)
  for (name in names(predictor_sets)) {
    
    # Step 4a: Create a linear model formula based on the current set of predictors
    lm_form <- as.formula(paste("voteshare ~", paste(predictor_sets[[name]], collapse = " + ")))
    
    # Step 4b: Fit the linear model using the masked data
    mdl <- lm(lm_form, data = dat_masked)
    
    # Step 4c: Prepare the prediction data by filtering for the selected election and selecting the relevant predictors
    pred_dat <- dat_masked %>% filter(elec_ind == elec) %>% select(predictor_sets[[name]])
    
    # Step 4d: Predict the vote share using the fitted model
    res_dat <- dat_list$dat_results
    res_dat$pred <- predict(mdl, pred_dat)
    
    # Step 4e: Add an identifier to the results indicating which predictor set was used
    res_dat$model_id <- paste0("model_", name)
    
    # Step 4f: Store the results in the list
    all_res_dat[[name]] <- res_dat
  }
  
  # Step 5: Combine the results from all models into a single data frame
  combined_res_dat <- do.call(rbind, all_res_dat)
  
  # Step 6: Return the combined results
  return(combined_res_dat)
}
