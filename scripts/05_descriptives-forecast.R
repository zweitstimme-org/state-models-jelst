#' ---
#' title: "Figures Forcast Data"
#' date: " 2024"
#' ---


###########################
# Prepare environment
###########################


source("_auxilary/packages.R")
source("_auxilary/functions.R")

###########################
# Load and combine forecast data
###########################

# Load Brandenburg data
load(file = "output/forecast/02_fcst_state-bb-days.Rdata")

# Define objects to process
myobjects <- c("fcst_ci_days", "fcst_ci_weeks", "fcst_ci_months",
               "fcstpoll_ci_days", "fcstpoll_ci_weeks", "fcstpoll_ci_months",
               "fcstfund_ci_days", "fcstfund_ci_weeks", "fcstfund_ci_months",
               "fcst_draws_weeks", "fcst_draws_days")

# Extract Brandenburg data
for (i in seq_along(myobjects)) {
  assign(str_c(myobjects[i], "_bb"), get(myobjects[i]) %>% filter(state == "bb"))
}

# Load and combine with Saxony and Thuringia data
load("output/forecast/02_fcst_state-prereg-sn-th.Rdata")

# Combine all data
for (i in seq_along(myobjects)) {
  assign(myobjects[i], get(myobjects[i]) %>% filter(state != "bb") %>% rbind(get(str_c(myobjects[i], "_bb"))))
}

# Define objects to process
myobjects <- c("fcst_ci_days", "fcst_ci_weeks", "fcst_ci_months",
               "fcstpoll_ci_days", "fcstpoll_ci_weeks", "fcstpoll_ci_months",
               "fcstfund_ci_days", "fcstfund_ci_weeks", "fcstfund_ci_months")

# Create combined dataframe
all_fcst <- data.frame()
for (myobject in myobjects) {
  all_fcst <- get(myobject) %>% 
    mutate(model = myobject) %>% 
    bind_rows(all_fcst) %>% 
    mutate(party_name = map_party_names(party))
}

# Clean up the combined dataframe
all_fcst <- all_fcst %>% 
  mutate(state = map_state_names(state),
         lead = str_extract(model, "days|weeks|months"),
         lead = factor(lead, levels = c("months", "weeks", "days")),
         model = str_remove(model, "days|weeks|months") %>% str_remove("_ci_"),
         model = case_when(
           model == "fcstpoll" ~ "Polls",
           model == "fcstfund" ~ "Fund.",
           model == "fcst" ~ "Polls + Fund.",
           TRUE ~ model
         ),
         model = factor(model, levels = c("Polls + Fund.", "Polls", "Fund.")))

###########################
# Create forecast plots
###########################

create_forecast_plot <- function(data, state_name, model_name, lead_time, results, party_colors, language = "en") {
  # Filter data for specific state, model and lead time
  plot_data <- data %>% 
    dplyr::filter(state == state_name, model == model_name, lead == lead_time)
  
  # Debug information
  cat(sprintf("Creating plot for state: %s, model: %s, lead: %s\n", state_name, model_name, lead_time))
  cat(sprintf("Number of rows in plot_data: %d\n", nrow(plot_data)))
  
  # Create base plot
  p <- ggplot(plot_data, aes(x = reorder(party_name, -fit), y = fit, color = party_name, fill = party_name)) +
    geom_col(data = results[results$state == state_name, ], aes(x = party_name, y = share, group = NA), 
             color = NA, fill = "grey", alpha = .2) +
    scale_color_manual(values = party_colors, labels = names(party_colors)) +
    scale_fill_manual(values = party_colors, labels = names(party_colors)) +
    geom_hline(yintercept = 0.05, linetype = "dotted", size = .5, color = "black") +
    geom_linerange(aes(ymin = lwr, ymax = upr), linewidth = 10, alpha = 0.3) +
    geom_point(size = 6, color = "white", shape = 21, stroke = 2) +
    geom_point(size = 2, fill = "white", shape = 21) +
    geom_label(aes(y = upr + 0.03, label = paste(round(fit * 100, 1), "%")), fill = NA, color = "black") +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.5)) +
    ylim(0, 0.55) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.text.x = element_text(size = 8, face = "bold", color = "black"),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # Add language-specific labels
  if (language == "de") {
    p <- p + 
      labs(
        title = str_c("Evaluation der Vorhersagen — ", state_name %>% str_replace_all(c("Thuringia" = "Thüringen", "Saxony" = "Sachsen"))),
        subtitle = paste0("2 ", lead_time %>% str_replace_all(c("days" = "Tage", "weeks" = "Wochen", "months" = "Monate")), 
                         " Vorlaufzeit — Modell ", model_name),
        caption = "Die Prognosen basieren auf einem bayesianischen Regressionsmodell, das auf Landtagswahlen von 1990 bis 2023 trainiert wurde.\nDie Intervalle zeigen 83%-Wahrscheinlichkeitsspannen, und der Punkt repräsentiert den posterioren Mittelwert.\nDie Balken zeigen die Wahlergebnisse.",
        x = NULL,
        y = NULL
      ) +
      scale_x_discrete(labels = function(x) {
        ifelse(x == "Greens", "Grüne", x)
      })
  } else {
    p <- p + 
      labs(
        title = str_c("Evaluation of the Forecasts — ", state_name),
        subtitle = paste0("2 ", lead_time %>% str_to_title(), " Lead Time — ", model_name, " Model"),
        caption = "The forecasts are based on a Bayesian regression model fitted on state elections from 1990 to 2023.\nThe intervals show 5/6 Credible Intervals, and the point represents the posterior mean.\nThe bars show the election results.",
        x = NULL,
        y = NULL
      )
  }
  
  return(p)
}

###########################
# Save forecast plots
###########################

save_forecast_plots <- function(all_fcst, results, party_colors, base_dir = "output/figures") {
  # Create base output directory if it doesn't exist
  if (!dir.exists(base_dir)) dir.create(base_dir, recursive = TRUE)
  
  # Get unique values from the data
  states <- unique(all_fcst$state)
  models <- unique(all_fcst$model)
  leads <- unique(all_fcst$lead)
  
  # Print unique values for debugging
  cat("Unique states:", paste(states, collapse = ", "), "\n")
  cat("Unique models:", paste(models, collapse = ", "), "\n")
  cat("Unique leads:", paste(leads, collapse = ", "), "\n")
  
  # Generate plots for each combination
  for (state_name in states) {
    for (model_name in models) {
      for (lead_time in leads) {
        # Create language-specific directories
        for (lang in c("en", "de")) {
          # Create state-specific directory
          state_dir <- file.path(base_dir, lang, state_name)
          if (!dir.exists(state_dir)) dir.create(state_dir, recursive = TRUE)
          
          # Generate plot
          p <- create_forecast_plot(all_fcst, state_name, model_name, lead_time, results, party_colors, lang)
          
          # Create filename with state, model, lead time, and language
          filename <- paste0(state_name, "_", model_name, "_", lead_time, "_", lang, ".pdf")
          
          # Save plot
          ggsave(filename = file.path(state_dir, filename), 
                 plot = p, height = 6, width = 6*1.5)
        }
      }
    }
  }
}

###########################
# Main execution
###########################

# Create results dataframe
results <- data.frame(
  party_name = rep(c("CDU", "SPD", "Greens", "FDP", "Linke", "AfD", "BSW"), 3),
  share = c(31.9, 7.3, 5.1, 0.9, 4.5, 30.6, 11.8,   # Saxony (SN) shares
            23.6, 6.1, 3.2, 1.1, 13.1, 32.8, 15.8, # Thuringia (TH) shares
            12.1, 30.9, 4.1, 1.2, 3.0, 29.2, 13.5) / 100,
  state = rep(c("Saxony", "Thuringia", "Brandenburg"), each = 7)
)

# Check data structure
cat("\nChecking data structure:\n")
cat("Number of rows in all_fcst:", nrow(all_fcst), "\n")
cat("Unique combinations of state, model, and lead:\n")
print(all_fcst %>% 
  group_by(state, model, lead) %>% 
  summarise(n = n()) %>% 
  arrange(state, model, lead))

# Generate and save plots
save_forecast_plots(all_fcst, results, party_colors)

# Calculate and plot evaluation metrics
plot_data <- all_fcst %>% 
  merge(results, by = c("state", "party_name")) %>%
  group_by(model, lead, state) %>% 
  summarise(
    mae = mean(abs(fit*100 - share*100)), 
    rmse = sqrt(mean((fit*100 - share*100)^2))
  ) %>%
  rename(predictors = model) %>% 
  mutate(lead = paste("2", lead),
         lead = factor(lead, levels = c("2 months", "2 weeks", "2 days")))

# Create and save evaluation plots
for (metric in c("mae", "rmse")) {
  p <- ggplot(plot_data,
              aes(x = .data[[metric]], y = lead, col = predictors, shape = predictors)) +
    geom_point(size = 5, alpha=0.8) +
    geom_text_repel(aes(label = paste(round(.data[[metric]], 2))), 
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
    scale_x_continuous(breaks = seq(0, max(plot_data[[metric]]), by = 1),
                      minor_breaks = seq(0, max(plot_data[[metric]]), by = 0.5)) +
    scale_color_grey() + 
    coord_flip() +
    labs(
      title = "Forecast Evaluation Forecast State Elections 2024",
      subtitle = "Forecast based on Bayesian Linear Regression",
      x = if(metric == "mae") "Mean Absolute Error" else "Root Mean Square Error",
      y = "Lead Time"
    ) +
    facet_wrap(~state)
  
  # Save plot
  ggsave(filename = paste0("output/figures/forecast_evaluation_", metric, ".pdf"), 
         plot = p, height = 6, width = 6*1.5)
}

# Calculate overall metrics
overall_metrics <- all_fcst %>% 
  merge(results, by = c("state", "party_name")) %>%
  group_by(model, lead) %>%
  summarise(
    mae = mean(abs(fit*100 - share*100), na.rm = T), 
    rmse = sqrt(mean((fit*100 - share*100)^2, na.rm = T))
  )

# Calculate interval coverage
coverage_by_model_lead <- all_fcst %>% 
  merge(results, by = c("state", "party_name")) %>% 
  mutate(correct_uncertainty = (share > lwr) & (share < upr)) %>%
  group_by(model, lead) %>% 
  summarise(correct_uncertainty = mean(correct_uncertainty, na.rm = T))

# Calculate overall coverage
coverage_by_model <- all_fcst %>% 
  merge(results, by = c("state", "party_name")) %>% 
  mutate(correct_uncertainty = (share > lwr) & (share < upr)) %>%
  group_by(model) %>% 
  summarise(correct_uncertainty = mean(correct_uncertainty, na.rm = T))

# Calculate coalition probabilities
# load("output/models/02_fcst_state-bb-days.Rdata")

# Helper function for coalition calculations
coal_majo <- function(share, share_above_hurdle) {
  if(any(share < 0.05)) {
    return(FALSE)
  } else {
    return(sum(share)/share_above_hurdle > 0.5)
  }
}

# Calculate coalition probabilities for each state
coalition_probs_bb <- fcst_draws_weeks %>%
  filter(state == "bb") %>%
  group_by(draw) %>%
  summarise(
    share_above_hurdle = sum(posterior_draw[posterior_draw > 0.05]),
    afd_larger = max(posterior_draw) == posterior_draw[party == "afd"],
    spd_larger = max(posterior_draw) == posterior_draw[party == "spd"],
    bsw_larger = max(posterior_draw) == posterior_draw[party == "bsw"],
    cdu_larger = max(posterior_draw) == posterior_draw[party == "cdu"],
    cdu_spd_greens = coal_majo(c(posterior_draw[party == "cdu"],
                                posterior_draw[party == "spd"],
                                posterior_draw[party == "gru"]),
                              share_above_hurdle),
    left_hurdle = posterior_draw[party == "lin"] > 0.05,
    greens_hurdle = posterior_draw[party == "gru"] > 0.05,
    cdu_bsw = coal_majo(c(posterior_draw[party == "cdu"],
                         posterior_draw[party == "bsw"]),
                       share_above_hurdle),
    cdu_bsw_spd = coal_majo(c(posterior_draw[party == "cdu"],
                             posterior_draw[party == "bsw"],
                             posterior_draw[party == "spd"]),
                           share_above_hurdle),
    afd_bsw = coal_majo(c(posterior_draw[party == "afd"],
                         posterior_draw[party == "bsw"]),
                       share_above_hurdle)
  ) %>%
  ungroup() %>%
  summarise(
    "largest_party_spd" = mean(spd_larger)*100,
    "largest_party_afd" = mean(afd_larger)*100,
    "largest_party_bsw" = mean(bsw_larger)*100,
    "largest_party_cdu" = mean(cdu_larger)*100,
    "coal_cduspdgreens" = mean(cdu_spd_greens)*100,
    "coal_cdubsw" = mean(cdu_bsw)*100,
    "coal_cdubswspd" = mean(cdu_bsw_spd)*100,
    "coal_without_afdbsw" = 100 - mean(afd_bsw)*100,
    "greens_above_hurdle" = mean(greens_hurdle)*100,
    "left_above_hurdle" = mean(left_hurdle)*100
  ) %>%
  round(0)

coalition_probs_sn <- fcst_draws_days %>%
  filter(state == "sn") %>%
  group_by(draw) %>%
  summarise(
    share_above_hurdle = sum(posterior_draw[posterior_draw > 0.05]),
    afd_larger = max(posterior_draw) == posterior_draw[party == "afd"],
    cdu_spd_greens = coal_majo(c(posterior_draw[party == "cdu"],
                                posterior_draw[party == "spd"],
                                posterior_draw[party == "gru"]),
                              share_above_hurdle),
    greens_hurdle = posterior_draw[party == "gru"] > 0.05,
    cdu_bsw = coal_majo(c(posterior_draw[party == "cdu"],
                         posterior_draw[party == "bsw"]),
                       share_above_hurdle),
    cdu_bsw_spd = coal_majo(c(posterior_draw[party == "cdu"],
                             posterior_draw[party == "bsw"],
                             posterior_draw[party == "spd"]),
                           share_above_hurdle)
  ) %>%
  ungroup() %>%
  summarise(
    "largest_party_afd" = mean(afd_larger)*100,
    "coal_cduspdgreens" = mean(cdu_spd_greens)*100,
    "coal_cdubsw" = mean(cdu_bsw)*100,
    "coal_cdubswspd" = mean(cdu_bsw_spd)*100,
    "greens_above_hurdle" = mean(greens_hurdle)*100
  )

coalition_probs_th <- fcst_draws_days %>%
  filter(state == "th") %>%
  group_by(draw) %>%
  summarise(
    share_above_hurdle = sum(posterior_draw[posterior_draw > 0.05]),
    afd = max(posterior_draw) == posterior_draw[party == "afd"],
    lin_spd_greens = coal_majo(c(posterior_draw[party == "lin"],
                                posterior_draw[party == "spd"],
                                posterior_draw[party == "gru"]),
                              share_above_hurdle),
    cdu_bsw_spd = coal_majo(c(posterior_draw[party == "cdu"],
                             posterior_draw[party == "spd"],
                             posterior_draw[party == "bsw"]),
                           share_above_hurdle),
    lin_bsw_spd = coal_majo(c(posterior_draw[party == "lin"],
                             posterior_draw[party == "spd"],
                             posterior_draw[party == "bsw"]),
                           share_above_hurdle),
    greens_hurdle = posterior_draw[party == "gru"] > 0.05
  ) %>%
  ungroup() %>%
  summarise(
    "coal_linspdgreens" = mean(lin_spd_greens)*100,
    "coal_cdubswspd" = mean(cdu_bsw_spd)*100,
    "coal_linbswspd" = mean(lin_bsw_spd)*100,
    "greens_above_hurdle" = mean(greens_hurdle)*100,
    "afd_largest_party" = mean(afd)*100
  )

# Save Forecast Draws in Wide Format
fcst_draws_weeks_wide <- fcst_draws_weeks %>% 
  pivot_wider(
    names_from = party,
    values_from = posterior_draw
  )

# Create tables for pre-registration
output_dir <- "output/tables"

# Define party name mapping
party_names <- c(
  "afd" = "AfD",
  "bsw" = "BSW",
  "cdu" = "CDU",
  "fdp" = "FDP",
  "gru" = "Greens",
  "lin" = "Linke",
  "spd" = "SPD"
)

# Function to format data for a given state
format_state_data <- function(data, state_code) {
  data %>%
    filter(state == state_code) %>%
    arrange(-fit) %>%
    mutate(
      Party = party_names[party],
      `Lower Bound` = scales::percent(lwr, accuracy = 0.1),
      `Expected Share` = scales::percent(fit, accuracy = 0.1),
      `Upper Bound` =  scales::percent(upr, accuracy = 0.1)
    ) %>%
    select(Party, `Lower Bound`, `Expected Share`, `Upper Bound`) 
}

# Generate and save LaTeX tables for all datasets
for (dataset in c("fcst_ci", "fcstpoll_ci", "fcstfund_ci")) {
  for (time in c("days", "weeks", "months")) {
    data <- get(paste0(dataset, "_", time))
    
    # Format data for each state
    saxony_table <- format_state_data(data, "sn")
    thuringia_table <- format_state_data(data, "th")
    brandenburg_table <- format_state_data(data, "bb")
    
    # Create LaTeX tables
    saxony_latex <- xtable(saxony_table, caption = "Forecast for Saxony", label = "tab:saxony")
    thuringia_latex <- xtable(thuringia_table, caption = "Forecast for Thuringia", label = "tab:thuringia")
    brandenburg_latex <- xtable(brandenburg_table, caption = "Forecast for Brandenburg", label = "tab:brandenburg")
    
    # Save tables
    print(saxony_latex, file = file.path(output_dir, paste0(dataset, "_", time, "_sn.tex")), 
          type = "latex", include.rownames = FALSE, floating = FALSE)
    print(thuringia_latex, file = file.path(output_dir, paste0(dataset, "_", time, "_th.tex")), 
          type = "latex", include.rownames = FALSE, floating = FALSE)
    print(brandenburg_latex, file = file.path(output_dir, paste0(dataset, "_", time, "_bb.tex")), 
          type = "latex", include.rownames = FALSE, floating = FALSE)
  }
}


 