#' ---
#' title: "Descriptives of Data"
#' date: "2024"
#' ---

###########################
# Prepare environment
###########################

source("_auxilary/packages.R")
source("_auxilary/functions.R")

###########################
# Load data
###########################

state_results <- read.csv("data/state-election-results.csv")
state_polls <- read.csv("data/state-polls.csv")

###########################
# Data Processing
###########################

# Convert date columns to Date class
state_polls <- state_polls %>% mutate(date = as_date(date))
state_results <- state_results %>% mutate(electiondate = as_date(electiondate))



###########################
# Create Polling Density Plot
###########################

# Create date sequence for x-axis
mydates <- c(seq(as.Date("1946-01-01"), as.Date("2024-12-31"), by = "5 years"), 
             as.Date("2024-12-31"))

# Calculate polling density by year
poll_density_by_year <- state_polls %>%
  group_by(land, year = year(date)) %>%
  summarise(
    poll_count = n(),
    min_date = min(date, na.rm = TRUE) %>% as_date(),
    max_date = max(date, na.rm = TRUE) %>% as_date()
  ) %>%
  mutate(state = map_state_names(land))

# Create density plot
ggplot() +
  geom_point(
    data = state_results %>% select(land, electiondate) %>% unique(),
    aes(x = land %>% map_state_names(), y = electiondate),
    color = "grey", size = 5, shape = 18
  ) +
  geom_segment(
    data = poll_density_by_year,
    aes(
      x = fct_reorder(state, state, .desc = TRUE),
      xend = fct_reorder(state, state, .desc = TRUE),
      y = min_date,
      yend = max_date,
      size = poll_count
    ),
    color = "black", alpha = .8
  ) +
  scale_size_continuous(range = c(0.5, 5)) +
  scale_y_date(date_labels = "%Y", breaks = mydates, 
               limits = c(ymd("1990-01-01"), NA)) +
  coord_flip() + 
  xlab("") + 
  ylab("") +
  labs(title = "State Election Dates and Polling Coverage") +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(10, 10, 10, 10),
    legend.position = "none"
  )

ggsave(filename = "output/figures/polls_coverage_density.pdf", width = 8, height = 4)

###########################
# Create State-Specific Polling Plots
###########################

if (!dir.exists("output/figures/polling")) {
  dir.create("output/figures/polling")
}

for (thisstate in unique(state_results$land)) {
  # Process state polls data
  dat <- state_polls %>%
    filter(land == thisstate) %>%
    filter(party %in% c("cdu", "spd", "fdp", "gru", "lin", "afd", "bsw")) %>%
    filter(party != "oth")
  
  # Add "oth" party rows
  oth_rows <- dat %>%
    distinct(date, land, institut) %>%
    mutate(party = "oth", poll_share = NA)
  
  dat <- dat %>%
    bind_rows(oth_rows) %>%
    group_by(date, land, institut) %>%
    mutate(poll_share = if_else(party == "oth", 
                               100 - sum(poll_share, na.rm = TRUE), 
                               poll_share)) %>%
    ungroup() %>%
    arrange(date, land, institut, party)
  
  dat$party_name <- map_party_names(dat$party)
  dat$date <- as.Date(dat$date)
  
  # Process state results data
  dat2 <- state_results %>%
    filter(party %in% c("cdu", "spd", "fdp", "gru", "lin", "afd", "bsw")) %>% 
    filter(land == thisstate) %>%
    filter(party != "oth")
  
  # Add "oth" party rows for results
  oth_rows2 <- dat2 %>%
    distinct(electiondate, land) %>%
    mutate(party = "oth", vote_share = NA)
  
  dat2 <- dat2 %>%
    anti_join(oth_rows2, by = c("electiondate", "land", "party")) %>%
    bind_rows(oth_rows2) %>%
    arrange(electiondate, land, party) %>%
    group_by(electiondate) %>%
    mutate(vote_share = if_else(party == "oth", 
                               100 - sum(vote_share, na.rm = TRUE), 
                               vote_share)) %>%
    ungroup()
  
  dat2$party_name <- map_party_names(dat2$party)
  dat2$electiondate <- as.Date(dat2$electiondate)
  
  # Calculate rolling mean
  complete_dat <- dat %>%
    group_by(party_name) %>%
    complete(date = seq(min(date), max(date), by = "day")) %>%
    arrange(party_name, date) %>%
    group_by(party_name) %>%
    mutate(
      poll_share = zoo::na.approx(poll_share, na.rm = FALSE),
      roll_share = zoo::rollmean(poll_share, k = 1000, fill = NA, align = "center"),
      roll_share = zoo::na.locf(roll_share, na.rm = FALSE)
    )
  
  dat3 <- complete_dat %>% filter(!is.na(poll_share))
  
  # Create plot
  p <- ggplot(data = dat2, 
              aes(x = electiondate, y = vote_share/100, 
                  shape = party_name, fill = party_name, color = party_name)) +
    geom_point(data = dat, 
               aes(x = date, y = poll_share/100, color = party_name, fill = party_name), 
               alpha = .3, shape = 21, show.legend = F, size = .5) +
    geom_step(data = dat3, 
              aes(x = date, y = roll_share/100, color = party_name), 
              size = 1, alpha = .2, show.legend = F) +
    geom_point(size = 5, color = "grey") +
    scale_color_manual(
      name = "Party", 
      labels = c("CDU", "SPD", "FDP", "Greens", "Linke", "AfD", "BSW", "Other"),
      values = party_colors[names(party_colors) %in% c("CDU", "SPD", "FDP", "Greens", "Linke", "AfD", "BSW", "Other")],
      breaks = c("CDU", "SPD", "FDP", "Greens", "Linke", "AfD", "BSW", "Other")
    ) +
    scale_fill_manual(
      name = "Party",
      labels = c("CDU", "SPD", "FDP", "Greens", "Linke", "AfD", "BSW", "Other"),
      values = party_colors[names(party_colors) %in% c("CDU", "SPD", "FDP", "Greens", "Linke", "AfD", "BSW", "Other")],
      breaks = c("CDU", "SPD", "FDP", "Greens", "Linke", "AfD", "BSW", "Other")
    ) +
    scale_shape_manual(
      name = "Party",
      labels = c("CDU", "SPD", "FDP", "Greens", "Linke", "AfD", "BSW", "Other"),
      values = c(21, 22, 23, 24, 25, 21, 22, 20),
      breaks = c("CDU", "SPD", "FDP", "Greens", "Linke", "AfD", "BSW", "Other")
    ) +
    scale_x_date(
      limits = c(min(dat$date), NA),
      breaks = (dat2 %>% filter(electiondate > min(dat$date, na.rm = T)))$electiondate %>% as.Date,
      date_labels = "%Y"
    ) +
    ylim(c(NA, .6)) +
    labs(title = str_c("Polling and Election Results in ", map_state_names(thisstate))) +
    ylab("") +
    xlab("") +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      axis.text.x = element_text(size = 12, face = "bold", color = "black"),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # Special handling for Hesse
  if(thisstate == "he") {
    p <- p + scale_x_date(
      limits = c(min(dat$date), NA),
      breaks = (dat2 %>% filter(electiondate > min(dat$date, na.rm = T) & 
                               electiondate != ymd("2008-01-27")))$electiondate %>% as.Date,
      date_labels = "%Y"
    )
  }
  
  ggsave(filename = str_c("output/figures/polling/fig_poll_", thisstate, ".pdf"), 
         width = 8, height = 4)
}

###########################
# Summary Statistics
###########################

# Number of polls
cat("Total number of polls:", 
    state_polls %>% select(land, date, institut) %>% unique %>% nrow, "\n")

# Date range of polls
cat("Date range of polls:", 
    state_polls %>% summarise(min_date = min(date, na.rm = T) %>% as_date()) %>% pull(), 
    "to", 
    state_polls %>% summarise(max_date = max(date, na.rm = T) %>% as_date()) %>% pull(), "\n")

# Number of election dates
cat("Total number of election dates:", 
    state_results %>% select(land, electiondate) %>% unique %>% nrow, "\n")

# Number of election dates with poll coverage
cat("Number of election dates with poll coverage:", 
    state_polls %>% 
      group_by(land) %>% 
      summarise(min_date = min(date, na.rm = T) %>% as_date()) %>% 
      arrange(min_date) %>% 
      select(land, min_date) %>% 
      merge(state_results %>% select(land, electiondate) %>% unique, by = "land") %>% 
      select(land, min_date, electiondate) %>% 
      arrange(min_date) %>% 
      filter(min_date < electiondate) %>% 
      nrow, "\n")

# Date range of state elections
cat("Date range of state elections:", 
    state_results %>% summarise(min_date = min(electiondate, na.rm = T) %>% as_date()) %>% pull(), 
    "to", 
    state_results %>% summarise(max_date = max(electiondate, na.rm = T) %>% as_date()) %>% pull(), "\n")

# Polls per year
cat("\nPolls per year:\n")
state_polls %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  arrange(year) %>% 
  print()

# Polling institutes summary
cat("\nPolling institutes summary:\n")
cat("Number of unique institutes:", state_polls$institut %>% unique %>% length, "\n")
cat("\nTop polling institutes:\n")
state_polls$institut %>% table %>% sort(decreasing = T) %>% head(10) %>% print()
