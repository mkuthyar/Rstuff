devtools::install_github(repo = "maksimhorowitz/nflscrapR")
library(nflscrapR)
library(tidyverse)
library(teamcolors)

# Gather Game IDs for 2018 week 2
week_2_games <- scrape_game_ids(2019, weeks = 17)

# Use Pander to provide more presentable table in console
week_2_games %>%
  pander::pander()

# Scrape play by play data for Steelers/Chiefs game
sea_vs_sf_pbp <- week_2_games %>%
  filter(home_team == "SEA") %>%
  pull(game_id) %>%
  scrape_json_play_by_play()

# Grab NFL team colors
nfl_teamcolors <- teamcolors %>% filter(league == "nfl")

sea_color <- nfl_teamcolors %>%
  filter(name == "Seattle Seahawks") %>%
  pull(primary)
sf_color <- nfl_teamcolors %>%
  filter(name == "San Francisco 49ers") %>%
  pull(primary)

# Filter where win probabilites exists, using seconds left and win probability added for X and Y,
sea_vs_sf_pbp %>%
  filter(!is.na(home_wp),
         !is.na(away_wp)) %>%
  dplyr::select(game_seconds_remaining,
                home_wp,
                away_wp) %>%
  gather(team, wpa, -game_seconds_remaining) %>%
  ggplot(aes(x = game_seconds_remaining, y = wpa, color = team)) +
  geom_line(size = 2) +
  geom_hline(yintercept = 0.5, color = "gray", linetype = "dashed") +
  scale_color_manual(labels = c("SF", "SEA"),
                     values = c(kc_color, pit_color),
                     guide = FALSE) +
  scale_x_reverse(breaks = seq(0, 3600, 300)) + 
  annotate("text", x = 3000, y = .75, label = "SF", color = sf_color, size = 8) + 
  annotate("text", x = 3000, y = .25, label = "SEA", color = sea_color, size = 8) +
  geom_vline(xintercept = 900, linetype = "dashed", black) + 
  geom_vline(xintercept = 1800, linetype = "dashed", black) + 
  geom_vline(xintercept = 2700, linetype = "dashed", black) + 
  geom_vline(xintercept = 0, linetype = "dashed", black) + 
  labs(
    x = "Time Remaining (seconds)",
    y = "Win Probability",
    title = "Week 17 Win Probability Chart",
    subtitle = "San Francisco 49ers vs. Seattle Seahawks",
    caption = "Data from nflscrapR"
  ) + theme_bw()