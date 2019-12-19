devtools::install_github(repo = "maksimhorowitz/nflscrapR")
library(nflscrapR)
library(tidyverse)
library(teamcolors)

# Gather Game IDs for 2018 week 2
week_2_games <- scrape_game_ids(2018, weeks = 2)

# Use Pander to provide more presentable table in console
week_2_games %>%
  pander::pander()

# Scrape play by play data for Steelers/Chiefs game
kc_vs_pit_pbp <- week_2_games %>%
  filter(home_team == "PIT") %>%
  pull(game_id) %>%
  scrape_json_play_by_play()

# Grab NFL team colors
nfl_teamcolors <- teamcolors %>% filter(league == "nfl")

pit_color <- nfl_teamcolors %>%
  filter(name == "Pittsburgh Steelers") %>%
  pull(primary)
kc_color <- nfl_teamcolors %>%
  filter(name == "Kansas City Chiefs") %>%
  pull(primary)

# Filter where win probabilites exists, using seconds left and win probability added for X and Y,
kc_vs_pit_pbp %>%
  filter(!is.na(home_wp),
         !is.na(away_wp)) %>%
  dplyr::select(game_seconds_remaining,
                home_wp,
                away_wp) %>%
  gather(team, wpa, -game_seconds_remaining) %>%
  ggplot(aes(x = game_seconds_remaining, y = wpa, color = team)) +
  geom_line(size = 2) +
  geom_hline(yintercept = 0.5, color = "gray", linetype = "dashed") +
  scale_color_manual(labels = c("KC", "PIT"),
                     values = c(kc_color, pit_color),
                     guide = FALSE) +
  scale_x_reverse(breaks = seq(0, 3600, 300)) + 
  annotate("text", x = 3000, y = .75, label = "KC", color = kc_color, size = 8) + 
  annotate("text", x = 3000, y = .25, label = "PIT", color = pit_color, size = 8) +
  geom_vline(xintercept = 900, linetype = "dashed", black) + 
  geom_vline(xintercept = 1800, linetype = "dashed", black) + 
  geom_vline(xintercept = 2700, linetype = "dashed", black) + 
  geom_vline(xintercept = 0, linetype = "dashed", black) + 
  labs(
    x = "Time Remaining (seconds)",
    y = "Win Probability",
    title = "Week 2 Win Probability Chart",
    subtitle = "Kansas City Chiefs vs. Pittsburgh Steelers",
    caption = "Data from nflscrapR"
  ) + theme_bw()