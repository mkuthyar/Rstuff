# Load necessary libraries
library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)
library(ggrepel)

# Read in data from Github into DF
pbp <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2019.csv"))

# Filter on plays that have an EPA value, and are pass, run, or penalties ("no play"); Removes kickoffs
pbp_rp <- pbp %>% 
  filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")

# Filter on penalties; shows that penalties don't count as pass/rush
pbp_rp %>% filter(play_type=="no_play") %>% select(desc, rush_attempt, pass_attempt)  %>% head

# Add columns for pass, rush, and success
# populated with 1 or 0 depending on type of play using indicator words, and EPA >0
pbp_rp <- pbp_rp %>%
  mutate(
    pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0)
  ) 

# Keep only run or pass plays (including penalties)
pbp_rp <- pbp_rp %>% filter(pass==1 | rush==1)

## BASIC STUFF PART 1 ##

# Filter on when LA has posession, play is a run, exclude 2PC, group by rusher name
# Display average EPA, average Success Rate, and ypc, along with # of plays
# Order in average EPA, greatest to lowest, and filter on over 40 plays
pbp_rp %>%
  filter(posteam == "SEA", rush == 1, down<=4) %>%
  group_by(rusher_player_name) %>%
  summarize(mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()) %>%
  arrange(desc(mean_epa)) %>%
  filter(plays>40)

## BASIC STUFF PART 3 ##

# Filter all pass plays, and show # of plays, average EPA per dropback, and average success rate per dropback
chart_data <- pbp_rp %>%
  filter(pass==1) %>%
  group_by(posteam) %>%
  summarise(
    num_db = n(),
    epa_per_db = sum(epa) / num_db,
    success_rate = sum(epa > 0) / num_db
  )

# Add team logo per team
nfl_logos_df <- read_csv("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")
chart <- chart_data %>% left_join(nfl_logos_df, by = c("posteam" = "team_code"))

# Create plot with X as success rate and  Y as EPS per dropback; mark points with team logo
chart %>%
  ggplot(aes(x = success_rate, y = epa_per_db)) +
  geom_image(aes(image = url), size = 0.05) +
  labs(x = "Success rate",
       y = "EPA per play",
       caption = "Data from nflscrapR",
       title = "Dropback success rate & EPA/play",
       subtitle = "2019") +
  theme_bw() +
  geom_hline(yintercept = 0.0, color = "gray", linetype = "dashed") +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

ggsave('DropbackSuccessRate&EPAplay.png', dpi=1000)

## BASIC STUFF PART 4 ##

# Filter on first and second down
# show total pass plays, total run plays, and EPA and Success rate per dropback and per Rush
chart_data <- pbp_rp %>%
  group_by(posteam) %>%
  filter(down=3) %>%
  summarise(
    n_dropbacks = sum(pass),
    n_rush = sum(rush),
    epa_per_db = sum(epa * pass) / n_dropbacks,
    epa_per_rush = sum(epa * rush) / n_rush,
    success_per_db = sum(success * pass) / n_dropbacks,
    success_per_rush = sum(success * rush) / n_rush
  )

chart <- chart_data %>% left_join(nfl_logos_df, by = c("posteam" = "team_code"))

# Create Plot with X as EPA per rush and Y as EPA per dropback, mark points with team logo 
chart %>%
  ggplot(aes(x = epa_per_rush, y = epa_per_db)) +
  geom_image(aes(image = url), size = 0.05) +
  labs(x = "Rush EPA/play",
       y = "Pass EPA/play",
       caption = "Data from nflscrapR",
       title = "3rd Down rush and pass EPA/play",
       subtitle = "2019") +
  theme_bw() +
  geom_abline(slope=1, intercept=0, alpha=.2) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

ggsave('earlydownEPApassvrush.png', dpi=1000)

# Create Plot with X as success rate per rush and Y as success rate per dropback, mark points with team logo 
chart %>%
  ggplot(aes(x = success_per_rush, y = success_per_db)) +
  geom_image(aes(image = url), size = 0.05) +
  labs(x = "Rush success rate",
       y = "Pass success rate",
       caption = "Data from nflscrapR",
       title = "Early-down rush and pass success rate",
       subtitle = "2019") +
  theme_bw() +
  geom_abline(slope=1, intercept=0, alpha=.2) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

ggsave('earlydownsuccessratepassvrush.png', dpi=1000)

## ADVANCED STUFF PART 1 ##

# Player names are NA for penalties
# Use string replacement to populate with names
pbp_players <- pbp_rp %>% 
  mutate(
    passer_player_name = ifelse(play_type == "no_play" & pass == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((pass)|(sack)|(scramble)))"),
                                passer_player_name),
    receiver_player_name = ifelse(play_type == "no_play" & str_detect(desc, "pass"), 
                                  str_extract(desc, 
                                              "(?<=to\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"),
                                  receiver_player_name),
    rusher_player_name = ifelse(play_type == "no_play" & rush == 1, 
                                str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((left end)|(left tackle)|(left guard)|		(up the middle)|(right guard)|(right tackle)|(right end)))"),
                                rusher_player_name)
  )

# Add columns for name, rusher, and receiver; since we're looking at drop backs, will only get QB names
# Show # of dropbacks, # of rushes, total # of plays, EPA per play, and success rate per play
qbs <- pbp_players %>% 
  mutate(
    name = ifelse(!is_na(passer_player_name), passer_player_name, rusher_player_name),
    rusher = rusher_player_name,
    receiver = receiver_player_name,
    play = 1
  ) %>%
  group_by(name, posteam) %>%
  summarize (
    n_dropbacks = sum(pass),
    n_rush = sum(rush),
    n_plays = sum(play),
    epa_per_play = sum(epa)/n_plays,
    success_per_play =sum(success)/n_plays
  ) %>%
  filter(n_dropbacks>=100)

# Create Plot with X as success rate per play and Y as EPA per play, mark points with QB Names
# Create x and y intercept for averages; Highlight Russ
# alphas makes dots transparent, cex makes them proportional to volume

qbs %>%
  ggplot(aes(x = success_per_play, y = epa_per_play)) +
  geom_hline(yintercept = mean(qbs$epa_per_play), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(qbs$success_per_play), color = "red", linetype = "dashed") +
  geom_point(color = ifelse(qbs$posteam == "SEA", "green", ifelse(qbs$posteam == "NE", "red", "black")), cex=qbs$n_plays/60, alpha=1/4) +
  geom_text_repel(aes(label=name),
                  force=1, point.padding=0,
                  segment.size=0.1) +
  labs(x = "Success rate",
       y = "EPA per play",
       caption = "Data from nflscrapR",
       title = "QB success rate and EPA/play",
       subtitle = "2019, min 100 pass attempts, includes all QB's rush and pass plays") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12))

ggsave('QBeffectiveness.png', dpi=1000)

# Filter on passing plays, add arod, early, mean_epa, and ypp columns
# early = 1 means 2009-2014, early -0 means 2015-2018; arod = 1 means Rodgers stats, arod = 0 means all other qb's
# data shows Rodgers advantage over others qb's drops significantly in second period (and is actually worse in yards per play)
pbp_all_rp %>% filter(pass==1 & !is.na(passer_player_name))%>% 
  mutate(
    arod = if_else(posteam=="GB"&passer_player_name=="R.Wilson",1,0),
    early = if_else(season<=2014,1,0)) %>%
  group_by(arod,early) %>%
  summarize(mean_epa=mean(epa), ypp=mean(yards_gained, na.rm = TRUE)) %>% 
  arrange(-early)
