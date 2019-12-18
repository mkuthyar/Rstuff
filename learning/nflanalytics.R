# Load necessary libraries
library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)
library(ggrepel)

# Read in data from Github into DF
pbp <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2018.csv"))

# Display head of data, selecting 4 columns
pbp %>% select(posteam, defteam, desc, play_type) %>% head

# Filter on plays that have an EPA value, and are pass, run, or penalties ("no play"); Removes kickoffs
pbp_rp <- pbp %>% 
  filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")

pbp_rp %>% select(posteam, desc, play_type) %>% head

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

# Show how penalties now count as run or pass plays
pbp_rp %>% filter(play_type=="no_play") %>% select(pass, rush, desc)  %>% head

# Keep only run or pass plays (including penalties)
pbp_rp <- pbp_rp %>% filter(pass==1 | rush==1)

## BASIC STUFF PART 1 ##

# Filter on when LA has posession, play is a run, exclude 2PC, group by rusher name
# Display average EPA, average Success Rate, and ypc, along with # of plays
# Order in average EPA, greatest to lowest, and filter on over 40 plays
pbp_rp %>%
  filter(posteam == "LA", rush == 1, down<=4) %>%
  group_by(rusher_player_name) %>%
  summarize(mean_epa = mean(epa), success_rate = mean(success), ypc=mean(yards_gained), plays=n()) %>%
  arrange(desc(mean_epa)) %>%
  filter(plays>40)

## BASIC STUFF PART 2 ##

# Group by team on offense, average time team passed the ball, show  how many total plays
# Filter on win probability between 20% and 80%, first or second down, outside of the 2 min drill

schotty <- pbp_rp %>%
  filter(wp>.20 & wp<.80 & down<=2 & qtr<=2 & half_seconds_remaining>120) %>%
  group_by(posteam) %>%
  summarize(mean_pass=mean(pass), plays=n()) %>%
  arrange(mean_pass)

schotty

# Create plot with X as teams, Y as pass frequency, order from highest to lowest pass frequency
ggplot(schotty, aes(x=reorder(posteam,-mean_pass), y=mean_pass)) +
  geom_text(aes(label=posteam)) + 
  labs(title="Early Down Pass Frequency by Team", subtitle= "From nflscrapR", x="team", y="pass frequency")

ggsave('passFreq.png', dpi=1000)

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
       subtitle = "2018") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

ggsave('teamPassingEffictiveness.png', dpi=1000)

## BASIC STUFF PART 4 ##

# Filter on first and second down
# show total pass plays, total run plays, and EPA and Success rate per dropback and per Rush
chart_data <- pbp_rp %>%
  group_by(posteam) %>%
  filter(down<=2) %>%
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
       title = "Early-down rush and pass EPA/play",
       subtitle = "2018") +
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
       subtitle = "2018") +
  theme_bw() +
  geom_abline(slope=1, intercept=0, alpha=.2) +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14),
        plot.caption = element_text(size = 12))

ggsave('earlydownsuccessratepassvrush.png', dpi=1000)

## ADVANCED STUFF PART 1 ##

# Show that player names are NA for penalties, something we will fix
pbp_rp %>% filter(play_type=="no_play") %>% 
  select(desc, pass, passer_player_name, rusher_player_name, receiver_player_name) %>% head()

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

#Show that player names are now present
pbp_players %>% filter(play_type=="no_play") %>% 
  select(desc, pass, passer_player_name, rusher_player_name, receiver_player_name) %>% head()

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
# Create x and y intercept for averages; Highlight 49ers QB's
# alphas makes dots transparent, cex makes them proportional to volume

qbs %>%
  ggplot(aes(x = success_per_play, y = epa_per_play)) +
  geom_hline(yintercept = mean(qbs$epa_per_play), color = "red", linetype = "dashed") +
  geom_vline(xintercept =  mean(qbs$success_per_play), color = "red", linetype = "dashed") +
  geom_point(color = ifelse(qbs$posteam == "SEA", "green", "black"), cex=qbs$n_plays/60, alpha=1/4) +
  geom_text_repel(aes(label=name),
                  force=1, point.padding=0,
                  segment.size=0.1) +
  labs(x = "Success rate",
       y = "EPA per play",
       caption = "Data from nflscrapR",
       title = "QB success rate and EPA/play",
       subtitle = "2018, min 100 pass attempts, includes all QB's rush and pass plays") +
  theme_bw() +
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        plot.title = element_text(size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12))

ggsave('QBeffectiveness.png', dpi=1000)


