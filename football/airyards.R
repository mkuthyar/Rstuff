### Load Libraries ###
library(jsonlite)
library(tidyverse)
library(dplyr)
library(ggimage)
library(ggplot2)

## Load Data ##
df_air <- fromJSON('http://airyards.com/2019/weeks')
df_snapcounts <- read.csv("https://raw.githubusercontent.com/mkuthyar/Rstuff/master/football/WRSnapCounts.csv")
df_fantasy <- read.csv("https://raw.githubusercontent.com/mkuthyar/Rstuff/master/football/WRfantasy.csv")

## Compile total by adding weekly numbers and performing calculations ##
air_totals <- df_air %>%
  group_by(player_id) %>%
  mutate(games_played = n(), 
  targets = sum(tar),
  receptions = sum(rec),
  receiving_yards = sum(rec_yards),
  totalyac = sum(yac),
  totalairyards = sum(air_yards),
  team_attempts = sum(tm_att),
  team_airyards = sum(team_air),
  receivingtds = sum((td - rush_td )),
  tds = sum(td),
  rushing_tds = sum(rush_td),
  rushing_yards = sum(rush_yards), 
  avgadot = (totalairyards/targets), 
  avgracr = (receiving_yards/totalairyards),
  potay = (totalairyards/team_airyards), #percentage of team air yards
  target_percentage = (targets/team_attempts),
  avgwopr = ((1.5 * target_percentage) + (.7 * potay))
  )

## Merge in snap data ##
air_totals <- merge(df_snapcounts,air_totals, all=TRUE)

## Merge in fantasy data ##
air_totals <- merge(df_fantasy,air_totals, all=TRUE)

## Filter by position, snap count and targets, and remove weekly data##
air_totals_filtered <- air_totals %>%
  filter(position == "WR", totalsnaps > 30, targets >15) %>%
  select (-week,-index,-tar,-td,-rush_td,-rec,-rec_yards,-rush_yards,-yac,-air_yards,-tm_att,-team_air,-aypt,-racr,-ms_air_yards,-target_share,-wopr) %>%
  distinct()

## Compare fantasy PPG to WOPR ##
fantasypointsvWOPR  <- air_totals_filtered%>% 
  filter(receiving_yards >300)%>%
  ggplot(aes(x = avgwopr, y = fantasypoints_pg )) +
  geom_point() +
  geom_text(aes(label=full_name),check_overlap = TRUE, size=3, hjust=.5, vjust=-.75) +
  labs(x = "WOPR",
       y = "fantasypoints",
       caption = "Data from airyards",
       title = "fantasypoints and WOPR",
       subtitle = "2019") +
  theme_bw() +
  #geom_hline(yintercept = 0.5, color = "green", linetype = "dashed") +
  #geom_vline(xintercept = 10, color = "blue", linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE) +
  annotate(x=.3, y=17, 
           label=paste("R = ", round(cor(air_totals_filtered$avgwopr, air_totals_filtered$fantasypoints_pg,use = "complete.obs"),2)), 
           geom="text", size=5)

fantasypointsvWOPR

## Compare RACR to WOPR
RACRvWOPR  <- air_totals_filtered%>% 
  filter(receiving_yards >300)%>%
  ggplot(aes(x = avgracr, y = avgwopr )) +
  geom_point() +
  geom_text(aes(label=full_name),check_overlap = TRUE, size=3, hjust=.5, vjust=-.75) +
  labs(x = "RACR",
       y = "WOPR",
       caption = "Data from airyards",
       title = "RACR and WOPR",
       subtitle = "2019") +
  theme_bw() +
  geom_hline(yintercept = 0.5, color = "green", linetype = "dashed")

RACRvWOPR

## Compare Air Yard and Target market share
potayvtarget_percentage  <- air_totals_filtered%>% 
  filter(receiving_yards >300)%>%
  ggplot(aes(x = target_percentage, y = potay )) +
  geom_point() +
  geom_text(aes(label=full_name),check_overlap = TRUE, size=3, hjust=.5, vjust=-.75) +
  labs(x = "target_percentage",
       y = "potay",
       caption = "Data from airyards",
       title = "target_percentage and potay",
       subtitle = "2019") +
  theme_bw() +
  geom_smooth(method = "lm", se = FALSE)
  #geom_hline(yintercept = 0.5, color = "green", linetype = "dashed")

potayvtarget_percentage

cor(air_totals_filtered$target_percentage,air_totals_filtered$potay)
  
## Compile per game stats ##
air_pergame <- air_totals_filtered %>%
  group_by(player_id) %>%
  mutate(targets_pg = (targets/games_played),
         receptions_pg = (receptions/games_played),
         receiving_yards_pg = (receiving_yards/games_played),
         airyards_pg = (totalairyards/games_played),
         yac_pg = (totalyac/games_played),
         team_attempts_pg = (team_attempts/games_played),
         team_airyards_pg = (team_airyards/games_played),
         receivingtds_pg = (receivingtds/games_played),
         receptions_pg = (receptions/games_played),
         receiving_yards_pg = (receiving_yards/games_played)
  ) %>%
  select (-targets,-receptions,-receiving_yards,-totalyac,-totalairyards,-team_attempts,-team_airyards,-receivingtds,-tds,-rushing_tds,-rushing_yards )

  